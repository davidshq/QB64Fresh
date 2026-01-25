//! Source code preprocessing for QB64Fresh.
//!
//! This module handles preprocessing of BASIC source code before lexing/parsing.
//! Currently it supports:
//!
//! - `$INCLUDE: 'filename'` - File inclusion (recursive)
//! - Line continuation with ` _` (space underscore at end of line)
//!
//! # Design
//!
//! The preprocessor operates on raw source text before lexing. It scans for
//! `$INCLUDE` directives and replaces them with the contents of the included file.
//! This approach:
//!
//! - Preserves line numbers in error messages (with include stack tracking)
//! - Supports recursive includes with cycle detection
//! - Is simple and matches traditional BASIC preprocessor behavior
//!
//! # Example
//!
//! ```no_run
//! use std::path::Path;
//! use qb64fresh::preprocessor::preprocess;
//!
//! let base_path = Path::new(".");
//! let source = r#"
//! PRINT "Before include"
//! $INCLUDE: 'utils.bas'
//! PRINT "After include"
//! "#;
//!
//! match preprocess(source, base_path, None) {
//!     Ok(processed) => println!("Processed source:\n{}", processed),
//!     Err(e) => eprintln!("Preprocessor error: {}", e),
//! }
//! ```

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

/// Preprocessor error types.
#[derive(Debug, Clone)]
pub enum PreprocessorError {
    /// Include file not found.
    FileNotFound {
        /// The path that was requested.
        path: String,
        /// The file that requested the include.
        from_file: PathBuf,
    },
    /// Error reading an include file.
    ReadError {
        /// The path that failed.
        path: PathBuf,
        /// The underlying error message.
        message: String,
    },
    /// Circular include detected.
    CircularInclude {
        /// The path that was included recursively.
        path: PathBuf,
        /// The include stack showing the cycle.
        include_stack: Vec<PathBuf>,
    },
    /// Maximum include depth exceeded.
    MaxDepthExceeded {
        /// The maximum depth allowed.
        max_depth: usize,
        /// The path that would exceed the limit.
        path: PathBuf,
    },
}

impl std::fmt::Display for PreprocessorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PreprocessorError::FileNotFound { path, from_file } => {
                write!(
                    f,
                    "Include file not found: '{}' (included from {})",
                    path,
                    from_file.display()
                )
            }
            PreprocessorError::ReadError { path, message } => {
                write!(f, "Error reading '{}': {}", path.display(), message)
            }
            PreprocessorError::CircularInclude {
                path,
                include_stack,
            } => {
                writeln!(f, "Circular include detected: '{}'", path.display())?;
                writeln!(f, "Include stack:")?;
                for (i, p) in include_stack.iter().enumerate() {
                    writeln!(f, "  {}. {}", i + 1, p.display())?;
                }
                Ok(())
            }
            PreprocessorError::MaxDepthExceeded { max_depth, path } => {
                write!(
                    f,
                    "Maximum include depth ({}) exceeded when including '{}'",
                    max_depth,
                    path.display()
                )
            }
        }
    }
}

impl std::error::Error for PreprocessorError {}

/// Maximum nesting depth for includes (to prevent stack overflow).
const MAX_INCLUDE_DEPTH: usize = 64;

/// Context for preprocessing, tracking the include stack and visited files.
struct PreprocessContext {
    /// Stack of currently active includes (for cycle detection and error messages).
    include_stack: Vec<PathBuf>,
    /// Set of canonical paths already visited (for cycle detection).
    visited: HashSet<PathBuf>,
    /// Current nesting depth.
    depth: usize,
}

impl PreprocessContext {
    fn new(initial_file: PathBuf) -> Self {
        let canonical = initial_file.canonicalize().unwrap_or(initial_file.clone());
        let mut visited = HashSet::new();
        visited.insert(canonical.clone());

        Self {
            include_stack: vec![canonical],
            visited,
            depth: 0,
        }
    }

    fn enter_include(&mut self, path: PathBuf) -> Result<(), PreprocessorError> {
        let canonical = path.canonicalize().unwrap_or(path.clone());

        // Check for circular includes
        if self.visited.contains(&canonical) {
            return Err(PreprocessorError::CircularInclude {
                path: canonical,
                include_stack: self.include_stack.clone(),
            });
        }

        // Check depth limit
        if self.depth >= MAX_INCLUDE_DEPTH {
            return Err(PreprocessorError::MaxDepthExceeded {
                max_depth: MAX_INCLUDE_DEPTH,
                path: canonical,
            });
        }

        self.visited.insert(canonical.clone());
        self.include_stack.push(canonical);
        self.depth += 1;
        Ok(())
    }

    fn exit_include(&mut self) {
        self.include_stack.pop();
        self.depth -= 1;
    }

    fn current_file(&self) -> &Path {
        self.include_stack
            .last()
            .map(|p| p.as_path())
            .unwrap_or(Path::new("."))
    }
}

/// Joins lines that end with ` _` (space underscore) continuation character.
///
/// In QB64/BASIC, a line ending with ` _` indicates that the statement continues
/// on the next line. This function joins such lines into single logical lines
/// before further processing.
///
/// **Important**: Line continuation is NOT active inside comments or strings.
/// A `_` inside a comment (after `'` or `REM`) does not trigger continuation.
///
/// # Example
///
/// ```text
/// IF condition OR _
///    condition2 THEN
/// ```
///
/// Becomes:
///
/// ```text
/// IF condition OR    condition2 THEN
/// ```
fn join_continued_lines(source: &str) -> String {
    let mut result = String::with_capacity(source.len());
    let mut continuation_buffer = String::new();
    let mut in_continuation = false;

    for line in source.lines() {
        // Check if line ends with ` _` (space underscore) for continuation
        // BUT NOT if that `_` is inside a comment
        let is_continuation = is_line_continuation(line);

        if in_continuation {
            // Append to continuation buffer (without the previous ` _`)
            continuation_buffer.push_str(line.trim_start());

            if is_continuation {
                // Remove the trailing ` _` and continue accumulating
                let trimmed = continuation_buffer.trim_end();
                let without_underscore = trimmed.strip_suffix('_').unwrap_or(trimmed);
                continuation_buffer = without_underscore.to_string();
                continuation_buffer.push(' '); // Replace with space
            } else {
                // End of continuation - output the joined line
                result.push_str(&continuation_buffer);
                result.push('\n');
                continuation_buffer.clear();
                in_continuation = false;
            }
        } else if is_continuation {
            // Start of a continuation
            let trimmed = line.trim_end();
            let without_underscore = trimmed.strip_suffix('_').unwrap_or(trimmed);
            continuation_buffer = without_underscore.to_string();
            continuation_buffer.push(' '); // Replace ` _` with space
            in_continuation = true;
        } else {
            // Normal line - output as-is
            result.push_str(line);
            result.push('\n');
        }
    }

    // Handle any remaining continuation at end of file
    if !continuation_buffer.is_empty() {
        result.push_str(&continuation_buffer);
        result.push('\n');
    }

    result
}

/// Checks if a line ends with a valid line continuation (` _` not in a comment).
///
/// A line continuation is valid when:
/// 1. The line ends with ` _` (space/tab followed by underscore)
/// 2. That underscore is NOT inside a comment (after `'`)
/// 3. That underscore is NOT inside a string literal
fn is_line_continuation(line: &str) -> bool {
    let trimmed = line.trim_end();

    // First check: does it even end with ` _`?
    let ends_with_continuation = trimmed
        .strip_suffix('_')
        .map(|s| s.ends_with(' ') || s.ends_with('\t'))
        .unwrap_or(false);

    if !ends_with_continuation {
        return false;
    }

    // Find the position of the trailing `_`
    let underscore_pos = trimmed.len() - 1;

    // Now check if that position is inside a comment or string
    let mut in_string = false;
    let chars: Vec<char> = trimmed.chars().collect();

    for (i, &c) in chars.iter().enumerate() {
        if c == '"' {
            in_string = !in_string;
        } else if !in_string && c == '\'' {
            // Found a comment marker - everything after is a comment
            // If the underscore is after this point, it's not a continuation
            if underscore_pos > i {
                return false;
            }
            break;
        }
    }

    // If we're still in a string at the underscore position, it's not a continuation
    !in_string
}

/// Preprocesses BASIC source code, expanding `$INCLUDE` directives.
///
/// This function recursively processes include directives, replacing them with
/// the contents of the included files.
///
/// # Arguments
///
/// * `source` - The source code to preprocess
/// * `base_path` - The directory containing the source file (for resolving relative paths)
/// * `source_path` - When provided, the actual path of the source file. Used for
///   `PreprocessContext` and for `FileNotFound.from_file` in error messages so
///   users see the real file (e.g. `myapp.bas`) instead of `main.bas`. Pass `None`
///   for backward compatibility; then `base_path.join("main.bas")` is used.
///
/// # Returns
///
/// The preprocessed source with all includes expanded, or an error if processing fails.
///
/// # Example
///
/// ```no_run
/// use std::path::Path;
/// use qb64fresh::preprocessor::preprocess;
///
/// // With known source file (e.g. from CLI):
/// let result = preprocess("$INCLUDE: 'header.bi'", Path::new("."), Some(Path::new("myapp.bas")));
///
/// // Without source path (backward compatible):
/// let result = preprocess("$INCLUDE: 'header.bi'", Path::new("."), None);
/// ```
pub fn preprocess(
    source: &str,
    base_path: &Path,
    source_path: Option<&Path>,
) -> Result<String, PreprocessorError> {
    // First, join continued lines (lines ending with ` _`)
    let joined = join_continued_lines(source);

    let initial_file = source_path
        .map(|p| p.canonicalize().unwrap_or_else(|_| p.to_path_buf()))
        .unwrap_or_else(|| base_path.join("main.bas"));
    let mut context = PreprocessContext::new(initial_file);
    preprocess_internal(&joined, base_path, &mut context)
}

/// Preprocesses a file by path.
///
/// This is a convenience function that reads the file and preprocesses it.
///
/// # Arguments
///
/// * `path` - Path to the BASIC source file
///
/// # Returns
///
/// The preprocessed source with all includes expanded.
pub fn preprocess_file(path: &Path) -> Result<String, PreprocessorError> {
    // Read file as bytes and convert with lossy UTF-8 to handle legacy encodings
    // (some QB64pe source files contain Windows-1252 or Code Page 437 characters)
    let bytes = fs::read(path).map_err(|e| PreprocessorError::ReadError {
        path: path.to_path_buf(),
        message: e.to_string(),
    })?;
    let source = String::from_utf8_lossy(&bytes).into_owned();

    // Join continued lines (lines ending with ` _`)
    let joined = join_continued_lines(&source);

    let base_path = path.parent().unwrap_or(Path::new("."));
    let canonical = path.canonicalize().unwrap_or(path.to_path_buf());
    let mut context = PreprocessContext::new(canonical);

    preprocess_internal(&joined, base_path, &mut context)
}

/// Internal preprocessing function with context tracking.
fn preprocess_internal(
    source: &str,
    base_path: &Path,
    context: &mut PreprocessContext,
) -> Result<String, PreprocessorError> {
    let mut result = String::with_capacity(source.len());

    for line in source.lines() {
        // Check for $INCLUDE directive
        if let Some(include_path) = parse_include_directive(line) {
            // Resolve the path relative to the current file's directory
            let include_full_path = if Path::new(&include_path).is_absolute() {
                PathBuf::from(&include_path)
            } else {
                base_path.join(&include_path)
            };

            // Check if file exists
            if !include_full_path.exists() {
                return Err(PreprocessorError::FileNotFound {
                    path: include_path,
                    from_file: context.current_file().to_path_buf(),
                });
            }

            // Enter the include (checks for cycles and depth)
            context.enter_include(include_full_path.clone())?;

            // Read the included file (using lossy UTF-8 for legacy encoding support)
            let include_bytes =
                fs::read(&include_full_path).map_err(|e| PreprocessorError::ReadError {
                    path: include_full_path.clone(),
                    message: e.to_string(),
                })?;
            let include_source = String::from_utf8_lossy(&include_bytes).into_owned();

            // Join continued lines in the included file
            let include_source = join_continued_lines(&include_source);

            // Get the include file's directory for nested includes
            let include_base = include_full_path.parent().unwrap_or(Path::new("."));

            // Add a comment marking the start of the include
            result.push_str(&format!("' >>> $INCLUDE: '{}'\n", include_path));

            // Recursively process the included content
            let processed = preprocess_internal(&include_source, include_base, context)?;
            result.push_str(&processed);

            // Ensure there's a newline after the included content
            if !processed.ends_with('\n') {
                result.push('\n');
            }

            // Add a comment marking the end of the include
            result.push_str(&format!("' <<< END $INCLUDE: '{}'\n", include_path));

            // Exit the include
            context.exit_include();
        } else {
            result.push_str(line);
            result.push('\n');
        }
    }

    Ok(result)
}

/// Parses a line to extract an include path if it's an include directive.
///
/// Handles various formats:
/// - `$INCLUDE: 'path'`
/// - `$INCLUDE:'path'`
/// - `'$INCLUDE: 'path'` (comment prefix, still valid)
///
/// Also normalizes Windows-style backslashes to forward slashes for cross-platform
/// compatibility. This allows QB64pe source (which uses `global\version.bas`) to
/// work on Linux/macOS.
///
/// Returns `None` if the line is not an include directive.
fn parse_include_directive(line: &str) -> Option<String> {
    let trimmed = line.trim();

    // Handle comment prefix (some BASIC dialects allow ' before $INCLUDE)
    let content = trimmed
        .strip_prefix('\'')
        .map(|s| s.trim())
        .unwrap_or(trimmed);

    // Check for $INCLUDE (case-insensitive)
    let upper = content.to_uppercase();
    if !upper.starts_with("$INCLUDE") {
        return None;
    }

    // Find the path within quotes
    // Format: $INCLUDE: 'path' or $INCLUDE:'path'
    let rest = &content[8..]; // Skip "$INCLUDE"
    let rest = rest.trim_start();

    // Skip optional colon
    let rest = rest.strip_prefix(':').unwrap_or(rest).trim_start();

    // Extract path from quotes (single quotes in BASIC)
    if let Some(start) = rest.find('\'') {
        let after_quote = &rest[start + 1..];
        if let Some(end) = after_quote.find('\'') {
            let path = &after_quote[..end];
            if !path.is_empty() {
                // Normalize Windows backslashes to forward slashes for cross-platform support
                let normalized_path = path.replace('\\', "/");
                return Some(normalized_path);
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::TempDir;

    #[test]
    fn test_parse_include_directive() {
        // Standard format
        assert_eq!(
            parse_include_directive("$INCLUDE: 'myfile.bi'"),
            Some("myfile.bi".to_string())
        );

        // No space after colon
        assert_eq!(
            parse_include_directive("$INCLUDE:'utils.bas'"),
            Some("utils.bas".to_string())
        );

        // Case insensitive
        assert_eq!(
            parse_include_directive("$include: 'test.bas'"),
            Some("test.bas".to_string())
        );

        // With leading whitespace
        assert_eq!(
            parse_include_directive("    $INCLUDE: 'header.bi'"),
            Some("header.bi".to_string())
        );

        // Not an include directive
        assert_eq!(parse_include_directive("PRINT \"Hello\""), None);
        assert_eq!(parse_include_directive("' Just a comment"), None);

        // Path with directory
        assert_eq!(
            parse_include_directive("$INCLUDE: 'inc/common.bi'"),
            Some("inc/common.bi".to_string())
        );

        // Windows-style backslashes should be normalized to forward slashes
        assert_eq!(
            parse_include_directive("'$INCLUDE:'global\\version.bas'"),
            Some("global/version.bas".to_string())
        );
        assert_eq!(
            parse_include_directive(
                "$INCLUDE: 'subs_functions\\extensions\\opengl\\opengl_global.bas'"
            ),
            Some("subs_functions/extensions/opengl/opengl_global.bas".to_string())
        );
    }

    #[test]
    fn test_preprocess_no_includes() {
        let source = "PRINT \"Hello\"\nx = 42\n";
        let result = preprocess(source, Path::new("."), None).unwrap();
        assert_eq!(result, "PRINT \"Hello\"\nx = 42\n");
    }

    #[test]
    fn test_preprocess_with_include() {
        // Create temp directory and files
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create the include file
        let include_path = temp_path.join("header.bi");
        let mut include_file = fs::File::create(&include_path).unwrap();
        writeln!(include_file, "CONST VERSION = 1").unwrap();

        // Main source with include
        let source = "PRINT \"Start\"\n$INCLUDE: 'header.bi'\nPRINT \"End\"\n";
        let result = preprocess(source, temp_path, None).unwrap();

        // Check that the include was expanded
        assert!(result.contains("CONST VERSION = 1"));
        assert!(result.contains("PRINT \"Start\""));
        assert!(result.contains("PRINT \"End\""));
        assert!(result.contains(">>> $INCLUDE: 'header.bi'"));
        assert!(result.contains("<<< END $INCLUDE: 'header.bi'"));
    }

    #[test]
    fn test_preprocess_nested_includes() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create level2.bi
        let level2_path = temp_path.join("level2.bi");
        let mut level2_file = fs::File::create(&level2_path).unwrap();
        writeln!(level2_file, "CONST LEVEL2 = 2").unwrap();

        // Create level1.bi that includes level2.bi
        let level1_path = temp_path.join("level1.bi");
        let mut level1_file = fs::File::create(&level1_path).unwrap();
        writeln!(level1_file, "CONST LEVEL1 = 1").unwrap();
        writeln!(level1_file, "$INCLUDE: 'level2.bi'").unwrap();

        // Main source
        let source = "$INCLUDE: 'level1.bi'\nPRINT LEVEL1 + LEVEL2\n";
        let result = preprocess(source, temp_path, None).unwrap();

        assert!(result.contains("CONST LEVEL1 = 1"));
        assert!(result.contains("CONST LEVEL2 = 2"));
        assert!(result.contains("PRINT LEVEL1 + LEVEL2"));
    }

    #[test]
    fn test_preprocess_circular_include_detection() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create file_a.bi that includes file_b.bi
        let file_a_path = temp_path.join("file_a.bi");
        let mut file_a = fs::File::create(&file_a_path).unwrap();
        writeln!(file_a, "$INCLUDE: 'file_b.bi'").unwrap();

        // Create file_b.bi that includes file_a.bi (circular!)
        let file_b_path = temp_path.join("file_b.bi");
        let mut file_b = fs::File::create(&file_b_path).unwrap();
        writeln!(file_b, "$INCLUDE: 'file_a.bi'").unwrap();

        // Try to process - should detect circular include
        let source = "$INCLUDE: 'file_a.bi'\n";
        let result = preprocess(source, temp_path, None);

        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            PreprocessorError::CircularInclude { .. }
        ));
    }

    #[test]
    fn test_preprocess_file_not_found() {
        let result = preprocess("$INCLUDE: 'nonexistent.bi'\n", Path::new("."), None);
        assert!(result.is_err());
        assert!(matches!(
            result.unwrap_err(),
            PreprocessorError::FileNotFound { .. }
        ));
    }

    #[test]
    fn test_preprocess_file_not_found_reports_actual_source_path() {
        // When source_path is Some(real_path), FileNotFound.from_file should be that path,
        // not "main.bas". (Regression test for preprocess() initial file fix.)
        let temp_dir = TempDir::new().unwrap();
        let entry_path = temp_dir.path().join("myapp.bas");
        std::fs::write(&entry_path, "$INCLUDE: 'nonexistent.bi'\n").unwrap();

        let base = entry_path.parent().unwrap();
        let result = preprocess(
            "$INCLUDE: 'nonexistent.bi'\n",
            base,
            Some(entry_path.as_path()),
        );

        let err = result.unwrap_err();
        match &err {
            PreprocessorError::FileNotFound { from_file, .. } => {
                assert!(
                    from_file.ends_with("myapp.bas"),
                    "from_file should be the actual source path, got: {}",
                    from_file.display()
                );
            }
            _ => panic!("expected FileNotFound, got {:?}", err),
        }
    }
}
