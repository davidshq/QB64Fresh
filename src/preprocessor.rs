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

use crate::library::LibraryManager;

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
    /// Set of canonical paths included with $INCLUDEONCE (should not be included again).
    include_once_visited: HashSet<PathBuf>,
    /// Current nesting depth.
    depth: usize,
    /// Library manager for $USELIBRARY directive support.
    library_manager: LibraryManager,
    /// Current line number (for referrer tracking).
    line_number: usize,
}

impl PreprocessContext {
    fn new(initial_file: PathBuf, base_path: &Path) -> Self {
        let canonical = initial_file.canonicalize().unwrap_or(initial_file.clone());
        let mut visited = HashSet::new();
        visited.insert(canonical.clone());

        Self {
            include_stack: vec![canonical],
            visited,
            include_once_visited: HashSet::new(),
            depth: 0,
            library_manager: LibraryManager::new(base_path),
            line_number: 1,
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

    /// Creates a referrer string for library tracking (file:line).
    fn referrer(&self) -> String {
        let file = self.current_file();
        let file_name = file
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown");
        format!("{}:{}", file_name, self.line_number)
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
    let mut context = PreprocessContext::new(initial_file, base_path);
    let result = preprocess_internal(&joined, base_path, &mut context)?;

    // Collect library file paths (need to collect before borrowing context)
    let at_top_paths: Vec<(String, PathBuf)> = context
        .library_manager
        .get_at_top_libraries()
        .iter()
        .filter_map(|lib| {
            lib.inc_at_top
                .as_ref()
                .map(|path| (lib.name.clone(), path.clone()))
        })
        .collect();

    let after_main_paths: Vec<(String, PathBuf)> = context
        .library_manager
        .get_after_main_libraries()
        .iter()
        .filter_map(|lib| {
            lib.inc_after_main
                .as_ref()
                .map(|path| (lib.name.clone(), path.clone()))
        })
        .collect();

    let at_bottom_paths: Vec<(String, PathBuf)> = context
        .library_manager
        .get_at_bottom_libraries()
        .iter()
        .filter_map(|lib| {
            lib.inc_at_bottom
                .as_ref()
                .map(|path| (lib.name.clone(), path.clone()))
        })
        .collect();

    // Build final output with library inclusions
    let mut final_output = String::new();

    // Include AtTop files at the very beginning
    // These files are preprocessed to handle $INCLUDE and other directives
    for (lib_name, path) in at_top_paths {
        final_output.push_str(&format!("' >>> $USELIBRARY:'{}' (AtTop)\n", lib_name));
        let content = fs::read_to_string(&path).map_err(|e| PreprocessorError::ReadError {
            path: path.clone(),
            message: e.to_string(),
        })?;
        // Preprocess the library file to handle $INCLUDE directives
        // Use a new context for library files to avoid include stack conflicts
        let lib_base = path.parent().unwrap_or(base_path);
        let lib_canonical = path.canonicalize().unwrap_or(path.clone());
        let mut lib_context = PreprocessContext::new(lib_canonical, lib_base);
        let processed = preprocess_internal(&content, lib_base, &mut lib_context)?;
        final_output.push_str(&processed);
        if !processed.ends_with('\n') {
            final_output.push('\n');
        }
        final_output.push_str(&format!("' <<< END $USELIBRARY:'{}' (AtTop)\n", lib_name));
    }

    // Add the main preprocessed content
    final_output.push_str(&result);

    // Include AfterMain files after main code
    // These files are preprocessed to handle $INCLUDE and other directives
    for (lib_name, path) in after_main_paths {
        final_output.push_str(&format!("' >>> $USELIBRARY:'{}' (AfterMain)\n", lib_name));
        let content = fs::read_to_string(&path).map_err(|e| PreprocessorError::ReadError {
            path: path.clone(),
            message: e.to_string(),
        })?;
        // Preprocess the library file to handle $INCLUDE directives
        // Use a new context for library files to avoid include stack conflicts
        let lib_base = path.parent().unwrap_or(base_path);
        let lib_canonical = path.canonicalize().unwrap_or(path.clone());
        let mut lib_context = PreprocessContext::new(lib_canonical, lib_base);
        let processed = preprocess_internal(&content, lib_base, &mut lib_context)?;
        final_output.push_str(&processed);
        if !processed.ends_with('\n') {
            final_output.push('\n');
        }
        final_output.push_str(&format!(
            "' <<< END $USELIBRARY:'{}' (AfterMain)\n",
            lib_name
        ));
    }

    // Include AtBottom files at the very end
    // These files are preprocessed to handle $INCLUDE and other directives
    for (lib_name, path) in at_bottom_paths {
        final_output.push_str(&format!("' >>> $USELIBRARY:'{}' (AtBottom)\n", lib_name));
        let content = fs::read_to_string(&path).map_err(|e| PreprocessorError::ReadError {
            path: path.clone(),
            message: e.to_string(),
        })?;
        // Preprocess the library file to handle $INCLUDE directives
        // Use a new context for library files to avoid include stack conflicts
        let lib_base = path.parent().unwrap_or(base_path);
        let lib_canonical = path.canonicalize().unwrap_or(path.clone());
        let mut lib_context = PreprocessContext::new(lib_canonical, lib_base);
        let processed = preprocess_internal(&content, lib_base, &mut lib_context)?;
        final_output.push_str(&processed);
        if !processed.ends_with('\n') {
            final_output.push('\n');
        }
        final_output.push_str(&format!(
            "' <<< END $USELIBRARY:'{}' (AtBottom)\n",
            lib_name
        ));
    }

    Ok(final_output)
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
    let mut context = PreprocessContext::new(canonical, base_path);
    let result = preprocess_internal(&joined, base_path, &mut context)?;

    // Collect library file paths (need to collect before borrowing context)
    let at_top_paths: Vec<(String, PathBuf)> = context
        .library_manager
        .get_at_top_libraries()
        .iter()
        .filter_map(|lib| {
            lib.inc_at_top
                .as_ref()
                .map(|path| (lib.name.clone(), path.clone()))
        })
        .collect();

    let after_main_paths: Vec<(String, PathBuf)> = context
        .library_manager
        .get_after_main_libraries()
        .iter()
        .filter_map(|lib| {
            lib.inc_after_main
                .as_ref()
                .map(|path| (lib.name.clone(), path.clone()))
        })
        .collect();

    let at_bottom_paths: Vec<(String, PathBuf)> = context
        .library_manager
        .get_at_bottom_libraries()
        .iter()
        .filter_map(|lib| {
            lib.inc_at_bottom
                .as_ref()
                .map(|path| (lib.name.clone(), path.clone()))
        })
        .collect();

    // Build final output with library inclusions
    let mut final_output = String::new();

    // Include AtTop files at the very beginning
    // These files are preprocessed to handle $INCLUDE and other directives
    for (lib_name, lib_path) in at_top_paths {
        final_output.push_str(&format!("' >>> $USELIBRARY:'{}' (AtTop)\n", lib_name));
        let content = fs::read_to_string(&lib_path).map_err(|e| PreprocessorError::ReadError {
            path: lib_path.clone(),
            message: e.to_string(),
        })?;
        // Preprocess the library file to handle $INCLUDE directives
        // Use a new context for library files to avoid include stack conflicts
        let lib_base = lib_path.parent().unwrap_or(base_path);
        let lib_canonical = lib_path.canonicalize().unwrap_or(lib_path.clone());
        let mut lib_context = PreprocessContext::new(lib_canonical, lib_base);
        let processed = preprocess_internal(&content, lib_base, &mut lib_context)?;
        final_output.push_str(&processed);
        if !processed.ends_with('\n') {
            final_output.push('\n');
        }
        final_output.push_str(&format!("' <<< END $USELIBRARY:'{}' (AtTop)\n", lib_name));
    }

    // Add the main preprocessed content
    final_output.push_str(&result);

    // Include AfterMain files after main code
    // These files are preprocessed to handle $INCLUDE and other directives
    for (lib_name, lib_path) in after_main_paths {
        final_output.push_str(&format!("' >>> $USELIBRARY:'{}' (AfterMain)\n", lib_name));
        let content = fs::read_to_string(&lib_path).map_err(|e| PreprocessorError::ReadError {
            path: lib_path.clone(),
            message: e.to_string(),
        })?;
        // Preprocess the library file to handle $INCLUDE directives
        // Use a new context for library files to avoid include stack conflicts
        let lib_base = lib_path.parent().unwrap_or(base_path);
        let lib_canonical = lib_path.canonicalize().unwrap_or(lib_path.clone());
        let mut lib_context = PreprocessContext::new(lib_canonical, lib_base);
        let processed = preprocess_internal(&content, lib_base, &mut lib_context)?;
        final_output.push_str(&processed);
        if !processed.ends_with('\n') {
            final_output.push('\n');
        }
        final_output.push_str(&format!(
            "' <<< END $USELIBRARY:'{}' (AfterMain)\n",
            lib_name
        ));
    }

    // Include AtBottom files at the very end
    // These files are preprocessed to handle $INCLUDE and other directives
    for (lib_name, lib_path) in at_bottom_paths {
        final_output.push_str(&format!("' >>> $USELIBRARY:'{}' (AtBottom)\n", lib_name));
        let content = fs::read_to_string(&lib_path).map_err(|e| PreprocessorError::ReadError {
            path: lib_path.clone(),
            message: e.to_string(),
        })?;
        // Preprocess the library file to handle $INCLUDE directives
        // Use a new context for library files to avoid include stack conflicts
        let lib_base = lib_path.parent().unwrap_or(base_path);
        let lib_canonical = lib_path.canonicalize().unwrap_or(lib_path.clone());
        let mut lib_context = PreprocessContext::new(lib_canonical, lib_base);
        let processed = preprocess_internal(&content, lib_base, &mut lib_context)?;
        final_output.push_str(&processed);
        if !processed.ends_with('\n') {
            final_output.push('\n');
        }
        final_output.push_str(&format!(
            "' <<< END $USELIBRARY:'{}' (AtBottom)\n",
            lib_name
        ));
    }

    Ok(final_output)
}

/// Internal preprocessing function with context tracking.
fn preprocess_internal(
    source: &str,
    base_path: &Path,
    context: &mut PreprocessContext,
) -> Result<String, PreprocessorError> {
    let mut result = String::with_capacity(source.len());

    for (line_idx, line) in source.lines().enumerate() {
        context.line_number = line_idx + 1;

        // Check for $USELIBRARY directive
        if let Some(library_name) = parse_uselibrary_directive(line) {
            let referrer = context.referrer();
            match context
                .library_manager
                .register_library(&library_name, &referrer)
            {
                Ok(Some(_)) => {
                    // Library registered successfully
                    result.push_str(&format!(
                        "' >>> $USELIBRARY:'{}' (registered from {})\n",
                        library_name, referrer
                    ));
                }
                Ok(None) => {
                    // Library already registered for this referrer (duplicate)
                    result.push_str(&format!(
                        "' >>> $USELIBRARY:'{}' (already registered, skipping)\n",
                        library_name
                    ));
                }
                Err(e) => {
                    // Library loading failed
                    return Err(e);
                }
            }
            continue;
        }

        // Check for $INCLUDE or $INCLUDEONCE directive
        if let Some((include_path, is_once)) = parse_include_directive(line) {
            // Resolve the path relative to the current file's directory
            let include_full_path = if Path::new(&include_path).is_absolute() {
                PathBuf::from(&include_path)
            } else {
                base_path.join(&include_path)
            };

            // Canonicalize for comparison
            let canonical = include_full_path
                .canonicalize()
                .unwrap_or(include_full_path.clone());

            // For $INCLUDEONCE, check if already included
            if is_once && context.include_once_visited.contains(&canonical) {
                // Skip this include - already included once
                result.push_str(&format!(
                    "' >>> $INCLUDEONCE: '{}' (already included, skipping)\n",
                    include_path
                ));
                continue;
            }

            // Check if file exists
            if !include_full_path.exists() {
                return Err(PreprocessorError::FileNotFound {
                    path: include_path,
                    from_file: context.current_file().to_path_buf(),
                });
            }

            // For $INCLUDEONCE, mark as visited (before entering to prevent cycles)
            if is_once {
                context.include_once_visited.insert(canonical.clone());
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
            let directive = if is_once { "$INCLUDEONCE" } else { "$INCLUDE" };
            result.push_str(&format!("' >>> {}: '{}'\n", directive, include_path));

            // Recursively process the included content
            let processed = preprocess_internal(&include_source, include_base, context)?;
            result.push_str(&processed);

            // Ensure there's a newline after the included content
            if !processed.ends_with('\n') {
                result.push('\n');
            }

            // Add a comment marking the end of the include
            let directive = if is_once { "$INCLUDEONCE" } else { "$INCLUDE" };
            result.push_str(&format!("' <<< END {}: '{}'\n", directive, include_path));

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
/// - `$INCLUDEONCE: 'path'`
/// - `$INCLUDEONCE:'path'`
/// - `'$INCLUDE: 'path'` (comment prefix, still valid)
///
/// Also normalizes Windows-style backslashes to forward slashes for cross-platform
/// compatibility. This allows QB64pe source (which uses `global\version.bas`) to
/// work on Linux/macOS.
///
/// Returns `None` if the line is not an include directive, or `Some((path, is_once))`
/// where `is_once` is true for $INCLUDEONCE directives.
fn parse_include_directive(line: &str) -> Option<(String, bool)> {
    let trimmed = line.trim();

    // Handle comment prefix (some BASIC dialects allow ' before $INCLUDE)
    let content = trimmed
        .strip_prefix('\'')
        .map(|s| s.trim())
        .unwrap_or(trimmed);

    // Check for $INCLUDE or $INCLUDEONCE (case-insensitive)
    let upper = content.to_uppercase();
    let is_once = upper.starts_with("$INCLUDEONCE");
    if !is_once && !upper.starts_with("$INCLUDE") {
        return None;
    }

    // Find the path within quotes
    // Format: $INCLUDE: 'path' or $INCLUDE:'path' or $INCLUDEONCE: 'path'
    let skip_len = if is_once { 13 } else { 8 }; // "$INCLUDEONCE" or "$INCLUDE"
    let rest = &content[skip_len..];
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
                return Some((normalized_path, is_once));
            }
        }
    }

    None
}

/// Parses a line to extract a library name if it's a $USELIBRARY directive.
///
/// Handles format: `$USELIBRARY: 'author/library'`
///
/// Returns `None` if the line is not a $USELIBRARY directive, or `Some(library_name)`
/// where `library_name` is the library identifier (e.g., "author/library").
fn parse_uselibrary_directive(line: &str) -> Option<String> {
    let trimmed = line.trim();

    // Handle comment prefix (some BASIC dialects allow ' before $USELIBRARY)
    let content = trimmed
        .strip_prefix('\'')
        .map(|s| s.trim())
        .unwrap_or(trimmed);

    // Check for $USELIBRARY (case-insensitive)
    let upper = content.to_uppercase();
    if !upper.starts_with("$USELIBRARY") {
        return None;
    }

    // Find the library name within quotes
    // Format: $USELIBRARY: 'author/library' or $USELIBRARY:'author/library'
    let skip_len = 12; // "$USELIBRARY"
    let rest = &content[skip_len..];
    let rest = rest.trim_start();

    // Skip optional colon
    let rest = rest.strip_prefix(':').unwrap_or(rest).trim_start();

    // Extract library name from quotes (single quotes in BASIC)
    if let Some(start) = rest.find('\'') {
        let after_quote = &rest[start + 1..];
        if let Some(end) = after_quote.find('\'') {
            let library_name = &after_quote[..end];
            if !library_name.is_empty() {
                return Some(library_name.to_string());
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
            Some(("myfile.bi".to_string(), false))
        );

        // No space after colon
        assert_eq!(
            parse_include_directive("$INCLUDE:'utils.bas'"),
            Some(("utils.bas".to_string(), false))
        );

        // Case insensitive
        assert_eq!(
            parse_include_directive("$include: 'test.bas'"),
            Some(("test.bas".to_string(), false))
        );

        // With leading whitespace
        assert_eq!(
            parse_include_directive("    $INCLUDE: 'header.bi'"),
            Some(("header.bi".to_string(), false))
        );

        // Not an include directive
        assert_eq!(parse_include_directive("PRINT \"Hello\""), None);
        assert_eq!(parse_include_directive("' Just a comment"), None);

        // Path with directory
        assert_eq!(
            parse_include_directive("$INCLUDE: 'inc/common.bi'"),
            Some(("inc/common.bi".to_string(), false))
        );

        // Windows-style backslashes should be normalized to forward slashes
        assert_eq!(
            parse_include_directive("'$INCLUDE:'global\\version.bas'"),
            Some(("global/version.bas".to_string(), false))
        );
        assert_eq!(
            parse_include_directive(
                "$INCLUDE: 'subs_functions\\extensions\\opengl\\opengl_global.bas'"
            ),
            Some((
                "subs_functions/extensions/opengl/opengl_global.bas".to_string(),
                false
            ))
        );

        // $INCLUDEONCE format
        assert_eq!(
            parse_include_directive("$INCLUDEONCE: 'myfile.bi'"),
            Some(("myfile.bi".to_string(), true))
        );
        assert_eq!(
            parse_include_directive("$INCLUDEONCE:'utils.bas'"),
            Some(("utils.bas".to_string(), true))
        );
        assert_eq!(
            parse_include_directive("$includeonce: 'test.bas'"),
            Some(("test.bas".to_string(), true))
        );
    }

    #[test]
    fn test_parse_uselibrary_directive() {
        // Standard format
        assert_eq!(
            parse_uselibrary_directive("$USELIBRARY: 'author/library'"),
            Some("author/library".to_string())
        );

        // No space after colon
        assert_eq!(
            parse_uselibrary_directive("$USELIBRARY:'test/lib'"),
            Some("test/lib".to_string())
        );

        // Case insensitive
        assert_eq!(
            parse_uselibrary_directive("$uselibrary: 'mylib'"),
            Some("mylib".to_string())
        );

        // With leading whitespace
        assert_eq!(
            parse_uselibrary_directive("    $USELIBRARY: 'author/library'"),
            Some("author/library".to_string())
        );

        // Not a USELIBRARY directive
        assert_eq!(parse_uselibrary_directive("PRINT \"Hello\""), None);
        assert_eq!(parse_uselibrary_directive("' Just a comment"), None);
        assert_eq!(parse_uselibrary_directive("$INCLUDE: 'file.bas'"), None);

        // With comment prefix
        assert_eq!(
            parse_uselibrary_directive("'$USELIBRARY:'author/library'"),
            Some("author/library".to_string())
        );
    }

    #[test]
    fn test_preprocess_with_uselibrary() {
        // Create temp directory and library structure
        let temp_dir =
            TempDir::new().expect("creating temp directory for USELIBRARY test should succeed");
        let temp_path = temp_dir.path();

        // Create library structure
        let descriptors_dir = temp_path.join("libraries").join("descriptors");
        fs::create_dir_all(&descriptors_dir).expect("creating descriptors dir should succeed");

        let includes_dir = temp_path
            .join("libraries")
            .join("includes")
            .join("test/lib");
        fs::create_dir_all(&includes_dir).expect("creating includes dir should succeed");

        // Create descriptor file (parent dir already exists from create_dir_all above)
        let descriptor_parent = descriptors_dir.join("test");
        fs::create_dir_all(&descriptor_parent).expect("creating test dir should succeed");
        let descriptor_path = descriptor_parent.join("lib.ini");
        let mut descriptor =
            fs::File::create(&descriptor_path).expect("creating descriptor file should succeed");
        writeln!(descriptor, "[LIBRARY INCLUDES]").expect("writing to descriptor should succeed");
        writeln!(descriptor, "IncAtTop = AtTop.bas").expect("writing to descriptor should succeed");
        writeln!(descriptor, "IncAfterMain = AfterMain.bas")
            .expect("writing to descriptor should succeed");

        // Create library source files
        let at_top_file = includes_dir.join("AtTop.bas");
        fs::write(&at_top_file, "CONST LIB_VERSION = 1").expect("writing AtTop.bas should succeed");

        let after_main_file = includes_dir.join("AfterMain.bas");
        fs::write(&after_main_file, "SUB LibraryInit\nEND SUB")
            .expect("writing AfterMain.bas should succeed");

        // Main source with USELIBRARY directive
        let source = "PRINT \"Start\"\n$USELIBRARY: 'test/lib'\nPRINT \"End\"\n";
        let result = preprocess(source, temp_path, None)
            .expect("preprocessing source with USELIBRARY should succeed");

        // Check that library files were included
        assert!(
            result.contains("CONST LIB_VERSION = 1"),
            "AtTop.bas should be included"
        );
        assert!(
            result.contains("SUB LibraryInit"),
            "AfterMain.bas should be included"
        );
        assert!(
            result.contains("PRINT \"Start\""),
            "Main code should be present"
        );
        assert!(
            result.contains("PRINT \"End\""),
            "Main code should be present"
        );
        assert!(
            result.contains(">>> $USELIBRARY:'test/lib'"),
            "Library inclusion markers should be present"
        );
    }

    #[test]
    fn test_preprocess_no_includes() {
        let source = "PRINT \"Hello\"\nx = 42\n";
        let result = preprocess(source, Path::new("."), None)
            .expect("preprocessing simple source without includes should succeed");
        assert_eq!(result, "PRINT \"Hello\"\nx = 42\n");
    }

    #[test]
    fn test_preprocess_with_include() {
        // Create temp directory and files
        let temp_dir =
            TempDir::new().expect("creating temp directory for include test should succeed");
        let temp_path = temp_dir.path();

        // Create the include file
        let include_path = temp_path.join("header.bi");
        let mut include_file =
            fs::File::create(&include_path).expect("creating include file should succeed");
        writeln!(include_file, "CONST VERSION = 1")
            .expect("writing to include file should succeed");

        // Main source with include
        let source = "PRINT \"Start\"\n$INCLUDE: 'header.bi'\nPRINT \"End\"\n";
        let result = preprocess(source, temp_path, None)
            .expect("preprocessing source with valid include should succeed");

        // Check that the include was expanded
        assert!(result.contains("CONST VERSION = 1"));
        assert!(result.contains("PRINT \"Start\""));
        assert!(result.contains("PRINT \"End\""));
        assert!(result.contains(">>> $INCLUDE: 'header.bi'"));
        assert!(result.contains("<<< END $INCLUDE: 'header.bi'"));
    }

    #[test]
    fn test_preprocess_nested_includes() {
        let temp_dir = TempDir::new()
            .expect("creating temp directory for nested includes test should succeed");
        let temp_path = temp_dir.path();

        // Create level2.bi
        let level2_path = temp_path.join("level2.bi");
        let mut level2_file =
            fs::File::create(&level2_path).expect("creating level2.bi should succeed");
        writeln!(level2_file, "CONST LEVEL2 = 2").expect("writing to level2.bi should succeed");

        // Create level1.bi that includes level2.bi
        let level1_path = temp_path.join("level1.bi");
        let mut level1_file =
            fs::File::create(&level1_path).expect("creating level1.bi should succeed");
        writeln!(level1_file, "CONST LEVEL1 = 1")
            .expect("writing first line to level1.bi should succeed");
        writeln!(level1_file, "$INCLUDE: 'level2.bi'")
            .expect("writing include directive to level1.bi should succeed");

        // Main source
        let source = "$INCLUDE: 'level1.bi'\nPRINT LEVEL1 + LEVEL2\n";
        let result = preprocess(source, temp_path, None)
            .expect("preprocessing source with nested includes should succeed");

        assert!(result.contains("CONST LEVEL1 = 1"));
        assert!(result.contains("CONST LEVEL2 = 2"));
        assert!(result.contains("PRINT LEVEL1 + LEVEL2"));
    }

    #[test]
    fn test_preprocess_circular_include_detection() {
        let temp_dir = TempDir::new()
            .expect("creating temp directory for circular include test should succeed");
        let temp_path = temp_dir.path();

        // Create file_a.bi that includes file_b.bi
        let file_a_path = temp_path.join("file_a.bi");
        let mut file_a = fs::File::create(&file_a_path).expect("creating file_a.bi should succeed");
        writeln!(file_a, "$INCLUDE: 'file_b.bi'").expect("writing to file_a.bi should succeed");

        // Create file_b.bi that includes file_a.bi (circular!)
        let file_b_path = temp_path.join("file_b.bi");
        let mut file_b = fs::File::create(&file_b_path).expect("creating file_b.bi should succeed");
        writeln!(file_b, "$INCLUDE: 'file_a.bi'").expect("writing to file_b.bi should succeed");

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
        let temp_dir =
            TempDir::new().expect("creating temp directory for source path test should succeed");
        let entry_path = temp_dir.path().join("myapp.bas");
        std::fs::write(&entry_path, "$INCLUDE: 'nonexistent.bi'\n")
            .expect("writing test source file should succeed");

        let base = entry_path
            .parent()
            .expect("entry_path should have a parent directory");
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
