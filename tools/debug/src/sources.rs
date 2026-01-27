//! Multi-file source management for the debugger.
//!
//! This module handles source file management including:
//!
//! - **Source file loading**: Load and track multiple source files
//! - **$INCLUDE file handling**: Track include hierarchies
//! - **Source mapping**: Map between compiled positions and source locations
//! - **Line information**: Get source lines for display
//!
//! In BASIC, `$INCLUDE` directives can pull in code from multiple files.
//! The debugger needs to track all these files to:
//! - Show the correct source when stepping
//! - Set breakpoints in included files
//! - Display the include stack when paused

use crate::error::{DebugError, DebugResult};
use crate::symbols::DebugSymbols;
use crate::SourceLocation;
use qb64fresh::ast::{Program, Span, StatementKind};
use qb64fresh::lexer::lex;
use qb64fresh::parser::Parser;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// A loaded source file.
#[derive(Debug)]
pub struct SourceFileInfo {
    /// File path.
    pub path: PathBuf,
    /// Source code content.
    pub source: String,
    /// Parsed AST (if parsing succeeded).
    pub ast: Option<Program>,
    /// Debug symbols extracted from this file.
    pub symbols: Option<DebugSymbols>,
    /// Line offsets (byte offset where each line starts).
    line_offsets: Vec<usize>,
    /// Parent file (if this is an $INCLUDE'd file).
    pub parent: Option<PathBuf>,
    /// Included files (files this file includes).
    pub includes: Vec<IncludeInfo>,
}

/// Information about an $INCLUDE directive.
#[derive(Debug, Clone)]
pub struct IncludeInfo {
    /// Path to the included file.
    pub path: PathBuf,
    /// Line in the parent file where the $INCLUDE appears.
    pub include_line: usize,
    /// Resolved (absolute) path.
    pub resolved_path: Option<PathBuf>,
}

impl SourceFileInfo {
    /// Creates a new source file info from path and content.
    pub fn new(path: PathBuf, source: String) -> Self {
        let line_offsets = Self::compute_line_offsets(&source);
        Self {
            path,
            source,
            ast: None,
            symbols: None,
            line_offsets,
            parent: None,
            includes: Vec::new(),
        }
    }

    /// Computes byte offsets for each line in the source.
    fn compute_line_offsets(source: &str) -> Vec<usize> {
        let mut offsets = vec![0]; // Line 1 starts at offset 0
        for (i, ch) in source.char_indices() {
            if ch == '\n' {
                offsets.push(i + 1);
            }
        }
        offsets
    }

    /// Parses the source file and extracts debug symbols.
    pub fn parse(&mut self) -> DebugResult<()> {
        let tokens = lex(&self.source);
        let mut parser = Parser::new(&tokens);

        let program = parser.parse().map_err(|errors| {
            let messages: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
            DebugError::ParseError {
                message: messages.join("; "),
            }
        })?;

        // Extract debug symbols
        let symbols = DebugSymbols::from_program(self.path.clone(), &program);
        self.symbols = Some(symbols);
        self.ast = Some(program);

        Ok(())
    }

    /// Returns the number of lines in the source.
    pub fn line_count(&self) -> usize {
        self.line_offsets.len()
    }

    /// Gets a specific line (1-indexed).
    pub fn get_line(&self, line: usize) -> Option<&str> {
        if line == 0 || line > self.line_offsets.len() {
            return None;
        }

        let start = self.line_offsets[line - 1];
        let end = if line < self.line_offsets.len() {
            self.line_offsets[line] - 1 // Exclude newline
        } else {
            self.source.len()
        };

        // Handle case where end < start (empty last line)
        if end >= start {
            Some(&self.source[start..end.min(self.source.len())])
        } else {
            Some("")
        }
    }

    /// Gets a range of lines (1-indexed, inclusive).
    pub fn get_lines(&self, start_line: usize, end_line: usize) -> Vec<(usize, &str)> {
        let mut lines = Vec::new();
        for line in start_line..=end_line {
            if let Some(text) = self.get_line(line) {
                lines.push((line, text));
            }
        }
        lines
    }

    /// Converts a byte offset to a line number (1-indexed).
    pub fn offset_to_line(&self, offset: usize) -> usize {
        match self.line_offsets.binary_search(&offset) {
            Ok(idx) => idx + 1,
            Err(idx) => idx,
        }
    }

    /// Converts a byte offset to (line, column) (both 1-indexed).
    pub fn offset_to_position(&self, offset: usize) -> (usize, usize) {
        let line = self.offset_to_line(offset);
        let line_start = if line > 0 && line <= self.line_offsets.len() {
            self.line_offsets[line - 1]
        } else {
            0
        };
        let column = offset.saturating_sub(line_start) + 1;
        (line, column)
    }

    /// Converts a (line, column) to a byte offset.
    pub fn position_to_offset(&self, line: usize, column: usize) -> Option<usize> {
        if line == 0 || line > self.line_offsets.len() {
            return None;
        }
        let line_start = self.line_offsets[line - 1];
        Some(line_start + column.saturating_sub(1))
    }

    /// Creates a source location from a span.
    pub fn span_to_location(&self, span: &Span) -> SourceLocation {
        let (line, column) = self.offset_to_position(span.start);
        SourceLocation::new(self.path.clone(), line, column)
    }
}

/// Manager for multiple source files.
#[derive(Debug)]
pub struct SourceManager {
    /// Loaded source files, keyed by absolute path.
    files: HashMap<PathBuf, SourceFileInfo>,
    /// Files currently being loaded (to prevent circular include loops).
    /// This tracks files that are in the process of being loaded but not yet inserted into `files`.
    loading: std::collections::HashSet<PathBuf>,
    /// Search paths for finding $INCLUDE files.
    search_paths: Vec<PathBuf>,
    /// The main (entry) source file.
    main_file: Option<PathBuf>,
}

impl SourceManager {
    /// Creates a new source manager.
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            loading: std::collections::HashSet::new(),
            search_paths: Vec::new(),
            main_file: None,
        }
    }

    /// Adds a search path for finding included files.
    pub fn add_search_path(&mut self, path: PathBuf) {
        if !self.search_paths.contains(&path) {
            self.search_paths.push(path);
        }
    }

    /// Loads the main source file.
    pub fn load_main<P: AsRef<Path>>(&mut self, path: P) -> DebugResult<()> {
        let path = self.canonicalize_path(path.as_ref())?;
        self.load_file(&path)?;
        self.main_file = Some(path.clone());

        // Add the main file's directory to search paths
        if let Some(parent) = path.parent() {
            self.add_search_path(parent.to_path_buf());
        }

        Ok(())
    }

    /// Loads a source file.
    pub fn load_file(&mut self, path: &Path) -> DebugResult<()> {
        let abs_path = self.canonicalize_path(path)?;

        // Don't reload if already loaded
        if self.files.contains_key(&abs_path) {
            return Ok(());
        }

        // Don't reload if currently being loaded (prevents circular include loops)
        if self.loading.contains(&abs_path) {
            return Ok(());
        }

        // Mark as being loaded
        self.loading.insert(abs_path.clone());

        // Use a scope to ensure we remove from loading even on error
        let result = (|| -> DebugResult<()> {
            let source = std::fs::read_to_string(&abs_path).map_err(|e| DebugError::ReadError {
                path: abs_path.clone(),
                source: e,
            })?;

            let mut file_info = SourceFileInfo::new(abs_path.clone(), source);
            file_info.parse()?;

            // Scan for $INCLUDE directives and load those files recursively
            self.scan_and_load_includes(&abs_path, &mut file_info)?;

            self.files.insert(abs_path.clone(), file_info);
            Ok(())
        })();

        // Remove from loading set (whether success or failure)
        self.loading.remove(&abs_path);

        result
    }

    /// Loads source from a string (for testing or in-memory debugging).
    ///
    /// Note: For in-memory sources, $INCLUDE paths may not resolve correctly
    /// since there's no file system path to resolve relative to. This is primarily
    /// useful for testing with simple programs that don't use includes.
    pub fn load_string(&mut self, name: &str, source: String) -> DebugResult<()> {
        let path = PathBuf::from(name);
        let mut file_info = SourceFileInfo::new(path.clone(), source);
        file_info.parse()?;

        // Try to scan for includes (may not resolve if path doesn't exist)
        // This is best-effort for in-memory sources
        if path.exists() {
            let _ = self.scan_and_load_includes(&path, &mut file_info);
        }

        if self.main_file.is_none() {
            self.main_file = Some(path.clone());
        }

        self.files.insert(path, file_info);
        Ok(())
    }

    /// Returns the main source file path.
    pub fn main_file(&self) -> Option<&Path> {
        self.main_file.as_deref()
    }

    /// Gets a source file by path.
    pub fn get_file<P: AsRef<Path>>(&self, path: P) -> Option<&SourceFileInfo> {
        // Try exact match first
        if let Some(file) = self.files.get(path.as_ref()) {
            return Some(file);
        }

        // Try canonicalized path
        if let Ok(abs_path) = self.canonicalize_path(path.as_ref()) {
            return self.files.get(&abs_path);
        }

        None
    }

    /// Gets a mutable reference to a source file.
    pub fn get_file_mut<P: AsRef<Path>>(&mut self, path: P) -> Option<&mut SourceFileInfo> {
        // Try exact match first
        if self.files.contains_key(path.as_ref()) {
            return self.files.get_mut(path.as_ref());
        }

        // Try canonicalized path
        if let Ok(abs_path) = self.canonicalize_path(path.as_ref()) {
            return self.files.get_mut(&abs_path);
        }

        None
    }

    /// Returns an iterator over all loaded files.
    pub fn files(&self) -> impl Iterator<Item = &SourceFileInfo> {
        self.files.values()
    }

    /// Returns the number of loaded files.
    pub fn file_count(&self) -> usize {
        self.files.len()
    }

    /// Finds a file by name (not full path).
    pub fn find_by_name(&self, name: &str) -> Option<&SourceFileInfo> {
        self.files.values().find(|f| {
            f.path
                .file_name()
                .map(|n| n.to_string_lossy().eq_ignore_ascii_case(name))
                .unwrap_or(false)
        })
    }

    /// Gets source at a specific location.
    pub fn get_source_at(&self, location: &SourceLocation) -> Option<&str> {
        self.get_file(&location.file)
            .and_then(|f| f.get_line(location.line))
    }

    /// Gets source context around a location (lines before and after).
    pub fn get_source_context(
        &self,
        location: &SourceLocation,
        lines_before: usize,
        lines_after: usize,
    ) -> Option<Vec<(usize, &str)>> {
        let file = self.get_file(&location.file)?;
        let start_line = location.line.saturating_sub(lines_before).max(1);
        let end_line = (location.line + lines_after).min(file.line_count());
        Some(file.get_lines(start_line, end_line))
    }

    /// Converts a span to a source location.
    pub fn span_to_location<P: AsRef<Path>>(&self, file: P, span: &Span) -> Option<SourceLocation> {
        self.get_file(file).map(|f| f.span_to_location(span))
    }

    /// Canonicalizes a path (makes it absolute).
    fn canonicalize_path(&self, path: &Path) -> DebugResult<PathBuf> {
        // If already absolute, try to canonicalize
        if path.is_absolute() {
            return path.canonicalize().map_err(|e| DebugError::ReadError {
                path: path.to_path_buf(),
                source: e,
            });
        }

        // Try relative to current directory
        if let Ok(abs) = std::env::current_dir()
            .map(|d| d.join(path))
            .and_then(|p| p.canonicalize())
        {
            return Ok(abs);
        }

        // Try each search path
        for search_path in &self.search_paths {
            let candidate = search_path.join(path);
            if let Ok(abs) = candidate.canonicalize() {
                return Ok(abs);
            }
        }

        // Return as-is if we can't resolve it
        Ok(path.to_path_buf())
    }

    /// Resolves an $INCLUDE path relative to a parent file.
    pub fn resolve_include(&self, include_path: &str, parent_file: &Path) -> Option<PathBuf> {
        // First try relative to parent file's directory
        if let Some(parent_dir) = parent_file.parent() {
            let candidate = parent_dir.join(include_path);
            if candidate.exists() {
                return candidate.canonicalize().ok();
            }
        }

        // Then try each search path
        for search_path in &self.search_paths {
            let candidate = search_path.join(include_path);
            if candidate.exists() {
                return candidate.canonicalize().ok();
            }
        }

        None
    }

    /// Scans a parsed file's AST for $INCLUDE directives and loads those files recursively.
    ///
    /// This method:
    /// - Extracts all `$INCLUDE` directives from the AST
    /// - Resolves each include path relative to the parent file
    /// - Recursively loads included files (which may themselves include other files)
    /// - Tracks the include relationships in `IncludeInfo`
    /// - Sets the parent file reference on included files
    ///
    /// # Arguments
    ///
    /// * `parent_path` - The path of the file being scanned
    /// * `file_info` - The file info containing the parsed AST
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - An include path cannot be resolved
    /// - An included file cannot be read
    /// - An included file fails to parse
    fn scan_and_load_includes(
        &mut self,
        parent_path: &Path,
        file_info: &mut SourceFileInfo,
    ) -> DebugResult<()> {
        let ast = match &file_info.ast {
            Some(ast) => ast,
            None => return Ok(()), // No AST means no includes to scan
        };

        // Scan all statements for $INCLUDE directives
        for stmt in &ast.statements {
            if let StatementKind::IncludeDirective { path } = &stmt.kind {
                let include_line = stmt.span.line;

                // Resolve the include path
                let resolved_path = self
                    .resolve_include(path, parent_path)
                    .ok_or_else(|| DebugError::ReadError {
                        path: PathBuf::from(path),
                        source: std::io::Error::new(
                            std::io::ErrorKind::NotFound,
                            format!("Could not resolve $INCLUDE path: {}", path),
                        ),
                    })?;

                // Record the include relationship (always track, even if file already loaded)
                let include_info = IncludeInfo {
                    path: PathBuf::from(path),
                    include_line,
                    resolved_path: Some(resolved_path.clone()),
                };
                file_info.includes.push(include_info);

                // Load the included file recursively (load_file handles duplicates and will
                // recursively scan for includes in the included file)
                self.load_file(&resolved_path)?;

                // Set the parent relationship on the loaded file
                // Only set parent if not already set (first include wins)
                // Note: In circular include scenarios (A includes B, B includes A), the first
                // file to be loaded will not have a parent, and the second will become its parent.
                // This is acceptable behavior - the important thing is that both files are loaded
                // and include relationships are tracked.
                if let Some(included_file) = self.files.get_mut(&resolved_path) {
                    if included_file.parent.is_none() {
                        included_file.parent = Some(parent_path.to_path_buf());
                    }
                }
            }
        }

        Ok(())
    }
}

impl Default for SourceManager {
    fn default() -> Self {
        Self::new()
    }
}

/// A position in source code that can span multiple files.
#[derive(Debug, Clone)]
pub struct SourcePosition {
    /// The location in the current file.
    pub location: SourceLocation,
    /// The include stack (from innermost to outermost).
    pub include_stack: Vec<SourceLocation>,
}

impl SourcePosition {
    /// Creates a new source position without an include stack.
    pub fn new(location: SourceLocation) -> Self {
        Self {
            location,
            include_stack: Vec::new(),
        }
    }

    /// Creates a source position with an include stack.
    pub fn with_stack(location: SourceLocation, include_stack: Vec<SourceLocation>) -> Self {
        Self {
            location,
            include_stack,
        }
    }

    /// Returns the full path including the include stack.
    pub fn full_path(&self) -> String {
        if self.include_stack.is_empty() {
            format!("{}:{}", self.location.file.display(), self.location.line)
        } else {
            let mut parts = vec![format!(
                "{}:{}",
                self.location.file.display(),
                self.location.line
            )];
            for loc in &self.include_stack {
                parts.push(format!("{}:{}", loc.file.display(), loc.line));
            }
            parts.join(" -> ")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_file_line_offsets() {
        let source = "line1\nline2\nline3";
        let file = SourceFileInfo::new(PathBuf::from("test.bas"), source.to_string());

        assert_eq!(file.line_count(), 3);
        assert_eq!(file.get_line(1), Some("line1"));
        assert_eq!(file.get_line(2), Some("line2"));
        assert_eq!(file.get_line(3), Some("line3"));
        assert_eq!(file.get_line(4), None);
    }

    #[test]
    fn test_offset_to_position() {
        let source = "abc\ndefgh\nij";
        let file = SourceFileInfo::new(PathBuf::from("test.bas"), source.to_string());

        assert_eq!(file.offset_to_position(0), (1, 1)); // 'a'
        assert_eq!(file.offset_to_position(2), (1, 3)); // 'c'
        assert_eq!(file.offset_to_position(4), (2, 1)); // 'd'
        assert_eq!(file.offset_to_position(6), (2, 3)); // 'f'
    }

    #[test]
    fn test_position_to_offset() {
        let source = "abc\ndefgh\nij";
        let file = SourceFileInfo::new(PathBuf::from("test.bas"), source.to_string());

        assert_eq!(file.position_to_offset(1, 1), Some(0));
        assert_eq!(file.position_to_offset(1, 3), Some(2));
        assert_eq!(file.position_to_offset(2, 1), Some(4));
        assert_eq!(file.position_to_offset(0, 1), None); // Invalid line
    }

    #[test]
    fn test_get_lines_range() {
        let source = "line1\nline2\nline3\nline4\nline5";
        let file = SourceFileInfo::new(PathBuf::from("test.bas"), source.to_string());

        let lines = file.get_lines(2, 4);
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], (2, "line2"));
        assert_eq!(lines[1], (3, "line3"));
        assert_eq!(lines[2], (4, "line4"));
    }

    #[test]
    fn test_source_manager_load_string() {
        let mut manager = SourceManager::new();
        let source = "DIM x AS INTEGER\nPRINT x";

        manager.load_string("test.bas", source.to_string()).unwrap();

        assert_eq!(manager.file_count(), 1);
        assert!(manager.get_file("test.bas").is_some());
        assert_eq!(
            manager.get_file("test.bas").unwrap().get_line(1),
            Some("DIM x AS INTEGER")
        );
    }

    #[test]
    fn test_source_position_display() {
        let loc = SourceLocation::new(PathBuf::from("main.bas"), 10, 1);
        let pos = SourcePosition::new(loc);

        assert_eq!(pos.full_path(), "main.bas:10");

        let loc_inner = SourceLocation::new(PathBuf::from("include.bas"), 5, 1);
        let loc_outer = SourceLocation::new(PathBuf::from("main.bas"), 10, 1);
        let pos_with_stack = SourcePosition::with_stack(loc_inner, vec![loc_outer]);

        assert_eq!(pos_with_stack.full_path(), "include.bas:5 -> main.bas:10");
    }

    #[test]
    fn test_include_scanning() {
        use std::fs;
        use std::io::Write;
        use tempfile::TempDir;

        // Create a temporary directory with test files
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create an included file
        let include_path = temp_path.join("included.bas");
        let mut include_file = fs::File::create(&include_path).unwrap();
        writeln!(include_file, "DIM y AS INTEGER").unwrap();
        writeln!(include_file, "y = 42").unwrap();
        include_file.flush().unwrap();
        drop(include_file);

        // Create a main file that includes the other file
        let main_path = temp_path.join("main.bas");
        let mut main_file = fs::File::create(&main_path).unwrap();
        writeln!(main_file, "DIM x AS INTEGER").unwrap();
        writeln!(main_file, "$INCLUDE: 'included.bas'").unwrap();
        writeln!(main_file, "PRINT x, y").unwrap();
        main_file.flush().unwrap();
        drop(main_file);

        // Load the main file
        let mut manager = SourceManager::new();
        manager.load_main(&main_path).unwrap();

        // Verify both files are loaded
        assert_eq!(manager.file_count(), 2);
        assert!(manager.get_file(&main_path).is_some());
        assert!(manager.get_file(&include_path).is_some());

        // Verify the include relationship
        let main_file_info = manager.get_file(&main_path).unwrap();
        assert_eq!(main_file_info.includes.len(), 1);
        assert_eq!(
            main_file_info.includes[0].include_line,
            2
        ); // $INCLUDE is on line 2

        // Verify the included file has the correct parent
        let included_file_info = manager.get_file(&include_path).unwrap();
        assert_eq!(
            included_file_info.parent.as_ref(),
            Some(&main_path)
        );

        // Verify we can access source from both files
        assert_eq!(
            manager.get_file(&main_path).unwrap().get_line(1),
            Some("DIM x AS INTEGER")
        );
        assert_eq!(
            manager.get_file(&include_path).unwrap().get_line(1),
            Some("DIM y AS INTEGER")
        );
    }

    #[test]
    fn test_multiple_includes_same_file() {
        use std::fs;
        use std::io::Write;
        use tempfile::TempDir;

        // Test that the same file can be included by multiple files
        // and that the first include sets the parent (first include wins)

        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create a shared include file
        let shared_path = temp_path.join("shared.bas");
        let mut shared_file = fs::File::create(&shared_path).unwrap();
        writeln!(shared_file, "DIM shared_var AS INTEGER").unwrap();
        writeln!(shared_file, "shared_var = 100").unwrap();
        shared_file.flush().unwrap();
        drop(shared_file);

        // Create first file that includes shared
        let file_a_path = temp_path.join("file_a.bas");
        let mut file_a = fs::File::create(&file_a_path).unwrap();
        writeln!(file_a, "DIM a AS INTEGER").unwrap();
        writeln!(file_a, "$INCLUDE: 'shared.bas'").unwrap();
        writeln!(file_a, "a = 1").unwrap();
        file_a.flush().unwrap();
        drop(file_a);

        // Create second file that also includes shared
        let file_b_path = temp_path.join("file_b.bas");
        let mut file_b = fs::File::create(&file_b_path).unwrap();
        writeln!(file_b, "DIM b AS INTEGER").unwrap();
        writeln!(file_b, "$INCLUDE: 'shared.bas'").unwrap();
        writeln!(file_b, "b = 2").unwrap();
        file_b.flush().unwrap();
        drop(file_b);

        // Create main file that includes both
        let main_path = temp_path.join("main.bas");
        let mut main_file = fs::File::create(&main_path).unwrap();
        writeln!(main_file, "$INCLUDE: 'file_a.bas'").unwrap();
        writeln!(main_file, "$INCLUDE: 'file_b.bas'").unwrap();
        writeln!(main_file, "PRINT a, b, shared_var").unwrap();
        main_file.flush().unwrap();
        drop(main_file);

        // Load the main file
        let mut manager = SourceManager::new();
        manager.load_main(&main_path).unwrap();

        // Verify all files are loaded (main, file_a, file_b, shared - 4 files)
        assert_eq!(manager.file_count(), 4);

        // Verify shared.bas is included by both file_a and file_b
        let file_a_info = manager.get_file(&file_a_path).unwrap();
        assert_eq!(file_a_info.includes.len(), 1);
        assert!(file_a_info.includes[0].resolved_path.as_ref().unwrap() == &shared_path);

        let file_b_info = manager.get_file(&file_b_path).unwrap();
        assert_eq!(file_b_info.includes.len(), 1);
        assert!(file_b_info.includes[0].resolved_path.as_ref().unwrap() == &shared_path);

        // Verify shared.bas's parent is file_a (first include wins)
        let shared_info = manager.get_file(&shared_path).unwrap();
        assert_eq!(
            shared_info.parent.as_ref(),
            Some(&file_a_path)
        );

        // Verify main includes both file_a and file_b
        let main_info = manager.get_file(&main_path).unwrap();
        assert_eq!(main_info.includes.len(), 2);
    }

    #[test]
    fn test_circular_includes() {
        use std::fs;
        use std::io::Write;
        use tempfile::TempDir;

        // Test circular includes: A includes B, B includes A
        // This should not cause infinite loops due to duplicate check

        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create file A that includes B
        let file_a_path = temp_path.join("file_a.bas");
        let mut file_a = fs::File::create(&file_a_path).unwrap();
        writeln!(file_a, "DIM a AS INTEGER").unwrap();
        writeln!(file_a, "$INCLUDE: 'file_b.bas'").unwrap();
        writeln!(file_a, "a = 1").unwrap();
        file_a.flush().unwrap();
        drop(file_a);

        // Create file B that includes A (circular!)
        let file_b_path = temp_path.join("file_b.bas");
        let mut file_b = fs::File::create(&file_b_path).unwrap();
        writeln!(file_b, "DIM b AS INTEGER").unwrap();
        writeln!(file_b, "$INCLUDE: 'file_a.bas'").unwrap();
        writeln!(file_b, "b = 2").unwrap();
        file_b.flush().unwrap();
        drop(file_b);

        // Load file A (which will trigger loading B, which will try to load A again)
        let mut manager = SourceManager::new();
        manager.load_main(&file_a_path).unwrap();

        // Verify both files are loaded (not stuck in infinite loop)
        assert_eq!(manager.file_count(), 2);
        assert!(manager.get_file(&file_a_path).is_some());
        assert!(manager.get_file(&file_b_path).is_some());

        // Verify include relationships are tracked
        let file_a_info = manager.get_file(&file_a_path).unwrap();
        assert_eq!(file_a_info.includes.len(), 1);
        assert!(file_a_info.includes[0].resolved_path.as_ref().unwrap() == &file_b_path);

        let file_b_info = manager.get_file(&file_b_path).unwrap();
        assert_eq!(file_b_info.includes.len(), 1);
        assert!(file_b_info.includes[0].resolved_path.as_ref().unwrap() == &file_a_path);

        // Verify both files are loaded (circular includes handled by duplicate check)
        let file_a_info = manager.get_file(&file_a_path).unwrap();
        let file_b_info = manager.get_file(&file_b_path).unwrap();
        
        // Verify both files are accessible
        assert_eq!(file_a_info.get_line(1), Some("DIM a AS INTEGER"));
        assert_eq!(file_b_info.get_line(1), Some("DIM b AS INTEGER"));
        
        // Verify include relationships are tracked in both directions
        assert_eq!(file_a_info.includes.len(), 1);
        assert_eq!(file_b_info.includes.len(), 1);
        assert!(file_a_info.includes[0].resolved_path.as_ref().unwrap() == &file_b_path);
        assert!(file_b_info.includes[0].resolved_path.as_ref().unwrap() == &file_a_path);
        
        // Note: Parent assignment in circular includes depends on load order.
        // The duplicate check prevents infinite loops, which is the critical behavior.
        // The exact parent relationship may vary, but both files should be loaded.
    }
}
