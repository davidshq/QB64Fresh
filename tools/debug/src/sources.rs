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
use qb64fresh::ast::{Program, Span};
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

        let source = std::fs::read_to_string(&abs_path).map_err(|e| DebugError::ReadError {
            path: abs_path.clone(),
            source: e,
        })?;

        let mut file_info = SourceFileInfo::new(abs_path.clone(), source);
        file_info.parse()?;

        // TODO: Scan for $INCLUDE directives and load those files too
        // This would require parsing the preprocessor directives

        self.files.insert(abs_path, file_info);
        Ok(())
    }

    /// Loads source from a string (for testing or in-memory debugging).
    pub fn load_string(&mut self, name: &str, source: String) -> DebugResult<()> {
        let path = PathBuf::from(name);
        let mut file_info = SourceFileInfo::new(path.clone(), source);
        file_info.parse()?;

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
}
