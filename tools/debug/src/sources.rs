//! Multi-file source management for the debugger.
//!
//! Tracks source files (main + $INCLUDE'd files) and line mappings
//! for breakpoint resolution and stack traces.

use std::collections::HashMap;
use std::path::PathBuf;

/// Manages loaded source files and their content.
#[derive(Debug, Default)]
pub struct SourceManager {
    /// Path -> full source content.
    files: HashMap<PathBuf, String>,
}

impl SourceManager {
    /// Create an empty source manager.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a source file by path and content.
    pub fn add_file(&mut self, path: PathBuf, content: String) {
        self.files.insert(path, content);
    }

    /// Get content for a path, if loaded.
    pub fn get(&self, path: &std::path::Path) -> Option<&str> {
        self.files.get(path).map(String::as_str)
    }

    /// Return the path of the main source file if exactly one is loaded.
    pub fn main_path(&self) -> Option<&PathBuf> {
        if self.files.len() == 1 {
            self.files.keys().next()
        } else {
            None
        }
    }
}
