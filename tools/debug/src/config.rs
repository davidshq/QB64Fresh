//! Configuration for the QB64Fresh debugger.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::error::{DebugError, DebugResult};

/// Debug output verbosity level.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
#[serde(rename_all = "lowercase")]
pub enum Verbosity {
    /// Minimal output - errors only.
    Quiet,
    /// Normal output - status and errors.
    #[default]
    Normal,
    /// Verbose output - detailed debug information.
    Verbose,
    /// Trace output - all internal operations.
    Trace,
}

impl Verbosity {
    /// Returns the display name for this verbosity level.
    pub fn name(&self) -> &'static str {
        match self {
            Verbosity::Quiet => "quiet",
            Verbosity::Normal => "normal",
            Verbosity::Verbose => "verbose",
            Verbosity::Trace => "trace",
        }
    }
}

/// Breakpoint types.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum BreakpointKind {
    /// Break on a specific source line.
    Line { file: PathBuf, line: usize },
    /// Break when entering a function/sub.
    Function { name: String },
    /// Break on a specific label.
    Label { name: String },
    /// Conditional breakpoint - break when expression is true.
    Conditional {
        file: PathBuf,
        line: usize,
        condition: String,
    },
}

/// A breakpoint definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Breakpoint {
    /// Unique identifier for this breakpoint.
    pub id: u32,
    /// The breakpoint specification.
    pub kind: BreakpointKind,
    /// Whether this breakpoint is currently enabled.
    pub enabled: bool,
    /// Hit count (number of times to ignore before breaking).
    pub ignore_count: u32,
    /// Current hit count.
    #[serde(default)]
    pub hit_count: u32,
}

impl Breakpoint {
    /// Creates a new line breakpoint.
    pub fn line(id: u32, file: PathBuf, line: usize) -> Self {
        Self {
            id,
            kind: BreakpointKind::Line { file, line },
            enabled: true,
            ignore_count: 0,
            hit_count: 0,
        }
    }

    /// Creates a new function breakpoint.
    pub fn function(id: u32, name: String) -> Self {
        Self {
            id,
            kind: BreakpointKind::Function { name },
            enabled: true,
            ignore_count: 0,
            hit_count: 0,
        }
    }

    /// Creates a new label breakpoint.
    pub fn label(id: u32, name: String) -> Self {
        Self {
            id,
            kind: BreakpointKind::Label { name },
            enabled: true,
            ignore_count: 0,
            hit_count: 0,
        }
    }

    /// Creates a new conditional breakpoint.
    pub fn conditional(id: u32, file: PathBuf, line: usize, condition: String) -> Self {
        Self {
            id,
            kind: BreakpointKind::Conditional {
                file,
                line,
                condition,
            },
            enabled: true,
            ignore_count: 0,
            hit_count: 0,
        }
    }
}

/// Debugger configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct DebugConfig {
    /// Output verbosity level.
    pub verbosity: Verbosity,

    /// Source file search paths (for finding included files).
    pub source_paths: Vec<PathBuf>,

    /// Whether to break on program entry.
    pub break_on_entry: bool,

    /// Whether to break on unhandled errors.
    pub break_on_error: bool,

    /// Default timeout for debug operations (in milliseconds).
    pub timeout_ms: u64,

    /// Port for debug adapter protocol communication.
    pub dap_port: Option<u16>,

    /// Initial breakpoints (can be set via config file).
    pub breakpoints: Vec<Breakpoint>,

    /// Environment variables to set for the debug target.
    pub environment: HashMap<String, String>,

    /// Working directory for the debug target.
    pub working_directory: Option<PathBuf>,

    /// Arguments to pass to the debug target.
    pub target_args: Vec<String>,
}

impl Default for DebugConfig {
    fn default() -> Self {
        Self {
            verbosity: Verbosity::Normal,
            source_paths: vec![],
            break_on_entry: false,
            break_on_error: true,
            timeout_ms: 30000,
            dap_port: None,
            breakpoints: vec![],
            environment: HashMap::new(),
            working_directory: None,
            target_args: vec![],
        }
    }
}

impl DebugConfig {
    /// Creates a configuration for interactive debugging.
    pub fn interactive() -> Self {
        Self {
            verbosity: Verbosity::Normal,
            break_on_entry: true,
            ..Default::default()
        }
    }

    /// Creates a configuration for DAP (Debug Adapter Protocol) mode.
    pub fn dap(port: u16) -> Self {
        Self {
            verbosity: Verbosity::Quiet,
            dap_port: Some(port),
            ..Default::default()
        }
    }

    /// Creates a verbose configuration for troubleshooting.
    pub fn verbose() -> Self {
        Self {
            verbosity: Verbosity::Verbose,
            break_on_entry: true,
            ..Default::default()
        }
    }

    /// Loads configuration from a TOML file.
    pub fn from_file(path: &Path) -> DebugResult<Self> {
        let content = std::fs::read_to_string(path).map_err(|e| DebugError::ConfigReadError {
            path: path.to_path_buf(),
            source: e,
        })?;

        toml::from_str(&content).map_err(|e| DebugError::ConfigParseError {
            path: path.to_path_buf(),
            message: e.to_string(),
        })
    }

    /// Tries to find and load a configuration file from standard locations.
    ///
    /// Searches for (in order):
    /// 1. `.qb64fresh-debug.toml` in the current directory
    /// 2. `qb64fresh-debug.toml` in the current directory
    /// 3. `.qb64fresh-debug.toml` in parent directories
    pub fn discover() -> Option<Self> {
        let config_names = [".qb64fresh-debug.toml", "qb64fresh-debug.toml"];

        // Check current directory first
        for name in &config_names {
            let path = Path::new(name);
            if path.exists() {
                if let Ok(config) = Self::from_file(path) {
                    return Some(config);
                }
            }
        }

        // Walk up parent directories
        let mut current = std::env::current_dir().ok()?;
        loop {
            for name in &config_names {
                let path = current.join(name);
                if path.exists() {
                    if let Ok(config) = Self::from_file(&path) {
                        return Some(config);
                    }
                }
            }

            if !current.pop() {
                break;
            }
        }

        None
    }

    /// Adds a source path for finding included files.
    pub fn add_source_path(&mut self, path: PathBuf) {
        if !self.source_paths.contains(&path) {
            self.source_paths.push(path);
        }
    }

    /// Sets an environment variable for the debug target.
    pub fn set_env(&mut self, key: String, value: String) {
        self.environment.insert(key, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = DebugConfig::default();
        assert_eq!(config.verbosity, Verbosity::Normal);
        assert!(!config.break_on_entry);
        assert!(config.break_on_error);
    }

    #[test]
    fn test_interactive_config() {
        let config = DebugConfig::interactive();
        assert!(config.break_on_entry);
    }

    #[test]
    fn test_dap_config() {
        let config = DebugConfig::dap(4711);
        assert_eq!(config.dap_port, Some(4711));
        assert_eq!(config.verbosity, Verbosity::Quiet);
    }

    #[test]
    fn test_breakpoint_creation() {
        let bp = Breakpoint::line(1, PathBuf::from("test.bas"), 10);
        assert_eq!(bp.id, 1);
        assert!(bp.enabled);
        assert!(matches!(bp.kind, BreakpointKind::Line { line: 10, .. }));
    }

    #[test]
    fn test_toml_roundtrip() {
        let config = DebugConfig::verbose();
        let toml_str = toml::to_string_pretty(&config).unwrap();
        let parsed: DebugConfig = toml::from_str(&toml_str).unwrap();

        assert_eq!(parsed.verbosity, config.verbosity);
        assert_eq!(parsed.break_on_entry, config.break_on_entry);
    }
}
