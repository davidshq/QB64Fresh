//! Configuration for the QB64Fresh debugger.
//!
//! Supports TOML config files and CLI options for breakpoints,
//! launch options, and DAP server settings.

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Root debug configuration (e.g. from `.qb64debug` or `qb64debug.toml`).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct DebugConfig {
    /// Breakpoints: source path and optional line numbers.
    #[serde(default)]
    pub breakpoints: BreakpointsConfig,

    /// Launch / run options (program path, args, working directory).
    #[serde(default)]
    pub launch: LaunchConfig,

    /// DAP server options (port, log level).
    #[serde(default)]
    pub server: ServerConfig,
}

/// Breakpoint configuration.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct BreakpointsConfig {
    /// Source file path (main .bas file).
    pub source: Option<PathBuf>,

    /// Line numbers to break on (1-based). If empty, use source only for context.
    #[serde(default)]
    pub lines: Vec<u32>,
}

/// Launch configuration for running the BASIC program.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct LaunchConfig {
    /// Program path (.bas file).
    pub program: Option<PathBuf>,

    /// Working directory when launching.
    pub cwd: Option<PathBuf>,

    /// Optional command-line arguments passed to the program.
    #[serde(default)]
    pub args: Vec<String>,
}

/// DAP server configuration.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ServerConfig {
    /// If true, use stdio for DAP (default). If false, TCP port can be used.
    #[serde(default = "default_stdio")]
    pub stdio: bool,

    /// TCP port when not using stdio (e.g. 4711).
    pub port: Option<u16>,
}

fn default_stdio() -> bool {
    true
}

impl DebugConfig {
    /// Load config from a TOML file.
    pub fn load(path: &std::path::Path) -> crate::DebugResult<Self> {
        let contents = std::fs::read_to_string(path).map_err(|e| crate::DebugError::ReadError {
            path: path.to_path_buf(),
            source: e,
        })?;
        toml::from_str(&contents).map_err(|e| crate::DebugError::ConfigParseError {
            path: path.to_path_buf(),
            source: e,
        })
    }

    /// Save config to a TOML file.
    pub fn save(&self, path: &std::path::Path) -> crate::DebugResult<()> {
        let contents =
            toml::to_string_pretty(self).map_err(|e| crate::DebugError::ConfigError {
                message: e.to_string(),
            })?;
        std::fs::write(path, contents).map_err(|e| crate::DebugError::ReadError {
            path: path.to_path_buf(),
            source: e,
        })?;
        Ok(())
    }
}
