//! Error types for the QB64Fresh debugger.

use std::path::PathBuf;
use thiserror::Error;

/// Result type for debugger operations.
pub type DebugResult<T> = Result<T, DebugError>;

/// Errors that can occur during debugging or adapter operation.
#[derive(Error, Debug)]
pub enum DebugError {
    /// Failed to read the source or config file.
    #[error("Failed to read file '{path}': {source}")]
    ReadError {
        path: PathBuf,
        source: std::io::Error,
    },

    /// The input file is not valid UTF-8.
    #[error("File '{path}' is not valid UTF-8")]
    InvalidUtf8 { path: PathBuf },

    /// Parser or semantic analysis failed.
    #[error("Compile error: {message}")]
    CompileError { message: String },

    /// Configuration file error.
    #[error("Configuration error: {message}")]
    ConfigError { message: String },

    /// Failed to parse configuration file.
    #[error("Failed to parse config file '{path}': {source}")]
    ConfigParseError {
        path: PathBuf,
        source: toml::de::Error,
    },

    /// DAP protocol or transport error.
    #[error("Protocol error: {message}")]
    ProtocolError { message: String },

    /// Runtime integration not yet available.
    #[error("Runtime integration not yet implemented: {message}")]
    NotImplemented { message: String },
}
