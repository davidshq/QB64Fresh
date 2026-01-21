//! Error types for the formatter.

use std::path::PathBuf;
use thiserror::Error;

/// Result type for formatting operations.
pub type FormatResult<T> = Result<T, FormatError>;

/// Errors that can occur during formatting.
#[derive(Error, Debug)]
pub enum FormatError {
    /// Failed to read the input file.
    #[error("Failed to read file '{path}': {source}")]
    ReadError {
        path: PathBuf,
        source: std::io::Error,
    },

    /// Failed to write the output file.
    #[error("Failed to write file '{path}': {source}")]
    WriteError {
        path: PathBuf,
        source: std::io::Error,
    },

    /// The input file is not valid UTF-8.
    #[error("File '{path}' is not valid UTF-8")]
    InvalidUtf8 { path: PathBuf },

    /// Lexer encountered an error.
    #[error("Lexer error at byte {offset}: {message}")]
    LexerError { offset: usize, message: String },

    /// Parser encountered an error.
    #[error("Parser error: {message}")]
    ParseError { message: String },

    /// Configuration error.
    #[error("Configuration error: {message}")]
    ConfigError { message: String },

    /// Internal formatter error (should not normally occur).
    #[error("Internal error: {message}")]
    InternalError { message: String },
}
