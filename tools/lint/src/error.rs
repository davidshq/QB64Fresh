//! Error types for the linter.

use std::path::PathBuf;
use thiserror::Error;

/// Result type for linting operations.
pub type LintResult<T> = Result<T, LintError>;

/// Errors that can occur during linting.
#[derive(Error, Debug)]
pub enum LintError {
    /// Failed to read the input file.
    #[error("Failed to read file '{path}': {source}")]
    ReadError {
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

    /// Semantic analysis error.
    #[error("Semantic error: {message}")]
    SemanticError { message: String },

    /// Configuration error.
    #[error("Configuration error: {message}")]
    ConfigError { message: String },

    /// Failed to read configuration file.
    #[error("Failed to read config file '{path}': {source}")]
    ConfigReadError {
        path: PathBuf,
        source: std::io::Error,
    },

    /// Failed to parse configuration file.
    #[error("Failed to parse config file '{path}': {message}")]
    ConfigParseError { path: PathBuf, message: String },
}
