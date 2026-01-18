//! Graphics error types.

use std::fmt;

/// Errors that can occur in graphics operations.
#[derive(Debug, Clone)]
pub struct GraphicsError {
    kind: GraphicsErrorKind,
    message: String,
}

/// Categories of graphics errors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GraphicsErrorKind {
    /// Backend not initialized
    NotInitialized,
    /// Backend already initialized
    AlreadyInitialized,
    /// System does not support graphics (e.g., no display available)
    NoDisplay,
    /// Invalid argument to graphics function
    InvalidArgument,
    /// Memory allocation failed
    OutOfMemory,
    /// Backend-specific error
    BackendError,
    /// Other errors
    Other,
}

impl GraphicsError {
    /// Create a new graphics error.
    pub fn new(kind: GraphicsErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
        }
    }

    /// Create a "not initialized" error.
    pub fn not_initialized() -> Self {
        Self::new(
            GraphicsErrorKind::NotInitialized,
            "Graphics backend not initialized",
        )
    }

    /// Create an "already initialized" error.
    pub fn already_initialized() -> Self {
        Self::new(
            GraphicsErrorKind::AlreadyInitialized,
            "Graphics backend already initialized",
        )
    }

    /// Create a "no display" error.
    pub fn no_display() -> Self {
        Self::new(
            GraphicsErrorKind::NoDisplay,
            "No display available (headless environment?)",
        )
    }

    /// Create an "invalid argument" error.
    pub fn invalid_argument(msg: impl Into<String>) -> Self {
        Self::new(GraphicsErrorKind::InvalidArgument, msg)
    }

    /// Create an "out of memory" error.
    pub fn out_of_memory() -> Self {
        Self::new(GraphicsErrorKind::OutOfMemory, "Out of memory")
    }

    /// Create a backend-specific error.
    pub fn backend_error(msg: impl Into<String>) -> Self {
        Self::new(GraphicsErrorKind::BackendError, msg)
    }

    /// Get the error kind.
    pub fn kind(&self) -> &GraphicsErrorKind {
        &self.kind
    }

    /// Get the error message.
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for GraphicsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Graphics error: {}", self.message)
    }
}

impl std::error::Error for GraphicsError {}
