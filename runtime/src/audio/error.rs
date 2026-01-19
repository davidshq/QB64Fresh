//! Audio error types for the audio backend.

use std::fmt;

/// Kinds of audio errors that can occur.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AudioErrorKind {
    /// Audio system not initialized.
    NotInitialized,
    /// Audio system already initialized.
    AlreadyInitialized,
    /// Audio system failed to initialize.
    InitializationFailed,
    /// Invalid sound handle.
    InvalidHandle,
    /// File not found or could not be opened.
    FileNotFound,
    /// Unsupported audio format.
    UnsupportedFormat,
    /// Audio backend error (library-specific).
    BackendError,
    /// Playback failed.
    PlaybackFailed,
    /// Invalid argument provided.
    InvalidArgument,
    /// Operation not supported by this backend.
    NotSupported,
}

/// An error from audio operations.
#[derive(Debug, Clone)]
pub struct AudioError {
    /// The kind of error.
    pub kind: AudioErrorKind,
    /// Human-readable description.
    pub message: String,
}

impl AudioError {
    /// Create a new audio error.
    pub fn new(kind: AudioErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
        }
    }

    /// Create a "not initialized" error.
    pub fn not_initialized() -> Self {
        Self::new(
            AudioErrorKind::NotInitialized,
            "Audio system not initialized",
        )
    }

    /// Create an "already initialized" error.
    pub fn already_initialized() -> Self {
        Self::new(
            AudioErrorKind::AlreadyInitialized,
            "Audio system already initialized",
        )
    }

    /// Create an "invalid handle" error.
    pub fn invalid_handle(handle: i32) -> Self {
        Self::new(
            AudioErrorKind::InvalidHandle,
            format!("Invalid sound handle: {}", handle),
        )
    }

    /// Create a "file not found" error.
    pub fn file_not_found(filename: &str) -> Self {
        Self::new(
            AudioErrorKind::FileNotFound,
            format!("Sound file not found: {}", filename),
        )
    }

    /// Create an "unsupported format" error.
    pub fn unsupported_format(format: &str) -> Self {
        Self::new(
            AudioErrorKind::UnsupportedFormat,
            format!("Unsupported audio format: {}", format),
        )
    }

    /// Create a "not supported" error.
    pub fn not_supported(operation: &str) -> Self {
        Self::new(
            AudioErrorKind::NotSupported,
            format!("Operation not supported: {}", operation),
        )
    }
}

impl fmt::Display for AudioError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}", self.kind, self.message)
    }
}

impl std::error::Error for AudioError {}
