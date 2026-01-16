//! Code generation error types.
//!
//! Errors that can occur during code generation. Most semantic errors should
//! be caught earlier, but some issues only become apparent during code generation.

use crate::ast::Span;
use std::fmt;

/// An error that occurred during code generation.
#[derive(Debug, Clone)]
pub struct CodeGenError {
    /// The kind of error.
    pub kind: CodeGenErrorKind,
    /// Source location where the error occurred.
    pub span: Option<Span>,
    /// Additional context about the error.
    pub context: Option<String>,
}

impl CodeGenError {
    /// Creates a new code generation error.
    pub fn new(kind: CodeGenErrorKind) -> Self {
        Self {
            kind,
            span: None,
            context: None,
        }
    }

    /// Adds a source span to the error.
    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    /// Adds context information to the error.
    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        self.context = Some(context.into());
        self
    }

    /// Creates an unsupported feature error.
    pub fn unsupported(feature: impl Into<String>) -> Self {
        Self::new(CodeGenErrorKind::UnsupportedFeature(feature.into()))
    }

    /// Creates an internal error (compiler bug).
    pub fn internal(message: impl Into<String>) -> Self {
        Self::new(CodeGenErrorKind::Internal(message.into()))
    }
}

impl fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "code generation error: {}", self.kind)?;
        if let Some(ctx) = &self.context {
            write!(f, " ({})", ctx)?;
        }
        if let Some(span) = &self.span {
            write!(f, " at {}..{}", span.start, span.end)?;
        }
        Ok(())
    }
}

impl std::error::Error for CodeGenError {}

/// The specific kind of code generation error.
#[derive(Debug, Clone)]
pub enum CodeGenErrorKind {
    /// A feature that isn't implemented yet.
    UnsupportedFeature(String),

    /// A type that can't be represented in the target.
    UnsupportedType(String),

    /// Internal compiler error (indicates a bug).
    Internal(String),

    /// Failed to write output.
    IoError(String),
}

impl fmt::Display for CodeGenErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CodeGenErrorKind::UnsupportedFeature(feature) => {
                write!(f, "unsupported feature: {}", feature)
            }
            CodeGenErrorKind::UnsupportedType(ty) => {
                write!(f, "type '{}' cannot be represented in target", ty)
            }
            CodeGenErrorKind::Internal(msg) => {
                write!(f, "internal error: {}", msg)
            }
            CodeGenErrorKind::IoError(msg) => {
                write!(f, "I/O error: {}", msg)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = CodeGenError::unsupported("ON ERROR GOTO");
        assert!(err.to_string().contains("unsupported feature"));
        assert!(err.to_string().contains("ON ERROR GOTO"));
    }

    #[test]
    fn test_error_with_span() {
        let err = CodeGenError::unsupported("feature").with_span(Span::new(10, 20));
        assert!(err.to_string().contains("10..20"));
    }

    #[test]
    fn test_error_with_context() {
        let err = CodeGenError::internal("missing case").with_context("in emit_expr");
        assert!(err.to_string().contains("in emit_expr"));
    }
}
