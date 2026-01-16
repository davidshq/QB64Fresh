//! Parser error types.
//!
//! Parse errors capture what went wrong and where, enabling good error messages.
//! The parser collects multiple errors rather than stopping at the first one,
//! which provides better feedback to users.

use crate::ast::Span;
use thiserror::Error;

/// A parse error with location and description.
#[derive(Debug, Error, Clone)]
pub enum ParseError {
    /// Expected a specific token but found something else.
    #[error("expected {expected}, found {found}")]
    UnexpectedToken {
        /// What we expected to find.
        expected: String,
        /// What we actually found.
        found: String,
        /// Location of the unexpected token.
        span: Span,
    },

    /// Reached end of input unexpectedly.
    #[error("unexpected end of file, expected {expected}")]
    UnexpectedEof {
        /// What we expected to find.
        expected: String,
    },

    /// Invalid expression.
    #[error("invalid expression")]
    InvalidExpression {
        /// Location of the invalid expression.
        span: Span,
        /// Additional context about what went wrong.
        message: String,
    },

    /// Invalid statement.
    #[error("invalid statement: {message}")]
    InvalidStatement {
        /// Location of the invalid statement.
        span: Span,
        /// Description of the problem.
        message: String,
    },

    /// Invalid number literal.
    #[error("invalid number: {message}")]
    InvalidNumber {
        /// Location of the invalid number.
        span: Span,
        /// Description of the problem.
        message: String,
    },

    /// Unterminated string literal.
    #[error("unterminated string literal")]
    UnterminatedString {
        /// Location where the string started.
        span: Span,
    },

    /// Missing END IF for block IF.
    #[error("missing END IF")]
    MissingEndIf {
        /// Location of the IF that's missing its END IF.
        if_span: Span,
    },

    /// Missing NEXT for FOR loop.
    #[error("missing NEXT for FOR loop")]
    MissingNext {
        /// Location of the FOR that's missing its NEXT.
        for_span: Span,
    },

    /// Missing WEND for WHILE loop.
    #[error("missing WEND for WHILE loop")]
    MissingWend {
        /// Location of the WHILE that's missing its WEND.
        while_span: Span,
    },

    /// Missing LOOP for DO loop.
    #[error("missing LOOP for DO")]
    MissingLoop {
        /// Location of the DO that's missing its LOOP.
        do_span: Span,
    },

    /// Missing END SELECT for SELECT CASE.
    #[error("missing END SELECT")]
    MissingEndSelect {
        /// Location of the SELECT that's missing its END SELECT.
        select_span: Span,
    },

    /// Missing END SUB for SUB definition.
    #[error("missing END SUB")]
    MissingEndSub {
        /// Location of the SUB that's missing its END SUB.
        sub_span: Span,
    },

    /// Missing END FUNCTION for FUNCTION definition.
    #[error("missing END FUNCTION")]
    MissingEndFunction {
        /// Location of the FUNCTION that's missing its END FUNCTION.
        function_span: Span,
    },

    /// Duplicate label definition.
    #[error("duplicate label: {name}")]
    DuplicateLabel {
        /// The duplicated label name.
        name: String,
        /// Location of the duplicate.
        span: Span,
    },

    /// General syntax error.
    #[error("{message}")]
    SyntaxError {
        /// Location of the error.
        span: Span,
        /// Description of the problem.
        message: String,
    },
}

impl ParseError {
    /// Returns the span of this error, if available.
    pub fn span(&self) -> Option<Span> {
        match self {
            ParseError::UnexpectedToken { span, .. } => Some(*span),
            ParseError::UnexpectedEof { .. } => None,
            ParseError::InvalidExpression { span, .. } => Some(*span),
            ParseError::InvalidStatement { span, .. } => Some(*span),
            ParseError::InvalidNumber { span, .. } => Some(*span),
            ParseError::UnterminatedString { span } => Some(*span),
            ParseError::MissingEndIf { if_span } => Some(*if_span),
            ParseError::MissingNext { for_span } => Some(*for_span),
            ParseError::MissingWend { while_span } => Some(*while_span),
            ParseError::MissingLoop { do_span } => Some(*do_span),
            ParseError::MissingEndSelect { select_span } => Some(*select_span),
            ParseError::MissingEndSub { sub_span } => Some(*sub_span),
            ParseError::MissingEndFunction { function_span } => Some(*function_span),
            ParseError::DuplicateLabel { span, .. } => Some(*span),
            ParseError::SyntaxError { span, .. } => Some(*span),
        }
    }

    /// Creates an "unexpected token" error.
    pub fn unexpected(expected: impl Into<String>, found: impl Into<String>, span: Span) -> Self {
        ParseError::UnexpectedToken {
            expected: expected.into(),
            found: found.into(),
            span,
        }
    }

    /// Creates an "unexpected EOF" error.
    pub fn eof(expected: impl Into<String>) -> Self {
        ParseError::UnexpectedEof {
            expected: expected.into(),
        }
    }

    /// Creates a syntax error with a message.
    pub fn syntax(message: impl Into<String>, span: Span) -> Self {
        ParseError::SyntaxError {
            span,
            message: message.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_span() {
        let err = ParseError::unexpected("THEN", "ELSE", Span::new(10, 14));
        assert_eq!(err.span(), Some(Span::new(10, 14)));
    }

    #[test]
    fn test_eof_error() {
        let err = ParseError::eof("expression");
        assert_eq!(err.span(), None);
        assert!(err.to_string().contains("end of file"));
    }

    #[test]
    fn test_syntax_error() {
        let err = ParseError::syntax("invalid operator", Span::new(0, 5));
        assert!(err.to_string().contains("invalid operator"));
    }
}
