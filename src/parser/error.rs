//! Parser error types for QB64Fresh.
//!
//! This module defines all errors that can occur during parsing. Parse errors capture
//! what went wrong and where, enabling good error messages that help users fix their code.
//!
//! ## Error Recovery
//!
//! The parser collects multiple errors rather than stopping at the first one. This provides
//! better feedback to users by reporting all problems in a single compilation pass.
//!
//! ## Error Categories
//!
//! Errors fall into several categories:
//!
//! - **Unexpected tokens** - The parser found something other than what it expected
//! - **Unclosed blocks** - IF without END IF, FOR without NEXT, etc.
//! - **Invalid syntax** - Malformed expressions, statements, or literals
//! - **Duplicate definitions** - Labels defined more than once
//!
//! ## Example
//!
//! ```ignore
//! use qb64fresh::parser::error::ParseError;
//!
//! let err = ParseError::unexpected("THEN", "ELSE", span);
//! println!("{}", err); // "expected THEN, found ELSE"
//! ```

use crate::ast::Span;
use thiserror::Error;

/// Errors that can occur during parsing of QB64 BASIC source code.
///
/// Each variant captures the specific type of error along with location information
/// (a [`Span`]) for accurate error reporting. The parser accumulates multiple errors
/// when possible, rather than stopping at the first error.
///
/// # Example
///
/// ```ignore
/// use qb64fresh::parser::error::ParseError;
/// use qb64fresh::ast::Span;
///
/// // Create an error for a missing keyword
/// let err = ParseError::unexpected("THEN", "END", Span::new(10, 13));
/// assert!(err.span().is_some());
/// ```
#[derive(Debug, Error, Clone)]
pub enum ParseError {
    /// Expected a specific token but found something else.
    ///
    /// This is the most common parse error, occurring when the syntax doesn't match
    /// expected patterns. Common causes include:
    /// - Missing keywords (e.g., `IF x > 5` without `THEN`)
    /// - Wrong punctuation (e.g., `;` instead of `,`)
    /// - Misspelled keywords
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// IF x > 5       ' Error: expected THEN, found newline
    ///     PRINT x
    /// END IF
    /// ```
    #[error("expected {expected}, found {found}")]
    UnexpectedToken {
        /// What the parser expected to find (e.g., "THEN", ")", "expression").
        expected: String,
        /// What was actually found (e.g., "END", "newline", "123").
        found: String,
        /// Location of the unexpected token in the source.
        span: Span,
    },

    /// Reached end of input while expecting more tokens.
    ///
    /// This typically indicates incomplete code, such as:
    /// - An unclosed block (IF without END IF)
    /// - An incomplete expression (e.g., `x +` at end of file)
    /// - Missing closing delimiter
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// FOR i = 1 TO 10
    ///     PRINT i
    /// ' Error: unexpected end of file, expected NEXT
    /// ```
    #[error("unexpected end of file, expected {expected}")]
    UnexpectedEof {
        /// What the parser was expecting when it hit EOF.
        expected: String,
    },

    /// An expression could not be parsed.
    ///
    /// This occurs when the parser encounters something that should be an expression
    /// but doesn't match any valid expression pattern. Common causes:
    /// - Invalid operator usage (e.g., `* x` at start of expression)
    /// - Mismatched parentheses
    /// - Invalid function call syntax
    #[error("invalid expression")]
    InvalidExpression {
        /// Location of the problematic expression.
        span: Span,
        /// Additional context about what went wrong.
        message: String,
    },

    /// A statement could not be parsed.
    ///
    /// This occurs when a line doesn't match any recognized statement pattern.
    /// The message field provides specifics about what was wrong.
    #[error("invalid statement: {message}")]
    InvalidStatement {
        /// Location of the problematic statement.
        span: Span,
        /// Human-readable description of the problem.
        message: String,
    },

    /// A numeric literal could not be parsed.
    ///
    /// This occurs when a number is syntactically recognized but invalid:
    /// - Number too large for its type
    /// - Invalid hex/octal/binary digits
    /// - Malformed floating-point notation
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// x% = 99999999999  ' Error: number too large for INTEGER
    /// y& = &HZZZZ       ' Error: invalid hex digit 'Z'
    /// ```
    #[error("invalid number: {message}")]
    InvalidNumber {
        /// Location of the invalid number.
        span: Span,
        /// Description of why the number is invalid.
        message: String,
    },

    /// A string literal was not properly closed.
    ///
    /// BASIC strings are delimited by double quotes and must be closed on
    /// the same line. This error occurs when a string spans multiple lines
    /// or reaches end-of-file without a closing quote.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// PRINT "Hello, World   ' Error: unterminated string literal
    /// ```
    #[error("unterminated string literal")]
    UnterminatedString {
        /// Location where the string started (the opening quote).
        span: Span,
    },

    /// A block IF statement is missing its END IF.
    ///
    /// Block IF statements (IF...THEN with the body on following lines)
    /// must be closed with END IF. Single-line IF statements don't need this.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// IF x > 5 THEN
    ///     PRINT "big"
    /// ' Error: missing END IF (IF at line 1)
    /// ```
    #[error("missing END IF")]
    MissingEndIf {
        /// Location of the IF keyword that's missing its END IF.
        if_span: Span,
    },

    /// A FOR loop is missing its NEXT statement.
    ///
    /// Every FOR must have a matching NEXT. The NEXT can optionally include
    /// the loop variable name (e.g., `NEXT i`).
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// FOR i = 1 TO 10
    ///     PRINT i
    /// ' Error: missing NEXT for FOR loop (FOR at line 1)
    /// ```
    #[error("missing NEXT for FOR loop")]
    MissingNext {
        /// Location of the FOR keyword that's missing its NEXT.
        for_span: Span,
    },

    /// A WHILE loop is missing its WEND.
    ///
    /// WHILE...WEND is the classic BASIC loop construct. Every WHILE
    /// must be closed with WEND.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// WHILE x < 10
    ///     x = x + 1
    /// ' Error: missing WEND for WHILE loop
    /// ```
    #[error("missing WEND for WHILE loop")]
    MissingWend {
        /// Location of the WHILE keyword that's missing its WEND.
        while_span: Span,
    },

    /// A DO loop is missing its LOOP statement.
    ///
    /// DO loops must end with LOOP (optionally with WHILE or UNTIL condition).
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// DO
    ///     x = x + 1
    /// ' Error: missing LOOP for DO
    /// ```
    #[error("missing LOOP for DO")]
    MissingLoop {
        /// Location of the DO keyword that's missing its LOOP.
        do_span: Span,
    },

    /// A SELECT CASE block is missing its END SELECT.
    ///
    /// SELECT CASE statements must be closed with END SELECT.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// SELECT CASE x
    ///     CASE 1: PRINT "one"
    /// ' Error: missing END SELECT
    /// ```
    #[error("missing END SELECT")]
    MissingEndSelect {
        /// Location of the SELECT keyword that's missing its END SELECT.
        select_span: Span,
    },

    /// A SUB definition is missing its END SUB.
    ///
    /// Subroutine definitions must be closed with END SUB.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// SUB DoSomething
    ///     PRINT "Hello"
    /// ' Error: missing END SUB
    /// ```
    #[error("missing END SUB")]
    MissingEndSub {
        /// Location of the SUB keyword that's missing its END SUB.
        sub_span: Span,
    },

    /// A FUNCTION definition is missing its END FUNCTION.
    ///
    /// Function definitions must be closed with END FUNCTION.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// FUNCTION Square(x)
    ///     Square = x * x
    /// ' Error: missing END FUNCTION
    /// ```
    #[error("missing END FUNCTION")]
    MissingEndFunction {
        /// Location of the FUNCTION keyword that's missing its END FUNCTION.
        function_span: Span,
    },

    /// A label was defined more than once in the same scope.
    ///
    /// Labels (both numeric line numbers and named labels) must be unique
    /// within their scope.
    ///
    /// # BASIC Example
    ///
    /// ```basic
    /// start:
    ///     PRINT "first"
    /// start:               ' Error: duplicate label: start
    ///     PRINT "second"
    /// ```
    #[error("duplicate label: {name}")]
    DuplicateLabel {
        /// The name of the duplicated label.
        name: String,
        /// Location of the duplicate definition.
        span: Span,
    },

    /// A general syntax error that doesn't fit other categories.
    ///
    /// This is a catch-all for syntax problems that don't have a more specific
    /// error variant. The message provides details about the specific issue.
    #[error("{message}")]
    SyntaxError {
        /// Location of the syntax error.
        span: Span,
        /// Human-readable description of the problem.
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
