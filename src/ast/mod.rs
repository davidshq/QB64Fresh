//! Abstract Syntax Tree (AST) definitions for QB64Fresh.
//!
//! The AST represents the hierarchical structure of a BASIC program after parsing.
//! Each node in the tree corresponds to a syntactic construct in the source code.
//!
//! # Structure
//!
//! ```text
//! Program
//! └── Vec<Statement>
//!     ├── Print { values: Vec<Expr>, ... }
//!     ├── Let { name, value: Expr }
//!     ├── If { condition: Expr, then_branch, else_branch }
//!     └── ...
//! ```
//!
//! # Design Decisions
//!
//! - **Owned nodes**: AST nodes own their children (no lifetimes). This simplifies
//!   later compiler phases since the AST can outlive the source text.
//! - **Span on every node**: Every node tracks its source location for error messages.
//! - **Separated expression/statement types**: Reflects BASIC's distinction between
//!   expressions (produce values) and statements (perform actions).

mod expr;
mod stmt;

pub use expr::*;
pub use stmt::*;

/// A span representing a range in the source text.
///
/// Spans track both byte offsets and line numbers for accurate error reporting
/// and debugging. Line numbers are 1-indexed (first line is line 1).
///
/// # Example
///
/// ```
/// use qb64fresh::ast::Span;
///
/// let span = Span::new(0, 5, 1);
/// assert_eq!(span.start, 0);
/// assert_eq!(span.end, 5);
/// assert_eq!(span.line, 1);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Byte offset of the start of the span (inclusive).
    pub start: usize,
    /// Byte offset of the end of the span (exclusive).
    pub end: usize,
    /// Line number where this span starts (1-indexed).
    pub line: usize,
}

impl Span {
    /// Creates a new span from start to end byte offsets and a line number.
    ///
    /// # Arguments
    ///
    /// * `start` - Byte offset of the start of the span (inclusive)
    /// * `end` - Byte offset of the end of the span (exclusive)
    /// * `line` - Line number where the span starts (1-indexed)
    pub fn new(start: usize, end: usize, line: usize) -> Self {
        Self { start, end, line }
    }

    /// Creates a span that covers both `self` and `other`.
    ///
    /// The merged span uses the earliest start position and latest end position.
    /// The line number is taken from the span that starts earliest (or `self` if equal).
    ///
    /// Useful for combining spans when building parent nodes from children.
    pub fn merge(&self, other: &Span) -> Span {
        let start = self.start.min(other.start);
        let end = self.end.max(other.end);
        // Use the line number from whichever span starts earlier
        let line = if self.start <= other.start {
            self.line
        } else {
            other.line
        };
        Span { start, end, line }
    }
}

impl From<std::ops::Range<usize>> for Span {
    /// Converts a byte range to a span, defaulting to line 1.
    ///
    /// **Note:** This is a fallback for cases where line numbers aren't available.
    /// Prefer creating spans with explicit line numbers from the lexer/parser.
    fn from(range: std::ops::Range<usize>) -> Self {
        Span::new(range.start, range.end, 1)
    }
}

/// A complete BASIC program.
///
/// A program is simply a sequence of statements executed in order.
/// In BASIC, the program structure is flat (no top-level declarations
/// required), though SUB and FUNCTION definitions create callable units.
#[derive(Debug, Clone)]
pub struct Program {
    /// The statements that make up the program.
    pub statements: Vec<Statement>,
}

impl Program {
    /// Creates a new program with the given statements.
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_new() {
        let span = Span::new(10, 20, 2);
        assert_eq!(span.start, 10);
        assert_eq!(span.end, 20);
        assert_eq!(span.line, 2);
    }

    #[test]
    fn test_span_merge() {
        let a = Span::new(5, 10, 1);
        let b = Span::new(15, 25, 2);
        let merged = a.merge(&b);
        assert_eq!(merged.start, 5);
        assert_eq!(merged.end, 25);
        assert_eq!(merged.line, 1); // Uses line from earlier span
    }

    #[test]
    fn test_span_merge_reverse_order() {
        let a = Span::new(15, 25, 2);
        let b = Span::new(5, 10, 1);
        let merged = a.merge(&b);
        assert_eq!(merged.start, 5);
        assert_eq!(merged.end, 25);
        assert_eq!(merged.line, 1); // Uses line from earlier span (b)
    }

    #[test]
    fn test_span_from_range() {
        let span: Span = (5..10).into();
        assert_eq!(span.start, 5);
        assert_eq!(span.end, 10);
        assert_eq!(span.line, 1); // Defaults to line 1
    }
}
