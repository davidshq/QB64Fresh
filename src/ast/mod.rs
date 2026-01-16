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
/// Spans are byte offsets from the start of the source, matching the lexer's spans.
/// Used for error reporting and source mapping.
///
/// # Example
///
/// ```
/// use qb64fresh::ast::Span;
///
/// let span = Span::new(0, 5);
/// assert_eq!(span.start, 0);
/// assert_eq!(span.end, 5);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Byte offset of the start of the span (inclusive).
    pub start: usize,
    /// Byte offset of the end of the span (exclusive).
    pub end: usize,
}

impl Span {
    /// Creates a new span from start to end byte offsets.
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// Creates a span that covers both `self` and `other`.
    ///
    /// Useful for combining spans when building parent nodes from children.
    pub fn merge(&self, other: &Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(range: std::ops::Range<usize>) -> Self {
        Span::new(range.start, range.end)
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
        let span = Span::new(10, 20);
        assert_eq!(span.start, 10);
        assert_eq!(span.end, 20);
    }

    #[test]
    fn test_span_merge() {
        let a = Span::new(5, 10);
        let b = Span::new(15, 25);
        let merged = a.merge(&b);
        assert_eq!(merged.start, 5);
        assert_eq!(merged.end, 25);
    }

    #[test]
    fn test_span_from_range() {
        let span: Span = (5..10).into();
        assert_eq!(span.start, 5);
        assert_eq!(span.end, 10);
    }
}
