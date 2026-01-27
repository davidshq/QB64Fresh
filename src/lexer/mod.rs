//! Lexical analysis for QB64Fresh.
//!
//! This module converts BASIC source code into a stream of [`Token`]s.
//! It handles:
//!
//! - Case-insensitive keyword recognition (BASIC tradition)
//! - Multiple number formats (decimal, hex `&H`, octal `&O`, binary `&B`)
//! - String literals with proper escaping
//! - Comments (both `'` and `REM` styles)
//! - Line continuations (underscore at end of line)
//! - QB64-specific extensions and metacommands
//!
//! ## Example
//!
//! ```
//! use qb64fresh::lexer::Lexer;
//!
//! let source = r#"PRINT "Hello, World!""#;
//! let mut lexer = Lexer::new(source);
//!
//! while let Some(token) = lexer.next_token() {
//!     println!("{}: {:?}", token.text, token.kind);
//! }
//! ```
//!
//! ## Design Notes
//!
//! The lexer is built on the [`logos`](https://docs.rs/logos) crate, which
//! generates a fast DFA-based lexer from our token definitions. This gives us:
//!
//! - O(n) lexing performance
//! - Zero-copy token extraction where possible
//! - Easy maintenance of token patterns
//!
//! We wrap logos in our own [`Lexer`] struct to provide:
//!
//! - A cleaner iterator interface
//! - Token position tracking
//! - Better error handling

mod token;

pub use token::{Token, TokenKind};

use logos::Logos;
use crate::ast::Span;

/// The lexer for QB64Fresh BASIC source code.
///
/// Wraps a `logos` lexer and provides a convenient iterator interface
/// that yields [`Token`]s with their source positions.
///
/// ## Example
///
/// ```
/// use qb64fresh::lexer::{Lexer, TokenKind};
///
/// let lexer = Lexer::new("PRINT 42");
/// let tokens: Vec<_> = lexer.collect();
///
/// assert_eq!(tokens.len(), 2);
/// assert_eq!(tokens[0].kind, TokenKind::Print);
/// assert_eq!(tokens[1].kind, TokenKind::IntegerLiteral);
/// assert_eq!(tokens[1].text, "42");
/// ```
pub struct Lexer<'source> {
    /// The underlying logos lexer
    inner: logos::Lexer<'source, TokenKind>,
    /// The original source (for error reporting)
    source: &'source str,
}

impl<'source> Lexer<'source> {
    /// Create a new lexer for the given source code.
    ///
    /// # Arguments
    ///
    /// * `source` - The BASIC source code to tokenize
    ///
    /// # Example
    ///
    /// ```
    /// use qb64fresh::lexer::Lexer;
    ///
    /// let lexer = Lexer::new("DIM x AS INTEGER");
    /// ```
    pub fn new(source: &'source str) -> Self {
        Self {
            inner: TokenKind::lexer(source),
            source,
        }
    }

    /// Get the original source code.
    ///
    /// Useful for error reporting and diagnostics.
    pub fn source(&self) -> &'source str {
        self.source
    }

    /// Get the next token, if any.
    ///
    /// Returns `None` when the end of input is reached.
    /// Invalid/unrecognized characters are skipped (in the future,
    /// we may want to return error tokens instead).
    ///
    /// # Example
    ///
    /// ```
    /// use qb64fresh::lexer::{Lexer, TokenKind};
    ///
    /// let mut lexer = Lexer::new("PRINT");
    ///
    /// let token = lexer.next_token().unwrap();
    /// assert_eq!(token.kind, TokenKind::Print);
    ///
    /// assert!(lexer.next_token().is_none());
    /// ```
    pub fn next_token(&mut self) -> Option<Token> {
        let kind = self.inner.next()?;
        let byte_span = self.inner.span();
        let text = self.inner.slice().to_string();

        let token_kind = match kind {
            Ok(k) => k,
            Err(()) => TokenKind::Error, // Unrecognized character
        };

        // Compute line number from byte offset
        let line = Self::line_number_at_offset(self.source, byte_span.start);
        let span = Span::new(byte_span.start, byte_span.end, line);

        Some(Token::new(token_kind, span, text))
    }

    /// Compute the 1-indexed line number at the given byte offset in the source.
    ///
    /// Counts newlines (`\n`) up to (but not including) the offset.
    fn line_number_at_offset(source: &str, offset: usize) -> usize {
        // Clamp offset to source length to avoid panics
        let clamped_offset = offset.min(source.len());
        
        // Count newlines in the prefix up to the offset
        // Line numbers are 1-indexed, so we start at 1 and add 1 for each newline
        source[..clamped_offset]
            .chars()
            .filter(|&c| c == '\n')
            .count()
            + 1
    }

    /// Collect all remaining tokens into a vector.
    ///
    /// This consumes the lexer.
    ///
    /// # Example
    ///
    /// ```
    /// use qb64fresh::lexer::Lexer;
    ///
    /// let tokens = Lexer::new("1 + 2").collect_tokens();
    /// assert_eq!(tokens.len(), 3);
    /// ```
    pub fn collect_tokens(self) -> Vec<Token> {
        self.collect()
    }
}

/// Implement Iterator so the lexer can be used with for loops and iterator adapters.
impl<'source> Iterator for Lexer<'source> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

/// Convenience function to lex source code directly.
///
/// # Example
///
/// ```
/// use qb64fresh::lexer::{lex, TokenKind};
///
/// let tokens = lex("PRINT 42");
/// assert_eq!(tokens[0].kind, TokenKind::Print);
/// ```
pub fn lex(source: &str) -> Vec<Token> {
    Lexer::new(source).collect_tokens()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_iterator() {
        let tokens: Vec<_> = Lexer::new("PRINT 42").collect();
        assert_eq!(tokens.len(), 2);
    }

    #[test]
    fn test_token_spans() {
        let tokens = lex("PRINT 42");

        // PRINT should span bytes 0..5, line 1
        assert_eq!(tokens[0].span.start, 0);
        assert_eq!(tokens[0].span.end, 5);
        assert_eq!(tokens[0].span.line, 1);
        assert_eq!(tokens[0].text, "PRINT");

        // 42 should span bytes 6..8, line 1
        assert_eq!(tokens[1].span.start, 6);
        assert_eq!(tokens[1].span.end, 8);
        assert_eq!(tokens[1].span.line, 1);
        assert_eq!(tokens[1].text, "42");
    }

    #[test]
    fn test_multiline() {
        let source = "x = 1\ny = 2";
        let tokens = lex(source);

        // Should get: x, =, 1, newline, y, =, 2
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                &TokenKind::Identifier,
                &TokenKind::Equals,
                &TokenKind::IntegerLiteral,
                &TokenKind::Newline,
                &TokenKind::Identifier,
                &TokenKind::Equals,
                &TokenKind::IntegerLiteral,
            ]
        );

        // Check line numbers
        assert_eq!(tokens[0].span.line, 1); // x on line 1
        assert_eq!(tokens[3].span.line, 1); // newline on line 1
        assert_eq!(tokens[4].span.line, 2); // y on line 2
    }

    #[test]
    fn test_lex_convenience_function() {
        let tokens = lex("DIM x AS INTEGER");
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].kind, TokenKind::Dim);
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].kind, TokenKind::As);
        assert_eq!(tokens[3].kind, TokenKind::Integer);
    }

    #[test]
    fn test_string_literal_content() {
        let tokens = lex(r#"PRINT "Hello, World!""#);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[1].kind, TokenKind::StringLiteral);
        assert_eq!(tokens[1].text, r#""Hello, World!""#);
    }

    #[test]
    fn test_complex_expression() {
        let source = "result = (a + b) * 2 / 3.14";
        let tokens = lex(source);
        let kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();

        assert_eq!(
            kinds,
            vec![
                &TokenKind::Identifier,     // result
                &TokenKind::Equals,         // =
                &TokenKind::LeftParen,      // (
                &TokenKind::Identifier,     // a
                &TokenKind::Plus,           // +
                &TokenKind::Identifier,     // b
                &TokenKind::RightParen,     // )
                &TokenKind::Star,           // *
                &TokenKind::IntegerLiteral, // 2
                &TokenKind::Slash,          // /
                &TokenKind::FloatLiteral,   // 3.14
            ]
        );
    }
}
