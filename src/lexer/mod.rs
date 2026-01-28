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

use crate::ast::Span;
use logos::Logos;

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
    /// Current line number (1-indexed)
    current_line: usize,
    /// Byte offset of the last newline we've seen
    last_newline_offset: usize,
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
            current_line: 1,
            last_newline_offset: 0,
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

        // Update line number by counting newlines since last token
        // This is O(k) where k is the distance since last token, not O(n)!
        if byte_span.start > self.last_newline_offset {
            let newlines = self.source[self.last_newline_offset..byte_span.start]
                .chars()
                .filter(|&c| c == '\n')
                .count();
            self.current_line += newlines;
            self.last_newline_offset = byte_span.start;
        }

        // Check if this token contains newlines (before creating span)
        let newlines_in_token = if byte_span.start < byte_span.end {
            self.source[byte_span.start..byte_span.end]
                .chars()
                .filter(|&c| c == '\n')
                .count()
        } else {
            0
        };

        // Create span with current line number (before incrementing for newlines)
        // The newline token itself is on the line it ends, not the next line
        let span = Span::new(byte_span.start, byte_span.end, self.current_line);

        // After creating the span, update line number for next token
        if newlines_in_token > 0 {
            self.current_line += newlines_in_token;
            // Find the last newline in this token
            if let Some(last_nl) = self.source[byte_span.start..byte_span.end].rfind('\n') {
                self.last_newline_offset = byte_span.start + last_nl + 1;
            }
        }

        Some(Token::new(token_kind, span, text))
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

/// Lex with progress reporting for large files.
///
/// Reports progress every 10,000 tokens to stderr (for verbose mode).
/// This helps track progress when lexing very large files.
pub fn lex_with_progress(source: &str, verbose: bool) -> Vec<Token> {
    if !verbose {
        return lex(source);
    }

    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    let mut last_report = 0;

    while let Some(token) = lexer.next_token() {
        tokens.push(token);

        // Report progress periodically
        if tokens.len() - last_report >= 10000 {
            let progress_pct = ((tokens.len() as f64 / source.len() as f64) * 100.0) as usize;
            eprint!(
                "\r[1/4] Lexing... {} tokens (~{}%)",
                tokens.len(),
                progress_pct.min(100)
            );
            use std::io::Write;
            let _ = std::io::stderr().flush();
            last_report = tokens.len();
        }
    }

    if verbose {
        eprintln!("\r[1/4] Lexing complete: {} tokens", tokens.len());
        use std::io::Write;
        let _ = std::io::stderr().flush();
    }

    tokens
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
