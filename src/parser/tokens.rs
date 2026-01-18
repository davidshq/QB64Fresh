//! Token navigation utilities for the parser.
//!
//! This module provides the low-level token stream operations that form
//! the foundation of the recursive descent parser:
//! - Peeking at tokens without consuming them
//! - Advancing through the token stream
//! - Matching and expecting specific tokens
//! - Error recovery (synchronization)

use crate::ast::Span;
use crate::lexer::TokenKind;

use super::{ParseError, Parser};

impl<'a> Parser<'a> {
    // ==================== Token Navigation ====================

    /// Returns the current token without consuming it.
    pub(super) fn peek(&self) -> Option<&crate::lexer::Token> {
        self.tokens.get(self.current)
    }

    /// Returns the kind of the current token.
    pub(super) fn peek_kind(&self) -> Option<&TokenKind> {
        self.peek().map(|t| &t.kind)
    }

    /// Looks ahead n tokens (0 = current token).
    pub(super) fn peek_ahead(&self, n: usize) -> Option<&crate::lexer::Token> {
        self.tokens.get(self.current + n)
    }

    /// Consumes and returns the current token.
    pub(super) fn advance(&mut self) -> Option<&crate::lexer::Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens.get(self.current - 1)
    }

    /// Returns true if we've reached the end of the token stream.
    pub(super) fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    /// Checks if the current token matches the expected kind.
    pub(super) fn check(&self, kind: &TokenKind) -> bool {
        self.peek_kind() == Some(kind)
    }

    /// Checks if the current token is one of the expected kinds.
    #[allow(dead_code)]
    pub(super) fn check_any(&self, kinds: &[TokenKind]) -> bool {
        self.peek_kind().is_some_and(|k| kinds.contains(k))
    }

    /// Consumes the current token if it matches, returns true if consumed.
    pub(super) fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Consumes the current token if it matches any of the kinds.
    #[allow(dead_code)]
    pub(super) fn match_any(&mut self, kinds: &[TokenKind]) -> Option<TokenKind> {
        if let Some(kind) = self.peek_kind().cloned()
            && kinds.contains(&kind)
        {
            self.advance();
            return Some(kind);
        }
        None
    }

    /// Expects the current token to match, or records an error.
    pub(super) fn expect(
        &mut self,
        kind: &TokenKind,
        expected_desc: &str,
    ) -> Result<&crate::lexer::Token, ()> {
        if self.check(kind) {
            Ok(self.advance().expect("advance after check"))
        } else {
            let (found, span) = if let Some(token) = self.peek() {
                (format!("{:?}", token.kind), token.span.clone().into())
            } else {
                self.errors.push(ParseError::eof(expected_desc));
                return Err(());
            };
            self.errors
                .push(ParseError::unexpected(expected_desc, found, span));
            Err(())
        }
    }

    /// Skips newline tokens.
    pub(super) fn skip_newlines(&mut self) {
        while self.check(&TokenKind::Newline) {
            self.advance();
        }
    }

    /// Attempts to recover from an error by skipping to a synchronization point.
    ///
    /// Synchronization points are:
    /// - Newlines (statement boundaries)
    /// - Statement-starting keywords (PRINT, IF, FOR, etc.)
    pub(super) fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            // Newline is a natural statement boundary
            if self.tokens.get(self.current - 1).map(|t| &t.kind) == Some(&TokenKind::Newline) {
                return;
            }

            // Statement-starting keywords are synchronization points
            match self.peek_kind() {
                Some(
                    TokenKind::Print
                    | TokenKind::If
                    | TokenKind::For
                    | TokenKind::While
                    | TokenKind::Do
                    | TokenKind::Dim
                    | TokenKind::Let
                    | TokenKind::Sub
                    | TokenKind::Function
                    | TokenKind::Select
                    | TokenKind::End,
                ) => return,
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Creates a span from start to current position.
    pub(super) fn span_from(&self, start: usize) -> Span {
        let end = self
            .tokens
            .get(self.current.saturating_sub(1))
            .map(|t| t.span.end)
            .unwrap_or(start);
        Span::new(start, end)
    }
}
