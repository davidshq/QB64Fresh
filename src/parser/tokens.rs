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

    /// Returns true if we're at the end of a statement (newline, colon, or EOF).
    ///
    /// Useful for determining if a statement has optional trailing arguments.
    pub(super) fn is_at_statement_end(&self) -> bool {
        self.is_at_end()
            || self.check(&TokenKind::Newline)
            || self.check(&TokenKind::Colon)
            || self.check(&TokenKind::Else)
    }

    /// Skips statement separators (newlines and colons).
    ///
    /// In BASIC, both newlines and colons separate statements:
    /// - `x = 1` / `y = 2` (newline separator)
    /// - `x = 1: y = 2` (colon separator on same line)
    pub(super) fn skip_statement_separators(&mut self) {
        while self.check(&TokenKind::Newline) || self.check(&TokenKind::Colon) {
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

    // ==================== Keyword-as-Identifier Handling ====================
    //
    // In BASIC, most keywords can be used as variable/parameter names since
    // context makes the meaning unambiguous. For example:
    //   DECLARE SUB Greet(name AS STRING)  ' "name" is NAME keyword but used as param
    //   DIM input AS INTEGER               ' "input" is INPUT keyword but used as var
    //
    // This helper allows keywords to be accepted in identifier positions.

    /// Checks if the current token can be used as an identifier name.
    ///
    /// Returns true for actual identifiers AND for keywords that are valid
    /// in name positions (most keywords in BASIC).
    pub(super) fn is_name_token(&self) -> bool {
        let Some(kind) = self.peek_kind() else {
            return false;
        };
        Self::token_kind_is_name(kind)
    }

    /// Checks if a token kind can be used as an identifier name.
    fn token_kind_is_name(kind: &TokenKind) -> bool {
        match kind {
            // Actual identifiers are always valid names
            TokenKind::Identifier => true,

            // Keywords that are commonly used as variable/parameter names
            // Most BASIC keywords can be used as identifiers in name contexts
            TokenKind::Name      // NAME statement - very common as param name
            | TokenKind::Input   // INPUT statement - common as param name
            | TokenKind::Output  // OUTPUT mode - common as param name
            | TokenKind::Print   // PRINT statement
            | TokenKind::Read    // READ statement
            | TokenKind::Write   // WRITE statement
            | TokenKind::Open    // OPEN statement
            | TokenKind::Close   // CLOSE statement
            | TokenKind::Get     // GET statement
            | TokenKind::Put     // PUT statement
            | TokenKind::Len     // LEN keyword
            | TokenKind::Line    // LINE statement
            | TokenKind::Color   // COLOR statement
            | TokenKind::Screen  // SCREEN statement
            | TokenKind::Width   // WIDTH statement
            | TokenKind::View    // VIEW statement
            | TokenKind::Window  // WINDOW statement
            | TokenKind::Draw    // DRAW statement
            | TokenKind::Play    // PLAY statement
            | TokenKind::Sound   // SOUND statement
            | TokenKind::Beep    // BEEP statement
            | TokenKind::Lock    // LOCK statement
            | TokenKind::Unlock  // UNLOCK statement
            | TokenKind::Seek    // SEEK statement
            | TokenKind::Kill    // KILL statement
            | TokenKind::Shell   // SHELL statement
            | TokenKind::Sleep   // SLEEP statement
            | TokenKind::Stop    // STOP statement
            | TokenKind::System  // SYSTEM statement
            | TokenKind::Swap    // SWAP statement
            | TokenKind::Erase   // ERASE statement
            | TokenKind::Restore // RESTORE statement
            | TokenKind::Randomize // RANDOMIZE statement
            | TokenKind::Access  // ACCESS keyword
            | TokenKind::Binary  // BINARY keyword
            | TokenKind::Random  // RANDOM keyword
            | TokenKind::Append  // APPEND keyword
            | TokenKind::Using   // USING keyword
            | TokenKind::Step    // STEP keyword
            | TokenKind::Call    // CALL keyword
            | TokenKind::Circle  // CIRCLE statement
            | TokenKind::Paint   // PAINT statement
            | TokenKind::Pset    // PSET statement
            | TokenKind::Preset  // PRESET statement
            | TokenKind::Cls     // CLS statement
            | TokenKind::Locate  // LOCATE statement
            | TokenKind::ErrorKw // ERROR keyword
            | TokenKind::Resume  // RESUME keyword
            | TokenKind::On      // ON keyword
            | TokenKind::Def     // DEF keyword
            | TokenKind::Fn      // FN keyword
            | TokenKind::Common  // COMMON keyword
            | TokenKind::Alias   // ALIAS keyword
            | TokenKind::Library // LIBRARY keyword
            | TokenKind::Dynamic // DYNAMIC keyword
            | TokenKind::Return  // RETURN keyword (valid as var name in some contexts)
            => true,

            // These are NOT valid as names (control flow, type specifiers, operators)
            _ => false,
        }
    }

    /// Expects a token that can be used as an identifier name.
    ///
    /// This accepts actual identifiers AND keywords that can be used as names.
    /// Use this instead of `expect(&TokenKind::Identifier, ...)` when parsing
    /// variable names, parameter names, etc.
    pub(super) fn expect_name(&mut self, expected_desc: &str) -> Result<&crate::lexer::Token, ()> {
        if self.is_name_token() {
            Ok(self.advance().expect("advance after is_name_token check"))
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
}
