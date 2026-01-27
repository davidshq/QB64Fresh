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

    /// Checks if the next token (after current) is ON or OFF.
    /// Used for keywords like _RESIZE that can be both statements and functions.
    pub(super) fn peek_is_on_or_off(&self) -> bool {
        self.peek_ahead(1)
            .is_some_and(|t| t.kind == TokenKind::On || t.kind == TokenKind::Off)
    }

    /// Consumes and returns the current token.
    ///
    /// Returns `None` when at EOF (i.e. when [`is_at_end()`](Self::is_at_end) is true).
    /// Does not increment `current` at EOF, so repeated calls at EOF keep returning `None`.
    pub(super) fn advance(&mut self) -> Option<&crate::lexer::Token> {
        if self.is_at_end() {
            return None;
        }
        self.current += 1;
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
                (format!("{:?}", token.kind), token.span)
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

    /// Skips comment and newline tokens.
    ///
    /// Useful after THEN when a comment may appear before the newline.
    pub(super) fn skip_comments_and_newlines(&mut self) {
        while self.check(&TokenKind::Comment) || self.check(&TokenKind::Newline) {
            self.advance();
        }
    }

    /// Returns true if we're at the end of a statement (newline, colon, comment, or EOF).
    ///
    /// Useful for determining if a statement has optional trailing arguments.
    /// Comments on the same line also indicate statement end since they continue to EOL.
    pub(super) fn is_at_statement_end(&self) -> bool {
        self.is_at_end()
            || self.check(&TokenKind::Newline)
            || self.check(&TokenKind::Colon)
            || self.check(&TokenKind::Else)
            || self.check(&TokenKind::Comment)
    }

    /// Skips statement separators (newlines, colons, and comments).
    ///
    /// In BASIC, both newlines and colons separate statements:
    /// - `x = 1` / `y = 2` (newline separator)
    /// - `x = 1: y = 2` (colon separator on same line)
    ///
    /// Comments can also appear between statements and should be skipped.
    ///
    /// This function also updates `at_line_start` to track whether we're at the
    /// beginning of a physical line (after newline) or mid-line (after colon only).
    /// This is important for distinguishing label definitions from procedure calls.
    pub(super) fn skip_statement_separators(&mut self) {
        // Track what separators we see
        let mut saw_newline = false;
        let mut saw_colon = false;
        while self.check(&TokenKind::Newline)
            || self.check(&TokenKind::Colon)
            || self.check(&TokenKind::Comment)
        {
            if self.check(&TokenKind::Newline) {
                saw_newline = true;
            } else if self.check(&TokenKind::Colon) {
                saw_colon = true;
            }
            // Comments don't affect line start status
            self.advance();
        }
        // We're at line start if:
        // 1. We saw a newline (regardless of colons - newline resets line start)
        // 2. We saw nothing and were already at line start (e.g., file start)
        if saw_newline {
            self.at_line_start = true;
        } else if saw_colon {
            // Only colon(s) - we're mid-line
            self.at_line_start = false;
        }
        // If we saw nothing, at_line_start stays unchanged (preserves file start state)
    }

    /// Attempts to recover from an error by skipping to a synchronization point.
    ///
    /// Synchronization points are:
    /// - Newlines (statement boundaries)
    /// - Statement-starting keywords (PRINT, IF, FOR, etc.)
    /// - Block-ending keywords (NEXT, WEND, LOOP, CASE, etc.)
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
                    // Statement starters
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
                    | TokenKind::End
                    // Block enders (important for reducing cascading errors)
                    | TokenKind::Next
                    | TokenKind::Wend
                    | TokenKind::Loop
                    | TokenKind::Case
                    | TokenKind::Else
                    | TokenKind::ElseIf
                    | TokenKind::EndIf
                    // Other common statements
                    | TokenKind::Goto
                    | TokenKind::Gosub
                    | TokenKind::Return
                    | TokenKind::Exit
                    | TokenKind::Type
                    | TokenKind::Const
                    | TokenKind::Static
                    | TokenKind::Shared
                    | TokenKind::Redim
                    | TokenKind::Input
                    | TokenKind::Line
                    | TokenKind::Open
                    | TokenKind::Close
                    | TokenKind::On
                    | TokenKind::Resume
                    | TokenKind::ErrorKw
                    | TokenKind::Data
                    | TokenKind::Read
                    | TokenKind::Restore
                    | TokenKind::Call
                    | TokenKind::Declare
                    // Metacommands
                    | TokenKind::MetaCommand
                    | TokenKind::MetaIf
                    | TokenKind::IncludeDirective,
                ) => return,
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Creates a span from start to current position.
    pub(super) fn span_from(&self, start: usize) -> Span {
        let last_token = self.tokens.get(self.current.saturating_sub(1));
        let end = last_token.map(|t| t.span.end).unwrap_or(start);
        let line = last_token.map(|t| t.span.line).unwrap_or(1);
        Span::new(start, end, line)
    }

    /// Advances and returns the start position of the consumed token.
    ///
    /// This is a safe alternative to `advance().expect("keyword").span.start` that
    /// reports an EOF error instead of panicking if no token is available.
    ///
    /// Use this at the start of parse functions that are called after matching
    /// a specific keyword token (e.g., `parse_screen()` called after seeing SCREEN).
    pub(super) fn advance_start(&mut self, keyword_desc: &str) -> Result<usize, ()> {
        if let Some(token) = self.advance() {
            Ok(token.span.start)
        } else {
            self.errors.push(ParseError::eof(keyword_desc));
            Err(())
        }
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
        Self::is_name_kind(kind)
    }

    /// Checks if a token kind can be used as an identifier name.
    ///
    /// This is used both for direct name checking and for lookahead in is_array_assignment.
    pub(super) fn is_name_kind(kind: &TokenKind) -> bool {
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
            | TokenKind::Console // _CONSOLE also a function that returns console handle
            | TokenKind::Dest    // _DEST also a function that returns current dest handle
            | TokenKind::Source  // _SOURCE also a function that returns current source handle
            | TokenKind::Display // _DISPLAY is both a statement and function
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
                (format!("{:?}", token.kind), token.span)
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
