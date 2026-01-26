//! System integration statement parsing for the parser.
//!
//! This module contains parsing for system-related statements:
//! - File system: KILL, NAME, MKDIR, RMDIR, CHDIR
//! - Shell commands: SHELL, _SHELLHIDE
//! - Memory: BLOAD, BSAVE, SETMEM
//! - Mouse: _MOUSEHIDE, _MOUSESHOW, _MOUSEMOVE
//! - Clipboard: _CLIPBOARD$ = ...

use crate::ast::{Statement, StatementKind};
use crate::lexer::TokenKind;

use super::Parser;

impl<'a> Parser<'a> {
    // ==================== File System Statements ====================

    /// Parses KILL statement.
    ///
    /// Syntax: `KILL filename$`
    pub(super) fn parse_kill(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("KILL keyword").span.start;
        let filename = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Kill { filename }, span))
    }

    /// Parses NAME statement.
    ///
    /// Syntax: `NAME oldname$ AS newname$`
    pub(super) fn parse_name(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("NAME keyword").span.start;
        let old_name = self.parse_expression()?;
        self.expect(&TokenKind::As, "AS")?;
        let new_name = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Rename { old_name, new_name },
            span,
        ))
    }

    /// Parses MKDIR statement.
    ///
    /// Syntax: `MKDIR path$`
    pub(super) fn parse_mkdir(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("MKDIR keyword").span.start;
        let path = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Mkdir { path }, span))
    }

    /// Parses RMDIR statement.
    ///
    /// Syntax: `RMDIR path$`
    pub(super) fn parse_rmdir(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RMDIR keyword").span.start;
        let path = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Rmdir { path }, span))
    }

    /// Parses CHDIR statement.
    ///
    /// Syntax: `CHDIR path$`
    pub(super) fn parse_chdir(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CHDIR keyword").span.start;
        let path = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Chdir { path }, span))
    }

    /// Parses ENVIRON statement.
    ///
    /// Syntax: `ENVIRON "name=value"`
    pub(super) fn parse_environ(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("ENVIRON keyword").span.start;
        let env_string = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Environ { env_string }, span))
    }

    // ==================== Shell Commands ====================

    /// Parses SHELL statement.
    ///
    /// Syntax: `SHELL [_HIDE] [_DONTWAIT] [command$]`
    /// Options can be combined, e.g., `SHELL _HIDE _DONTWAIT "cmd"`
    pub(super) fn parse_shell(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SHELL keyword").span.start;

        // Parse options (can have multiple in any order)
        let mut hide = false;
        let mut _dontwait = false;
        let mut _dontclose = false;

        loop {
            if let Some(token) = self.peek()
                && token.kind == TokenKind::Identifier
            {
                let text = &token.text;
                if text.eq_ignore_ascii_case("_HIDE") {
                    self.advance();
                    hide = true;
                    continue;
                }
                if text.eq_ignore_ascii_case("_DONTWAIT") {
                    self.advance();
                    _dontwait = true;
                    continue;
                }
                if text.eq_ignore_ascii_case("_DONTCLOSE") {
                    self.advance();
                    _dontclose = true;
                    continue;
                }
            }
            break;
        }

        let command = if self.is_at_end_of_statement() {
            None
        } else {
            Some(self.parse_expression()?)
        };

        let span = self.span_from(start);

        // If hidden, use ShellHide variant; otherwise ShellCmd
        if hide {
            if let Some(cmd) = command {
                Ok(Statement::new(
                    StatementKind::ShellHide { command: cmd },
                    span,
                ))
            } else {
                // SHELL _HIDE without command - treat as error or empty command
                Ok(Statement::new(
                    StatementKind::ShellCmd { command: None },
                    span,
                ))
            }
        } else {
            Ok(Statement::new(StatementKind::ShellCmd { command }, span))
        }
    }

    /// Parses _SHELLHIDE statement.
    ///
    /// Syntax: `_SHELLHIDE command$`
    pub(super) fn parse_shellhide(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SHELLHIDE keyword").span.start;
        let command = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::ShellHide { command }, span))
    }

    // ==================== Memory Operations ====================

    /// Parses BLOAD statement.
    ///
    /// Syntax: `BLOAD filename$[, address]`
    pub(super) fn parse_bload(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("BLOAD keyword").span.start;
        let filename = self.parse_expression()?;

        let address = if self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Bload { filename, address },
            span,
        ))
    }

    /// Parses BSAVE statement.
    ///
    /// Syntax: `BSAVE filename$, address, length`
    pub(super) fn parse_bsave(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("BSAVE keyword").span.start;
        let filename = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let address = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let length = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Bsave {
                filename,
                address,
                length,
            },
            span,
        ))
    }

    /// Parses SETMEM statement.
    ///
    /// Syntax: `SETMEM bytes`
    pub(super) fn parse_setmem(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SETMEM keyword").span.start;
        let bytes = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Setmem { bytes }, span))
    }

    // ==================== Mouse Statements ====================

    /// Parses _MOUSEHIDE statement.
    ///
    /// Syntax: `_MOUSEHIDE`
    pub(super) fn parse_mousehide(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_MOUSEHIDE keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::MouseHide, span))
    }

    /// Parses _MOUSESHOW statement.
    ///
    /// Syntax: `_MOUSESHOW`
    pub(super) fn parse_mouseshow(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_MOUSESHOW keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::MouseShow, span))
    }

    /// Parses _MOUSEMOVE statement.
    ///
    /// Syntax: `_MOUSEMOVE x%, y%`
    pub(super) fn parse_mousemove(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_MOUSEMOVE keyword").span.start;
        let x = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let y = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::MouseMoveStmt { x, y }, span))
    }

    // ==================== Clipboard Statement ====================

    /// Parses _CLIPBOARD$ = text$ statement.
    ///
    /// Syntax: `_CLIPBOARD$ = text$`
    pub(super) fn parse_clipboard_set(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_CLIPBOARD$ keyword").span.start;
        self.expect(&TokenKind::Equals, "=")?;
        let text = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::ClipboardSet { text }, span))
    }
}
