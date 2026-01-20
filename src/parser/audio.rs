//! Audio statement parsing for the parser.
//!
//! This module contains parsing for audio-related statements:
//! - Classic BASIC: BEEP, SOUND, PLAY
//! - QB64 extensions: _SNDCLOSE, _SNDPLAY, _SNDSTOP, _SNDPAUSE, _SNDLOOP, _SNDVOL, _SNDBAL, _SNDRAW

use crate::ast::{Statement, StatementKind};
use crate::lexer::TokenKind;

use super::Parser;

impl<'a> Parser<'a> {
    /// Parses BEEP statement.
    ///
    /// Syntax: `BEEP`
    pub(super) fn parse_beep(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("BEEP keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Beep, span))
    }

    /// Parses SOUND statement.
    ///
    /// Syntax: `SOUND frequency, duration`
    pub(super) fn parse_sound(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SOUND keyword").span.start;
        let frequency = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let duration = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::SoundStmt {
                frequency,
                duration,
            },
            span,
        ))
    }

    /// Parses PLAY statement.
    ///
    /// Syntax: `PLAY commands$`
    pub(super) fn parse_play(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PLAY keyword").span.start;
        let commands = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::PlayStmt { commands }, span))
    }

    /// Parses _SNDCLOSE statement.
    ///
    /// Syntax: `_SNDCLOSE handle&`
    pub(super) fn parse_sndclose(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SNDCLOSE keyword").span.start;
        let handle = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::SndClose { handle }, span))
    }

    /// Parses _SNDPLAY statement.
    ///
    /// Syntax: `_SNDPLAY handle&`
    pub(super) fn parse_sndplay(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SNDPLAY keyword").span.start;
        let handle = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::SndPlay { handle }, span))
    }

    /// Parses _SNDSTOP statement.
    ///
    /// Syntax: `_SNDSTOP handle&`
    pub(super) fn parse_sndstop(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SNDSTOP keyword").span.start;
        let handle = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::SndStop { handle }, span))
    }

    /// Parses _SNDPAUSE statement.
    ///
    /// Syntax: `_SNDPAUSE handle&`
    pub(super) fn parse_sndpause(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SNDPAUSE keyword").span.start;
        let handle = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::SndPause { handle }, span))
    }

    /// Parses _SNDLOOP statement.
    ///
    /// Syntax: `_SNDLOOP handle&`
    pub(super) fn parse_sndloop(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SNDLOOP keyword").span.start;
        let handle = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::SndLoop { handle }, span))
    }

    /// Parses _SNDVOL statement.
    ///
    /// Syntax: `_SNDVOL handle&, volume!`
    pub(super) fn parse_sndvol(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SNDVOL keyword").span.start;
        let handle = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let volume = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::SndVol { handle, volume },
            span,
        ))
    }

    /// Parses _SNDBAL statement.
    ///
    /// Syntax: `_SNDBAL handle&, [x!], [y!], [z!], [channel&]`
    pub(super) fn parse_sndbal(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SNDBAL keyword").span.start;
        let handle = self.parse_expression()?;

        // Parse optional parameters: x, y, z, channel
        let x = if self.match_token(&TokenKind::Comma) {
            // Could be empty (just comma) or an expression
            if self.check(&TokenKind::Comma) || self.is_at_end_of_statement() {
                None // Empty position
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        let y = if self.match_token(&TokenKind::Comma) {
            if self.check(&TokenKind::Comma) || self.is_at_end_of_statement() {
                None
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        let z = if self.match_token(&TokenKind::Comma) {
            if self.check(&TokenKind::Comma) || self.is_at_end_of_statement() {
                None
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        let channel = if self.match_token(&TokenKind::Comma) {
            if self.is_at_end_of_statement() {
                None
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::SndBal {
                handle,
                x,
                y,
                z,
                channel,
            },
            span,
        ))
    }

    /// Parses _SNDRAW statement.
    ///
    /// Syntax: `_SNDRAW sample!` or `_SNDRAW left!, right!`
    pub(super) fn parse_sndraw(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SNDRAW keyword").span.start;
        let left = self.parse_expression()?;

        let right = if self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::SndRaw { left, right }, span))
    }
}
