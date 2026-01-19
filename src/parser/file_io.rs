//! File I/O statement parsing for the parser.
//!
//! This module contains parsing for file I/O statements:
//! - File management: OPEN, CLOSE, SEEK
//! - Data transfer: GET, PUT, WRITE #
//! - Note: Graphics GET/PUT variants are also handled here since they share the same keywords

use crate::ast::{FileAccess, FileLock, FileMode, PutAction, Statement, StatementKind};
use crate::lexer::TokenKind;

use super::{ParseError, Parser};

impl<'a> Parser<'a> {
    /// Parses an OPEN statement.
    ///
    /// Syntax: `OPEN filename FOR mode [ACCESS access] [lock] AS [#]filenum [LEN=reclen]`
    pub(super) fn parse_open(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("OPEN keyword").span.start;

        let filename = self.parse_expression()?;
        self.expect(&TokenKind::For, "FOR")?;
        let mode = self.parse_file_mode()?;

        let access = if self.match_token(&TokenKind::Access) {
            Some(self.parse_file_access()?)
        } else {
            None
        };

        let lock = self.parse_file_lock()?;

        self.expect(&TokenKind::As, "AS")?;
        self.match_token(&TokenKind::Hash);

        let file_num = self.parse_expression()?;

        let record_len = if self.match_token(&TokenKind::Len) {
            self.expect(&TokenKind::Equals, "=")?;
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::OpenFile {
                filename,
                mode,
                access,
                lock,
                file_num,
                record_len,
            },
            span,
        ))
    }

    /// Parses the file mode (INPUT, OUTPUT, APPEND, BINARY, RANDOM).
    fn parse_file_mode(&mut self) -> Result<FileMode, ()> {
        if self.match_token(&TokenKind::Input) {
            Ok(FileMode::Input)
        } else if self.match_token(&TokenKind::Output) {
            Ok(FileMode::Output)
        } else if self.match_token(&TokenKind::Append) {
            Ok(FileMode::Append)
        } else if self.match_token(&TokenKind::Binary) {
            Ok(FileMode::Binary)
        } else if self.match_token(&TokenKind::Random) {
            Ok(FileMode::Random)
        } else {
            let span = self.current_span();
            self.errors.push(ParseError::syntax(
                "expected file mode (INPUT, OUTPUT, APPEND, BINARY, or RANDOM)",
                span,
            ));
            Err(())
        }
    }

    /// Parses the ACCESS mode (READ, WRITE, READ WRITE).
    fn parse_file_access(&mut self) -> Result<FileAccess, ()> {
        if self.match_token(&TokenKind::Read) {
            if self.match_token(&TokenKind::Write) {
                Ok(FileAccess::ReadWrite)
            } else {
                Ok(FileAccess::Read)
            }
        } else if self.match_token(&TokenKind::Write) {
            Ok(FileAccess::Write)
        } else {
            let span = self.current_span();
            self.errors.push(ParseError::syntax(
                "expected access mode (READ, WRITE, or READ WRITE)",
                span,
            ));
            Err(())
        }
    }

    /// Parses optional file lock mode.
    ///
    /// Supports:
    /// - SHARED - other processes can read and write
    /// - LOCK READ - other processes cannot read
    /// - LOCK WRITE - other processes cannot write
    /// - LOCK READ WRITE - exclusive access
    /// - ONLY - exclusive file access (QB4.5 syntax)
    fn parse_file_lock(&mut self) -> Result<Option<FileLock>, ()> {
        if self.match_token(&TokenKind::Shared) {
            Ok(Some(FileLock::Shared))
        } else if self.match_token(&TokenKind::Only) {
            // QB4.5 ONLY keyword - exclusive file access
            Ok(Some(FileLock::Only))
        } else if self.match_token(&TokenKind::Lock) {
            if self.match_token(&TokenKind::Read) {
                if self.match_token(&TokenKind::Write) {
                    Ok(Some(FileLock::LockReadWrite))
                } else {
                    Ok(Some(FileLock::LockRead))
                }
            } else if self.match_token(&TokenKind::Write) {
                Ok(Some(FileLock::LockWrite))
            } else {
                let span = self.current_span();
                self.errors.push(ParseError::syntax(
                    "expected READ or WRITE after LOCK",
                    span,
                ));
                Err(())
            }
        } else {
            Ok(None)
        }
    }

    /// Parses a CLOSE statement.
    ///
    /// Syntax: `CLOSE [[#]filenum [, [#]filenum]...]`
    pub(super) fn parse_close(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CLOSE keyword").span.start;

        let mut file_nums = Vec::new();

        if !self.is_at_end() && !self.check(&TokenKind::Newline) && !self.check(&TokenKind::Colon) {
            loop {
                self.match_token(&TokenKind::Hash);
                let file_num = self.parse_expression()?;
                file_nums.push(file_num);

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::CloseFile { file_nums }, span))
    }

    /// Parses a WRITE # statement.
    ///
    /// Syntax: `WRITE #filenum, [expression [, expression]...]`
    pub(super) fn parse_write(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("WRITE keyword").span.start;

        self.expect(&TokenKind::Hash, "`#` after WRITE")?;

        let file_num = self.parse_expression()?;
        self.expect(&TokenKind::Comma, "`,` after file number")?;

        let mut values = Vec::new();
        if !self.is_at_end() && !self.check(&TokenKind::Newline) && !self.check(&TokenKind::Colon) {
            loop {
                let expr = self.parse_expression()?;
                values.push(expr);

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::FileWrite { file_num, values },
            span,
        ))
    }

    /// Parses a GET statement.
    ///
    /// File syntax: `GET [#]filenum, [position], variable`
    /// Graphics syntax: `GET (x1, y1)-(x2, y2), array[(index)]`
    pub(super) fn parse_get(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("GET keyword").span.start;

        // Check if this is graphics GET (starts with parenthesis) or file GET
        if self.check(&TokenKind::LeftParen) {
            return self.parse_graphics_get(start);
        }

        // File GET
        self.match_token(&TokenKind::Hash);

        let file_num = self.parse_expression()?;
        self.expect(&TokenKind::Comma, "`,` after file number")?;

        let position = if self.check(&TokenKind::Comma) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.expect(&TokenKind::Comma, "`,` before variable")?;

        let var_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let variable = var_token.text.to_string();

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::FileGet {
                file_num,
                position,
                variable,
            },
            span,
        ))
    }

    /// Parses graphics GET: `GET (x1, y1)-(x2, y2), array[(index)]`
    fn parse_graphics_get(&mut self, start: usize) -> Result<Statement, ()> {
        self.expect(&TokenKind::LeftParen, "`(` for coordinates")?;
        let x1 = self.parse_expression()?;
        self.expect(&TokenKind::Comma, "`,` between x1 and y1")?;
        let y1 = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, "`)` after y1")?;

        self.expect(&TokenKind::Minus, "`-` between coordinate pairs")?;

        let step2 = self.match_token(&TokenKind::Step);

        self.expect(&TokenKind::LeftParen, "`(` for second coordinates")?;
        let x2 = self.parse_expression()?;
        self.expect(&TokenKind::Comma, "`,` between x2 and y2")?;
        let y2 = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, "`)` after y2")?;

        self.expect(&TokenKind::Comma, "`,` before array name")?;

        let array_token = self.expect(&TokenKind::Identifier, "array name")?;
        let array_name = array_token.text.to_string();

        let array_index = if self.match_token(&TokenKind::LeftParen) {
            let idx = self.parse_expression()?;
            self.expect(&TokenKind::RightParen, "`)` after array index")?;
            Some(idx)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::GraphicsGet {
                x1,
                y1,
                x2,
                y2,
                step2,
                array_name,
                array_index,
            },
            span,
        ))
    }

    /// Parses a PUT statement.
    ///
    /// File syntax: `PUT [#]filenum, [position], variable`
    /// Graphics syntax: `PUT (x, y), array[(index)][, action]`
    pub(super) fn parse_put(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PUT keyword").span.start;

        // Check if this is graphics PUT (starts with ( or STEP) or file PUT
        if self.check(&TokenKind::LeftParen) || self.check(&TokenKind::Step) {
            return self.parse_graphics_put(start);
        }

        // File PUT
        self.match_token(&TokenKind::Hash);

        let file_num = self.parse_expression()?;
        self.expect(&TokenKind::Comma, "`,` after file number")?;

        let position = if self.check(&TokenKind::Comma) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.expect(&TokenKind::Comma, "`,` before variable")?;

        let var_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let variable = var_token.text.to_string();

        let index = if self.match_token(&TokenKind::LeftParen) {
            let idx = self.parse_expression()?;
            self.expect(&TokenKind::RightParen, "`)` after array index")?;
            Some(idx)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::FilePut {
                file_num,
                position,
                variable,
                index,
            },
            span,
        ))
    }

    /// Parses graphics PUT: `PUT (x, y), array[(index)][, action]`
    fn parse_graphics_put(&mut self, start: usize) -> Result<Statement, ()> {
        let step = self.match_token(&TokenKind::Step);

        self.expect(&TokenKind::LeftParen, "`(` for coordinates")?;
        let x = self.parse_expression()?;
        self.expect(&TokenKind::Comma, "`,` between x and y")?;
        let y = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, "`)` after y")?;

        self.expect(&TokenKind::Comma, "`,` before array name")?;

        let array_token = self.expect(&TokenKind::Identifier, "array name")?;
        let array_name = array_token.text.to_string();

        let array_index = if self.match_token(&TokenKind::LeftParen) {
            let idx = self.parse_expression()?;
            self.expect(&TokenKind::RightParen, "`)` after array index")?;
            Some(idx)
        } else {
            None
        };

        let (clip, action, transparent_color) = if self.match_token(&TokenKind::Comma) {
            let clip = self.match_token(&TokenKind::Clip);

            let action = if self.match_token(&TokenKind::Pset) {
                PutAction::Pset
            } else if self.match_token(&TokenKind::Preset) {
                PutAction::Preset
            } else if self.match_token(&TokenKind::And) {
                PutAction::And
            } else if self.match_token(&TokenKind::Or) {
                PutAction::Or
            } else if self.match_token(&TokenKind::Xor) {
                PutAction::Xor
            } else {
                self.errors.push(ParseError::syntax(
                    "expected PUT action: PSET, PRESET, AND, OR, or XOR",
                    self.span_from(start),
                ));
                return Err(());
            };

            let transparent_color = if clip && self.match_token(&TokenKind::Comma) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            (clip, action, transparent_color)
        } else {
            (false, PutAction::default(), None)
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::GraphicsPut {
                x,
                y,
                step,
                array_name,
                array_index,
                clip,
                action,
                transparent_color,
            },
            span,
        ))
    }

    /// Parses a SEEK statement.
    ///
    /// Syntax: `SEEK [#]filenum, position`
    pub(super) fn parse_seek(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SEEK keyword").span.start;

        self.match_token(&TokenKind::Hash);

        let file_num = self.parse_expression()?;
        self.expect(&TokenKind::Comma, "`,` after file number")?;

        let position = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::FileSeek { file_num, position },
            span,
        ))
    }
}
