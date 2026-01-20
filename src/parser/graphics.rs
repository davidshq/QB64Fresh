//! Graphics statement parsing for the parser.
//!
//! This module contains parsing for graphics-related statements:
//! - Screen management: SCREEN, CLS, COLOR, LOCATE, WIDTH
//! - Drawing primitives: PSET, PRESET, LINE, CIRCLE, PAINT, DRAW
//! - Viewport control: VIEW, WINDOW, PCOPY, PALETTE
//! - QB64 extensions: _DISPLAY, _FREEIMAGE, _PUTIMAGE, _SOURCE, _DEST, _PRINTSTRING, _AUTODISPLAY

use crate::ast::{ImageScaleMode, Statement, StatementKind, ViewCoords};
use crate::lexer::TokenKind;

use super::Parser;

impl<'a> Parser<'a> {
    // ==================== Screen Management ====================

    /// Parses SCREEN statement.
    ///
    /// Full QB45 syntax: `SCREEN [mode][,[colorswitch]][,[apage]][,[vpage]]`
    ///
    /// All arguments are optional, but if you want to specify later ones,
    /// you need the commas: `SCREEN , , 1, 0` (just page arguments)
    pub(super) fn parse_screen(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SCREEN keyword").span.start;

        // Parse mode (first argument) - may be omitted
        let mode = if !self.is_at_statement_end() && !self.check(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Parse colorswitch (second argument)
        let color_switch = if self.match_token(&TokenKind::Comma) {
            if !self.is_at_statement_end() && !self.check(&TokenKind::Comma) {
                Some(self.parse_expression()?)
            } else {
                None
            }
        } else {
            None
        };

        // Parse active page (third argument)
        let active_page = if self.match_token(&TokenKind::Comma) {
            if !self.is_at_statement_end() && !self.check(&TokenKind::Comma) {
                Some(self.parse_expression()?)
            } else {
                None
            }
        } else {
            None
        };

        // Parse visual page (fourth argument)
        let visual_page = if self.match_token(&TokenKind::Comma) {
            if !self.is_at_statement_end() {
                Some(self.parse_expression()?)
            } else {
                None
            }
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Screen {
                mode,
                color_switch,
                active_page,
                visual_page,
            },
            span,
        ))
    }

    /// Parses CLS statement.
    ///
    /// Syntax: `CLS [mode]`
    /// mode: 0=clear graphics and text, 1=clear graphics only, 2=clear text only
    pub(super) fn parse_cls(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CLS keyword").span.start;

        // Check if there's an optional mode argument (not at end of statement)
        let mode = if !self.is_at_statement_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Cls { mode }, span))
    }

    /// Parses COLOR statement.
    ///
    /// Syntax: `COLOR foreground[, background[, border]]`
    ///
    /// In text mode, the third parameter sets the border color (CGA/EGA legacy).
    pub(super) fn parse_color(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("COLOR keyword").span.start;
        let foreground = self.parse_expression()?;

        let background = if self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Third parameter is border color (text mode only, CGA/EGA legacy)
        let border = if background.is_some() && self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Color {
                foreground,
                background,
                border,
            },
            span,
        ))
    }

    /// Parses LOCATE statement.
    ///
    /// Syntax: `LOCATE [row][, col][, cursor][, start, stop]`
    ///
    /// All parameters are optional. A comma before an omitted parameter is still
    /// required if you want to specify parameters after it.
    pub(super) fn parse_locate(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("LOCATE keyword").span.start;

        // Parse optional row (may be empty if we see comma first)
        let row = if self.check(&TokenKind::Comma)
            || self.check(&TokenKind::Colon)
            || self.check(&TokenKind::Newline)
            || self.check(&TokenKind::Comment)
            || self.is_at_end()
        {
            None
        } else {
            Some(self.parse_expression()?)
        };

        // Parse optional column after comma
        let col = if self.match_token(&TokenKind::Comma) {
            if self.check(&TokenKind::Comma)
                || self.check(&TokenKind::Colon)
                || self.check(&TokenKind::Newline)
                || self.check(&TokenKind::Comment)
                || self.is_at_end()
            {
                None
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        // Skip remaining optional parameters (cursor, start, stop)
        while self.match_token(&TokenKind::Comma) {
            if !self.check(&TokenKind::Comma)
                && !self.check(&TokenKind::Colon)
                && !self.check(&TokenKind::Newline)
                && !self.check(&TokenKind::Comment)
                && !self.is_at_end()
            {
                let _ = self.parse_expression()?; // consume and discard
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Locate { row, col }, span))
    }

    /// Parses WIDTH statement.
    ///
    /// Syntax: `WIDTH columns[, rows]`
    pub(super) fn parse_width(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("WIDTH keyword").span.start;
        let columns = self.parse_expression()?;

        let rows = if self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Width { columns, rows }, span))
    }

    // ==================== Drawing Primitives ====================

    /// Parses PSET statement.
    ///
    /// Syntax: `PSET [STEP](x, y)[, color]`
    pub(super) fn parse_pset(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PSET keyword").span.start;

        // Check for STEP (relative coordinates)
        let step = self.match_token(&TokenKind::Step);

        self.expect(&TokenKind::LeftParen, "(")?;
        let x = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let y = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        let color = if self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Pset { step, x, y, color },
            span,
        ))
    }

    /// Parses PRESET statement.
    ///
    /// Syntax: `PRESET [STEP](x, y)`
    pub(super) fn parse_preset(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PRESET keyword").span.start;

        // Check for STEP (relative coordinates)
        let step = self.match_token(&TokenKind::Step);

        self.expect(&TokenKind::LeftParen, "(")?;
        let x = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let y = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Preset { step, x, y }, span))
    }

    /// Parses LINE statement for graphics.
    ///
    /// Syntax: `LINE [(x1, y1)]-(x2, y2)[, color][, B|BF]`
    ///
    /// Note: LINE INPUT is handled separately in parse_line_statement.
    pub(super) fn parse_line_graphics(&mut self, start: usize) -> Result<Statement, ()> {
        // Parse optional start point
        let (x1, y1) = if self.check(&TokenKind::LeftParen) {
            self.advance();
            let x1 = self.parse_expression()?;
            self.expect(&TokenKind::Comma, ",")?;
            let y1 = self.parse_expression()?;
            self.expect(&TokenKind::RightParen, ")")?;
            (Some(x1), Some(y1))
        } else {
            (None, None)
        };

        self.expect(&TokenKind::Minus, "-")?;

        let step2 = self.match_token(&TokenKind::Step);

        self.expect(&TokenKind::LeftParen, "(")?;
        let x2 = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let y2 = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        // Optional color
        let color = if self.match_token(&TokenKind::Comma) {
            if let Some(token) = self.peek() {
                if token.text.eq_ignore_ascii_case("B")
                    || token.text.eq_ignore_ascii_case("BF")
                    || token.kind == TokenKind::Comma
                {
                    None
                } else {
                    Some(self.parse_expression()?)
                }
            } else {
                None
            }
        } else {
            None
        };

        // Optional box style (B or BF)
        let box_style = if self.match_token(&TokenKind::Comma) || color.is_none() {
            if let Some(token) = self.peek() {
                if token.text.eq_ignore_ascii_case("BF") {
                    self.advance();
                    Some(true)
                } else if token.text.eq_ignore_ascii_case("B") {
                    self.advance();
                    Some(false)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Line {
                x1,
                y1,
                x2,
                y2,
                step2,
                color,
                box_style,
            },
            span,
        ))
    }

    /// Parses CIRCLE statement.
    ///
    /// Syntax: `CIRCLE [STEP](x, y), radius[, color][, , , , F]`
    pub(super) fn parse_circle(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CIRCLE keyword").span.start;

        // Check for STEP (relative coordinates)
        let step = self.match_token(&TokenKind::Step);

        self.expect(&TokenKind::LeftParen, "(")?;
        let x = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let y = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        self.expect(&TokenKind::Comma, ",")?;
        let radius = self.parse_expression()?;

        let color = if self.match_token(&TokenKind::Comma) {
            if self.check(&TokenKind::Comma) {
                None
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        // Check for F (filled)
        let mut filled = false;
        while self.match_token(&TokenKind::Comma) {
            if let Some(token) = self.peek() {
                if token.text.eq_ignore_ascii_case("F") {
                    self.advance();
                    filled = true;
                    break;
                } else if !self.check(&TokenKind::Comma) {
                    let _ = self.parse_expression();
                }
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Circle {
                step,
                x,
                y,
                radius,
                color,
                filled,
            },
            span,
        ))
    }

    /// Parses PAINT statement.
    ///
    /// Syntax: `PAINT [STEP](x, y)[, color][, border]`
    pub(super) fn parse_paint(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PAINT keyword").span.start;

        // Check for STEP (relative coordinates)
        let step = self.match_token(&TokenKind::Step);

        self.expect(&TokenKind::LeftParen, "(")?;
        let x = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let y = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        let color = if self.match_token(&TokenKind::Comma) {
            if self.check(&TokenKind::Comma) {
                None
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        let border = if self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Paint {
                step,
                x,
                y,
                color,
                border,
            },
            span,
        ))
    }

    /// Parses DRAW statement.
    ///
    /// Syntax: `DRAW commands$`
    pub(super) fn parse_draw(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("DRAW keyword").span.start;
        let commands = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::DrawCmd { commands }, span))
    }

    // ==================== Viewport Control ====================

    /// Parses VIEW statement.
    ///
    /// Syntax: `VIEW [[SCREEN] (x1, y1)-(x2, y2)[, color[, border]]]`
    pub(super) fn parse_view(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("VIEW keyword").span.start;

        // Check for VIEW PRINT (text viewport)
        if self.check(&TokenKind::Print) {
            return self.parse_view_print(start);
        }

        // Check for VIEW with no arguments (reset viewport)
        if self.is_at_end_of_statement() {
            let span = self.span_from(start);
            return Ok(Statement::new(
                StatementKind::View {
                    screen: false,
                    coords: None,
                    fill_color: None,
                    border_color: None,
                },
                span,
            ));
        }

        let screen = if self.check(&TokenKind::Screen) {
            self.advance();
            true
        } else {
            false
        };

        let coords = if self.check(&TokenKind::LeftParen) {
            Some(self.parse_view_coords()?)
        } else {
            None
        };

        let fill_color = if self.match_token(&TokenKind::Comma) {
            if self.check(&TokenKind::Comma) {
                None
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        let border_color = if self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::View {
                screen,
                coords,
                fill_color,
                border_color,
            },
            span,
        ))
    }

    /// Parses VIEW PRINT statement (text viewport).
    ///
    /// Syntax: `VIEW PRINT [topRow TO bottomRow]`
    fn parse_view_print(&mut self, start: usize) -> Result<Statement, ()> {
        self.advance(); // consume PRINT

        if self.is_at_end_of_statement() {
            let span = self.span_from(start);
            return Ok(Statement::new(
                StatementKind::ViewPrint {
                    top: None,
                    bottom: None,
                },
                span,
            ));
        }

        let top = self.parse_expression()?;
        self.expect(&TokenKind::To, "TO")?;
        let bottom = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::ViewPrint {
                top: Some(top),
                bottom: Some(bottom),
            },
            span,
        ))
    }

    /// Parses WINDOW statement.
    ///
    /// Syntax: `WINDOW [[SCREEN] (x1, y1)-(x2, y2)]`
    pub(super) fn parse_window(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("WINDOW keyword").span.start;

        if self.is_at_end_of_statement() {
            let span = self.span_from(start);
            return Ok(Statement::new(
                StatementKind::WindowCoords {
                    screen: false,
                    coords: None,
                },
                span,
            ));
        }

        let screen = if self.check(&TokenKind::Screen) {
            self.advance();
            true
        } else {
            false
        };

        let coords = if self.check(&TokenKind::LeftParen) {
            Some(self.parse_view_coords()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::WindowCoords { screen, coords },
            span,
        ))
    }

    /// Parses coordinate pair for VIEW/WINDOW: `(x1, y1)-(x2, y2)`
    pub(super) fn parse_view_coords(&mut self) -> Result<ViewCoords, ()> {
        self.expect(&TokenKind::LeftParen, "(")?;
        let x1 = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let y1 = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        self.expect(&TokenKind::Minus, "-")?;

        self.expect(&TokenKind::LeftParen, "(")?;
        let x2 = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let y2 = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        Ok(ViewCoords { x1, y1, x2, y2 })
    }

    /// Parses _DISPLAY statement.
    ///
    /// Syntax: `_DISPLAY`
    pub(super) fn parse_display(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_DISPLAY keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::GfxDisplay, span))
    }

    /// Parses PALETTE statement.
    ///
    /// Syntax: `PALETTE [attribute, color]`
    pub(super) fn parse_palette(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PALETTE keyword").span.start;

        let (attribute, color) = if self.is_at_end_of_statement() {
            (None, None)
        } else {
            let attr = self.parse_expression()?;
            self.expect(&TokenKind::Comma, ",")?;
            let col = self.parse_expression()?;
            (Some(attr), Some(col))
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Palette { attribute, color },
            span,
        ))
    }

    /// Parses PCOPY statement.
    ///
    /// Syntax: `PCOPY source%, dest%`
    pub(super) fn parse_pcopy(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PCOPY keyword").span.start;

        let source = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let dest = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Pcopy { source, dest }, span))
    }

    // ==================== QB64 Graphics Extensions ====================

    /// Parses _FREEIMAGE statement.
    ///
    /// Syntax: `_FREEIMAGE handle&`
    pub(super) fn parse_freeimage(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_FREEIMAGE keyword").span.start;
        let handle = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::FreeImage { handle }, span))
    }

    /// Parses _PUTIMAGE statement.
    ///
    /// Syntax: `_PUTIMAGE [(dx1,dy1)-(dx2,dy2)][, src&][, dest&][, (sx1,sy1)-(sx2,sy2)][, _SMOOTH|_STRETCH]`
    pub(super) fn parse_putimage(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_PUTIMAGE keyword").span.start;

        let dest_coords = if self.check(&TokenKind::LeftParen) {
            Some(Box::new(self.parse_view_coords()?))
        } else {
            None
        };

        let source = if self.match_token(&TokenKind::Comma) {
            if self.check(&TokenKind::Comma) || self.check(&TokenKind::LeftParen) {
                None
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        let dest = if self.match_token(&TokenKind::Comma) {
            if self.check(&TokenKind::Comma) || self.check(&TokenKind::LeftParen) {
                None
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        let source_coords = if self.match_token(&TokenKind::Comma) {
            if self.check(&TokenKind::LeftParen) {
                Some(Box::new(self.parse_view_coords()?))
            } else {
                None
            }
        } else {
            None
        };

        // Parse optional scale mode: SMOOTH or STRETCH
        // Note: QB64 uses _SMOOTH/_STRETCH but we recognize both with and without underscore
        let scale_mode = if self.match_token(&TokenKind::Comma) {
            if self.match_token(&TokenKind::Smooth) {
                ImageScaleMode::Smooth
            } else if self.match_token(&TokenKind::Stretch) {
                ImageScaleMode::Stretch
            } else {
                ImageScaleMode::Default
            }
        } else {
            ImageScaleMode::Default
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::PutImage {
                dest_coords,
                source,
                dest,
                source_coords,
                scale_mode,
            },
            span,
        ))
    }

    /// Parses _SOURCE statement.
    ///
    /// Syntax: `_SOURCE handle&`
    pub(super) fn parse_source(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SOURCE keyword").span.start;
        let handle = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::SourceImg { handle }, span))
    }

    /// Parses _DEST statement.
    ///
    /// Syntax: `_DEST handle&`
    pub(super) fn parse_dest(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_DEST keyword").span.start;
        let handle = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::DestImg { handle }, span))
    }

    /// Parses _PRINTSTRING statement.
    ///
    /// Syntax: `_PRINTSTRING (x, y), text$`
    pub(super) fn parse_printstring(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_PRINTSTRING keyword").span.start;

        self.expect(&TokenKind::LeftParen, "(")?;
        let x = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let y = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        self.expect(&TokenKind::Comma, ",")?;
        let text = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::PrintStringStmt { x, y, text },
            span,
        ))
    }

    /// Parses _AUTODISPLAY statement.
    ///
    /// Syntax: `_AUTODISPLAY {ON|OFF}` or just `_AUTODISPLAY` (defaults to ON)
    pub(super) fn parse_autodisplay(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_AUTODISPLAY keyword").span.start;

        let enabled = if let Some(token) = self.peek() {
            if token.text.eq_ignore_ascii_case("ON") {
                self.advance();
                true
            } else if token.text.eq_ignore_ascii_case("OFF") {
                self.advance();
                false
            } else {
                true
            }
        } else {
            true
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::AutoDisplay { enabled }, span))
    }
}
