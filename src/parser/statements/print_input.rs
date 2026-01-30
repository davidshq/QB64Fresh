//! Print and Input statement parsing.
//!
//! This module handles parsing of all print and input related statements:
//! - PRINT and PRINT #filenum
//! - PRINT USING
//! - INPUT and INPUT #filenum
//! - LINE INPUT and LINE INPUT #filenum

use crate::ast::{Expr, PrintItem, PrintSeparator, Span, Statement, StatementKind};
use crate::lexer::TokenKind;

use crate::parser::{ParseError, Parser};

impl<'a> Parser<'a> {
    // ==================== PRINT with File Support ====================

    /// Parses PRINT, PRINT #filenum (file output), or PRINT USING (formatted).
    pub(in crate::parser) fn parse_print_or_file_print(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PRINT keyword").span.start;

        // Check for file number: PRINT #filenum, ...
        if self.check(&TokenKind::Hash) {
            self.advance(); // consume #
            let file_num = self.parse_expression()?;
            self.expect(&TokenKind::Comma, "`,` after file number")?;
            return self.parse_file_print(start, file_num);
        }

        // Check for PRINT USING format$; value1, value2, ...
        if self.check(&TokenKind::Using) {
            return self.parse_print_using(start);
        }

        // Regular PRINT statement
        self.parse_print_items(start)
    }

    /// Parses PRINT USING format$; value1, value2, ...
    fn parse_print_using(&mut self, start: usize) -> Result<Statement, ()> {
        self.advance(); // consume USING

        // Parse the format string
        let format = self.parse_expression()?;

        // Expect semicolon after format string
        self.expect(&TokenKind::Semicolon, "`;` after format string")?;

        // Parse values separated by commas or semicolons
        let mut values: Vec<Expr> = Vec::new();
        let mut newline = true;

        while !self.is_at_end()
            && !self.check(&TokenKind::Newline)
            && !self.check(&TokenKind::Colon)
        {
            let expr = self.parse_expression()?;
            values.push(expr);

            // Check for comma or semicolon separator
            if self.check(&TokenKind::Comma) {
                self.advance();
            } else if self.check(&TokenKind::Semicolon) {
                self.advance();
                // Trailing semicolon suppresses newline
                if self.is_at_end()
                    || self.check(&TokenKind::Newline)
                    || self.check(&TokenKind::Colon)
                {
                    newline = false;
                    break;
                }
            } else {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::PrintUsing {
                format,
                values,
                newline,
            },
            span,
        ))
    }

    /// Parses the items in a PRINT statement (shared between PRINT and PRINT #).
    fn parse_print_items(&mut self, start: usize) -> Result<Statement, ()> {
        let mut values: Vec<PrintItem> = Vec::new();
        let mut newline = true;

        while !self.is_at_end() && !self.is_print_terminator() {
            if self.check(&TokenKind::Semicolon) || self.check(&TokenKind::Comma) {
                let sep = if self.match_token(&TokenKind::Semicolon) {
                    PrintSeparator::Semicolon
                } else {
                    self.advance();
                    PrintSeparator::Comma
                };

                if let Some(last) = values.last_mut() {
                    last.separator = Some(sep);
                }

                if self.is_at_end() || self.is_print_terminator() {
                    newline = sep != PrintSeparator::Semicolon;
                    break;
                }
            }

            let expr = self.parse_expression()?;
            values.push(PrintItem {
                expr,
                separator: None,
            });
        }

        if values.last().map(|v| v.separator) == Some(Some(PrintSeparator::Semicolon)) {
            newline = false;
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Print { values, newline },
            span,
        ))
    }

    /// Checks if current token terminates a PRINT statement.
    /// This includes newlines, colons, comments, ELSE, and ELSEIF (for single-line IF...THEN...ELSEIF...ELSE).
    fn is_print_terminator(&self) -> bool {
        self.check(&TokenKind::Newline)
            || self.check(&TokenKind::Colon)
            || self.check(&TokenKind::ElseIf)
            || self.check(&TokenKind::Else)
            || self.check(&TokenKind::Comment)
    }

    /// Parses PRINT #filenum, items.
    fn parse_file_print(
        &mut self,
        start: usize,
        file_num: crate::ast::Expr,
    ) -> Result<Statement, ()> {
        let mut values: Vec<PrintItem> = Vec::new();
        let mut newline = true;

        while !self.is_at_end() && !self.is_print_terminator() {
            if self.check(&TokenKind::Semicolon) || self.check(&TokenKind::Comma) {
                let sep = if self.match_token(&TokenKind::Semicolon) {
                    PrintSeparator::Semicolon
                } else {
                    self.advance();
                    PrintSeparator::Comma
                };

                if let Some(last) = values.last_mut() {
                    last.separator = Some(sep);
                }

                if self.is_at_end() || self.is_print_terminator() {
                    newline = sep != PrintSeparator::Semicolon;
                    break;
                }
            }

            let expr = self.parse_expression()?;
            values.push(PrintItem {
                expr,
                separator: None,
            });
        }

        if values.last().map(|v| v.separator) == Some(Some(PrintSeparator::Semicolon)) {
            newline = false;
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::FilePrint {
                file_num,
                values,
                newline,
            },
            span,
        ))
    }

    // ==================== INPUT with File Support ====================

    /// Parses INPUT or INPUT #filenum (file input) or LINE INPUT.
    pub(in crate::parser) fn parse_input_or_file_input(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("INPUT keyword").span.start;

        // Check for file number: INPUT #filenum, ...
        if self.check(&TokenKind::Hash) {
            self.advance(); // consume #
            let file_num = self.parse_expression()?;
            self.expect(&TokenKind::Comma, "`,` after file number")?;
            return self.parse_file_input(start, file_num);
        }

        // Regular INPUT statement
        self.parse_console_input(start)
    }

    /// Parses a console INPUT statement (original logic).
    ///
    /// Syntax: `INPUT [;] ["prompt" {; | ,}] variable[, variable...]`
    ///
    /// The optional leading semicolon keeps cursor on same line after input.
    fn parse_console_input(&mut self, start: usize) -> Result<Statement, ()> {
        let mut prompt = None;
        let mut show_question_mark = true;

        // Check for leading semicolon (keep cursor on same line after input)
        let same_line = self.match_token(&TokenKind::Semicolon);

        if self.check(&TokenKind::StringLiteral) {
            let token = self.advance().expect("prompt string");
            let prompt_span: Span = token.span;
            prompt = Some(token.text[1..token.text.len() - 1].to_string());

            if self.match_token(&TokenKind::Semicolon) {
                show_question_mark = true;
            } else if self.match_token(&TokenKind::Comma) {
                show_question_mark = false;
            } else {
                self.errors.push(ParseError::syntax(
                    "expected `;` or `,` after INPUT prompt string",
                    prompt_span,
                ));
                return Err(());
            }
        }

        // Parse comma-separated input targets (variables, array elements, fields)
        let mut targets = Vec::new();
        loop {
            let target = self.parse_input_target()?;
            targets.push(target);

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Input {
                prompt,
                show_question_mark,
                same_line,
                targets,
            },
            span,
        ))
    }

    /// Parses INPUT #filenum, variables.
    fn parse_file_input(
        &mut self,
        start: usize,
        file_num: crate::ast::Expr,
    ) -> Result<Statement, ()> {
        let mut targets = Vec::new();
        loop {
            let target = self.parse_input_target()?;
            targets.push(target);

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::FileInput { file_num, targets },
            span,
        ))
    }

    /// Parses a single input target (variable, array element, or field access).
    pub(in crate::parser) fn parse_input_target(&mut self) -> Result<crate::ast::InputTarget, ()> {
        use crate::ast::InputTarget;

        let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let name = name_token.text.to_string();

        // Check for array subscript: name(indices)
        if self.match_token(&TokenKind::LeftParen) {
            let mut indices = Vec::new();
            loop {
                let idx = self.parse_expression()?;
                indices.push(idx);
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
            self.expect(&TokenKind::RightParen, ")")?;

            // Check for field access chain: .field.subfield...
            let mut fields = Vec::new();
            while self.match_token(&TokenKind::Dot) {
                let field_token = self.expect(&TokenKind::Identifier, "field name")?;
                fields.push(field_token.text.to_string());
            }

            if fields.is_empty() {
                Ok(InputTarget::ArrayElement { name, indices })
            } else {
                Ok(InputTarget::ArrayElementField {
                    name,
                    indices,
                    fields,
                })
            }
        } else if self.match_token(&TokenKind::Dot) {
            // Simple UDT field access: name.field
            let mut fields = Vec::new();
            let field_token = self.expect(&TokenKind::Identifier, "field name")?;
            fields.push(field_token.text.to_string());
            while self.match_token(&TokenKind::Dot) {
                let field_token = self.expect(&TokenKind::Identifier, "field name")?;
                fields.push(field_token.text.to_string());
            }
            Ok(InputTarget::Field { name, fields })
        } else {
            // Simple variable
            Ok(InputTarget::Variable(name))
        }
    }

    // ==================== LINE Statement ====================

    /// Parses LINE INPUT, LINE INPUT #filenum, or LINE graphics.
    pub(in crate::parser) fn parse_line_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("LINE keyword").span.start;

        // Check if this is LINE INPUT or LINE graphics
        if self.match_token(&TokenKind::Input) {
            // LINE INPUT - continue to file input handling below
        } else {
            // LINE graphics: LINE [(x1, y1)]-(x2, y2)[, color][, B|BF]
            return self.parse_line_graphics(start);
        }

        // Check for file number: LINE INPUT #filenum, ...
        if self.check(&TokenKind::Hash) {
            self.advance(); // consume #
            let file_num = self.parse_expression()?;
            self.expect(&TokenKind::Comma, "`,` after file number")?;

            // Parse target (variable or array element)
            let target = self.parse_input_target()?;

            let span = self.span_from(start);
            return Ok(Statement::new(
                StatementKind::FileLineInput { file_num, target },
                span,
            ));
        }

        // Console LINE INPUT
        // Optional leading semicolon suppresses newline after input
        // LINE INPUT ; "prompt"; var$
        let suppress_newline = self.match_token(&TokenKind::Semicolon);

        let mut prompt = None;

        if self.check(&TokenKind::StringLiteral) {
            let token = self.advance().expect("prompt string");
            prompt = Some(token.text[1..token.text.len() - 1].to_string());
            // QB allows either semicolon or comma after the prompt
            // LINE INPUT "prompt"; var$ OR LINE INPUT "prompt", var$
            if !self.match_token(&TokenKind::Semicolon) {
                self.expect(&TokenKind::Comma, "`;` or `,` after LINE INPUT prompt")?;
            }
        }

        // Parse target (variable or array element)
        let target = self.parse_input_target()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::LineInput {
                suppress_newline,
                prompt,
                target,
            },
            span,
        ))
    }

    /// Parses LPRINT statement (print to printer).
    ///
    /// Syntax: `LPRINT [expr {;|,} expr...]`
    pub(in crate::parser) fn parse_lprint(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("LPRINT keyword").span.start;

        let mut values = Vec::new();
        let mut newline = true;

        // Parse print items (similar to PRINT)
        while !self.is_at_statement_end() {
            let expr = self.parse_expression()?;

            let separator = if self.match_token(&TokenKind::Semicolon) {
                Some(PrintSeparator::Semicolon)
            } else if self.match_token(&TokenKind::Comma) {
                Some(PrintSeparator::Comma)
            } else {
                None
            };

            // Trailing separator suppresses newline
            if separator.is_some() && self.is_at_statement_end() {
                newline = false;
            }

            values.push(PrintItem { expr, separator });

            if separator.is_none() {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Lprint { values, newline },
            span,
        ))
    }
}
