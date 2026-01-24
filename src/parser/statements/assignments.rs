//! Assignment statement parsing.
//!
//! This module handles parsing of all assignment-related statements:
//! - Simple variable assignments: `x = 5`
//! - Array element assignments: `arr(i) = value`
//! - Field assignments: `record.field = value`
//! - MID$ statement: `MID$(str$, start, len) = value$`
//! - ASC statement: `ASC(str$, pos) = value`

use crate::ast::{Expr, ExprKind, Statement, StatementKind};
use crate::lexer::TokenKind;

use crate::parser::Parser;

impl<'a> Parser<'a> {
    // ==================== Assignment Statements ====================

    /// Parses a LET statement (explicit LET keyword).
    pub(in crate::parser) fn parse_let_explicit(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("LET keyword").span.start; // consume LET
        self.parse_assignment(start)
    }

    /// Parses an assignment statement (variable = expression) or array element assignment.
    ///
    /// Handles:
    /// - `variable = expression`
    /// - `array(index) = expression`
    /// - `array(i, j).field = expression`
    /// - `MID$(str$, start, len) = value$` (string manipulation)
    /// - `ASC(str$, position) = value` (character manipulation)
    pub(in crate::parser) fn parse_assignment(&mut self, start: usize) -> Result<Statement, ()> {
        let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let name = name_token.text.to_string();

        // Check if this is an array element assignment or special function assignment
        if self.check(&TokenKind::LeftParen) {
            // Check for MID$ statement: MID$(str$, start [, len]) = value$
            if name.eq_ignore_ascii_case("MID$") {
                return self.parse_mid_statement(start);
            }

            // Check for ASC statement: ASC(str$, position) = value
            // This modifies a character in a string by its ASCII value
            if name.eq_ignore_ascii_case("ASC") {
                return self.parse_asc_statement(start);
            }

            // Regular array assignment
            return self.parse_array_assignment_with_name(start, name);
        }

        self.expect(&TokenKind::Equals, "=")?;

        let value = self.parse_expression()?;
        let span = self.span_from(start);

        Ok(Statement::new(StatementKind::Let { name, value }, span))
    }

    /// Parses an array element assignment: `array(i, j, ...) = value`
    /// or array field assignment: `array(i).field = value`
    /// or MID$ statement: `MID$(str$, start, len) = value$`
    pub(in crate::parser) fn parse_array_assignment(
        &mut self,
        start: usize,
    ) -> Result<Statement, ()> {
        let name_token = self.expect(&TokenKind::Identifier, "array name")?;
        let name = name_token.text.to_string();

        // Check for MID$ statement: MID$(str$, start [, len]) = value$
        if name.eq_ignore_ascii_case("MID$") {
            return self.parse_mid_statement(start);
        }

        // Check for ASC statement: ASC(str$, position) = value
        // This modifies a character in a string by its ASCII value
        if name.eq_ignore_ascii_case("ASC") {
            return self.parse_asc_statement(start);
        }

        self.parse_array_assignment_with_name(start, name)
    }

    /// Parses a MID$ statement: `MID$(str$, start [, len]) = value$`
    /// This replaces a portion of the string with a new value.
    /// The target can be a simple variable, array element, or UDT field.
    fn parse_mid_statement(&mut self, start: usize) -> Result<Statement, ()> {
        self.expect(&TokenKind::LeftParen, "(")?;

        // Parse target string expression (variable, array element, or field access)
        // We need to be careful here because commas separate arguments,
        // so we parse the lvalue manually rather than using parse_expression.
        let target_token = self.expect(&TokenKind::Identifier, "string variable")?;
        let target_name = target_token.text.to_string();
        let target_start = target_token.span.start;

        // Check for array index: varname$(index, ...)
        let target = if self.match_token(&TokenKind::LeftParen) {
            let mut args = Vec::new();
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
            self.expect(&TokenKind::RightParen, "`)` after array indices")?;

            let array_span = self.span_from(target_start);
            let array_expr = Expr::new(
                ExprKind::FunctionCall {
                    name: target_name,
                    args,
                },
                array_span,
            );

            // Check for field access: arr$(i).field$
            if self.match_token(&TokenKind::Dot) {
                let field_token = self.expect(&TokenKind::Identifier, "field name")?;
                let field_name = field_token.text.to_string();
                let span = self.span_from(target_start);
                Expr::new(
                    ExprKind::FieldAccess {
                        object: Box::new(array_expr),
                        field: field_name,
                    },
                    span,
                )
            } else {
                array_expr
            }
        } else {
            // Simple variable
            let span = self.span_from(target_start);
            Expr::new(ExprKind::Identifier(target_name), span)
        };

        self.expect(&TokenKind::Comma, ",")?;

        // Parse start position
        let start_pos = self.parse_expression()?;

        // Optional length
        let length = if self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect(&TokenKind::RightParen, ")")?;
        self.expect(&TokenKind::Equals, "=")?;

        // Parse replacement value
        let value = self.parse_expression()?;
        let span = self.span_from(start);

        Ok(Statement::new(
            StatementKind::MidAssignment {
                target,
                start: start_pos,
                length,
                value,
            },
            span,
        ))
    }

    /// Parses an ASC statement: `ASC(str$, position) = value`
    /// This modifies a character in a string by setting its ASCII value.
    fn parse_asc_statement(&mut self, start: usize) -> Result<Statement, ()> {
        self.expect(&TokenKind::LeftParen, "(")?;

        // Parse target string expression
        let target_token = self.expect(&TokenKind::Identifier, "string variable")?;
        let target_name = target_token.text.to_string();
        let target_start = target_token.span.start;

        // Check for array index: varname$(index, ...)
        let target = if self.match_token(&TokenKind::LeftParen) {
            let mut args = Vec::new();
            loop {
                args.push(self.parse_expression()?);
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
            self.expect(&TokenKind::RightParen, "`)` after array indices")?;

            let array_span = self.span_from(target_start);
            Expr::new(
                ExprKind::FunctionCall {
                    name: target_name,
                    args,
                },
                array_span,
            )
        } else {
            // Simple variable
            let span = self.span_from(target_start);
            Expr::new(ExprKind::Identifier(target_name), span)
        };

        self.expect(&TokenKind::Comma, ",")?;

        // Parse position expression
        let position = self.parse_expression()?;

        self.expect(&TokenKind::RightParen, ")")?;
        self.expect(&TokenKind::Equals, "=")?;

        // Parse ASCII value
        let value = self.parse_expression()?;
        let span = self.span_from(start);

        Ok(Statement::new(
            StatementKind::AscAssignment {
                target,
                position,
                value,
            },
            span,
        ))
    }

    /// Parses array assignment when the name has already been consumed.
    pub(in crate::parser) fn parse_array_assignment_with_name(
        &mut self,
        start: usize,
        name: String,
    ) -> Result<Statement, ()> {
        self.expect(&TokenKind::LeftParen, "(")?;

        // Parse index expressions
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
        // Use expect_name to allow keywords as field names (e.g., .name, .type)
        let mut fields = Vec::new();
        while self.match_token(&TokenKind::Dot) {
            let field_token = self.expect_name("field name")?;
            fields.push(field_token.text.to_string());
        }

        self.expect(&TokenKind::Equals, "=")?;

        let value = self.parse_expression()?;
        let span = self.span_from(start);

        if fields.is_empty() {
            Ok(Statement::new(
                StatementKind::ArrayAssignment {
                    name,
                    indices,
                    value,
                },
                span,
            ))
        } else {
            Ok(Statement::new(
                StatementKind::ArrayFieldAssignment {
                    name,
                    indices,
                    fields,
                    value,
                },
                span,
            ))
        }
    }

    /// Checks if the current tokens form an array/field assignment pattern:
    /// - `id(...) = value` (array element assignment)
    /// - `id(...).field = value` (UDT array member assignment)
    /// - `id(...).field.subfield = value` (nested UDT member assignment)
    pub(in crate::parser) fn is_array_assignment(&self) -> bool {
        // The token immediately after the identifier MUST be LeftParen
        // This prevents matching `x = arr(1) = 5` as an array assignment
        // (where `arr(1) = 5` is actually a comparison expression)
        let pos = self.current + 1;
        if pos >= self.tokens.len() || self.tokens[pos].kind != TokenKind::LeftParen {
            return false;
        }

        // Now scan to find the matching close paren and check for = after it
        let mut depth = 0;
        let mut pos = self.current + 1;

        while pos < self.tokens.len() {
            match self.tokens[pos].kind {
                TokenKind::LeftParen => depth += 1,
                TokenKind::RightParen => {
                    depth -= 1;
                    if depth == 0 {
                        // After closing paren, check for = or .field chain followed by =
                        pos += 1;

                        // Skip any .field chains
                        // Field names can be keywords (e.g., .name, .type) so check is_name_kind
                        while pos + 1 < self.tokens.len()
                            && self.tokens[pos].kind == TokenKind::Dot
                            && Self::is_name_kind(&self.tokens[pos + 1].kind)
                        {
                            pos += 2; // skip . and field name
                        }

                        // Check if we now have =
                        if pos < self.tokens.len() {
                            return self.tokens[pos].kind == TokenKind::Equals;
                        }
                        return false;
                    }
                }
                TokenKind::Newline | TokenKind::Colon => return false,
                _ => {}
            }
            pos += 1;
        }
        false
    }

    /// Checks if the current tokens form a field assignment pattern:
    /// - `id.field = value`
    /// - `id.field.subfield = value`
    pub(in crate::parser) fn is_field_assignment(&self) -> bool {
        // Start at the identifier (current position)
        let mut pos = self.current;

        // Skip the initial identifier
        if pos >= self.tokens.len() || self.tokens[pos].kind != TokenKind::Identifier {
            return false;
        }
        pos += 1;

        // Now we expect one or more .field chains
        while pos + 1 < self.tokens.len()
            && self.tokens[pos].kind == TokenKind::Dot
            && Self::is_name_kind(&self.tokens[pos + 1].kind)
        {
            pos += 2; // skip . and field name
        }

        // Check if we have at least one .field and now have =
        pos > self.current + 1
            && pos < self.tokens.len()
            && self.tokens[pos].kind == TokenKind::Equals
    }

    /// Parses a field assignment: `id.field = value` or `id.field.subfield = value`
    pub(in crate::parser) fn parse_field_assignment(
        &mut self,
        start: usize,
    ) -> Result<Statement, ()> {
        // Parse the base identifier
        let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let name = name_token.text.to_string();

        // Collect field names
        let mut fields = Vec::new();
        while self.match_token(&TokenKind::Dot) {
            let field_token = self.expect_name("field name after `.`")?;
            fields.push(field_token.text.to_string());
        }

        // Expect = and value
        self.expect(&TokenKind::Equals, "=")?;
        let value = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::FieldAssignment {
                name,
                fields,
                value,
            },
            span,
        ))
    }
}
