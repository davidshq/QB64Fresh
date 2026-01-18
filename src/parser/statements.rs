//! Statement parsing for the parser.
//!
//! This module contains the main statement dispatcher and parsing for
//! simple statements like PRINT, LET, DIM, CONST, GOTO, INPUT, etc.
//!
//! More complex statements (control flow, procedures, directives) are
//! handled in their respective modules.

use crate::ast::{
    ArrayDimension, ContinueType, DataValue, ExitType, ExprKind, PrintItem, PrintSeparator, Span,
    Statement, StatementKind,
};
use crate::lexer::TokenKind;

use super::{ParseError, Parser};

impl<'a> Parser<'a> {
    // ==================== Statement Dispatcher ====================

    /// Parses a single statement.
    pub(super) fn parse_statement(&mut self) -> Result<Statement, ()> {
        let token = match self.peek() {
            Some(t) => t,
            None => {
                self.errors.push(ParseError::eof("statement"));
                return Err(());
            }
        };

        let _start = token.span.start;

        match &token.kind {
            // I/O statements
            TokenKind::Print => self.parse_print(),
            TokenKind::Input => self.parse_input(),

            // Variable statements
            TokenKind::Let => self.parse_let_explicit(),
            TokenKind::Dim => self.parse_dim(),
            TokenKind::Const => self.parse_const(),
            TokenKind::Swap => self.parse_swap(),

            // Control flow (delegated to control_flow.rs)
            TokenKind::If => self.parse_if(),
            TokenKind::Select => self.parse_select_case(),
            TokenKind::For => self.parse_for(),
            TokenKind::While => self.parse_while(),
            TokenKind::Do => self.parse_do_loop(),
            TokenKind::Goto => self.parse_goto(),
            TokenKind::Gosub => self.parse_gosub(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Exit => self.parse_exit(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::End => self.parse_end(),
            TokenKind::Stop => self.parse_stop(),

            // DATA statements
            TokenKind::Data => self.parse_data(),
            TokenKind::Read => self.parse_read(),
            TokenKind::Restore => self.parse_restore(),

            // Procedure definitions (delegated to procedures.rs)
            TokenKind::Sub => self.parse_sub(),
            TokenKind::Function => self.parse_function(),
            TokenKind::Type => self.parse_type_definition(),
            TokenKind::Call => self.parse_call(),

            // Preprocessor directives (delegated to directives.rs)
            TokenKind::IncludeDirective => self.parse_include_directive(),
            TokenKind::MetaCommand => self.parse_meta_command(),

            // Other
            TokenKind::Comment => self.parse_comment(),
            TokenKind::Identifier => self.parse_identifier_statement(),

            _ => {
                let span: Span = token.span.clone().into();
                self.errors.push(ParseError::InvalidStatement {
                    span,
                    message: format!("unexpected token {:?}", token.kind),
                });
                self.advance();
                Err(())
            }
        }
    }

    // ==================== PRINT Statement ====================

    /// Parses a PRINT statement.
    pub(super) fn parse_print(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PRINT keyword").span.start; // consume PRINT
        let mut values: Vec<PrintItem> = Vec::new();
        let mut newline = true;

        // Parse print items until end of statement
        while !self.is_at_end()
            && !self.check(&TokenKind::Newline)
            && !self.check(&TokenKind::Colon)
        {
            // Check for trailing separator
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

                // Check if this is a trailing separator
                if self.is_at_end()
                    || self.check(&TokenKind::Newline)
                    || self.check(&TokenKind::Colon)
                {
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

        // Check for trailing semicolon
        if values.last().map(|v| v.separator) == Some(Some(PrintSeparator::Semicolon)) {
            newline = false;
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Print { values, newline },
            span,
        ))
    }

    // ==================== Assignment Statements ====================

    /// Parses a LET statement (explicit LET keyword).
    fn parse_let_explicit(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("LET keyword").span.start; // consume LET
        self.parse_assignment(start)
    }

    /// Parses an assignment statement (variable = expression).
    pub(super) fn parse_assignment(&mut self, start: usize) -> Result<Statement, ()> {
        let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let name = name_token.text.to_string();

        self.expect(&TokenKind::Equals, "=")?;

        let value = self.parse_expression()?;
        let span = self.span_from(start);

        Ok(Statement::new(StatementKind::Let { name, value }, span))
    }

    /// Parses an array element assignment: `array(i, j, ...) = value`
    pub(super) fn parse_array_assignment(&mut self, start: usize) -> Result<Statement, ()> {
        let name_token = self.expect(&TokenKind::Identifier, "array name")?;
        let name = name_token.text.to_string();

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
        self.expect(&TokenKind::Equals, "=")?;

        let value = self.parse_expression()?;
        let span = self.span_from(start);

        Ok(Statement::new(
            StatementKind::ArrayAssignment {
                name,
                indices,
                value,
            },
            span,
        ))
    }

    /// Parses an identifier statement (assignment or procedure call).
    pub(super) fn parse_identifier_statement(&mut self) -> Result<Statement, ()> {
        let start = self.peek().expect("identifier token").span.start;

        // Look ahead to determine if this is assignment or call
        if let Some(next) = self.peek_ahead(1)
            && next.kind == TokenKind::Equals
        {
            return self.parse_assignment(start);
        }

        // Check for array assignment: identifier(...)  = value
        // We need to look for identifier followed by ( ... ) followed by =
        if let Some(next) = self.peek_ahead(1)
            && next.kind == TokenKind::LeftParen
        {
            // Try to find the matching ) and check if = follows
            if self.is_array_assignment() {
                return self.parse_array_assignment(start);
            }
        }

        // Otherwise, parse as a procedure call or expression statement
        let expr = self.parse_expression()?;
        let span = self.span_from(start);

        // If it's a function call, convert to Call statement
        if let ExprKind::FunctionCall { name, args } = expr.kind {
            Ok(Statement::new(StatementKind::Call { name, args }, span))
        } else if let ExprKind::Identifier(name) = expr.kind {
            // Could be a call with no arguments
            Ok(Statement::new(
                StatementKind::Call {
                    name,
                    args: Vec::new(),
                },
                span,
            ))
        } else {
            Ok(Statement::new(StatementKind::Expression(expr), span))
        }
    }

    /// Checks if the current tokens form an array assignment pattern: id(...) =
    fn is_array_assignment(&self) -> bool {
        // Start after the identifier (at position self.current + 1 should be LeftParen)
        let mut depth = 0;
        let mut pos = self.current + 1;

        while pos < self.tokens.len() {
            match self.tokens[pos].kind {
                TokenKind::LeftParen => depth += 1,
                TokenKind::RightParen => {
                    depth -= 1;
                    if depth == 0 {
                        // Check if next token is =
                        if pos + 1 < self.tokens.len() {
                            return self.tokens[pos + 1].kind == TokenKind::Equals;
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

    // ==================== DIM Statement ====================

    /// Parses a DIM statement.
    pub(super) fn parse_dim(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("DIM keyword").span.start; // consume DIM

        let shared = self.match_token(&TokenKind::Shared);

        let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let name = name_token.text.to_string();

        // Check for array dimensions
        let dimensions = if self.match_token(&TokenKind::LeftParen) {
            let dims = self.parse_array_dimensions()?;
            self.expect(&TokenKind::RightParen, ")")?;
            dims
        } else {
            Vec::new()
        };

        // Check for AS type
        let type_spec = if self.match_token(&TokenKind::As) {
            Some(self.parse_type_spec()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Dim {
                name,
                dimensions,
                type_spec,
                shared,
            },
            span,
        ))
    }

    /// Parses array dimensions.
    fn parse_array_dimensions(&mut self) -> Result<Vec<ArrayDimension>, ()> {
        let mut dims = Vec::new();

        loop {
            let first = self.parse_expression()?;

            let dim = if self.match_token(&TokenKind::To) {
                let upper = self.parse_expression()?;
                ArrayDimension {
                    lower: Some(first),
                    upper,
                }
            } else {
                ArrayDimension {
                    lower: None,
                    upper: first,
                }
            };

            dims.push(dim);

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(dims)
    }

    // ==================== CONST Statement ====================

    /// Parses a CONST statement.
    pub(super) fn parse_const(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CONST keyword").span.start; // consume CONST

        let name_token = self.expect(&TokenKind::Identifier, "constant name")?;
        let name = name_token.text.to_string();

        self.expect(&TokenKind::Equals, "=")?;

        let value = self.parse_expression()?;
        let span = self.span_from(start);

        Ok(Statement::new(StatementKind::Const { name, value }, span))
    }

    // ==================== INPUT Statement ====================

    /// Parses an INPUT statement.
    ///
    /// Syntax: `INPUT [;] ["prompt"{;|,}] variable[, variable...]`
    ///
    /// - Semicolon after prompt: shows "?" after the prompt
    /// - Comma after prompt: no "?" shown
    pub(super) fn parse_input(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("INPUT keyword").span.start; // consume INPUT

        let mut prompt = None;
        let mut show_question_mark = true;

        // Check for prompt string
        if self.check(&TokenKind::StringLiteral) {
            let token = self.advance().expect("prompt string");
            let prompt_span: Span = token.span.clone().into();
            prompt = Some(token.text[1..token.text.len() - 1].to_string());

            // Separator after prompt is REQUIRED per QB spec
            if self.match_token(&TokenKind::Semicolon) {
                show_question_mark = true;
            } else if self.match_token(&TokenKind::Comma) {
                show_question_mark = false;
            } else {
                // Missing separator is a syntax error
                self.errors.push(ParseError::syntax(
                    "expected `;` or `,` after INPUT prompt string",
                    prompt_span,
                ));
                return Err(());
            }
        }

        // Parse variable list
        let mut variables = Vec::new();
        loop {
            let var_token = self.expect(&TokenKind::Identifier, "variable name")?;
            variables.push(var_token.text.to_string());

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Input {
                prompt,
                show_question_mark,
                variables,
            },
            span,
        ))
    }

    // ==================== Simple Flow Control ====================

    /// Parses a GOTO statement.
    pub(super) fn parse_goto(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("GOTO keyword").span.start; // consume GOTO
        let target_token = self.expect(&TokenKind::Identifier, "label")?;
        let target = target_token.text.to_string();
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Goto { target }, span))
    }

    /// Parses a GOSUB statement.
    pub(super) fn parse_gosub(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("GOSUB keyword").span.start; // consume GOSUB
        let target_token = self.expect(&TokenKind::Identifier, "label")?;
        let target = target_token.text.to_string();
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Gosub { target }, span))
    }

    /// Parses a RETURN statement.
    pub(super) fn parse_return(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RETURN keyword").span.start; // consume RETURN
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Return, span))
    }

    /// Parses an EXIT statement.
    pub(super) fn parse_exit(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("EXIT keyword").span.start; // consume EXIT

        let token = match self.peek() {
            Some(t) => t,
            None => {
                self.errors
                    .push(ParseError::eof("FOR, WHILE, DO, SUB, or FUNCTION"));
                return Err(());
            }
        };

        // Extract values before borrowing self again
        let token_kind = token.kind.clone();
        let token_span: Span = token.span.clone().into();

        let exit_type = match &token_kind {
            TokenKind::For => ExitType::For,
            TokenKind::While => ExitType::While,
            TokenKind::Do => ExitType::Do,
            TokenKind::Sub => ExitType::Sub,
            TokenKind::Function => ExitType::Function,
            _ => {
                self.errors.push(ParseError::syntax(
                    format!(
                        "expected FOR, WHILE, DO, SUB, or FUNCTION after EXIT, found {:?}",
                        token_kind
                    ),
                    token_span,
                ));
                return Err(());
            }
        };

        self.advance();
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Exit { exit_type }, span))
    }

    /// Parses an END statement.
    pub(super) fn parse_end(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("END keyword").span.start; // consume END
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::End, span))
    }

    /// Parses a STOP statement.
    pub(super) fn parse_stop(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("STOP keyword").span.start; // consume STOP
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Stop, span))
    }

    /// Parses a SWAP statement.
    pub(super) fn parse_swap(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SWAP keyword").span.start; // consume SWAP

        // Parse first variable (as expression to allow array elements)
        let left = self.parse_expression()?;

        // Expect comma
        self.expect(&TokenKind::Comma, "`,` between SWAP variables")?;

        // Parse second variable
        let right = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Swap { left, right }, span))
    }

    /// Parses a _CONTINUE statement.
    pub(super) fn parse_continue(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_CONTINUE keyword").span.start; // consume _CONTINUE

        // Check for optional loop type specifier
        let continue_type = if self.match_token(&TokenKind::For) {
            ContinueType::For
        } else if self.match_token(&TokenKind::While) {
            ContinueType::While
        } else if self.match_token(&TokenKind::Do) {
            ContinueType::Do
        } else {
            // Default to innermost loop - use Do as a generic marker
            ContinueType::Do
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Continue { continue_type },
            span,
        ))
    }

    // ==================== DATA/READ/RESTORE ====================

    /// Parses a DATA statement.
    pub(super) fn parse_data(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("DATA keyword").span.start; // consume DATA

        let mut values = Vec::new();

        loop {
            // Parse a literal value (numeric or string)
            let value = self.parse_data_value()?;
            values.push(value);

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Data { values }, span))
    }

    /// Parses a single DATA value (numeric or string literal).
    fn parse_data_value(&mut self) -> Result<DataValue, ()> {
        // Check for negative number
        let negative = self.match_token(&TokenKind::Minus);

        if let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::IntegerLiteral => {
                    let token = self.advance().expect("integer literal");
                    let mut value: i64 = token
                        .text
                        .replace('_', "") // Allow underscores as digit separators
                        .parse()
                        .unwrap_or(0);
                    if negative {
                        value = -value;
                    }
                    Ok(DataValue::Integer(value))
                }
                TokenKind::FloatLiteral => {
                    let token = self.advance().expect("float literal");
                    let mut value: f64 = token.text.replace('_', "").parse().unwrap_or(0.0);
                    if negative {
                        value = -value;
                    }
                    Ok(DataValue::Float(value))
                }
                TokenKind::StringLiteral => {
                    if negative {
                        let span: Span = token.span.clone().into();
                        self.errors.push(ParseError::syntax(
                            "unexpected `-` before string literal",
                            span,
                        ));
                        return Err(());
                    }
                    let str_token = self.advance().expect("string literal");
                    // Remove surrounding quotes
                    let text = &str_token.text[1..str_token.text.len() - 1];
                    Ok(DataValue::String(text.to_string()))
                }
                // Unquoted strings in DATA - anything that's not a literal is treated as unquoted string
                TokenKind::Identifier => {
                    if negative {
                        let span: Span = token.span.clone().into();
                        self.errors.push(ParseError::syntax(
                            "unexpected `-` before identifier in DATA",
                            span,
                        ));
                        return Err(());
                    }
                    let id_token = self.advance().expect("identifier");
                    Ok(DataValue::String(id_token.text.to_string()))
                }
                _ => {
                    let span: Span = token.span.clone().into();
                    self.errors.push(ParseError::syntax(
                        "expected numeric or string literal in DATA statement",
                        span,
                    ));
                    Err(())
                }
            }
        } else {
            self.errors.push(ParseError::eof("DATA value"));
            Err(())
        }
    }

    /// Parses a READ statement.
    pub(super) fn parse_read(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("READ keyword").span.start; // consume READ

        let mut variables = Vec::new();

        loop {
            let var_token = self.expect(&TokenKind::Identifier, "variable name")?;
            variables.push(var_token.text.to_string());

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Read { variables }, span))
    }

    /// Parses a RESTORE statement.
    pub(super) fn parse_restore(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RESTORE keyword").span.start; // consume RESTORE

        // Optional label
        let label = if self.check(&TokenKind::Identifier) {
            let token = self.advance().expect("label");
            Some(token.text.to_string())
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Restore { label }, span))
    }

    // ==================== Other Statements ====================

    /// Parses a CALL statement.
    pub(super) fn parse_call(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CALL keyword").span.start; // consume CALL

        let name_token = self.expect(&TokenKind::Identifier, "procedure name")?;
        let name = name_token.text.to_string();

        let args = if self.match_token(&TokenKind::LeftParen) {
            let a = self.parse_argument_list()?;
            self.expect(&TokenKind::RightParen, ")")?;
            a
        } else {
            Vec::new()
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Call { name, args }, span))
    }

    /// Parses a comment.
    pub(super) fn parse_comment(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("comment token");
        let text = token.text.to_string();
        let span: Span = token.span.clone().into();
        Ok(Statement::new(StatementKind::Comment(text), span))
    }
}
