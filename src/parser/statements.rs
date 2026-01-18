//! Statement parsing for the parser.
//!
//! This module contains the main statement dispatcher and parsing for
//! simple statements like PRINT, LET, DIM, CONST, GOTO, INPUT, etc.
//!
//! More complex statements (control flow, procedures, directives) are
//! handled in their respective modules.

use crate::ast::{
    ArrayDimension, CommonVariable, ContinueType, DataValue, ExitType, ExprKind, FileAccess,
    FileLock, FileMode, PrintItem, PrintSeparator, ResumeTarget, Span, Statement, StatementKind,
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
            TokenKind::Print => self.parse_print_or_file_print(),
            TokenKind::Input => self.parse_input_or_file_input(),
            TokenKind::Line => self.parse_line_statement(),

            // File I/O statements
            TokenKind::Open => self.parse_open(),
            TokenKind::Close => self.parse_close(),
            TokenKind::Write => self.parse_write(),
            TokenKind::Get => self.parse_get(),
            TokenKind::Put => self.parse_put(),
            TokenKind::Seek => self.parse_seek(),

            // Variable statements
            TokenKind::Let => self.parse_let_explicit(),
            TokenKind::Dim => self.parse_dim(),
            TokenKind::Redim => self.parse_redim(),
            TokenKind::Const => self.parse_const(),
            TokenKind::Swap => self.parse_swap(),
            TokenKind::Common => self.parse_common(),

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
            TokenKind::On => self.parse_on_statement(),

            // Error handling
            TokenKind::Resume => self.parse_resume(),
            TokenKind::ErrorKw => self.parse_error_stmt(),

            // DATA statements
            TokenKind::Data => self.parse_data(),
            TokenKind::Read => self.parse_read(),
            TokenKind::Restore => self.parse_restore(),

            // Procedure definitions (delegated to procedures.rs)
            TokenKind::Sub => self.parse_sub(),
            TokenKind::Function => self.parse_function(),
            TokenKind::Type => self.parse_type_definition(),
            TokenKind::Call => self.parse_call(),
            TokenKind::Def => self.parse_def_fn(),

            // Preprocessor directives (delegated to directives.rs)
            TokenKind::IncludeDirective => self.parse_include_directive(),
            TokenKind::MetaCommand => self.parse_meta_command(),

            // Other
            TokenKind::Comment => self.parse_comment(),
            TokenKind::RemComment => self.parse_rem_comment(),
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

    /// Parses a REM comment.
    pub(super) fn parse_rem_comment(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("REM comment token");
        let text = token.text.to_string();
        let span: Span = token.span.clone().into();
        Ok(Statement::new(StatementKind::Comment(text), span))
    }

    // ==================== PRINT with File Support ====================

    /// Parses PRINT or PRINT #filenum (file output).
    pub(super) fn parse_print_or_file_print(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PRINT keyword").span.start;

        // Check for file number: PRINT #filenum, ...
        if self.check(&TokenKind::Hash) {
            self.advance(); // consume #
            let file_num = self.parse_expression()?;
            self.expect(&TokenKind::Comma, "`,` after file number")?;
            return self.parse_file_print(start, file_num);
        }

        // Regular PRINT statement
        self.parse_print_items(start)
    }

    /// Parses the items in a PRINT statement (shared between PRINT and PRINT #).
    fn parse_print_items(&mut self, start: usize) -> Result<Statement, ()> {
        let mut values: Vec<PrintItem> = Vec::new();
        let mut newline = true;

        while !self.is_at_end()
            && !self.check(&TokenKind::Newline)
            && !self.check(&TokenKind::Colon)
        {
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

        if values.last().map(|v| v.separator) == Some(Some(PrintSeparator::Semicolon)) {
            newline = false;
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Print { values, newline },
            span,
        ))
    }

    /// Parses PRINT #filenum, items.
    fn parse_file_print(
        &mut self,
        start: usize,
        file_num: crate::ast::Expr,
    ) -> Result<Statement, ()> {
        let mut values: Vec<PrintItem> = Vec::new();
        let mut newline = true;

        while !self.is_at_end()
            && !self.check(&TokenKind::Newline)
            && !self.check(&TokenKind::Colon)
        {
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
    pub(super) fn parse_input_or_file_input(&mut self) -> Result<Statement, ()> {
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
    fn parse_console_input(&mut self, start: usize) -> Result<Statement, ()> {
        let mut prompt = None;
        let mut show_question_mark = true;

        if self.check(&TokenKind::StringLiteral) {
            let token = self.advance().expect("prompt string");
            let prompt_span: Span = token.span.clone().into();
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

    /// Parses INPUT #filenum, variables.
    fn parse_file_input(
        &mut self,
        start: usize,
        file_num: crate::ast::Expr,
    ) -> Result<Statement, ()> {
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
            StatementKind::FileInput {
                file_num,
                variables,
            },
            span,
        ))
    }

    // ==================== LINE Statement ====================

    /// Parses LINE INPUT or LINE INPUT #filenum.
    pub(super) fn parse_line_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("LINE keyword").span.start;

        // LINE INPUT expected
        if !self.match_token(&TokenKind::Input) {
            let span = self.span_from(start);
            self.errors
                .push(ParseError::syntax("expected INPUT after LINE", span));
            return Err(());
        }

        // Check for file number: LINE INPUT #filenum, ...
        if self.check(&TokenKind::Hash) {
            self.advance(); // consume #
            let file_num = self.parse_expression()?;
            self.expect(&TokenKind::Comma, "`,` after file number")?;

            let var_token = self.expect(&TokenKind::Identifier, "string variable")?;
            let variable = var_token.text.to_string();

            let span = self.span_from(start);
            return Ok(Statement::new(
                StatementKind::FileLineInput { file_num, variable },
                span,
            ));
        }

        // Console LINE INPUT
        let mut prompt = None;

        if self.check(&TokenKind::StringLiteral) {
            let token = self.advance().expect("prompt string");
            prompt = Some(token.text[1..token.text.len() - 1].to_string());
            self.expect(&TokenKind::Semicolon, "`;` after LINE INPUT prompt")?;
        }

        let var_token = self.expect(&TokenKind::Identifier, "string variable")?;
        let variable = var_token.text.to_string();

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::LineInput { prompt, variable },
            span,
        ))
    }

    // ==================== File I/O Statements ====================

    /// Parses an OPEN statement.
    ///
    /// Syntax: `OPEN filename FOR mode [ACCESS access] [lock] AS [#]filenum [LEN=reclen]`
    pub(super) fn parse_open(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("OPEN keyword").span.start;

        // Parse filename expression
        let filename = self.parse_expression()?;

        // Expect FOR keyword
        self.expect(&TokenKind::For, "FOR")?;

        // Parse file mode
        let mode = self.parse_file_mode()?;

        // Optional ACCESS clause
        let access = if self.match_token(&TokenKind::Access) {
            Some(self.parse_file_access()?)
        } else {
            None
        };

        // Optional lock mode (SHARED, LOCK READ, LOCK WRITE, LOCK READ WRITE)
        let lock = self.parse_file_lock()?;

        // Expect AS keyword
        self.expect(&TokenKind::As, "AS")?;

        // Optional # before file number
        self.match_token(&TokenKind::Hash);

        // Parse file number
        let file_num = self.parse_expression()?;

        // Optional LEN = reclen
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
    fn parse_file_lock(&mut self) -> Result<Option<FileLock>, ()> {
        if self.match_token(&TokenKind::Shared) {
            Ok(Some(FileLock::Shared))
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

        // Check if there are any file numbers
        if !self.is_at_end() && !self.check(&TokenKind::Newline) && !self.check(&TokenKind::Colon) {
            loop {
                // Optional # before file number
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

        // WRITE requires # for file operations
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
    /// Syntax: `GET [#]filenum, [position], variable`
    pub(super) fn parse_get(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("GET keyword").span.start;

        // Optional # before file number
        self.match_token(&TokenKind::Hash);

        let file_num = self.parse_expression()?;
        self.expect(&TokenKind::Comma, "`,` after file number")?;

        // Optional position (may be empty: GET #1, , var)
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

    /// Parses a PUT statement.
    ///
    /// Syntax: `PUT [#]filenum, [position], variable`
    pub(super) fn parse_put(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PUT keyword").span.start;

        // Optional # before file number
        self.match_token(&TokenKind::Hash);

        let file_num = self.parse_expression()?;
        self.expect(&TokenKind::Comma, "`,` after file number")?;

        // Optional position (may be empty: PUT #1, , var)
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
            StatementKind::FilePut {
                file_num,
                position,
                variable,
            },
            span,
        ))
    }

    /// Parses a SEEK statement.
    ///
    /// Syntax: `SEEK [#]filenum, position`
    pub(super) fn parse_seek(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SEEK keyword").span.start;

        // Optional # before file number
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

    // ==================== Error Handling Statements ====================

    /// Parses an ON statement (ON ERROR or ON...GOTO/GOSUB).
    ///
    /// Syntax: `ON ERROR GOTO label` or `ON ERROR RESUME NEXT`
    ///         `ON expr GOTO label1, label2, ...`
    ///         `ON expr GOSUB label1, label2, ...`
    pub(super) fn parse_on_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("ON keyword").span.start;

        // Check for ON ERROR
        if self.match_token(&TokenKind::ErrorKw) {
            return self.parse_on_error(start);
        }

        // ON expr GOTO/GOSUB
        let selector = self.parse_expression()?;

        if self.match_token(&TokenKind::Goto) {
            let targets = self.parse_label_list()?;
            let span = self.span_from(start);
            Ok(Statement::new(
                StatementKind::OnGoto { selector, targets },
                span,
            ))
        } else if self.match_token(&TokenKind::Gosub) {
            let targets = self.parse_label_list()?;
            let span = self.span_from(start);
            Ok(Statement::new(
                StatementKind::OnGosub { selector, targets },
                span,
            ))
        } else {
            let span = self.current_span();
            self.errors.push(ParseError::syntax(
                "expected GOTO or GOSUB after ON expression",
                span,
            ));
            Err(())
        }
    }

    /// Parses ON ERROR GOTO/RESUME.
    fn parse_on_error(&mut self, start: usize) -> Result<Statement, ()> {
        if self.match_token(&TokenKind::Goto) {
            // ON ERROR GOTO label or ON ERROR GOTO 0
            let target = if self.check(&TokenKind::IntegerLiteral) {
                let token = self.advance().expect("integer literal");
                token.text.to_string()
            } else {
                let token = self.expect(&TokenKind::Identifier, "label or 0")?;
                token.text.to_string()
            };

            let span = self.span_from(start);
            Ok(Statement::new(StatementKind::OnErrorGoto { target }, span))
        } else if self.match_token(&TokenKind::Resume) {
            // ON ERROR RESUME NEXT
            self.expect(&TokenKind::Next, "NEXT after RESUME")?;
            let span = self.span_from(start);
            Ok(Statement::new(StatementKind::OnErrorResumeNext, span))
        } else {
            let span = self.current_span();
            self.errors.push(ParseError::syntax(
                "expected GOTO or RESUME after ON ERROR",
                span,
            ));
            Err(())
        }
    }

    /// Parses a comma-separated list of labels.
    fn parse_label_list(&mut self) -> Result<Vec<String>, ()> {
        let mut labels = Vec::new();

        loop {
            let token = self.expect(&TokenKind::Identifier, "label")?;
            labels.push(token.text.to_string());

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(labels)
    }

    /// Parses a RESUME statement.
    ///
    /// Syntax: `RESUME` or `RESUME NEXT` or `RESUME label`
    pub(super) fn parse_resume(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RESUME keyword").span.start;

        let target = if self.match_token(&TokenKind::Next) {
            Some(ResumeTarget::Next)
        } else if self.check(&TokenKind::Identifier) {
            let token = self.advance().expect("label");
            Some(ResumeTarget::Label(token.text.to_string()))
        } else if self.check(&TokenKind::IntegerLiteral) {
            let token = self.advance().expect("line number");
            Some(ResumeTarget::Label(token.text.to_string()))
        } else {
            None // Plain RESUME - retry the statement that caused the error
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::ResumeStmt { target }, span))
    }

    /// Parses an ERROR statement.
    ///
    /// Syntax: `ERROR code`
    pub(super) fn parse_error_stmt(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("ERROR keyword").span.start;

        let code = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::ErrorStmt { code }, span))
    }

    // ==================== DEF FN ====================

    /// Parses a DEF FN statement.
    ///
    /// Syntax: `DEF FNname[(params)] = expression`
    pub(super) fn parse_def_fn(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("DEF keyword").span.start;

        // Expect FN keyword or identifier starting with FN
        let name = if self.match_token(&TokenKind::Fn) {
            // DEF FN name
            let name_token = self.expect(&TokenKind::Identifier, "function name")?;
            name_token.text.to_string()
        } else if self.check(&TokenKind::Identifier) {
            let token = self.advance().expect("identifier");
            let text = token.text.to_uppercase();
            if let Some(stripped) = text.strip_prefix("FN") {
                stripped.to_string()
            } else {
                let span: Span = token.span.clone().into();
                self.errors.push(ParseError::syntax(
                    "DEF function name must start with FN",
                    span,
                ));
                return Err(());
            }
        } else {
            let span = self.current_span();
            self.errors.push(ParseError::syntax(
                "expected FN or function name after DEF",
                span,
            ));
            return Err(());
        };

        // Optional parameters
        let params = if self.match_token(&TokenKind::LeftParen) {
            let p = self.parse_parameter_list()?;
            self.expect(&TokenKind::RightParen, ")")?;
            p
        } else {
            Vec::new()
        };

        // Expect = and expression
        self.expect(&TokenKind::Equals, "=")?;

        let body = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::DefFn { name, params, body },
            span,
        ))
    }

    // ==================== Variable/Scope Statements ====================

    /// Parses a COMMON statement.
    ///
    /// Syntax: `COMMON [SHARED] variable [, variable]...`
    pub(super) fn parse_common(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("COMMON keyword").span.start;

        let shared = self.match_token(&TokenKind::Shared);

        let mut variables = Vec::new();

        loop {
            let var = self.parse_common_variable()?;
            variables.push(var);

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::CommonStmt { shared, variables },
            span,
        ))
    }

    /// Parses a single variable in a COMMON statement.
    fn parse_common_variable(&mut self) -> Result<CommonVariable, ()> {
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

        Ok(CommonVariable {
            name,
            dimensions,
            type_spec,
        })
    }

    /// Parses a REDIM statement.
    ///
    /// Syntax: `REDIM [_PRESERVE] array(dimensions) [AS type]`
    pub(super) fn parse_redim(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("REDIM keyword").span.start;

        let preserve = self.match_token(&TokenKind::Preserve);

        let name_token = self.expect(&TokenKind::Identifier, "array name")?;
        let name = name_token.text.to_string();

        self.expect(&TokenKind::LeftParen, "(")?;
        let dimensions = self.parse_array_dimensions()?;
        self.expect(&TokenKind::RightParen, ")")?;

        let type_spec = if self.match_token(&TokenKind::As) {
            Some(self.parse_type_spec()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Redim {
                preserve,
                name,
                dimensions,
                type_spec,
            },
            span,
        ))
    }

    /// Returns the current token's span (for error reporting).
    fn current_span(&self) -> Span {
        self.peek()
            .map(|t| t.span.clone().into())
            .unwrap_or(Span::new(0, 0))
    }
}
