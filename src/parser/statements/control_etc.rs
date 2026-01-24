//! Control flow helpers and miscellaneous statements.
//!
//! This module contains:
//! - Control flow helpers (GOTO, GOSUB, RETURN, etc.)
//! - Error handling (ON ERROR, RESUME, ERROR)
//! - DEF FN and DEF SEG
//! - Line number and identifier statement parsing
//! - Event control statements (KEY, TIMER, etc.)
//! - Miscellaneous statements (OPTION BASE, SWAP, etc.)

use crate::ast::{
    AllowFullScreenMode, ContinueType, EventControlMode, ExitType, FieldSpec, Parameter,
    ResumeTarget, Span, Statement, StatementKind,
};
use crate::lexer::TokenKind;

use crate::parser::{ParseError, Parser};

impl<'a> Parser<'a> {
    // ==================== Line Number and Label Statements ====================

    /// Parses a line number at the start of a line.
    /// The line number becomes a label, then we parse the rest of the statement.
    pub(in crate::parser) fn parse_line_number_statement(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("line number");
        let start = token.span.start;

        // Create label from line number
        let label = token.text.to_string();
        let span = self.span_from(start);

        Ok(Statement::new(StatementKind::Label { name: label }, span))
    }

    /// Parses a statement that starts with an identifier.
    /// This could be: assignment, array assignment, field assignment, procedure call, or label.
    pub(in crate::parser) fn parse_identifier_statement(&mut self) -> Result<Statement, ()> {
        let token = self.peek().expect("identifier");
        let start = token.span.start;
        let name = token.text.to_string();

        // Special handling for _MEMPUT with AS type clause
        // Syntax: _MEMPUT mem, offset, value AS type
        if name.eq_ignore_ascii_case("_MEMPUT") {
            self.advance(); // consume _MEMPUT
            let mem = self.parse_expression()?;
            self.expect(&TokenKind::Comma, ",")?;
            let offset = self.parse_expression()?;
            self.expect(&TokenKind::Comma, ",")?;
            let value = self.parse_expression()?;

            // Check for AS type clause
            if self.match_token(&TokenKind::As) {
                let type_name = self.parse_type_name()?;
                let span = self.span_from(start);
                return Ok(Statement::new(
                    StatementKind::MemPutTyped {
                        mem,
                        offset,
                        value,
                        value_type: type_name,
                    },
                    span,
                ));
            } else {
                // Regular _MEMPUT without AS type - treat as function call
                let span = self.span_from(start);
                return Ok(Statement::new(
                    StatementKind::Call {
                        name,
                        args: vec![mem, offset, value],
                    },
                    span,
                ));
            }
        }

        // Check for label definition: `labelName:` at the start of a line
        // Labels are only valid at the START of a logical line (after newline, not after colon)
        if self.at_line_start
            && self
                .peek_ahead(1)
                .is_some_and(|t| t.kind == TokenKind::Colon)
        {
            self.advance(); // consume identifier
            self.advance(); // consume colon
            let span = self.span_from(start);
            return Ok(Statement::new(StatementKind::Label { name }, span));
        }

        // Check for field assignment: id.field = value
        if self.is_field_assignment() {
            return self.parse_field_assignment(start);
        }

        // Check for array/function-call pattern
        if self.is_array_assignment() {
            return self.parse_array_assignment(start);
        }

        // Check for simple assignment: `name = value`
        if self
            .peek_ahead(1)
            .is_some_and(|t| t.kind == TokenKind::Equals)
        {
            return self.parse_assignment(start);
        }

        // Must be a procedure call
        self.advance(); // consume identifier

        // Parse arguments in classic BASIC style: name arg1, arg2, ...
        // This handles both:
        //   SubName arg1, arg2         (unparenthesized args)
        //   SubName (arg1), (arg2)     (parenthesized args for BYVAL passing)
        //
        // Note: We don't use special handling for name(args) function-call style
        // because that conflicts with name (arg) where parens wrap the first arg.
        // The CALL statement handles explicit CALL name(args) syntax separately.
        let mut args = Vec::new();
        while !self.is_at_statement_end() && !self.check(&TokenKind::Else) {
            args.push(self.parse_expression()?);
            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Call { name, args }, span))
    }

    // ==================== Simple Control Flow ====================

    /// Parses a GOTO statement.
    pub(in crate::parser) fn parse_goto(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("GOTO keyword").span.start;
        let target = self.parse_label_target()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Goto { target }, span))
    }

    /// Parses a GOSUB statement.
    pub(in crate::parser) fn parse_gosub(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("GOSUB keyword").span.start;
        let target = self.parse_label_target()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Gosub { target }, span))
    }

    /// Parses a label target (identifier or line number).
    ///
    /// Also handles QB64 error handler modifiers:
    /// - `_NEWHANDLER label` - Push a new error handler
    /// - `_LASTHANDLER` - Pop to previous error handler
    pub(in crate::parser) fn parse_label_target(&mut self) -> Result<String, ()> {
        let token = match self.peek() {
            Some(t) => t,
            None => {
                self.errors.push(ParseError::eof("label or line number"));
                return Err(());
            }
        };

        match &token.kind {
            TokenKind::Identifier | TokenKind::IntegerLiteral | TokenKind::FloatLiteral => {
                let label = token.text.to_string();
                self.advance();

                // Check for QB64 error handler modifiers
                if label.eq_ignore_ascii_case("_NEWHANDLER") {
                    // _NEWHANDLER is followed by the actual label name
                    let actual_label = self.parse_label_target()?;
                    Ok(format!("_NEWHANDLER {}", actual_label))
                } else if label.eq_ignore_ascii_case("_LASTHANDLER") {
                    // _LASTHANDLER is a standalone target (restore previous handler)
                    Ok(label)
                } else {
                    Ok(label)
                }
            }
            _ => {
                let span: Span = token.span.clone().into();
                self.errors.push(ParseError::syntax(
                    format!("expected label or line number, found {:?}", token.kind),
                    span,
                ));
                Err(())
            }
        }
    }

    /// Parses a RETURN statement.
    pub(in crate::parser) fn parse_return(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RETURN keyword").span.start;

        // RETURN in BASIC doesn't take a label - it returns from GOSUB
        // If there's a label, that's actually a GOTO RETURN style (not standard)
        // For now, just parse as simple RETURN
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Return, span))
    }

    /// Parses an EXIT statement.
    pub(in crate::parser) fn parse_exit(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("EXIT keyword").span.start;

        // Get what we're exiting
        let token = match self.peek() {
            Some(t) => t,
            None => {
                self.errors.push(ParseError::eof("EXIT target"));
                return Err(());
            }
        };

        let exit_type = match &token.kind {
            TokenKind::For => {
                self.advance();
                ExitType::For
            }
            TokenKind::While => {
                self.advance();
                ExitType::While
            }
            TokenKind::Do => {
                self.advance();
                ExitType::Do
            }
            TokenKind::Sub => {
                self.advance();
                ExitType::Sub
            }
            TokenKind::Function => {
                self.advance();
                ExitType::Function
            }
            _ => {
                let span: Span = token.span.clone().into();
                self.errors.push(ParseError::syntax(
                    format!(
                        "expected FOR, WHILE, DO, SUB, or FUNCTION after EXIT, found {:?}",
                        token.kind
                    ),
                    span,
                ));
                return Err(());
            }
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Exit { exit_type }, span))
    }

    /// Parses a CONTINUE statement.
    pub(in crate::parser) fn parse_continue(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CONTINUE keyword").span.start;

        // Check for optional loop type
        let continue_type = if let Some(token) = self.peek() {
            match &token.kind {
                TokenKind::For => {
                    self.advance();
                    ContinueType::For
                }
                TokenKind::While => {
                    self.advance();
                    ContinueType::While
                }
                TokenKind::Do => {
                    self.advance();
                    ContinueType::Do
                }
                _ => {
                    // Bare _CONTINUE - continue innermost loop
                    ContinueType::Innermost
                }
            }
        } else {
            ContinueType::Innermost
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Continue { continue_type },
            span,
        ))
    }

    /// Parses an END statement.
    pub(in crate::parser) fn parse_end(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("END keyword").span.start;

        // Check for optional exit code: END exitcode
        let exit_code = if !self.is_at_statement_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::End { exit_code }, span))
    }

    /// Parses a STOP statement.
    pub(in crate::parser) fn parse_stop(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("STOP keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Stop, span))
    }

    /// Parses a SYSTEM statement.
    pub(in crate::parser) fn parse_system(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SYSTEM keyword").span.start;

        // Optional exit code
        let exit_code = if !self.is_at_statement_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::System { exit_code }, span))
    }

    /// Parses a SLEEP statement.
    pub(in crate::parser) fn parse_sleep(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SLEEP keyword").span.start;

        // Optional duration in seconds
        let seconds = if !self.is_at_statement_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Sleep { seconds }, span))
    }

    /// Parses a WAIT statement.
    pub(in crate::parser) fn parse_wait(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("WAIT keyword").span.start;
        let port = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let and_mask = self.parse_expression()?;

        // Optional XOR mask
        let xor_mask = if self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Wait {
                port,
                and_mask,
                xor_mask,
            },
            span,
        ))
    }

    /// Parses a POKE statement.
    pub(in crate::parser) fn parse_poke(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("POKE keyword").span.start;
        let address = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let value = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Poke { address, value }, span))
    }

    /// Parses a _DELAY statement.
    pub(in crate::parser) fn parse_delay(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_DELAY keyword").span.start;
        let seconds = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Delay { seconds }, span))
    }

    /// Parses a _LIMIT statement.
    pub(in crate::parser) fn parse_limit(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_LIMIT keyword").span.start;
        let fps = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Limit { fps }, span))
    }

    /// Parses an ERASE statement.
    pub(in crate::parser) fn parse_erase(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("ERASE keyword").span.start;

        let mut arrays = Vec::new();
        loop {
            let name_token = self.expect(&TokenKind::Identifier, "array name")?;
            arrays.push(name_token.text.to_string());

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Erase { arrays }, span))
    }

    /// Parses a _KEYCLEAR statement.
    pub(in crate::parser) fn parse_keyclear(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_KEYCLEAR keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::KeyClear, span))
    }

    /// Parses a SWAP statement.
    pub(in crate::parser) fn parse_swap(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SWAP keyword").span.start;
        let left = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let right = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Swap { left, right }, span))
    }

    // ==================== ON Statement ====================

    /// Parses ON statements: ON expr GOTO/GOSUB, ON ERROR, ON KEY, etc.
    pub(in crate::parser) fn parse_on_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("ON keyword").span.start;

        // Check for ON ERROR
        if self.check(&TokenKind::ErrorKw) {
            return self.parse_on_error(start);
        }

        // Check for ON STRIG
        if self.check(&TokenKind::Strig) {
            return self.parse_on_strig(start);
        }

        // Check for ON KEY
        if self.check(&TokenKind::Key) {
            return self.parse_on_key(start);
        }

        // Check for ON TIMER
        if self.check(&TokenKind::Timer) {
            return self.parse_on_timer(start);
        }

        // Check for ON UEVENT
        if self.check(&TokenKind::Uevent) {
            return self.parse_on_uevent(start);
        }

        // Must be ON expr GOTO/GOSUB
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
            let span = self.span_from(start);
            self.errors.push(ParseError::syntax(
                "expected GOTO or GOSUB after ON expression".to_string(),
                span,
            ));
            Err(())
        }
    }

    /// Parses ON ERROR statement.
    fn parse_on_error(&mut self, start: usize) -> Result<Statement, ()> {
        self.advance(); // consume ERROR

        if self.match_token(&TokenKind::Resume) {
            // ON ERROR RESUME NEXT
            self.expect(&TokenKind::Next, "NEXT after ON ERROR RESUME")?;
            let span = self.span_from(start);
            Ok(Statement::new(StatementKind::OnErrorResumeNext, span))
        } else {
            // ON ERROR GOTO label/line
            self.expect(&TokenKind::Goto, "GOTO after ON ERROR")?;
            let target = self.parse_label_target()?;
            let span = self.span_from(start);
            Ok(Statement::new(StatementKind::OnErrorGoto { target }, span))
        }
    }

    /// Parses ON STRIG statement.
    fn parse_on_strig(&mut self, start: usize) -> Result<Statement, ()> {
        self.advance(); // consume STRIG

        self.expect(&TokenKind::LeftParen, "(")?;
        let button_num = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        self.expect(&TokenKind::Gosub, "GOSUB")?;
        let target = self.parse_label_target()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::OnStrig { button_num, target },
            span,
        ))
    }

    /// Parses ON KEY statement.
    fn parse_on_key(&mut self, start: usize) -> Result<Statement, ()> {
        self.advance(); // consume KEY

        self.expect(&TokenKind::LeftParen, "(")?;
        let key_num = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        self.expect(&TokenKind::Gosub, "GOSUB")?;
        let target = self.parse_label_target()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::OnKey { key_num, target },
            span,
        ))
    }

    /// Parses ON TIMER statement.
    fn parse_on_timer(&mut self, start: usize) -> Result<Statement, ()> {
        self.advance(); // consume TIMER

        self.expect(&TokenKind::LeftParen, "(")?;
        let interval = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        self.expect(&TokenKind::Gosub, "GOSUB")?;
        let target = self.parse_label_target()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::OnTimer { interval, target },
            span,
        ))
    }

    /// Parses ON UEVENT statement.
    fn parse_on_uevent(&mut self, start: usize) -> Result<Statement, ()> {
        self.advance(); // consume UEVENT

        self.expect(&TokenKind::Gosub, "GOSUB")?;
        let target = self.parse_label_target()?;

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::OnUevent { target }, span))
    }

    /// Parses a comma-separated list of labels.
    pub(in crate::parser) fn parse_label_list(&mut self) -> Result<Vec<String>, ()> {
        let mut targets = Vec::new();

        loop {
            let target = self.parse_label_target()?;
            targets.push(target);

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(targets)
    }

    // ==================== Error Handling ====================

    /// Parses a RESUME statement.
    pub(in crate::parser) fn parse_resume(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RESUME keyword").span.start;

        let target = if self.match_token(&TokenKind::Next) {
            Some(ResumeTarget::Next)
        } else if !self.is_at_statement_end() {
            let label = self.parse_label_target()?;
            Some(ResumeTarget::Label(label))
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::ResumeStmt { target }, span))
    }

    /// Parses an ERROR statement (raises an error).
    pub(in crate::parser) fn parse_error_stmt(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("ERROR keyword").span.start;
        let code = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::ErrorStmt { code }, span))
    }

    // ==================== DEF FN and DEF SEG ====================

    /// Parses DEF FN or DEF SEG statement.
    pub(in crate::parser) fn parse_def_fn(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("DEF keyword").span.start;

        // Check for DEF SEG
        if self.check(&TokenKind::Seg) {
            return self.parse_def_seg(start);
        }

        // DEF FNname(params) = expression
        // The function name should start with FN
        let name_token = self.expect(&TokenKind::Identifier, "FN function name")?;
        let full_name = name_token.text.to_string();

        // Parse parameters
        let mut params: Vec<Parameter> = Vec::new();
        if self.match_token(&TokenKind::LeftParen) {
            loop {
                let param_token = self.expect(&TokenKind::Identifier, "parameter name")?;
                let param_name = param_token.text.to_string();

                // Infer type from suffix if present
                let type_spec = self.type_from_suffix(&param_name);

                params.push(Parameter {
                    name: param_name,
                    type_spec,
                    by_val: false,
                    is_array: false,
                });

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
            self.expect(&TokenKind::RightParen, ")")?;
        }

        self.expect(&TokenKind::Equals, "=")?;
        let body = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::DefFn {
                name: full_name,
                params,
                body,
            },
            span,
        ))
    }

    /// Parses DEF SEG [= segment].
    fn parse_def_seg(&mut self, start: usize) -> Result<Statement, ()> {
        self.advance(); // consume SEG

        // Optional segment assignment
        let segment = if self.match_token(&TokenKind::Equals) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::DefSeg { segment }, span))
    }

    // ==================== OPTION Statement ====================

    /// Parses an OPTION statement (currently only OPTION BASE).
    pub(in crate::parser) fn parse_option(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("OPTION keyword").span.start;

        // Currently only OPTION BASE is supported
        if self.check(&TokenKind::Base) {
            self.advance(); // consume BASE

            // Expect 0 or 1
            let token = match self.peek() {
                Some(t) => t,
                None => {
                    self.errors.push(ParseError::eof("OPTION BASE value"));
                    return Err(());
                }
            };

            if token.kind != TokenKind::IntegerLiteral {
                let span: Span = token.span.clone().into();
                self.errors.push(ParseError::syntax(
                    "OPTION BASE requires 0 or 1".to_string(),
                    span,
                ));
                return Err(());
            }

            let base: i32 = token.text.parse().unwrap_or(0);
            self.advance();

            if base != 0 && base != 1 {
                let span = self.span_from(start);
                self.errors.push(ParseError::syntax(
                    "OPTION BASE requires 0 or 1".to_string(),
                    span,
                ));
                return Err(());
            }

            let span = self.span_from(start);
            Ok(Statement::new(
                StatementKind::OptionBase { base: base as i64 },
                span,
            ))
        } else {
            let span = self.span_from(start);
            self.errors.push(ParseError::syntax(
                "expected BASE after OPTION".to_string(),
                span,
            ));
            Err(())
        }
    }

    // ==================== CALL Statement ====================

    /// Parses a CALL statement.
    pub(in crate::parser) fn parse_call(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CALL keyword").span.start;
        let name_token = self.expect(&TokenKind::Identifier, "procedure name")?;
        let name = name_token.text.to_string();

        // Parse arguments (parentheses optional for CALL)
        let args = if self.match_token(&TokenKind::LeftParen) {
            let mut args = Vec::new();
            if !self.check(&TokenKind::RightParen) {
                loop {
                    args.push(self.parse_expression()?);
                    if !self.match_token(&TokenKind::Comma) {
                        break;
                    }
                }
            }
            self.expect(&TokenKind::RightParen, ")")?;
            args
        } else {
            Vec::new()
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Call { name, args }, span))
    }

    /// Parses a CALLS statement (call with segment).
    pub(in crate::parser) fn parse_calls(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CALLS keyword").span.start;
        let name_token = self.expect(&TokenKind::Identifier, "procedure name")?;
        let name = name_token.text.to_string();

        let args = if self.match_token(&TokenKind::LeftParen) {
            let mut args = Vec::new();
            if !self.check(&TokenKind::RightParen) {
                loop {
                    args.push(self.parse_expression()?);
                    if !self.match_token(&TokenKind::Comma) {
                        break;
                    }
                }
            }
            self.expect(&TokenKind::RightParen, ")")?;
            args
        } else {
            Vec::new()
        };

        let span = self.span_from(start);
        // CALLS is legacy syntax - treat same as CALL
        Ok(Statement::new(StatementKind::Call { name, args }, span))
    }

    // ==================== Comment Statements ====================

    /// Parses a comment (').
    pub(in crate::parser) fn parse_comment(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("comment");
        let span: Span = token.span.clone().into();
        let text = token.text[1..].to_string(); // Remove leading '
        Ok(Statement::new(StatementKind::Comment(text), span))
    }

    /// Parses a REM comment.
    pub(in crate::parser) fn parse_rem_comment(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("REM comment");
        let span: Span = token.span.clone().into();
        // Remove "REM" prefix (case insensitive)
        let text = if token.text.len() > 3 {
            token.text[3..].trim_start().to_string()
        } else {
            String::new()
        };
        Ok(Statement::new(StatementKind::Comment(text), span))
    }

    // ==================== Phase 7 Additional Statements ====================

    /// Parses `RUN [target]`.
    pub(in crate::parser) fn parse_run(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RUN keyword").span.start;

        let target = if !self.is_at_statement_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Run { target }, span))
    }

    /// Parses `CHAIN filename$`.
    pub(in crate::parser) fn parse_chain(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CHAIN keyword").span.start;
        let filename = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Chain { filename }, span))
    }

    /// Parses `TRON`.
    pub(in crate::parser) fn parse_tron(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("TRON keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Tron, span))
    }

    /// Parses `TROFF`.
    pub(in crate::parser) fn parse_troff(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("TROFF keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Troff, span))
    }

    /// Parses `FILES [filespec$]`.
    pub(in crate::parser) fn parse_files(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("FILES keyword").span.start;

        let filespec = if !self.is_at_statement_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::FilesStmt { filespec }, span))
    }

    /// Parses `FIELD [#]filenum, width AS var$ [, width AS var$]...`.
    pub(in crate::parser) fn parse_field(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("FIELD keyword").span.start;

        // Optional # before file number
        self.match_token(&TokenKind::Hash);
        let file_num = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;

        let mut fields = Vec::new();
        loop {
            let width = self.parse_expression()?;
            self.expect(&TokenKind::As, "AS")?;
            let var_token = self.expect(&TokenKind::Identifier, "field variable")?;
            let variable = var_token.text.to_string();

            fields.push(FieldSpec { width, variable });

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::FieldStmt { file_num, fields },
            span,
        ))
    }

    /// Parses `LSET var$ = string$`.
    pub(in crate::parser) fn parse_lset(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("LSET keyword").span.start;
        let var_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let variable = var_token.text.to_string();
        self.expect(&TokenKind::Equals, "=")?;
        let value = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Lset { variable, value },
            span,
        ))
    }

    /// Parses `RSET var$ = string$`.
    pub(in crate::parser) fn parse_rset(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RSET keyword").span.start;
        let var_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let variable = var_token.text.to_string();
        self.expect(&TokenKind::Equals, "=")?;
        let value = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Rset { variable, value },
            span,
        ))
    }

    /// Parses KEY statement.
    pub(in crate::parser) fn parse_key_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("KEY keyword").span.start;

        // Check for KEY(n) ON|OFF|STOP form
        if self.match_token(&TokenKind::LeftParen) {
            let key_num = self.parse_expression()?;
            self.expect(&TokenKind::RightParen, ")")?;

            let mode = self.parse_event_control_mode()?;
            let span = self.span_from(start);
            return Ok(Statement::new(
                StatementKind::KeyControl { key_num, mode },
                span,
            ));
        }

        // Otherwise it's KEY n, string$
        let key_num = self.parse_expression()?;
        if self.match_token(&TokenKind::Comma) {
            let key_string = self.parse_expression()?;
            let span = self.span_from(start);
            return Ok(Statement::new(
                StatementKind::Call {
                    name: "KEY".to_string(),
                    args: vec![key_num, key_string],
                },
                span,
            ));
        }

        let span = self.span_from(start);
        self.error_at_span_msg(span, "expected '(' or key number");
        Err(())
    }

    /// Parses event control mode: ON, OFF, or STOP.
    pub(in crate::parser) fn parse_event_control_mode(&mut self) -> Result<EventControlMode, ()> {
        if self.check(&TokenKind::On) {
            self.advance();
            Ok(EventControlMode::On)
        } else if self.check(&TokenKind::Off) {
            self.advance();
            Ok(EventControlMode::Off)
        } else if self.check(&TokenKind::Stop) {
            self.advance();
            Ok(EventControlMode::Stop)
        } else {
            let span = self.current_span();
            self.error_at_span_msg(span, "expected ON, OFF, or STOP");
            Err(())
        }
    }

    /// Parses STRIG statement.
    pub(in crate::parser) fn parse_strig_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("STRIG keyword").span.start;

        self.expect(&TokenKind::LeftParen, "(")?;
        let button_num = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        let mode = self.parse_event_control_mode()?;
        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::StrigControl { button_num, mode },
            span,
        ))
    }

    /// Parses COM statement.
    pub(in crate::parser) fn parse_com_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("COM keyword").span.start;

        self.expect(&TokenKind::LeftParen, "(")?;
        let port_num = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        let mode = self.parse_event_control_mode()?;
        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::ComControl { port_num, mode },
            span,
        ))
    }

    /// Parses PEN statement.
    pub(in crate::parser) fn parse_pen_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("PEN keyword").span.start;

        let mode = self.parse_event_control_mode()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::PenControl { mode }, span))
    }

    /// Parses UEVENT statement.
    pub(in crate::parser) fn parse_uevent_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("UEVENT keyword").span.start;

        // Check if it's a control statement or a trigger
        if self.is_at_statement_end() {
            let span = self.span_from(start);
            return Ok(Statement::new(StatementKind::UeventTrigger, span));
        }

        let mode = self.parse_event_control_mode()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::UeventControl { mode }, span))
    }

    /// Parses SIGNAL statement.
    pub(in crate::parser) fn parse_signal_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SIGNAL keyword").span.start;

        self.expect(&TokenKind::LeftParen, "(")?;
        let signal_num = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;

        let mode = self.parse_event_control_mode()?;
        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::SignalControl { signal_num, mode },
            span,
        ))
    }

    /// Parses TIMER statement.
    pub(in crate::parser) fn parse_timer_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("TIMER keyword").span.start;

        let mode = self.parse_event_control_mode()?;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::TimerControl { mode }, span))
    }

    /// Parses OUT statement.
    pub(in crate::parser) fn parse_out_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("OUT keyword").span.start;

        let port = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let value = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::OutPort { port, value }, span))
    }

    /// Parses INTERRUPT statement.
    pub(in crate::parser) fn parse_interrupt_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("INTERRUPT keyword").span.start;

        let int_num = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let in_regs_token = self.expect(&TokenKind::Identifier, "input registers variable")?;
        let in_regs = in_regs_token.text.to_string();
        self.expect(&TokenKind::Comma, ",")?;
        let out_regs_token = self.expect(&TokenKind::Identifier, "output registers variable")?;
        let out_regs = out_regs_token.text.to_string();

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::InterruptStmt {
                int_num,
                in_regs,
                out_regs,
            },
            span,
        ))
    }

    /// Parses INTERRUPTX statement.
    pub(in crate::parser) fn parse_interruptx_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("INTERRUPTX keyword").span.start;

        let int_num = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let in_regs_token = self.expect(&TokenKind::Identifier, "input registers variable")?;
        let in_regs = in_regs_token.text.to_string();
        self.expect(&TokenKind::Comma, ",")?;
        let out_regs_token = self.expect(&TokenKind::Identifier, "output registers variable")?;
        let out_regs = out_regs_token.text.to_string();

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::InterruptXStmt {
                int_num,
                in_regs,
                out_regs,
            },
            span,
        ))
    }

    /// Parses IOCTL statement.
    pub(in crate::parser) fn parse_ioctl_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("IOCTL keyword").span.start;

        self.match_token(&TokenKind::Hash);

        let file_num = self.parse_expression()?;
        self.expect(&TokenKind::Comma, ",")?;
        let control_string = self.parse_expression()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::IoctlStmt {
                file_num,
                control_string,
            },
            span,
        ))
    }

    /// Parses FREE statement.
    pub(in crate::parser) fn parse_free_statement(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("FREE keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::FreeStmt, span))
    }

    /// Parses CLEAR statement.
    pub(in crate::parser) fn parse_clear(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CLEAR keyword").span.start;

        let stack_size = if !self.is_at_statement_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::ClearStmt { stack_size },
            span,
        ))
    }

    /// Parses RESET statement.
    pub(in crate::parser) fn parse_reset(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RESET keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::ResetStmt, span))
    }

    // ==================== Window/Desktop Statements ====================

    /// Parses _ALLOWFULLSCREEN statement.
    pub(in crate::parser) fn parse_allowfullscreen(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_ALLOWFULLSCREEN keyword").span.start;

        let mode = if self.check_identifier_text("_SQUAREPIXELS") {
            self.advance();
            AllowFullScreenMode::SquarePixels
        } else if self.check_identifier_text("_STRETCH") {
            self.advance();
            AllowFullScreenMode::Stretch
        } else if self.check_identifier_text("_ALL") {
            self.advance();
            AllowFullScreenMode::All
        } else if self.check_identifier_text("_OFF") {
            self.advance();
            AllowFullScreenMode::Off
        } else {
            AllowFullScreenMode::All
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::AllowFullScreenStmt { mode },
            span,
        ))
    }

    /// Parses _SCREENICON statement.
    pub(in crate::parser) fn parse_screenicon(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_SCREENICON keyword").span.start;
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::ScreenIconStmt, span))
    }

    /// Parses _CONSOLETITLE statement.
    pub(in crate::parser) fn parse_consoletitle(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_CONSOLETITLE keyword").span.start;
        let title = self.parse_expression()?;
        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::ConsoleTitleStmt { title },
            span,
        ))
    }

    /// Parses _CONSOLE statement.
    pub(in crate::parser) fn parse_console(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_CONSOLE keyword").span.start;

        let visible = if self.check(&TokenKind::On) {
            self.advance();
            true
        } else if self.check(&TokenKind::Off) {
            self.advance();
            false
        } else {
            true
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::ConsoleStmt { visible }, span))
    }

    /// Parses _ASSERT statement.
    pub(in crate::parser) fn parse_assert(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_ASSERT keyword").span.start;
        let condition = self.parse_expression()?;

        let message = if self.match_token(&TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::AssertStmt { condition, message },
            span,
        ))
    }

    // ==================== Helper Functions ====================

    /// Helper to check if current token is identifier with specific text (case-insensitive).
    pub(in crate::parser) fn check_identifier_text(&self, text: &str) -> bool {
        if let Some(token) = self.peek() {
            token.kind == TokenKind::Identifier && token.text.eq_ignore_ascii_case(text)
        } else {
            false
        }
    }

    /// Helper to record error.
    pub(in crate::parser) fn error_at_span_msg(&mut self, span: Span, message: &str) {
        self.errors.push(ParseError::InvalidStatement {
            span,
            message: message.to_string(),
        });
    }
}
