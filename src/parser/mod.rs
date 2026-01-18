//! Parser for QB64Fresh BASIC.
//!
//! The parser transforms a stream of tokens into an Abstract Syntax Tree (AST).
//! It uses recursive descent for statements and Pratt parsing (precedence climbing)
//! for expressions.
//!
//! # Example
//!
//! ```
//! use qb64fresh::lexer::lex;
//! use qb64fresh::parser::Parser;
//!
//! let source = r#"
//!     PRINT "Hello, World!"
//!     x = 1 + 2
//! "#;
//!
//! let tokens = lex(source);
//! let mut parser = Parser::new(&tokens);
//! let program = parser.parse().expect("parse failed");
//!
//! assert_eq!(program.statements.len(), 2);
//! ```
//!
//! # Error Recovery
//!
//! The parser attempts to recover from errors and continue parsing to report
//! multiple errors at once. This provides better feedback to users than stopping
//! at the first error.

mod error;

pub use error::ParseError;

use crate::ast::{
    ArrayDimension, BinaryOp, CaseClause, CaseCompareOp, CaseMatch, ContinueType, DataValue,
    DoCondition, ExitType, Expr, ExprKind, Parameter, PrintItem, PrintSeparator, Program, Span,
    Statement, StatementKind, TypeMember, TypeSpec, UnaryOp,
};
use crate::lexer::{Token, TokenKind};

/// Parser for BASIC source code.
///
/// The parser consumes a slice of tokens and produces an AST.
/// Errors are collected and returned at the end rather than failing immediately.
pub struct Parser<'a> {
    /// The tokens to parse.
    tokens: &'a [Token],
    /// Current position in the token stream.
    current: usize,
    /// Collected parse errors.
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser for the given tokens.
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    /// Parses the token stream into a program AST.
    ///
    /// Returns the program if successful, or the collected errors if parsing failed.
    pub fn parse(&mut self) -> Result<Program, Vec<ParseError>> {
        let statements = self.parse_program();

        if self.errors.is_empty() {
            Ok(Program::new(statements))
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Parses a complete program (sequence of statements).
    fn parse_program(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            // Skip any leading newlines
            self.skip_newlines();

            if self.is_at_end() {
                break;
            }

            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(()) => {
                    // Error already recorded; try to recover
                    self.synchronize();
                }
            }
        }

        statements
    }

    // ==================== Token Navigation ====================

    /// Returns the current token without consuming it.
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    /// Returns the kind of the current token.
    fn peek_kind(&self) -> Option<&TokenKind> {
        self.peek().map(|t| &t.kind)
    }

    /// Looks ahead n tokens (0 = current token).
    fn peek_ahead(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.current + n)
    }

    /// Consumes and returns the current token.
    fn advance(&mut self) -> Option<&Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens.get(self.current - 1)
    }

    /// Returns true if we've reached the end of the token stream.
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    /// Checks if the current token matches the expected kind.
    fn check(&self, kind: &TokenKind) -> bool {
        self.peek_kind() == Some(kind)
    }

    /// Checks if the current token is one of the expected kinds.
    #[allow(dead_code)]
    fn check_any(&self, kinds: &[TokenKind]) -> bool {
        self.peek_kind().is_some_and(|k| kinds.contains(k))
    }

    /// Consumes the current token if it matches, returns true if consumed.
    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Consumes the current token if it matches any of the kinds.
    #[allow(dead_code)]
    fn match_any(&mut self, kinds: &[TokenKind]) -> Option<TokenKind> {
        if let Some(kind) = self.peek_kind().cloned()
            && kinds.contains(&kind)
        {
            self.advance();
            return Some(kind);
        }
        None
    }

    /// Expects the current token to match, or records an error.
    fn expect(&mut self, kind: &TokenKind, expected_desc: &str) -> Result<&Token, ()> {
        if self.check(kind) {
            Ok(self.advance().expect("advance after check"))
        } else {
            let (found, span) = if let Some(token) = self.peek() {
                (format!("{:?}", token.kind), token.span.clone().into())
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
    fn skip_newlines(&mut self) {
        while self.check(&TokenKind::Newline) {
            self.advance();
        }
    }

    /// Attempts to recover from an error by skipping to a synchronization point.
    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            // Newline is a natural statement boundary
            if self.tokens.get(self.current - 1).map(|t| &t.kind) == Some(&TokenKind::Newline) {
                return;
            }

            // Statement-starting keywords are synchronization points
            match self.peek_kind() {
                Some(
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
                    | TokenKind::End,
                ) => return,
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Creates a span from start to current position.
    fn span_from(&self, start: usize) -> Span {
        let end = self
            .tokens
            .get(self.current.saturating_sub(1))
            .map(|t| t.span.end)
            .unwrap_or(start);
        Span::new(start, end)
    }

    // ==================== Expression Parsing (Pratt Parser) ====================

    /// Parses an expression.
    ///
    /// Returns `Err(())` on parse failure; actual errors are accumulated in `self.errors`.
    /// This pattern allows error recovery and reporting multiple errors.
    #[allow(clippy::result_unit_err)]
    pub fn parse_expression(&mut self) -> Result<Expr, ()> {
        self.parse_expr_precedence(Precedence::Lowest)
    }

    /// Parses an expression with the given minimum precedence.
    fn parse_expr_precedence(&mut self, min_prec: Precedence) -> Result<Expr, ()> {
        // Parse prefix (primary expression or unary operator)
        let mut left = self.parse_prefix()?;

        // Parse infix operators while they have sufficient precedence
        while let Some(token) = self.peek() {
            let op_prec = Self::get_precedence(&token.kind);
            if op_prec <= min_prec {
                break;
            }

            left = self.parse_infix(left, op_prec)?;
        }

        Ok(left)
    }

    /// Parses a prefix expression (literal, identifier, unary op, or grouped).
    fn parse_prefix(&mut self) -> Result<Expr, ()> {
        let token = match self.peek() {
            Some(t) => t,
            None => {
                self.errors.push(ParseError::eof("expression"));
                return Err(());
            }
        };

        let _start = token.span.start;

        match &token.kind {
            TokenKind::IntegerLiteral => self.parse_integer_literal(),
            TokenKind::FloatLiteral => self.parse_float_literal(),
            TokenKind::HexLiteral => self.parse_hex_literal(),
            TokenKind::OctalLiteral => self.parse_octal_literal(),
            TokenKind::BinaryLiteral => self.parse_binary_literal(),
            TokenKind::StringLiteral => self.parse_string_literal(),
            TokenKind::UnterminatedString => {
                let span = token.span.clone().into();
                self.advance(); // Consume the unterminated string token
                self.errors.push(ParseError::UnterminatedString { span });
                Err(())
            }
            TokenKind::Identifier => self.parse_identifier_or_call(),
            TokenKind::LeftParen => self.parse_grouped(),
            TokenKind::Minus => self.parse_unary(UnaryOp::Negate),
            TokenKind::Not => self.parse_unary(UnaryOp::Not),
            _ => {
                let span = token.span.clone().into();
                self.errors.push(ParseError::InvalidExpression {
                    span,
                    message: format!("unexpected token {:?}", token.kind),
                });
                self.advance();
                Err(())
            }
        }
    }

    /// Parses an infix expression (binary operation).
    fn parse_infix(&mut self, left: Expr, precedence: Precedence) -> Result<Expr, ()> {
        let op_token = match self.advance() {
            Some(t) => t,
            None => {
                self.errors.push(ParseError::eof("operator"));
                return Err(());
            }
        };

        // Extract values we need before borrowing self again
        let op_kind = op_token.kind.clone();
        let op_span: Span = op_token.span.clone().into();

        let op = match Self::token_to_binary_op(&op_kind) {
            Some(o) => o,
            None => {
                self.errors.push(ParseError::syntax(
                    format!("expected operator, found {:?}", op_kind),
                    op_span,
                ));
                return Err(());
            }
        };

        // For right-associative operators, use lower precedence for right side
        let right_prec = if op.is_right_associative() {
            Precedence::from_u8(precedence as u8 - 1)
        } else {
            precedence
        };

        let right = self.parse_expr_precedence(right_prec)?;

        let span = left.span.merge(&right.span);

        Ok(Expr::new(
            ExprKind::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            span,
        ))
    }

    /// Parses an integer literal.
    fn parse_integer_literal(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("integer literal token");
        let span: Span = token.span.clone().into();

        let value: i64 = token.text.parse().map_err(|e| {
            self.errors.push(ParseError::InvalidNumber {
                span,
                message: format!("invalid integer: {}", e),
            });
        })?;

        Ok(Expr::new(ExprKind::IntegerLiteral(value), span))
    }

    /// Parses a floating-point literal.
    fn parse_float_literal(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("float literal token");
        let span: Span = token.span.clone().into();

        // BASIC uses D for double exponents, convert to E for Rust parsing
        let text = token.text.replace('D', "E").replace('d', "e");
        let value: f64 = text.parse().map_err(|e| {
            self.errors.push(ParseError::InvalidNumber {
                span,
                message: format!("invalid float: {}", e),
            });
        })?;

        Ok(Expr::new(ExprKind::FloatLiteral(value), span))
    }

    /// Parses a hexadecimal literal (&HFF).
    fn parse_hex_literal(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("hex literal token");
        let span: Span = token.span.clone().into();

        // Skip the &H prefix
        let hex_str = &token.text[2..];
        let value = i64::from_str_radix(hex_str, 16).map_err(|e| {
            self.errors.push(ParseError::InvalidNumber {
                span,
                message: format!("invalid hex literal: {}", e),
            });
        })?;

        Ok(Expr::new(ExprKind::IntegerLiteral(value), span))
    }

    /// Parses an octal literal (&O77).
    fn parse_octal_literal(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("octal literal token");
        let span: Span = token.span.clone().into();

        // Skip the &O prefix
        let oct_str = &token.text[2..];
        let value = i64::from_str_radix(oct_str, 8).map_err(|e| {
            self.errors.push(ParseError::InvalidNumber {
                span,
                message: format!("invalid octal literal: {}", e),
            });
        })?;

        Ok(Expr::new(ExprKind::IntegerLiteral(value), span))
    }

    /// Parses a binary literal (&B1010).
    fn parse_binary_literal(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("binary literal token");
        let span: Span = token.span.clone().into();

        // Skip the &B prefix
        let bin_str = &token.text[2..];
        let value = i64::from_str_radix(bin_str, 2).map_err(|e| {
            self.errors.push(ParseError::InvalidNumber {
                span,
                message: format!("invalid binary literal: {}", e),
            });
        })?;

        Ok(Expr::new(ExprKind::IntegerLiteral(value), span))
    }

    /// Parses a string literal.
    ///
    /// # QBasic String Escape Semantics
    ///
    /// QBasic/QB64 does NOT support C-style escape sequences (`\n`, `\t`, etc.).
    /// The only escape sequence is a doubled quote (`""`) which represents a
    /// single literal quote character.
    ///
    /// Examples:
    /// - `"Hello World"` → `Hello World`
    /// - `"Say ""Hi""!"` → `Say "Hi"!`
    /// - `"Path\nName"` → `Path\nName` (literal backslash-n, not newline)
    ///
    /// To include special characters like newlines, QBasic programs use CHR$():
    /// - `"Line1" + CHR$(10) + "Line2"` for a string with embedded newline
    fn parse_string_literal(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("string literal token");
        let span: Span = token.span.clone().into();

        // Remove surrounding quotes and handle doubled-quote escape (QBasic style)
        // Note: Backslashes are literal characters, not escape introducers
        let text = &token.text;
        let inner = &text[1..text.len() - 1];
        let value = inner.replace("\"\"", "\"");

        Ok(Expr::new(ExprKind::StringLiteral(value), span))
    }

    /// Parses an identifier, function call, or field access chain.
    ///
    /// Handles:
    /// - Simple identifiers: `x`
    /// - Function calls / array access: `func(args)` / `arr(i)`
    /// - Field access: `obj.field`
    /// - Chained access: `obj.field.subfield`, `arr(i).field`
    fn parse_identifier_or_call(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("identifier token");
        let name = token.text.to_string();
        let start_span: Span = token.span.clone().into();

        // Check for function call (identifier followed by parenthesis)
        let mut expr = if self.check(&TokenKind::LeftParen) {
            self.advance(); // consume (
            let args = self.parse_argument_list()?;
            self.expect(&TokenKind::RightParen, ")")?;
            let span = self.span_from(start_span.start);
            Expr::new(ExprKind::FunctionCall { name, args }, span)
        } else {
            Expr::new(ExprKind::Identifier(name), start_span)
        };

        // Handle field access chain: obj.field.subfield
        while self.check(&TokenKind::Dot) {
            self.advance(); // consume .
            let field_token = self.expect(&TokenKind::Identifier, "field name after `.`")?;
            let field = field_token.text.to_string();
            let span = self.span_from(start_span.start);
            expr = Expr::new(
                ExprKind::FieldAccess {
                    object: Box::new(expr),
                    field,
                },
                span,
            );
        }

        Ok(expr)
    }

    /// Parses a parenthesized expression.
    fn parse_grouped(&mut self) -> Result<Expr, ()> {
        let start = self.advance().expect("left paren").span.start; // consume (
        let inner = self.parse_expression()?;
        self.expect(&TokenKind::RightParen, ")")?;
        let span = self.span_from(start);
        Ok(Expr::new(ExprKind::Grouped(Box::new(inner)), span))
    }

    /// Parses a unary expression.
    fn parse_unary(&mut self, op: UnaryOp) -> Result<Expr, ()> {
        let start = self.advance().expect("unary operator").span.start; // consume operator
        let operand = self.parse_expr_precedence(Precedence::Unary)?;
        let span = self.span_from(start);
        Ok(Expr::new(
            ExprKind::Unary {
                op,
                operand: Box::new(operand),
            },
            span,
        ))
    }

    /// Parses a comma-separated argument list.
    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ()> {
        let mut args = Vec::new();

        if !self.check(&TokenKind::RightParen) {
            args.push(self.parse_expression()?);

            while self.match_token(&TokenKind::Comma) {
                args.push(self.parse_expression()?);
            }
        }

        Ok(args)
    }

    /// Returns the precedence of a token (for infix operators).
    fn get_precedence(kind: &TokenKind) -> Precedence {
        match kind {
            TokenKind::Caret => Precedence::Power,
            TokenKind::Star | TokenKind::Slash | TokenKind::Backslash | TokenKind::Mod => {
                Precedence::Multiplicative
            }
            TokenKind::Plus | TokenKind::Minus => Precedence::Additive,
            TokenKind::Equals
            | TokenKind::NotEquals
            | TokenKind::NotEqualsLegacy
            | TokenKind::LessThan
            | TokenKind::LessEquals
            | TokenKind::LessEqualsLegacy
            | TokenKind::GreaterThan
            | TokenKind::GreaterEquals
            | TokenKind::GreaterEqualsLegacy => Precedence::Comparison,
            TokenKind::And => Precedence::And,
            TokenKind::Or | TokenKind::Xor => Precedence::Or,
            TokenKind::Eqv | TokenKind::Imp => Precedence::EqvImp,
            _ => Precedence::Lowest,
        }
    }

    /// Converts a token kind to a binary operator.
    fn token_to_binary_op(kind: &TokenKind) -> Option<BinaryOp> {
        match kind {
            TokenKind::Plus => Some(BinaryOp::Add),
            TokenKind::Minus => Some(BinaryOp::Subtract),
            TokenKind::Star => Some(BinaryOp::Multiply),
            TokenKind::Slash => Some(BinaryOp::Divide),
            TokenKind::Backslash => Some(BinaryOp::IntDivide),
            TokenKind::Mod => Some(BinaryOp::Modulo),
            TokenKind::Caret => Some(BinaryOp::Power),
            TokenKind::Equals => Some(BinaryOp::Equal),
            TokenKind::NotEquals | TokenKind::NotEqualsLegacy => Some(BinaryOp::NotEqual),
            TokenKind::LessThan => Some(BinaryOp::LessThan),
            TokenKind::LessEquals | TokenKind::LessEqualsLegacy => Some(BinaryOp::LessEqual),
            TokenKind::GreaterThan => Some(BinaryOp::GreaterThan),
            TokenKind::GreaterEquals | TokenKind::GreaterEqualsLegacy => {
                Some(BinaryOp::GreaterEqual)
            }
            TokenKind::And => Some(BinaryOp::And),
            TokenKind::Or => Some(BinaryOp::Or),
            TokenKind::Xor => Some(BinaryOp::Xor),
            TokenKind::Eqv => Some(BinaryOp::Eqv),
            TokenKind::Imp => Some(BinaryOp::Imp),
            _ => None,
        }
    }

    // ==================== Statement Parsing ====================

    /// Parses a single statement.
    fn parse_statement(&mut self) -> Result<Statement, ()> {
        let token = match self.peek() {
            Some(t) => t,
            None => {
                self.errors.push(ParseError::eof("statement"));
                return Err(());
            }
        };

        let _start = token.span.start;

        match &token.kind {
            TokenKind::Print => self.parse_print(),
            TokenKind::Let => self.parse_let_explicit(),
            TokenKind::Dim => self.parse_dim(),
            TokenKind::Const => self.parse_const(),
            TokenKind::If => self.parse_if(),
            TokenKind::Select => self.parse_select_case(),
            TokenKind::For => self.parse_for(),
            TokenKind::While => self.parse_while(),
            TokenKind::Do => self.parse_do_loop(),
            TokenKind::Goto => self.parse_goto(),
            TokenKind::Gosub => self.parse_gosub(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Exit => self.parse_exit(),
            TokenKind::End => self.parse_end(),
            TokenKind::Stop => self.parse_stop(),
            TokenKind::Input => self.parse_input(),
            TokenKind::Swap => self.parse_swap(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::Sub => self.parse_sub(),
            TokenKind::Function => self.parse_function(),
            TokenKind::Type => self.parse_type_definition(),
            TokenKind::Data => self.parse_data(),
            TokenKind::Read => self.parse_read(),
            TokenKind::Restore => self.parse_restore(),
            TokenKind::Comment => self.parse_comment(),
            TokenKind::Identifier => self.parse_identifier_statement(),
            TokenKind::Call => self.parse_call(),
            TokenKind::IncludeDirective => self.parse_include_directive(),
            TokenKind::MetaCommand => self.parse_meta_command(),
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

    /// Parses a PRINT statement.
    fn parse_print(&mut self) -> Result<Statement, ()> {
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

    /// Parses a LET statement (explicit LET keyword).
    fn parse_let_explicit(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("LET keyword").span.start; // consume LET
        self.parse_assignment(start)
    }

    /// Parses an assignment statement (variable = expression).
    fn parse_assignment(&mut self, start: usize) -> Result<Statement, ()> {
        let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let name = name_token.text.to_string();

        self.expect(&TokenKind::Equals, "=")?;

        let value = self.parse_expression()?;
        let span = self.span_from(start);

        Ok(Statement::new(StatementKind::Let { name, value }, span))
    }

    /// Parses an array element assignment: `array(i, j, ...) = value`
    fn parse_array_assignment(&mut self, start: usize) -> Result<Statement, ()> {
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
    fn parse_identifier_statement(&mut self) -> Result<Statement, ()> {
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

    /// Parses a DIM statement.
    fn parse_dim(&mut self) -> Result<Statement, ()> {
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

    /// Parses a type specification.
    fn parse_type_spec(&mut self) -> Result<TypeSpec, ()> {
        // Check for UNSIGNED prefix
        let unsigned = self.match_token(&TokenKind::Unsigned);

        let token = match self.peek() {
            Some(t) => t,
            None => {
                self.errors.push(ParseError::eof("type name"));
                return Err(());
            }
        };

        let base_type = match &token.kind {
            TokenKind::Integer => {
                self.advance();
                TypeSpec::Integer
            }
            TokenKind::Long => {
                self.advance();
                TypeSpec::Long
            }
            TokenKind::Single => {
                self.advance();
                TypeSpec::Single
            }
            TokenKind::Double => {
                self.advance();
                TypeSpec::Double
            }
            TokenKind::String_ => {
                self.advance();
                // Check for fixed-length string
                if self.match_token(&TokenKind::Star) {
                    let len_token = self.expect(&TokenKind::IntegerLiteral, "string length")?;
                    // Extract values before borrowing self again
                    let len_text = len_token.text.clone();
                    let len_span: Span = len_token.span.clone().into();
                    let len: usize = match len_text.parse() {
                        Ok(n) => n,
                        Err(_) => {
                            self.errors
                                .push(ParseError::syntax("invalid string length", len_span));
                            return Err(());
                        }
                    };
                    TypeSpec::FixedString(len)
                } else {
                    TypeSpec::String
                }
            }
            TokenKind::Byte => {
                self.advance();
                TypeSpec::Byte
            }
            TokenKind::Integer64 => {
                self.advance();
                TypeSpec::Integer64
            }
            TokenKind::Float => {
                self.advance();
                TypeSpec::Float
            }
            TokenKind::Identifier => {
                let name = token.text.to_string();
                self.advance();
                TypeSpec::UserDefined(name)
            }
            _ => {
                let span: Span = token.span.clone().into();
                self.errors.push(ParseError::syntax(
                    format!("expected type name, found {:?}", token.kind),
                    span,
                ));
                return Err(());
            }
        };

        if unsigned {
            Ok(TypeSpec::Unsigned(Box::new(base_type)))
        } else {
            Ok(base_type)
        }
    }

    /// Parses a CONST statement.
    fn parse_const(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CONST keyword").span.start; // consume CONST

        let name_token = self.expect(&TokenKind::Identifier, "constant name")?;
        let name = name_token.text.to_string();

        self.expect(&TokenKind::Equals, "=")?;

        let value = self.parse_expression()?;
        let span = self.span_from(start);

        Ok(Statement::new(StatementKind::Const { name, value }, span))
    }

    /// Parses an IF statement.
    fn parse_if(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("IF keyword").span.start; // consume IF

        let condition = self.parse_expression()?;
        self.expect(&TokenKind::Then, "THEN")?;

        // Check if this is single-line IF
        if !self.check(&TokenKind::Newline) && !self.is_at_end() {
            // Single-line IF
            let then_stmt = self.parse_statement()?;
            let else_branch = if self.match_token(&TokenKind::Else) {
                Some(vec![self.parse_statement()?])
            } else {
                None
            };

            let span = self.span_from(start);
            return Ok(Statement::new(
                StatementKind::If {
                    condition,
                    then_branch: vec![then_stmt],
                    elseif_branches: Vec::new(),
                    else_branch,
                },
                span,
            ));
        }

        // Multi-line IF
        self.skip_newlines();

        let mut then_branch = Vec::new();
        let mut elseif_branches = Vec::new();
        let mut else_branch = None;

        // Parse THEN branch
        while !self.is_at_end() {
            if self.check(&TokenKind::ElseIf) || self.check(&TokenKind::Else) || self.check_end_if()
            {
                break;
            }
            self.skip_newlines();
            if self.check(&TokenKind::ElseIf) || self.check(&TokenKind::Else) || self.check_end_if()
            {
                break;
            }
            then_branch.push(self.parse_statement()?);
            self.skip_newlines();
        }

        // Parse ELSEIF branches
        while self.match_token(&TokenKind::ElseIf) {
            let elseif_condition = self.parse_expression()?;
            self.expect(&TokenKind::Then, "THEN")?;
            self.skip_newlines();

            let mut elseif_body = Vec::new();
            while !self.is_at_end() {
                if self.check(&TokenKind::ElseIf)
                    || self.check(&TokenKind::Else)
                    || self.check_end_if()
                {
                    break;
                }
                self.skip_newlines();
                if self.check(&TokenKind::ElseIf)
                    || self.check(&TokenKind::Else)
                    || self.check_end_if()
                {
                    break;
                }
                elseif_body.push(self.parse_statement()?);
                self.skip_newlines();
            }
            elseif_branches.push((elseif_condition, elseif_body));
        }

        // Parse ELSE branch
        if self.match_token(&TokenKind::Else) {
            self.skip_newlines();
            let mut else_body = Vec::new();
            while !self.is_at_end() && !self.check_end_if() {
                self.skip_newlines();
                if self.check_end_if() {
                    break;
                }
                else_body.push(self.parse_statement()?);
                self.skip_newlines();
            }
            else_branch = Some(else_body);
        }

        // Expect END IF
        self.expect_end_if()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::If {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            },
            span,
        ))
    }

    /// Checks for END IF (handles both "END IF" and "ENDIF").
    fn check_end_if(&self) -> bool {
        if self.check(&TokenKind::End)
            && let Some(next) = self.peek_ahead(1)
        {
            return next.kind == TokenKind::If;
        }
        false
    }

    /// Expects END IF.
    fn expect_end_if(&mut self) -> Result<(), ()> {
        self.expect(&TokenKind::End, "END")?;
        self.expect(&TokenKind::If, "IF")?;
        Ok(())
    }

    /// Parses a SELECT CASE statement.
    fn parse_select_case(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SELECT keyword").span.start; // consume SELECT
        self.expect(&TokenKind::Case, "CASE")?;

        let test_expr = self.parse_expression()?;
        self.skip_newlines();

        let mut cases = Vec::new();
        let mut case_else = None;

        while !self.is_at_end() {
            self.skip_newlines();

            if self.check(&TokenKind::End) {
                break;
            }

            if !self.match_token(&TokenKind::Case) {
                break;
            }

            // Check for CASE ELSE
            if self.match_token(&TokenKind::Else) {
                let mut body = Vec::new();
                self.skip_newlines();
                while !self.is_at_end()
                    && !self.check(&TokenKind::End)
                    && !self.check(&TokenKind::Case)
                {
                    body.push(self.parse_statement()?);
                    self.skip_newlines();
                }
                case_else = Some(body);
                break;
            }

            // Parse CASE matches
            let matches = self.parse_case_matches()?;
            self.skip_newlines();

            let mut body = Vec::new();
            while !self.is_at_end() && !self.check(&TokenKind::Case) && !self.check(&TokenKind::End)
            {
                body.push(self.parse_statement()?);
                self.skip_newlines();
            }

            cases.push(CaseClause { matches, body });
        }

        self.expect(&TokenKind::End, "END")?;
        self.expect(&TokenKind::Select, "SELECT")?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::SelectCase {
                test_expr,
                cases,
                case_else,
            },
            span,
        ))
    }

    /// Parses CASE match expressions.
    fn parse_case_matches(&mut self) -> Result<Vec<CaseMatch>, ()> {
        let mut matches = Vec::new();

        loop {
            // Check for CASE IS comparison
            if self.match_token(&TokenKind::Is) {
                let op = self.parse_case_compare_op()?;
                let value = self.parse_expression()?;
                matches.push(CaseMatch::Comparison { op, value });
            } else {
                let first = self.parse_expression()?;

                // Check for range (TO)
                if self.match_token(&TokenKind::To) {
                    let to = self.parse_expression()?;
                    matches.push(CaseMatch::Range { from: first, to });
                } else {
                    matches.push(CaseMatch::Single(first));
                }
            }

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(matches)
    }

    /// Parses a comparison operator for CASE IS.
    fn parse_case_compare_op(&mut self) -> Result<CaseCompareOp, ()> {
        let token = match self.peek() {
            Some(t) => t,
            None => {
                self.errors.push(ParseError::eof("comparison operator"));
                return Err(());
            }
        };

        // Extract values before borrowing self again
        let token_kind = token.kind.clone();
        let token_span: Span = token.span.clone().into();

        let op = match &token_kind {
            TokenKind::Equals => CaseCompareOp::Equal,
            TokenKind::NotEquals => CaseCompareOp::NotEqual,
            TokenKind::LessThan => CaseCompareOp::LessThan,
            TokenKind::LessEquals => CaseCompareOp::LessEqual,
            TokenKind::GreaterThan => CaseCompareOp::GreaterThan,
            TokenKind::GreaterEquals => CaseCompareOp::GreaterEqual,
            _ => {
                self.errors.push(ParseError::syntax(
                    format!("expected comparison operator, found {:?}", token_kind),
                    token_span,
                ));
                return Err(());
            }
        };

        self.advance();
        Ok(op)
    }

    /// Parses a FOR loop.
    fn parse_for(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("FOR keyword").span.start; // consume FOR

        let var_token = self.expect(&TokenKind::Identifier, "loop variable")?;
        let variable = var_token.text.to_string();

        self.expect(&TokenKind::Equals, "=")?;
        let start_expr = self.parse_expression()?;

        self.expect(&TokenKind::To, "TO")?;
        let end_expr = self.parse_expression()?;

        let step = if self.match_token(&TokenKind::Step) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.skip_newlines();

        // Parse body until NEXT
        let mut body = Vec::new();
        while !self.is_at_end() && !self.check(&TokenKind::Next) {
            self.skip_newlines();
            if self.check(&TokenKind::Next) {
                break;
            }
            body.push(self.parse_statement()?);
            self.skip_newlines();
        }

        self.expect(&TokenKind::Next, "NEXT")?;

        // Optional variable name after NEXT (for validation in semantic analysis)
        let next_variable = if self.check(&TokenKind::Identifier) {
            let token = self.advance().expect("NEXT variable");
            Some(token.text.to_string())
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::For {
                variable,
                start: start_expr,
                end: end_expr,
                step,
                body,
                next_variable,
            },
            span,
        ))
    }

    /// Parses a WHILE loop.
    fn parse_while(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("WHILE keyword").span.start; // consume WHILE

        let condition = self.parse_expression()?;
        self.skip_newlines();

        let mut body = Vec::new();
        while !self.is_at_end() && !self.check(&TokenKind::Wend) {
            self.skip_newlines();
            if self.check(&TokenKind::Wend) {
                break;
            }
            body.push(self.parse_statement()?);
            self.skip_newlines();
        }

        self.expect(&TokenKind::Wend, "WEND")?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::While { condition, body },
            span,
        ))
    }

    /// Parses a DO loop.
    fn parse_do_loop(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("DO keyword").span.start; // consume DO

        // Check for pre-condition (DO WHILE/UNTIL)
        let pre_condition = if self.match_token(&TokenKind::While) {
            Some(DoCondition {
                is_while: true,
                condition: self.parse_expression()?,
            })
        } else if self.match_token(&TokenKind::Until) {
            Some(DoCondition {
                is_while: false,
                condition: self.parse_expression()?,
            })
        } else {
            None
        };

        self.skip_newlines();

        let mut body = Vec::new();
        while !self.is_at_end() && !self.check(&TokenKind::Loop) {
            self.skip_newlines();
            if self.check(&TokenKind::Loop) {
                break;
            }
            body.push(self.parse_statement()?);
            self.skip_newlines();
        }

        self.expect(&TokenKind::Loop, "LOOP")?;

        // Check for post-condition (LOOP WHILE/UNTIL)
        let post_condition = if self.match_token(&TokenKind::While) {
            Some(DoCondition {
                is_while: true,
                condition: self.parse_expression()?,
            })
        } else if self.match_token(&TokenKind::Until) {
            Some(DoCondition {
                is_while: false,
                condition: self.parse_expression()?,
            })
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::DoLoop {
                pre_condition,
                body,
                post_condition,
            },
            span,
        ))
    }

    /// Parses a GOTO statement.
    fn parse_goto(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("GOTO keyword").span.start; // consume GOTO
        let target_token = self.expect(&TokenKind::Identifier, "label")?;
        let target = target_token.text.to_string();
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Goto { target }, span))
    }

    /// Parses a GOSUB statement.
    fn parse_gosub(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("GOSUB keyword").span.start; // consume GOSUB
        let target_token = self.expect(&TokenKind::Identifier, "label")?;
        let target = target_token.text.to_string();
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Gosub { target }, span))
    }

    /// Parses a RETURN statement.
    fn parse_return(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RETURN keyword").span.start; // consume RETURN
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Return, span))
    }

    /// Parses an EXIT statement.
    fn parse_exit(&mut self) -> Result<Statement, ()> {
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
    fn parse_end(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("END keyword").span.start; // consume END
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::End, span))
    }

    /// Parses a STOP statement.
    fn parse_stop(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("STOP keyword").span.start; // consume STOP
        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Stop, span))
    }

    /// Parses a SWAP statement.
    ///
    /// Syntax: `SWAP var1, var2`
    ///
    /// Exchanges the values of two variables. Both must be of the same type.
    fn parse_swap(&mut self) -> Result<Statement, ()> {
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
    ///
    /// Syntax: `_CONTINUE` or `_CONTINUE FOR|WHILE|DO`
    ///
    /// Skips to the next iteration of a loop.
    fn parse_continue(&mut self) -> Result<Statement, ()> {
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
            // Semantic analysis will verify we're in a loop
            ContinueType::Do
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Continue { continue_type },
            span,
        ))
    }

    /// Parses a DATA statement.
    ///
    /// Syntax: `DATA value1, value2, value3, ...`
    ///
    /// DATA statements define literal values that can be read using READ.
    /// Values can be numeric literals or string literals.
    fn parse_data(&mut self) -> Result<Statement, ()> {
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
    ///
    /// Syntax: `READ var1, var2, var3, ...`
    ///
    /// READ consumes values from the DATA pool in order.
    fn parse_read(&mut self) -> Result<Statement, ()> {
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
    ///
    /// Syntax: `RESTORE [label]`
    ///
    /// RESTORE resets the DATA read pointer. Optional label specifies
    /// which DATA statement to restore to.
    fn parse_restore(&mut self) -> Result<Statement, ()> {
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

    /// Parses an INPUT statement.
    ///
    /// Syntax: `INPUT [;] ["prompt"{;|,}] variable[, variable...]`
    ///
    /// - Semicolon after prompt: shows "?" after the prompt
    /// - Comma after prompt: no "?" shown
    /// - A separator (`;` or `,`) is REQUIRED after a prompt string per QB spec
    fn parse_input(&mut self) -> Result<Statement, ()> {
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

    /// Parses a SUB definition.
    fn parse_sub(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SUB keyword").span.start; // consume SUB

        let name_token = self.expect(&TokenKind::Identifier, "SUB name")?;
        let name = name_token.text.to_string();

        // Parse parameters
        let params = if self.match_token(&TokenKind::LeftParen) {
            let p = self.parse_parameter_list()?;
            self.expect(&TokenKind::RightParen, ")")?;
            p
        } else {
            Vec::new()
        };

        let is_static = self.match_token(&TokenKind::Static);
        self.skip_newlines();

        // Parse body
        let mut body = Vec::new();
        while !self.is_at_end() && !self.check_end_sub() {
            self.skip_newlines();
            if self.check_end_sub() {
                break;
            }
            body.push(self.parse_statement()?);
            self.skip_newlines();
        }

        self.expect(&TokenKind::End, "END")?;
        self.expect(&TokenKind::Sub, "SUB")?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::SubDefinition {
                name,
                params,
                body,
                is_static,
            },
            span,
        ))
    }

    /// Checks for END SUB.
    fn check_end_sub(&self) -> bool {
        if self.check(&TokenKind::End)
            && let Some(next) = self.peek_ahead(1)
        {
            return next.kind == TokenKind::Sub;
        }
        false
    }

    /// Parses a FUNCTION definition.
    fn parse_function(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("FUNCTION keyword").span.start; // consume FUNCTION

        let name_token = self.expect(&TokenKind::Identifier, "FUNCTION name")?;
        let name = name_token.text.to_string();

        // Parse parameters
        let params = if self.match_token(&TokenKind::LeftParen) {
            let p = self.parse_parameter_list()?;
            self.expect(&TokenKind::RightParen, ")")?;
            p
        } else {
            Vec::new()
        };

        // Parse return type (AS type)
        let return_type = if self.match_token(&TokenKind::As) {
            Some(self.parse_type_spec()?)
        } else {
            None
        };

        let is_static = self.match_token(&TokenKind::Static);
        self.skip_newlines();

        // Parse body
        let mut body = Vec::new();
        while !self.is_at_end() && !self.check_end_function() {
            self.skip_newlines();
            if self.check_end_function() {
                break;
            }
            body.push(self.parse_statement()?);
            self.skip_newlines();
        }

        self.expect(&TokenKind::End, "END")?;
        self.expect(&TokenKind::Function, "FUNCTION")?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::FunctionDefinition {
                name,
                params,
                return_type,
                body,
                is_static,
            },
            span,
        ))
    }

    /// Checks for END FUNCTION.
    fn check_end_function(&self) -> bool {
        if self.check(&TokenKind::End)
            && let Some(next) = self.peek_ahead(1)
        {
            return next.kind == TokenKind::Function;
        }
        false
    }

    /// Parses a TYPE definition.
    ///
    /// Syntax:
    /// ```basic
    /// TYPE TypeName
    ///     member1 AS type1
    ///     member2 AS type2
    ///     ...
    /// END TYPE
    /// ```
    fn parse_type_definition(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("TYPE keyword").span.start; // consume TYPE

        // Parse type name
        let name_token = self.expect(&TokenKind::Identifier, "type name")?;
        let name = name_token.text.to_string();

        // Skip any newlines after the type name
        self.skip_newlines();

        // Parse member definitions until END TYPE
        let mut members = Vec::new();
        while !self.check_end_type() && !self.is_at_end() {
            // Skip empty lines
            if self.check(&TokenKind::Newline) {
                self.advance();
                continue;
            }

            // Skip comments inside TYPE
            if self.check(&TokenKind::Comment) {
                self.advance();
                continue;
            }

            // Parse member: name AS type
            let member_name_token = self.expect(&TokenKind::Identifier, "member name")?;
            let member_name = member_name_token.text.to_string();

            self.expect(&TokenKind::As, "AS in type member definition")?;

            let type_spec = self.parse_type_spec()?;

            members.push(TypeMember {
                name: member_name,
                type_spec,
            });

            // Skip newline after member
            if self.check(&TokenKind::Newline) {
                self.advance();
            }
        }

        // Consume END TYPE
        self.expect(&TokenKind::End, "END")?;
        self.expect(&TokenKind::Type, "TYPE")?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::TypeDefinition { name, members },
            span,
        ))
    }

    /// Checks for END TYPE.
    fn check_end_type(&self) -> bool {
        if self.check(&TokenKind::End)
            && let Some(next) = self.peek_ahead(1)
        {
            return next.kind == TokenKind::Type;
        }
        false
    }

    /// Parses a parameter list for SUB/FUNCTION.
    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ()> {
        let mut params = Vec::new();

        if self.check(&TokenKind::RightParen) {
            return Ok(params);
        }

        loop {
            let by_val = self.match_token(&TokenKind::ByVal);

            let name_token = self.expect(&TokenKind::Identifier, "parameter name")?;
            let name = name_token.text.to_string();

            let type_spec = if self.match_token(&TokenKind::As) {
                Some(self.parse_type_spec()?)
            } else {
                None
            };

            params.push(Parameter {
                name,
                type_spec,
                by_val,
            });

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }

    /// Parses a CALL statement.
    fn parse_call(&mut self) -> Result<Statement, ()> {
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
    fn parse_comment(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("comment token");
        let text = token.text.to_string();
        let span: Span = token.span.clone().into();
        Ok(Statement::new(StatementKind::Comment(text), span))
    }

    // ==================== Preprocessor Directives ====================

    /// Parses a `$INCLUDE: 'filename'` directive.
    ///
    /// The lexer captures the entire directive as a single token in the format:
    /// `$INCLUDE: 'path/to/file.bas'`
    fn parse_include_directive(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$INCLUDE directive");
        let span: Span = token.span.clone().into();

        // Extract the path from the directive text
        // Format: $INCLUDE: 'path'
        let text = &token.text;
        let path = if let Some(start) = text.find('\'') {
            let after_quote = &text[start + 1..];
            if let Some(end) = after_quote.find('\'') {
                after_quote[..end].to_string()
            } else {
                text.to_string() // Fallback if malformed
            }
        } else {
            text.to_string() // Fallback if malformed
        };

        Ok(Statement::new(
            StatementKind::IncludeDirective { path },
            span,
        ))
    }

    /// Parses a `$metacommand` directive.
    ///
    /// Handles `$IF`, `$ELSEIF`, `$ELSE`, `$END IF`, and other meta-commands.
    fn parse_meta_command(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("meta-command token");
        let span: Span = token.span.clone().into();
        let text = token.text.to_string();

        // Extract command name (after the $)
        let command = text[1..].to_uppercase();

        // Check for $IF (conditional compilation)
        if command == "IF" {
            return self.parse_conditional_block(span);
        }

        // For $ELSEIF, $ELSE, $END - these are handled within parse_conditional_block
        // If we see them at the top level, they're mismatched
        if command == "ELSEIF" || command == "ELSE" || command == "END" {
            self.errors.push(ParseError::syntax(
                format!("${} without matching $IF", command),
                span,
            ));
            return Err(());
        }

        // Other meta-commands (e.g., $DYNAMIC, $STATIC, $ERROR)
        // Parse any arguments on the rest of the line
        let args = self.parse_meta_command_args();

        Ok(Statement::new(
            StatementKind::MetaCommand { command, args },
            span,
        ))
    }

    /// Parses a `$IF ... $END IF` conditional compilation block.
    fn parse_conditional_block(&mut self, start_span: Span) -> Result<Statement, ()> {
        // Parse the condition (rest of line after $IF)
        let condition = self.parse_meta_command_args().unwrap_or_default();

        // Skip THEN if present (optional in QB64 $IF)
        self.match_token(&TokenKind::Then);
        self.skip_newlines();

        let mut then_branch = Vec::new();
        let mut elseif_branches = Vec::new();
        let mut else_branch = None;

        // Parse until $END IF, $ELSEIF, or $ELSE
        loop {
            self.skip_newlines();

            if self.is_at_end() {
                self.errors.push(ParseError::syntax(
                    "$IF without matching $END IF",
                    start_span,
                ));
                return Err(());
            }

            // Check for $ELSEIF, $ELSE, $END IF
            if let Some(token) = self.peek()
                && token.kind == TokenKind::MetaCommand
            {
                let cmd = token.text[1..].to_uppercase();
                if cmd == "END" {
                    // $END IF
                    self.advance();
                    // Skip IF if present
                    self.match_token(&TokenKind::If);
                    break;
                } else if cmd == "ELSEIF" {
                    self.advance();
                    let elseif_condition = self.parse_meta_command_args().unwrap_or_default();
                    self.match_token(&TokenKind::Then);
                    self.skip_newlines();

                    let mut elseif_body = Vec::new();
                    loop {
                        self.skip_newlines();
                        if self.is_at_end() || self.check_meta_command_end() {
                            break;
                        }
                        elseif_body.push(self.parse_statement()?);
                        self.skip_newlines();
                    }
                    elseif_branches.push((elseif_condition, elseif_body));
                    continue;
                } else if cmd == "ELSE" {
                    self.advance();
                    self.skip_newlines();

                    let mut else_body = Vec::new();
                    loop {
                        self.skip_newlines();
                        if self.is_at_end() || self.check_meta_command_end() {
                            break;
                        }
                        else_body.push(self.parse_statement()?);
                        self.skip_newlines();
                    }
                    else_branch = Some(else_body);

                    // After $ELSE body, expect $END IF
                    if let Some(token) = self.peek()
                        && token.kind == TokenKind::MetaCommand
                        && token.text[1..].to_uppercase() == "END"
                    {
                        self.advance();
                        self.match_token(&TokenKind::If);
                    }
                    break;
                }
            }

            // Parse regular statement
            then_branch.push(self.parse_statement()?);
            self.skip_newlines();
        }

        let span = self.span_from(start_span.start);
        Ok(Statement::new(
            StatementKind::ConditionalBlock {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            },
            span,
        ))
    }

    /// Checks if current token is $END, $ELSEIF, or $ELSE.
    fn check_meta_command_end(&self) -> bool {
        if let Some(token) = self.peek()
            && token.kind == TokenKind::MetaCommand
        {
            let cmd = token.text[1..].to_uppercase();
            return cmd == "END" || cmd == "ELSEIF" || cmd == "ELSE";
        }
        false
    }

    /// Parses meta-command arguments (rest of line until newline).
    fn parse_meta_command_args(&mut self) -> Option<String> {
        let mut args = String::new();

        while !self.is_at_end() && !self.check(&TokenKind::Newline) {
            if let Some(token) = self.advance() {
                if !args.is_empty() {
                    args.push(' ');
                }
                args.push_str(&token.text);
            }
        }

        if args.is_empty() { None } else { Some(args) }
    }
}

/// Operator precedence levels for Pratt parsing.
///
/// Higher values mean higher precedence (bind tighter).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    Lowest = 0,
    EqvImp = 1,         // EQV, IMP
    Or = 2,             // OR, XOR
    And = 3,            // AND
    Not = 4,            // NOT (handled as unary)
    Comparison = 5,     // =, <>, <, >, <=, >=
    Additive = 6,       // +, -
    Multiplicative = 7, // *, /, \, MOD
    Unary = 8,          // - (negation), NOT
    Power = 9,          // ^
}

impl Precedence {
    fn from_u8(val: u8) -> Self {
        match val {
            0 => Precedence::Lowest,
            1 => Precedence::EqvImp,
            2 => Precedence::Or,
            3 => Precedence::And,
            4 => Precedence::Not,
            5 => Precedence::Comparison,
            6 => Precedence::Additive,
            7 => Precedence::Multiplicative,
            8 => Precedence::Unary,
            _ => Precedence::Power,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;

    fn parse(source: &str) -> Result<Program, Vec<ParseError>> {
        let tokens = lex(source);
        let mut parser = Parser::new(&tokens);
        parser.parse()
    }

    #[test]
    fn test_parse_integer_literal() {
        let program = parse("PRINT 42").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_string_literal() {
        let program = parse(r#"PRINT "Hello""#).unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_binary_expression() {
        let program = parse("PRINT 1 + 2").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_precedence() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let program = parse("x = 1 + 2 * 3").unwrap();
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn test_parse_assignment() {
        let program = parse("x = 5").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0].kind,
            StatementKind::Let { .. }
        ));
    }

    #[test]
    fn test_parse_dim() {
        let program = parse("DIM x AS INTEGER").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0].kind,
            StatementKind::Dim { .. }
        ));
    }

    #[test]
    fn test_parse_if_single_line() {
        let program = parse("IF x > 0 THEN PRINT x").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0].kind,
            StatementKind::If { .. }
        ));
    }

    #[test]
    fn test_parse_for_loop() {
        let program = parse(
            r#"
FOR i = 1 TO 10
    PRINT i
NEXT
"#,
        )
        .unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0].kind,
            StatementKind::For { .. }
        ));
    }

    #[test]
    fn test_parse_while_loop() {
        let program = parse(
            r#"
WHILE x > 0
    x = x - 1
WEND
"#,
        )
        .unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            program.statements[0].kind,
            StatementKind::While { .. }
        ));
    }

    #[test]
    fn test_parse_multiple_statements() {
        let program = parse(
            r#"
PRINT "Hello"
x = 5
PRINT x
"#,
        )
        .unwrap();
        assert_eq!(program.statements.len(), 3);
    }

    #[test]
    fn test_parse_unterminated_string() {
        // Unterminated string (missing closing quote)
        let result = parse(r#"PRINT "Hello"#);
        assert!(result.is_err());

        // The error should be an UnterminatedString error
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
        assert!(matches!(errors[0], ParseError::UnterminatedString { .. }));
    }

    #[test]
    fn test_parse_input_with_prompt_semicolon() {
        // INPUT "Name"; x$ - valid, shows "?"
        let program = parse(r#"INPUT "Enter name"; name$"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Input {
                prompt: Some(_),
                show_question_mark: true,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_input_with_prompt_comma() {
        // INPUT "Name", x$ - valid, no "?"
        let program = parse(r#"INPUT "Enter name", name$"#).unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Input {
                prompt: Some(_),
                show_question_mark: false,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_input_prompt_missing_separator() {
        // INPUT "Name" x$ - invalid, missing separator
        let result = parse(r#"INPUT "Enter name" name$"#);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_input_no_prompt() {
        // INPUT x$ - valid, no prompt
        let program = parse("INPUT name$").unwrap();
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(
            &program.statements[0].kind,
            StatementKind::Input { prompt: None, .. }
        ));
    }
}
