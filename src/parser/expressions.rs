//! Expression parsing using Pratt (precedence climbing) parsing.
//!
//! Pratt parsing is an elegant technique for parsing expressions with
//! operator precedence. It handles:
//! - Binary operators with correct precedence and associativity
//! - Unary operators (negation, NOT)
//! - Parenthesized expressions
//! - Function calls and array access
//!
//! # Precedence Levels (lowest to highest)
//!
//! 1. EQV, IMP (logical equivalence/implication)
//! 2. OR, XOR (logical)
//! 3. AND (logical)
//! 4. Comparison (=, <>, <, >, <=, >=)
//! 5. Addition (+, -)
//! 6. Multiplication (*, /, \, MOD)
//! 7. Unary (-, NOT)
//! 8. Power (^)

use crate::ast::{BinaryOp, Expr, ExprKind, Span, UnaryOp};
use crate::lexer::TokenKind;

use super::{ParseError, Parser, Precedence};

impl<'a> Parser<'a> {
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
    pub(super) fn parse_expr_precedence(&mut self, min_prec: Precedence) -> Result<Expr, ()> {
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
    fn parse_string_literal(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("string literal token");
        let span: Span = token.span.clone().into();

        // Remove surrounding quotes and handle doubled-quote escape (QBasic style)
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
    pub(super) fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ()> {
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
    pub(super) fn get_precedence(kind: &TokenKind) -> Precedence {
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
    pub(super) fn token_to_binary_op(kind: &TokenKind) -> Option<BinaryOp> {
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
}
