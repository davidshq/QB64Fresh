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
                let span = token.span;
                self.advance(); // Consume the unterminated string token
                self.errors.push(ParseError::UnterminatedString { span });
                Err(())
            }
            TokenKind::Identifier => self.parse_identifier_or_call(),
            TokenKind::LeftParen => self.parse_grouped(),
            TokenKind::Minus => self.parse_unary(UnaryOp::Negate),
            TokenKind::Not => self.parse_unary(UnaryOp::Not),

            // String function that's also a keyword (LEN is used in OPEN...LEN=n)
            TokenKind::Len => self.parse_builtin_function("LEN"),

            // Phase 5: System Integration functions
            TokenKind::FileExists => self.parse_builtin_function("_FILEEXISTS"),
            TokenKind::DirExists => self.parse_builtin_function("_DIREXISTS"),
            TokenKind::Dir => self.parse_builtin_function("_DIR$"),

            // Phase 5: Mouse Input functions
            TokenKind::MouseX => self.parse_builtin_function("_MOUSEX"),
            TokenKind::MouseY => self.parse_builtin_function("_MOUSEY"),
            TokenKind::MouseButton => self.parse_builtin_function("_MOUSEBUTTON"),
            TokenKind::MouseInput => self.parse_builtin_function("_MOUSEINPUT"),
            TokenKind::MouseMovementX => self.parse_builtin_function("_MOUSEMOVEMENTX"),
            TokenKind::MouseMovementY => self.parse_builtin_function("_MOUSEMOVEMENTY"),
            TokenKind::MouseWheel => self.parse_builtin_function("_MOUSEWHEEL"),

            // Phase 5: Clipboard function
            TokenKind::Clipboard => self.parse_builtin_function("_CLIPBOARD$"),

            // Phase 5: Networking functions
            TokenKind::OpenHost => self.parse_builtin_function("_OPENHOST"),
            TokenKind::OpenConnection => self.parse_builtin_function("_OPENCONNECTION"),
            TokenKind::OpenClient => self.parse_builtin_function("_OPENCLIENT"),
            TokenKind::Connected => self.parse_builtin_function("_CONNECTED"),

            // Image buffer functions
            TokenKind::NewImage => self.parse_builtin_function("_NEWIMAGE"),
            TokenKind::LoadImage => self.parse_builtin_function("_LOADIMAGE"),
            TokenKind::CopyImage => self.parse_builtin_function("_COPYIMAGE"),
            TokenKind::ImageWidth => self.parse_builtin_function("_WIDTH"),
            TokenKind::ImageHeight => self.parse_builtin_function("_HEIGHT"),

            // Text metrics function
            TokenKind::PrintWidth => self.parse_builtin_function("_PRINTWIDTH"),

            // QB64 Window/Display functions (that are also statements)
            TokenKind::Resize => self.parse_builtin_function("_RESIZE"),

            // QB64 Shell functions (also statements, but return exit code as function)
            TokenKind::ShellHide => self.parse_builtin_function("_SHELLHIDE"),

            // QB64 Unicode functions (also statements for setting, functions for getting)
            TokenKind::MapUnicode => self.parse_builtin_function("_MAPUNICODE"),

            // QB4.5 Event Handling / Input Functions (keywords that are also functions)
            TokenKind::Timer => self.parse_builtin_function("TIMER"),
            TokenKind::Stick => self.parse_builtin_function("STICK"),
            TokenKind::Strig => self.parse_builtin_function("STRIG"),
            TokenKind::Fre => self.parse_builtin_function("FRE"),
            TokenKind::Inp => self.parse_builtin_function("INP"),
            TokenKind::Pen => self.parse_builtin_function("PEN"),
            TokenKind::Erdev => self.parse_builtin_function("ERDEV"),
            TokenKind::Ioctl => self.parse_builtin_function("IOCTL$"),
            TokenKind::Key => self.parse_builtin_function("KEY"),
            TokenKind::InputDollar => self.parse_builtin_function("INPUT$"),

            // QB64 Sound functions (return handles or values)
            TokenKind::SndOpen => self.parse_builtin_function("_SNDOPEN"),
            TokenKind::SndOpenRaw => self.parse_builtin_function("_SNDOPENRAW"),
            TokenKind::SndCopy => self.parse_builtin_function("_SNDCOPY"),
            TokenKind::SndPlaying => self.parse_builtin_function("_SNDPLAYING"),
            TokenKind::SndGetPos => self.parse_builtin_function("_SNDGETPOS"),
            TokenKind::SndLen => self.parse_builtin_function("_SNDLEN"),
            TokenKind::SndPaused => self.parse_builtin_function("_SNDPAUSED"),

            // QB64 Windows-only desktop functions
            TokenKind::ScreenImage => self.parse_builtin_function("_SCREENIMAGE"),

            // QB64 Procedure pointer (for callbacks)
            TokenKind::ProcPtr => self.parse_procptr(),

            // QB64 type conversion functions
            TokenKind::CvFunc => self.parse_cv_func(),
            TokenKind::MkDollarFunc => self.parse_mk_func(),
            TokenKind::CastFunc => self.parse_cast_func(),

            // QB64 Memory functions (_MEM is both a type and a function)
            TokenKind::MemType => self.parse_builtin_function("_MEM"),

            // QB64 Short-circuit operators (can be used as functions or infix operators)
            TokenKind::AndAlso => self.parse_builtin_function("_ANDALSO"),
            TokenKind::OrElse => self.parse_builtin_function("_ORELSE"),

            // Keywords that can be used as variable names in expression context
            // In BASIC, keywords like NAME, INPUT, OUTPUT can be used as variable names
            // when context makes it unambiguous that an identifier is expected.
            _ if self.is_name_token() => self.parse_identifier_or_call(),

            _ => {
                let span = token.span;
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
        let op_span: Span = op_token.span;

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
        let span: Span = token.span;

        // Strip optional type suffix before parsing
        let text = token.text.trim_end_matches(['%', '&', '!', '#']);

        let value: i64 = text.parse().map_err(|e| {
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
        let span: Span = token.span;

        // Strip optional type suffix (! for SINGLE, # for DOUBLE) before parsing
        let text = token.text.trim_end_matches(['!', '#']);
        // BASIC uses D for double exponents, convert to E for Rust parsing
        let text = text.replace('D', "E").replace('d', "e");
        let value: f64 = text.parse().map_err(|e| {
            self.errors.push(ParseError::InvalidNumber {
                span,
                message: format!("invalid float: {}", e),
            });
        })?;

        Ok(Expr::new(ExprKind::FloatLiteral(value), span))
    }

    /// Parses a hexadecimal literal (&HFF, &HE0~%%, etc.).
    fn parse_hex_literal(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("hex literal token");
        let span: Span = token.span;

        // Skip the &H prefix, find where hex digits end (type suffix starts)
        let after_prefix = &token.text[2..];
        let digit_end = after_prefix
            .find(|c: char| !c.is_ascii_hexdigit())
            .unwrap_or(after_prefix.len());
        let hex_str = &after_prefix[..digit_end];

        let value = i64::from_str_radix(hex_str, 16).map_err(|e| {
            self.errors.push(ParseError::InvalidNumber {
                span,
                message: format!("invalid hex literal: {}", e),
            });
        })?;

        Ok(Expr::new(ExprKind::IntegerLiteral(value), span))
    }

    /// Parses an octal literal (&O77, &O377~%%, etc.).
    fn parse_octal_literal(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("octal literal token");
        let span: Span = token.span;

        // Skip the &O prefix, find where octal digits end (type suffix starts)
        let after_prefix = &token.text[2..];
        let digit_end = after_prefix
            .find(|c: char| !matches!(c, '0'..='7'))
            .unwrap_or(after_prefix.len());
        let oct_str = &after_prefix[..digit_end];

        let value = i64::from_str_radix(oct_str, 8).map_err(|e| {
            self.errors.push(ParseError::InvalidNumber {
                span,
                message: format!("invalid octal literal: {}", e),
            });
        })?;

        Ok(Expr::new(ExprKind::IntegerLiteral(value), span))
    }

    /// Parses a binary literal (&B1010, &B11111111~%%, etc.).
    fn parse_binary_literal(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("binary literal token");
        let span: Span = token.span;

        // Skip the &B prefix, find where binary digits end (type suffix starts)
        let after_prefix = &token.text[2..];
        let digit_end = after_prefix
            .find(|c: char| !matches!(c, '0' | '1'))
            .unwrap_or(after_prefix.len());
        let bin_str = &after_prefix[..digit_end];

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
        let span: Span = token.span;

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
    /// - VAL with type specifier: `VAL(string$, _INTEGER64)`
    fn parse_identifier_or_call(&mut self) -> Result<Expr, ()> {
        let token = self.advance().expect("identifier token");
        let name = token.text.to_string();
        let start_span: Span = token.span;

        // Check for function call (identifier followed by parenthesis)
        let mut expr = if self.check(&TokenKind::LeftParen) {
            self.advance(); // consume (

            // Special handling for VAL with type specifier: VAL(string$, _INTEGER64)
            if name.eq_ignore_ascii_case("VAL") {
                // Parse first argument (the string expression)
                let value = self.parse_expression()?;

                // Check if there's a comma and a type specifier
                if self.check(&TokenKind::Comma) {
                    self.advance(); // consume comma

                    // Check if next token looks like a type (type keyword or _UNSIGNED)
                    if self.is_type_specifier_token() {
                        let type_name = self.parse_type_name()?;
                        self.expect(&TokenKind::RightParen, ")")?;
                        let span = self.span_from(start_span.start);
                        Expr::new(
                            ExprKind::ValWithType {
                                value: Box::new(value),
                                target_type: type_name,
                            },
                            span,
                        )
                    } else {
                        // Second argument is an expression, not a type - regular VAL call
                        let second_arg = self.parse_expression()?;
                        let mut args = vec![value, second_arg];
                        while self.match_token(&TokenKind::Comma) {
                            args.push(self.parse_expression()?);
                        }
                        self.expect(&TokenKind::RightParen, ")")?;
                        let span = self.span_from(start_span.start);
                        Expr::new(ExprKind::FunctionCall { name, args }, span)
                    }
                } else {
                    // Single-argument VAL - regular function call
                    self.expect(&TokenKind::RightParen, ")")?;
                    let span = self.span_from(start_span.start);
                    Expr::new(
                        ExprKind::FunctionCall {
                            name,
                            args: vec![value],
                        },
                        span,
                    )
                }
            // Special handling for _MEMGET with type specifier: _MEMGET(mem, offset, AS type)
            } else if name.eq_ignore_ascii_case("_MEMGET") {
                // Parse first argument (mem expression)
                let mem = self.parse_expression()?;
                self.expect(&TokenKind::Comma, ",")?;

                // Parse second argument (offset expression)
                let offset = self.parse_expression()?;

                // Check for optional third argument: AS type
                if self.match_token(&TokenKind::Comma) {
                    // Expect AS keyword
                    if self.match_token(&TokenKind::As) {
                        let type_name = self.parse_type_name()?;
                        self.expect(&TokenKind::RightParen, ")")?;
                        let span = self.span_from(start_span.start);
                        Expr::new(
                            ExprKind::MemGetTyped {
                                mem: Box::new(mem),
                                offset: Box::new(offset),
                                target_type: type_name,
                            },
                            span,
                        )
                    } else {
                        // Third argument without AS - regular function call (error case)
                        let third_arg = self.parse_expression()?;
                        self.expect(&TokenKind::RightParen, ")")?;
                        let span = self.span_from(start_span.start);
                        Expr::new(
                            ExprKind::FunctionCall {
                                name,
                                args: vec![mem, offset, third_arg],
                            },
                            span,
                        )
                    }
                } else {
                    // Two-argument _MEMGET - regular function call (returns Unknown type)
                    self.expect(&TokenKind::RightParen, ")")?;
                    let span = self.span_from(start_span.start);
                    Expr::new(
                        ExprKind::FunctionCall {
                            name,
                            args: vec![mem, offset],
                        },
                        span,
                    )
                }
            } else {
                let args = self.parse_argument_list()?;
                self.expect(&TokenKind::RightParen, ")")?;
                let span = self.span_from(start_span.start);
                Expr::new(ExprKind::FunctionCall { name, args }, span)
            }
        } else {
            Expr::new(ExprKind::Identifier(name), start_span)
        };

        // Handle field access chain: obj.field.subfield
        // Use expect_name to allow keywords as field names (e.g., .name, .type)
        while self.check(&TokenKind::Dot) {
            self.advance(); // consume .
            let field_token = self.expect_name("field name after `.`")?;
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

    /// Parses a built-in function call with a known name.
    ///
    /// Used for Phase 5 functions that have their own tokens (e.g., _FILEEXISTS, _MOUSEX).
    fn parse_builtin_function(&mut self, name: &str) -> Result<Expr, ()> {
        let start = self.advance().expect("builtin function token").span.start;

        // Check for parenthesized arguments
        let args = if self.check(&TokenKind::LeftParen) {
            self.advance(); // consume (
            let args = self.parse_argument_list()?;
            self.expect(&TokenKind::RightParen, ")")?;
            args
        } else {
            // No arguments (e.g., _MOUSEX with no parens)
            Vec::new()
        };

        let span = self.span_from(start);
        Ok(Expr::new(
            ExprKind::FunctionCall {
                name: name.to_string(),
                args,
            },
            span,
        ))
    }

    /// Parses _PROCPTR(procedureName) expression.
    ///
    /// Returns a pointer to a BASIC procedure for use as a C callback.
    fn parse_procptr(&mut self) -> Result<Expr, ()> {
        let start = self.advance().expect("_PROCPTR token").span.start;

        self.expect(&TokenKind::LeftParen, "(")?;

        // Get the procedure name
        let name_token = self.expect(&TokenKind::Identifier, "procedure name")?;
        let name = name_token.text.to_string();

        self.expect(&TokenKind::RightParen, ")")?;

        let span = self.span_from(start);
        Ok(Expr::new(ExprKind::ProcPtr { name }, span))
    }

    /// Parses _CV(type, string$) expression.
    ///
    /// Converts a string's raw bytes to a value of the specified type.
    fn parse_cv_func(&mut self) -> Result<Expr, ()> {
        let start = self.advance().expect("_CV token").span.start;

        self.expect(&TokenKind::LeftParen, "(")?;

        // Get the type name (can be INTEGER, SINGLE, DOUBLE, etc.)
        let type_name = self.parse_type_name()?;

        self.expect(&TokenKind::Comma, ",")?;

        // Get the string expression
        let value = self.parse_expression()?;

        self.expect(&TokenKind::RightParen, ")")?;

        let span = self.span_from(start);
        Ok(Expr::new(
            ExprKind::CvFunc {
                target_type: type_name,
                value: Box::new(value),
            },
            span,
        ))
    }

    /// Parses _MK$(type, value) expression.
    ///
    /// Converts a value to a string of raw bytes.
    fn parse_mk_func(&mut self) -> Result<Expr, ()> {
        let start = self.advance().expect("_MK$ token").span.start;

        self.expect(&TokenKind::LeftParen, "(")?;

        // Get the type name
        let type_name = self.parse_type_name()?;

        self.expect(&TokenKind::Comma, ",")?;

        // Get the value expression
        let value = self.parse_expression()?;

        self.expect(&TokenKind::RightParen, ")")?;

        let span = self.span_from(start);
        Ok(Expr::new(
            ExprKind::MkDollarFunc {
                source_type: type_name,
                value: Box::new(value),
            },
            span,
        ))
    }

    /// Parses _CAST(type, value) expression.
    ///
    /// Explicitly converts a value to the specified type.
    fn parse_cast_func(&mut self) -> Result<Expr, ()> {
        let start = self.advance().expect("_CAST token").span.start;

        self.expect(&TokenKind::LeftParen, "(")?;

        // Get the type name
        let type_name = self.parse_type_name()?;

        self.expect(&TokenKind::Comma, ",")?;

        // Get the value expression
        let value = self.parse_expression()?;

        self.expect(&TokenKind::RightParen, ")")?;

        let span = self.span_from(start);
        Ok(Expr::new(
            ExprKind::CastFunc {
                target_type: type_name,
                value: Box::new(value),
            },
            span,
        ))
    }

    /// Checks if the current token could be the start of a type specifier.
    ///
    /// Used to distinguish between `VAL(string$, expression)` and `VAL(string$, _INTEGER64)`.
    fn is_type_specifier_token(&self) -> bool {
        if let Some(token) = self.peek() {
            matches!(
                &token.kind,
                TokenKind::Unsigned
                    | TokenKind::Integer
                    | TokenKind::Long
                    | TokenKind::Single
                    | TokenKind::Double
                    | TokenKind::String_
                    | TokenKind::Byte
                    | TokenKind::BitType
                    | TokenKind::Integer64
                    | TokenKind::Float
                    | TokenKind::Offset
            )
        } else {
            false
        }
    }

    /// Parses a type name for _CV, _MK$, _CAST, VAL, _MEMGET, _MEMPUT functions.
    ///
    /// Returns the type name as a string (e.g., "INTEGER", "SINGLE", "_INTEGER64").
    pub(in crate::parser) fn parse_type_name(&mut self) -> Result<String, ()> {
        // Check for _UNSIGNED modifier
        let mut prefix = String::new();
        if self.match_token(&TokenKind::Unsigned) {
            prefix = "_UNSIGNED ".to_string();
        }

        // Parse the base type
        let type_str = if let Some(token) = self.peek() {
            let type_name = match &token.kind {
                TokenKind::Integer => "INTEGER",
                TokenKind::Long => "LONG",
                TokenKind::Single => "SINGLE",
                TokenKind::Double => "DOUBLE",
                TokenKind::String_ => "STRING",
                TokenKind::Byte => "_BYTE",
                TokenKind::BitType => "_BIT",
                TokenKind::Integer64 => "_INTEGER64",
                TokenKind::Float => "_FLOAT",
                TokenKind::Offset => "_OFFSET",
                _ => {
                    let span = self.current_span();
                    self.errors.push(ParseError::syntax(
                        "expected type name (INTEGER, SINGLE, DOUBLE, etc.)".to_string(),
                        span,
                    ));
                    return Err(());
                }
            };
            self.advance();
            type_name.to_string()
        } else {
            let span = self.current_span();
            self.errors
                .push(ParseError::syntax("expected type name".to_string(), span));
            return Err(());
        };

        Ok(format!("{}{}", prefix, type_str))
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
            TokenKind::And | TokenKind::AndAlso => Precedence::And,
            TokenKind::Or | TokenKind::Xor | TokenKind::OrElse => Precedence::Or,
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
            TokenKind::AndAlso => Some(BinaryOp::AndAlso),
            TokenKind::Or => Some(BinaryOp::Or),
            TokenKind::OrElse => Some(BinaryOp::OrElse),
            TokenKind::Xor => Some(BinaryOp::Xor),
            TokenKind::Eqv => Some(BinaryOp::Eqv),
            TokenKind::Imp => Some(BinaryOp::Imp),
            _ => None,
        }
    }
}
