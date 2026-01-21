//! Preprocessor directive parsing.
//!
//! This module handles parsing of QB64 preprocessor directives:
//! - `$INCLUDE: 'filename'` - File inclusion
//! - `$IF/$ELSEIF/$ELSE/$END IF` - Conditional compilation
//! - Other meta-commands (`$DYNAMIC`, `$STATIC`, etc.)
//!
//! Unlike C preprocessor directives, QB64 directives are parsed as part of
//! the regular AST rather than being processed in a separate pass.

use crate::ast::{Span, Statement, StatementKind};
use crate::lexer::TokenKind;

use super::{ParseError, Parser};

impl<'a> Parser<'a> {
    // ==================== $INCLUDE Directive ====================

    /// Parses a `$INCLUDE: 'filename'` directive.
    ///
    /// The lexer captures the entire directive as a single token in the format:
    /// `$INCLUDE: 'path/to/file.bas'`
    pub(super) fn parse_include_directive(&mut self) -> Result<Statement, ()> {
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

    // ==================== Meta Commands ====================

    /// Parses a `$metacommand` directive.
    ///
    /// Handles `$IF`, `$ELSEIF`, `$ELSE`, `$END IF`, and other meta-commands.
    pub(super) fn parse_meta_command(&mut self) -> Result<Statement, ()> {
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

    // ==================== Conditional Compilation ====================

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

            // Check for $ELSEIF, $ELSE, $END IF (both new specific tokens and legacy MetaCommand)
            if let Some(token) = self.peek() {
                // Handle new specific tokens
                if token.kind == TokenKind::MetaEndIf {
                    self.advance();
                    break;
                } else if token.kind == TokenKind::MetaElseIf {
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
                } else if token.kind == TokenKind::MetaElse {
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
                    if self.check(&TokenKind::MetaEndIf) {
                        self.advance();
                    }
                    break;
                }
                // Handle legacy MetaCommand tokens for backwards compatibility
                else if token.kind == TokenKind::MetaCommand {
                    let cmd = token.text[1..].to_uppercase();
                    if cmd == "END" {
                        self.advance();
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

    /// Checks if current token is $END IF, $ELSEIF, or $ELSE.
    fn check_meta_command_end(&self) -> bool {
        if let Some(token) = self.peek() {
            // Check new specific tokens
            if matches!(
                token.kind,
                TokenKind::MetaEndIf | TokenKind::MetaElseIf | TokenKind::MetaElse
            ) {
                return true;
            }
            // Check legacy MetaCommand tokens
            if token.kind == TokenKind::MetaCommand {
                let cmd = token.text[1..].to_uppercase();
                return cmd == "END" || cmd == "ELSEIF" || cmd == "ELSE";
            }
        }
        false
    }

    /// Parses meta-command arguments (rest of line until newline).
    fn parse_meta_command_args(&mut self) -> Option<String> {
        let mut args = String::new();

        while !self.is_at_end() && !self.check(&TokenKind::Newline) {
            // Stop before THEN keyword for $IF conditions
            if self.check(&TokenKind::Then) {
                break;
            }
            if let Some(token) = self.advance() {
                if !args.is_empty() {
                    args.push(' ');
                }
                args.push_str(&token.text);
            }
        }

        if args.is_empty() { None } else { Some(args) }
    }

    // ==================== New Phase 2 Metacommand Parsers ====================

    /// Parses a `$IF` conditional compilation directive.
    ///
    /// Delegates to the existing conditional block parsing logic.
    pub(super) fn parse_meta_if(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$IF token");
        let span: Span = token.span.clone().into();
        self.parse_conditional_block(span)
    }

    /// Parses a `$LET` compile-time variable assignment.
    ///
    /// Syntax: `$LET variable = value`
    pub(super) fn parse_meta_let(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("$LET token").span.start;

        // Parse variable name
        let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let name = name_token.text.to_uppercase();

        // Expect equals sign
        self.expect(&TokenKind::Equals, "=")?;

        // Parse value (integer or boolean-like)
        let value = if self.check(&TokenKind::IntegerLiteral) {
            let val_token = self.advance().expect("integer literal");
            val_token.text.parse::<i64>().unwrap_or(0)
        } else if self.check(&TokenKind::Minus) {
            self.advance(); // consume minus
            let val_token = self.expect(&TokenKind::IntegerLiteral, "integer")?;
            -val_token.text.parse::<i64>().unwrap_or(0)
        } else {
            // Default to -1 for TRUE-like assignment
            -1
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::MetaLet { name, value }, span))
    }

    /// Parses a `$CHECKING` directive.
    ///
    /// Syntax: `$CHECKING:ON` or `$CHECKING:OFF`
    pub(super) fn parse_meta_checking(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("$CHECKING token").span.start;

        // Expect colon
        self.expect(&TokenKind::Colon, ":")?;

        // Parse ON or OFF
        let enabled = if let Some(token) = self.peek() {
            match token.text.to_uppercase().as_str() {
                "ON" => {
                    self.advance();
                    true
                }
                "OFF" => {
                    self.advance();
                    false
                }
                _ => {
                    let span = self.span_from(start);
                    self.errors.push(ParseError::syntax(
                        "expected ON or OFF after $CHECKING:",
                        span,
                    ));
                    return Err(());
                }
            }
        } else {
            let span = self.span_from(start);
            self.errors.push(ParseError::syntax(
                "expected ON or OFF after $CHECKING:",
                span,
            ));
            return Err(());
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::MetaChecking { enabled },
            span,
        ))
    }

    /// Parses a `$CONSOLE` or `$CONSOLE:ONLY` directive.
    ///
    /// - `$CONSOLE` enables console window alongside graphics
    /// - `$CONSOLE:ONLY` runs as console-only (no graphics window)
    pub(super) fn parse_meta_console(&mut self, only: bool) -> Result<Statement, ()> {
        let token = self.advance().expect("$CONSOLE token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaConsole { only }, span))
    }

    /// Parses a `$SCREENHIDE` directive.
    ///
    /// Hides the graphics window on program startup.
    pub(super) fn parse_meta_screenhide(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$SCREENHIDE token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaScreenHide, span))
    }

    /// Parses a `$SCREENSHOW` directive.
    ///
    /// Shows the graphics window on program startup (default behavior).
    pub(super) fn parse_meta_screenshow(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$SCREENSHOW token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaScreenShow, span))
    }

    /// Parses a `$RESIZE:ON` or `$RESIZE:OFF` directive.
    pub(super) fn parse_meta_resize(&mut self, enabled: bool) -> Result<Statement, ()> {
        let token = self.advance().expect("$RESIZE token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaResize { enabled }, span))
    }

    /// Parses a `$RESIZE:STRETCH` directive.
    pub(super) fn parse_meta_resize_stretch(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$RESIZE:STRETCH token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaResizeStretch, span))
    }

    /// Parses a `$RESIZE:SMOOTH` directive.
    pub(super) fn parse_meta_resize_smooth(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$RESIZE:SMOOTH token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaResizeSmooth, span))
    }

    /// Parses a `$STATIC` directive.
    pub(super) fn parse_meta_static(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$STATIC token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaStatic, span))
    }

    /// Parses a `$DYNAMIC` directive.
    pub(super) fn parse_meta_dynamic(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$DYNAMIC token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaDynamic, span))
    }

    /// Parses a `$DEBUG` directive.
    pub(super) fn parse_meta_debug(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$DEBUG token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaDebug, span))
    }

    /// Parses a `$INCLUDEONCE` directive.
    pub(super) fn parse_meta_includeonce(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$INCLUDEONCE token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaIncludeOnce, span))
    }

    /// Parses a `$EXEICON:'filename'` directive.
    pub(super) fn parse_meta_exeicon(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$EXEICON token");
        let span: Span = token.span.clone().into();

        // Extract filename from the token text (e.g., "$EXEICON:'icon.ico'")
        let text = &token.text;
        let filename = if let Some(start) = text.find('\'') {
            if let Some(end) = text[start + 1..].find('\'') {
                text[start + 1..start + 1 + end].to_string()
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        Ok(Statement::new(
            StatementKind::MetaExeIcon { filename },
            span,
        ))
    }

    /// Parses a `$VERSIONINFO:key=value` directive.
    pub(super) fn parse_meta_versioninfo(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$VERSIONINFO token");
        let span: Span = token.span.clone().into();

        // Extract key and value from the token text (e.g., "$VERSIONINFO:CompanyName=My Company")
        let text = &token.text;
        let (key, value) = if let Some(colon_pos) = text.find(':') {
            let rest = &text[colon_pos + 1..];
            if let Some(eq_pos) = rest.find('=') {
                let key = rest[..eq_pos].trim().to_string();
                let value = rest[eq_pos + 1..].trim().to_string();
                (key, value)
            } else {
                (String::new(), String::new())
            }
        } else {
            (String::new(), String::new())
        };

        Ok(Statement::new(
            StatementKind::MetaVersionInfo { key, value },
            span,
        ))
    }

    /// Parses a `$ERROR message` directive.
    pub(super) fn parse_meta_error(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$ERROR token");
        let span: Span = token.span.clone().into();

        // Extract message from the token text (e.g., "$ERROR This is an error message")
        let text = &token.text;
        let message = if text.len() > 7 {
            text[7..].trim().to_string() // Skip "$ERROR " prefix
        } else {
            String::new()
        };

        Ok(Statement::new(
            StatementKind::MetaErrorDirective { message },
            span,
        ))
    }

    /// Parses a `$EMBED:'filename'` directive.
    pub(super) fn parse_meta_embed(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$EMBED token");
        let span: Span = token.span.clone().into();

        // Extract filename from the token text (e.g., "$EMBED:'icon.png'")
        let text = &token.text;
        let filename = if let Some(start) = text.find('\'') {
            if let Some(end) = text[start + 1..].find('\'') {
                text[start + 1..start + 1 + end].to_string()
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        Ok(Statement::new(StatementKind::MetaEmbed { filename }, span))
    }

    /// Parses a `$MIDISOUNDFONT:'file.sf2'` directive.
    pub(super) fn parse_meta_midisoundfont(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$MIDISOUNDFONT token");
        let span: Span = token.span.clone().into();

        // Extract filename from the token text (e.g., "$MIDISOUNDFONT:'soundfont.sf2'")
        let text = &token.text;
        let filename = if let Some(start) = text.find('\'') {
            if let Some(end) = text[start + 1..].find('\'') {
                text[start + 1..start + 1 + end].to_string()
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        Ok(Statement::new(
            StatementKind::MetaMidiSoundFont { filename },
            span,
        ))
    }

    /// Parses a `$UNSTABLE:feature` directive.
    pub(super) fn parse_meta_unstable(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$UNSTABLE token");
        let span: Span = token.span.clone().into();

        // Extract feature name from the token text (e.g., "$UNSTABLE:http")
        let text = &token.text;
        let feature = if let Some(colon_pos) = text.find(':') {
            text[colon_pos + 1..].trim().to_string()
        } else {
            String::new()
        };

        Ok(Statement::new(
            StatementKind::MetaUnstable { feature },
            span,
        ))
    }

    /// Parses a `$FORMAT` directive.
    pub(super) fn parse_meta_format(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$FORMAT token");
        let span: Span = token.span.clone().into();

        Ok(Statement::new(StatementKind::MetaFormat, span))
    }

    /// Parses a `$USELIBRARY:'library'` directive.
    pub(super) fn parse_meta_uselibrary(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("$USELIBRARY token");
        let span: Span = token.span.clone().into();

        // Extract library from the token text (e.g., "$USELIBRARY:'opengl32'")
        let text = &token.text;
        let library = if let Some(start) = text.find('\'') {
            if let Some(end) = text[start + 1..].find('\'') {
                text[start + 1..start + 1 + end].to_string()
            } else {
                String::new()
            }
        } else {
            String::new()
        };

        Ok(Statement::new(
            StatementKind::MetaUseLibrary { library },
            span,
        ))
    }
}
