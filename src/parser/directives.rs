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
