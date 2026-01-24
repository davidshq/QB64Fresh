//! DECLARE statement parsing.
//!
//! This module handles parsing of all DECLARE-related statements:
//! - DECLARE SUB - Forward declaration of subroutines
//! - DECLARE FUNCTION - Forward declaration of functions
//! - DECLARE LIBRARY - External C library bindings

use crate::ast::{
    DeclareParam, ExternalDeclaration, ExternalParam, Span, Statement, StatementKind, TypeSpec,
};
use crate::lexer::TokenKind;

use crate::parser::{ParseError, Parser};

impl<'a> Parser<'a> {
    // ==================== DECLARE Statements ====================

    /// Parses DECLARE statements.
    ///
    /// Handles three forms:
    /// - `DECLARE SUB name(params)` - Forward declaration of SUB
    /// - `DECLARE FUNCTION name(params)` - Forward declaration of FUNCTION
    /// - `DECLARE [DYNAMIC] LIBRARY` - External C library binding
    pub(in crate::parser) fn parse_declare(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("DECLARE keyword").span.start;

        // Check what follows DECLARE
        if self.check(&TokenKind::Sub) {
            return self.parse_declare_sub(start);
        } else if self.check(&TokenKind::Function) {
            return self.parse_declare_function(start);
        }

        // Must be DECLARE LIBRARY
        self.parse_declare_library_body(start)
    }

    /// Parses DECLARE SUB - forward declaration of a subroutine.
    /// In classic BASIC, these are used to declare SUB signatures before use.
    /// We parse and store them but they're mainly for documentation/validation.
    ///
    /// Supports legacy syntax: `DECLARE SUB name CDECL [ALIAS "alias"] ([params])`
    fn parse_declare_sub(&mut self, start: usize) -> Result<Statement, ()> {
        self.advance(); // consume SUB

        let name_token = self.expect(&TokenKind::Identifier, "subroutine name")?;
        let name = name_token.text.to_string();

        // Skip CDECL modifier if present (legacy, ignored)
        self.match_token(&TokenKind::Cdecl);

        // Skip SEG modifier if present (legacy, ignored)
        self.match_token(&TokenKind::Seg);

        // Skip ALIAS "name" if present (legacy, ignored)
        if self.match_token(&TokenKind::Alias) {
            self.match_token(&TokenKind::StringLiteral);
        }

        // Parse optional parameter list
        let params = if self.match_token(&TokenKind::LeftParen) {
            let params = self.parse_declare_params()?;
            self.expect(&TokenKind::RightParen, ")")?;
            params
        } else {
            Vec::new()
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::DeclareSub { name, params },
            span,
        ))
    }

    /// Parses DECLARE FUNCTION - forward declaration of a function.
    ///
    /// Supports legacy syntax: `DECLARE FUNCTION name CDECL [ALIAS "alias"] ([params]) [AS type]`
    fn parse_declare_function(&mut self, start: usize) -> Result<Statement, ()> {
        self.advance(); // consume FUNCTION

        let name_token = self.expect(&TokenKind::Identifier, "function name")?;
        let name = name_token.text.to_string();

        // Skip CDECL modifier if present (legacy, ignored)
        self.match_token(&TokenKind::Cdecl);

        // Skip SEG modifier if present (legacy, ignored)
        self.match_token(&TokenKind::Seg);

        // Skip ALIAS "name" if present (legacy, ignored)
        if self.match_token(&TokenKind::Alias) {
            self.match_token(&TokenKind::StringLiteral);
        }

        // Parse optional parameter list
        let params = if self.match_token(&TokenKind::LeftParen) {
            let params = self.parse_declare_params()?;
            self.expect(&TokenKind::RightParen, ")")?;
            params
        } else {
            Vec::new()
        };

        // Parse optional return type (AS type)
        let return_type = if self.match_token(&TokenKind::As) {
            Some(self.parse_type_spec()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::DeclareFunction {
                name,
                params,
                return_type,
            },
            span,
        ))
    }

    /// Parses parameter list for DECLARE SUB/FUNCTION.
    /// Parameters can have type suffixes or AS TYPE clauses.
    ///
    /// Uses `expect_name()` to allow keywords to be used as parameter names,
    /// which is valid BASIC syntax (e.g., `DECLARE SUB Greet(name AS STRING)`).
    fn parse_declare_params(&mut self) -> Result<Vec<DeclareParam>, ()> {
        let mut params = Vec::new();

        if self.check(&TokenKind::RightParen) {
            return Ok(params);
        }

        loop {
            let name_token = self.expect_name("parameter name")?;
            let name = name_token.text.to_string();

            // Check for array parameter: name()
            let is_array = if self.match_token(&TokenKind::LeftParen) {
                self.expect(&TokenKind::RightParen, "`)` after array parameter")?;
                true
            } else {
                false
            };

            // Check for AS TYPE
            let param_type = if self.match_token(&TokenKind::As) {
                match self.advance() {
                    Some(type_token) => Some(type_token.text.to_string()),
                    None => {
                        self.errors.push(ParseError::eof("type name"));
                        return Err(());
                    }
                }
            } else {
                None
            };

            params.push(DeclareParam {
                name,
                param_type,
                is_array,
            });

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }

    /// Parses DECLARE LIBRARY block body (after DECLARE keyword consumed).
    fn parse_declare_library_body(&mut self, start: usize) -> Result<Statement, ()> {
        // Check for DYNAMIC keyword
        let is_dynamic = self.match_token(&TokenKind::Dynamic);

        // Expect LIBRARY keyword
        self.expect(&TokenKind::Library, "LIBRARY")?;

        // Optional library name (string literal)
        let library_name = if self.check(&TokenKind::StringLiteral) {
            let token = self.advance().expect("string literal");
            // Remove quotes from string literal
            let text = &token.text;
            Some(text[1..text.len() - 1].to_string())
        } else {
            None
        };

        // Skip to next line
        self.skip_newlines();

        // Parse declarations until END DECLARE
        let mut declarations = Vec::new();
        loop {
            // Skip empty lines
            self.skip_newlines();

            // Check for END DECLARE
            if self.check(&TokenKind::End)
                && let Some(next) = self.peek_ahead(1)
                && next.kind == TokenKind::Declare
            {
                self.advance(); // consume END
                self.advance(); // consume DECLARE
                break;
            }

            // Check for EOF
            if self.is_at_end() {
                let span = self.span_from(start);
                self.errors
                    .push(ParseError::syntax("expected END DECLARE".to_string(), span));
                return Err(());
            }

            // Parse function or sub declaration
            if let Some(decl) = self.parse_external_declaration()? {
                declarations.push(decl);
            }

            // Skip to next line
            self.skip_newlines();
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::DeclareLibrary {
                library_name,
                is_dynamic,
                declarations,
            },
            span,
        ))
    }

    /// Parses a single external function or sub declaration.
    ///
    /// Returns None for blank lines or comments.
    fn parse_external_declaration(&mut self) -> Result<Option<ExternalDeclaration>, ()> {
        let token = match self.peek() {
            Some(t) => t,
            None => return Ok(None),
        };

        let is_function = match token.kind {
            TokenKind::Function => true,
            TokenKind::Sub => false,
            TokenKind::Comment => {
                // Skip comment
                self.advance();
                return Ok(None);
            }
            _ => {
                // Unknown token inside DECLARE LIBRARY - skip line
                return Ok(None);
            }
        };

        self.advance(); // consume FUNCTION or SUB

        // Parse function/sub name (identifier with optional type suffix)
        let name_token = self.expect(&TokenKind::Identifier, "function/sub name")?;
        let name = name_token.text.to_string();

        // Determine return type from name suffix (for functions)
        let return_type = if is_function {
            self.type_from_suffix(&name)
        } else {
            None
        };

        // Parse parameter list (optional)
        let params = if self.check(&TokenKind::LeftParen) {
            self.advance(); // consume (
            let params = self.parse_external_param_list()?;
            self.expect(&TokenKind::RightParen, ")")?;
            params
        } else {
            Vec::new()
        };

        // Parse optional ALIAS clause
        let alias = if self.match_token(&TokenKind::Alias) {
            if self.check(&TokenKind::StringLiteral) {
                let token = self.advance().expect("string literal");
                let text = &token.text;
                Some(text[1..text.len() - 1].to_string())
            } else {
                let span = self.current_span();
                self.errors.push(ParseError::syntax(
                    "expected string literal after ALIAS".to_string(),
                    span,
                ));
                return Err(());
            }
        } else {
            None
        };

        Ok(Some(ExternalDeclaration {
            name,
            alias,
            params,
            return_type,
            is_function,
        }))
    }

    /// Parses external function parameter list.
    ///
    /// Uses `expect_name()` to allow keywords to be used as parameter names.
    fn parse_external_param_list(&mut self) -> Result<Vec<ExternalParam>, ()> {
        let mut params = Vec::new();

        if self.check(&TokenKind::RightParen) {
            return Ok(params);
        }

        loop {
            // Check for BYVAL
            let is_byval = self.match_token(&TokenKind::ByVal);

            // Parse parameter name
            let name_token = self.expect_name("parameter name")?;
            let name = name_token.text.to_string();

            // Parse type (AS clause required for external functions)
            let type_spec = if self.match_token(&TokenKind::As) {
                self.parse_type_spec()?
            } else {
                // Try to infer from name suffix or default to LONG
                self.type_from_suffix(&name).unwrap_or(TypeSpec::Long)
            };

            params.push(ExternalParam {
                name,
                type_spec,
                is_byval,
            });

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }

    /// Extracts type from BASIC name suffix (e.g., "add_values&" -> Long).
    pub(in crate::parser) fn type_from_suffix(&self, name: &str) -> Option<TypeSpec> {
        if name.ends_with("&&") {
            Some(TypeSpec::Integer64)
        } else if name.ends_with("##") {
            Some(TypeSpec::Float)
        } else if name.ends_with("%%") {
            Some(TypeSpec::Byte)
        } else if name.ends_with("%&") {
            Some(TypeSpec::Offset)
        } else if name.ends_with('&') {
            Some(TypeSpec::Long)
        } else if name.ends_with('%') {
            Some(TypeSpec::Integer)
        } else if name.ends_with('!') {
            Some(TypeSpec::Single)
        } else if name.ends_with('#') {
            Some(TypeSpec::Double)
        } else if name.ends_with('$') {
            Some(TypeSpec::String)
        } else {
            None
        }
    }

    /// Returns the current token's span (for error reporting).
    pub(in crate::parser) fn current_span(&self) -> Span {
        self.peek()
            .map(|t| t.span.clone().into())
            .unwrap_or(Span::new(0, 0))
    }

    /// Helper to check if we're at the end of a statement.
    pub(in crate::parser) fn is_at_end_of_statement(&self) -> bool {
        match self.peek() {
            None => true,
            Some(token) => matches!(
                token.kind,
                TokenKind::Newline | TokenKind::Colon | TokenKind::Comment | TokenKind::RemComment
            ),
        }
    }
}
