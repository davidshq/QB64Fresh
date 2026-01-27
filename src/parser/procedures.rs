//! Procedure and type definition parsing.
//!
//! This module handles parsing of:
//! - SUB definitions
//! - FUNCTION definitions
//! - TYPE (user-defined type) definitions
//! - Parameter lists for SUB/FUNCTION

use crate::ast::{Parameter, Statement, StatementKind, TypeMember, TypeSpec};
use crate::lexer::TokenKind;

use super::{ParseError, Parser};

impl<'a> Parser<'a> {
    // ==================== SUB Definition ====================

    /// Parses a SUB definition.
    pub(super) fn parse_sub(&mut self) -> Result<Statement, ()> {
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
            self.skip_statement_separators();
            if self.check_end_sub() {
                break;
            }
            body.push(self.parse_statement()?);
            self.skip_statement_separators();
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

    // ==================== FUNCTION Definition ====================

    /// Parses a FUNCTION definition.
    pub(super) fn parse_function(&mut self) -> Result<Statement, ()> {
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
            self.skip_statement_separators();
            if self.check_end_function() {
                break;
            }
            body.push(self.parse_statement()?);
            self.skip_statement_separators();
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

    // ==================== TYPE Definition ====================

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
    ///
    /// Also supports QB4.5 CUSTOMTYPE modifier:
    /// ```basic
    /// TYPE CStruct CUSTOMTYPE
    ///     value AS LONG
    /// END TYPE
    /// ```
    pub(super) fn parse_type_definition(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("TYPE keyword").span.start; // consume TYPE

        // Parse type name
        let name_token = self.expect(&TokenKind::Identifier, "type name")?;
        let name = name_token.text.to_string();

        // Check for optional CUSTOMTYPE modifier (QB4.5)
        let custom_type = self.match_token(&TokenKind::CustomType);

        // Skip any newlines after the type name (and optional modifier)
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

            // Check for QB64 alternate syntax: AS type member1, member2, ...
            if self.check(&TokenKind::As) {
                self.advance(); // consume AS
                let type_spec = self.parse_type_spec()?;

                // Parse one or more member names separated by commas
                // Use expect_name to allow keywords as member names (e.g., name, type)
                loop {
                    let member_name_token = self.expect_name("member name")?;
                    let member_name = member_name_token.text.to_string();

                    members.push(TypeMember {
                        name: member_name,
                        type_spec: type_spec.clone(),
                    });

                    // Continue if comma, otherwise break
                    if !self.match_token(&TokenKind::Comma) {
                        break;
                    }
                }
            } else {
                // Standard syntax: member AS type
                // Use expect_name to allow keywords as member names (e.g., name, type)
                let member_name_token = self.expect_name("member name")?;
                let member_name = member_name_token.text.to_string();

                self.expect(&TokenKind::As, "AS in type member definition")?;

                let type_spec = self.parse_type_spec()?;

                members.push(TypeMember {
                    name: member_name,
                    type_spec,
                });
            }

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
            StatementKind::TypeDefinition {
                name,
                members,
                custom_type,
            },
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

    // ==================== Parameter List ====================

    /// Parses a parameter list for SUB/FUNCTION.
    ///
    /// Uses `expect_name()` to allow keywords to be used as parameter names,
    /// which is valid BASIC syntax (e.g., `SUB Greet(name AS STRING)` where
    /// "name" is the NAME keyword).
    pub(super) fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ()> {
        let mut params = Vec::new();

        if self.check(&TokenKind::RightParen) {
            return Ok(params);
        }

        loop {
            let by_val = self.match_token(&TokenKind::ByVal);

            let name_token = self.expect_name("parameter name")?;
            let name = name_token.text.to_string();

            // Check for array parameter: name()
            let is_array = if self.match_token(&TokenKind::LeftParen) {
                self.expect(&TokenKind::RightParen, "`)` after array parameter")?;
                true
            } else {
                false
            };

            let type_spec = if self.match_token(&TokenKind::As) {
                Some(self.parse_type_spec()?)
            } else {
                None
            };

            params.push(Parameter {
                name,
                type_spec,
                by_val,
                is_array,
            });

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }

    // ==================== Type Specification ====================

    /// Parses a type specification.
    pub(super) fn parse_type_spec(&mut self) -> Result<TypeSpec, ()> {
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
                    let len_span = len_token.span;
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
            TokenKind::Offset => {
                self.advance();
                TypeSpec::Offset
            }
            TokenKind::BitType => {
                self.advance();
                TypeSpec::Bit
            }
            TokenKind::MemType => {
                self.advance();
                TypeSpec::Mem
            }
            TokenKind::Identifier => {
                let name = token.text.to_string();
                self.advance();
                TypeSpec::UserDefined(name)
            }
            _ => {
                let span = token.span;
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
}
