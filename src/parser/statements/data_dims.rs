//! Data and dimension statement parsing.
//!
//! This module handles parsing of:
//! - DIM, REDIM, STATIC - Variable and array declarations
//! - CONST - Constant definitions
//! - DATA, READ, RESTORE - Data statements
//! - DEFtype - Default type declarations

use crate::ast::{
    ArrayDimension, CommonVariable, DataValue, DefTypeKind, DimVariable, ReadTarget, Span,
    Statement, StatementKind,
};
use crate::lexer::TokenKind;

use crate::parser::{ParseError, Parser};

impl<'a> Parser<'a> {
    // ==================== DIM Statement ====================

    /// Parses a DIM statement.
    ///
    /// Syntax: `DIM [SHARED] variable[(dims)] [AS type], ...`
    /// Also supports QB64 alternate syntax: `DIM [SHARED] AS type var1, var2, ...`
    pub(in crate::parser) fn parse_dim(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("DIM keyword").span.start;

        // Check for SHARED modifier
        let shared = self.match_token(&TokenKind::Shared);

        let mut variables = Vec::new();

        // Check for QB64 alternate syntax: DIM [SHARED] AS type var1, var2, ...
        if self.check(&TokenKind::As) {
            self.advance(); // consume AS
            let common_type = self.parse_type_spec()?;

            // Parse variable names (all share the same type)
            loop {
                let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
                let name = name_token.text.to_string();

                // Optional array dimensions
                let dimensions = if self.match_token(&TokenKind::LeftParen) {
                    let dims = self.parse_array_dimensions()?;
                    self.expect(&TokenKind::RightParen, ")")?;
                    dims
                } else {
                    Vec::new()
                };

                variables.push(DimVariable {
                    name,
                    dimensions,
                    type_spec: Some(common_type.clone()),
                });

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        } else {
            // Standard syntax: DIM var(dims) AS type, ...
            loop {
                let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
                let name = name_token.text.to_string();

                // Optional array dimensions
                let dimensions = if self.match_token(&TokenKind::LeftParen) {
                    let dims = self.parse_array_dimensions()?;
                    self.expect(&TokenKind::RightParen, ")")?;
                    dims
                } else {
                    Vec::new()
                };

                // Optional type specification
                let type_spec = if self.match_token(&TokenKind::As) {
                    Some(self.parse_type_spec()?)
                } else {
                    None
                };

                variables.push(DimVariable {
                    name,
                    dimensions,
                    type_spec,
                });

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Dim { shared, variables },
            span,
        ))
    }

    /// Parses array dimension specifications: lower TO upper, or just upper.
    /// Dimensions are separated by commas.
    pub(in crate::parser) fn parse_array_dimensions(&mut self) -> Result<Vec<ArrayDimension>, ()> {
        let mut dims = Vec::new();

        // Handle empty parentheses: arr()
        if self.check(&TokenKind::RightParen) {
            return Ok(dims);
        }

        loop {
            let first_expr = self.parse_expression()?;

            if self.match_token(&TokenKind::To) {
                // lower TO upper
                let upper_expr = self.parse_expression()?;
                dims.push(ArrayDimension {
                    lower: Some(first_expr),
                    upper: upper_expr,
                });
            } else {
                // Just upper bound (lower is OPTION BASE dependent)
                dims.push(ArrayDimension {
                    lower: None,
                    upper: first_expr,
                });
            }

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(dims)
    }

    // ==================== REDIM Statement ====================

    /// Parses a REDIM statement.
    ///
    /// Syntax: `REDIM [SHARED] [_PRESERVE] array1(dims) [AS type], array2(dims) [AS type], ...`
    /// Also supports QB64 alternate syntax: `REDIM [SHARED] AS type var1(dims), var2, ...`
    /// Also allows scalar variables without parentheses: `REDIM arr(100), scalar AS LONG`
    /// Note: SHARED and _PRESERVE can appear in either order for compatibility.
    pub(in crate::parser) fn parse_redim(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("REDIM keyword").span.start;

        // SHARED and _PRESERVE can appear in either order
        let mut shared = self.match_token(&TokenKind::Shared);
        let preserve = self.match_token(&TokenKind::Preserve);
        // Check for SHARED after _PRESERVE too (both orderings are valid)
        if !shared {
            shared = self.match_token(&TokenKind::Shared);
        }

        let mut variables = Vec::new();

        // Check for QB64 alternate syntax: REDIM [SHARED] AS type var1, var2, ...
        if self.check(&TokenKind::As) {
            self.advance(); // consume AS
            let common_type = self.parse_type_spec()?;

            // Parse variable names (all share the same type)
            loop {
                let name_token = self.expect(&TokenKind::Identifier, "array name")?;
                let name = name_token.text.to_string();

                // Array dimensions are optional (scalar variables allowed)
                let dimensions = if self.match_token(&TokenKind::LeftParen) {
                    let dims = self.parse_array_dimensions()?;
                    self.expect(&TokenKind::RightParen, ")")?;
                    dims
                } else {
                    Vec::new()
                };

                variables.push(DimVariable {
                    name,
                    dimensions,
                    type_spec: Some(common_type.clone()),
                });

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        } else {
            // Standard syntax: REDIM arr(dims) AS type, ...
            // Also allows scalar variables without parentheses
            loop {
                let name_token = self.expect(&TokenKind::Identifier, "array name")?;
                let name = name_token.text.to_string();

                // Array dimensions are optional (scalar variables allowed in REDIM)
                let dimensions = if self.match_token(&TokenKind::LeftParen) {
                    let dims = self.parse_array_dimensions()?;
                    self.expect(&TokenKind::RightParen, ")")?;
                    dims
                } else {
                    Vec::new()
                };

                let type_spec = if self.match_token(&TokenKind::As) {
                    Some(self.parse_type_spec()?)
                } else {
                    None
                };

                variables.push(DimVariable {
                    name,
                    dimensions,
                    type_spec,
                });

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Redim {
                preserve,
                shared,
                variables,
            },
            span,
        ))
    }

    // ==================== STATIC Statement ====================

    /// Parses a STATIC statement (inside SUB/FUNCTION for local statics).
    ///
    /// Syntax: `STATIC variable[(dims)] [AS type], ...`
    /// Also supports QB64 alternate syntax: `STATIC AS type var1, var2, ...`
    pub(in crate::parser) fn parse_static_stmt(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("STATIC keyword").span.start;

        let mut variables = Vec::new();

        // Check for QB64 alternate syntax: STATIC AS type var1, var2, ...
        if self.check(&TokenKind::As) {
            self.advance(); // consume AS
            let common_type = self.parse_type_spec()?;

            // Parse variable names (all share the same type)
            loop {
                let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
                let name = name_token.text.to_string();

                // Optional array dimensions
                let dimensions = if self.match_token(&TokenKind::LeftParen) {
                    let dims = self.parse_array_dimensions()?;
                    self.expect(&TokenKind::RightParen, ")")?;
                    dims
                } else {
                    Vec::new()
                };

                variables.push(DimVariable {
                    name,
                    dimensions,
                    type_spec: Some(common_type.clone()),
                });

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        } else {
            // Standard syntax: STATIC var(dims) AS type, ...
            loop {
                let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
                let name = name_token.text.to_string();

                // Optional array dimensions
                let dimensions = if self.match_token(&TokenKind::LeftParen) {
                    let dims = self.parse_array_dimensions()?;
                    self.expect(&TokenKind::RightParen, ")")?;
                    dims
                } else {
                    Vec::new()
                };

                // Optional type specification
                let type_spec = if self.match_token(&TokenKind::As) {
                    Some(self.parse_type_spec()?)
                } else {
                    None
                };

                variables.push(DimVariable {
                    name,
                    dimensions,
                    type_spec,
                });

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::StaticStmt { variables },
            span,
        ))
    }

    // ==================== CONST Statement ====================

    /// Parses a CONST statement.
    ///
    /// Syntax: `CONST name = value [, name = value]...`
    pub(in crate::parser) fn parse_const(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("CONST keyword").span.start;

        let mut definitions = Vec::new();

        loop {
            let name_token = self.expect(&TokenKind::Identifier, "constant name")?;
            let name = name_token.text.to_string();

            // Optional AS type clause before the = sign (ignored for now - type inferred from value)
            if self.match_token(&TokenKind::As) {
                let _ = self.parse_type_spec()?;
            }

            self.expect(&TokenKind::Equals, "=")?;

            // Parse constant value expression
            let value = self.parse_expression()?;

            definitions.push((name, value));

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Const { definitions }, span))
    }

    // ==================== DATA Statements ====================

    /// Parses a DATA statement.
    ///
    /// Syntax: `DATA value1, value2, ...`
    pub(in crate::parser) fn parse_data(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("DATA keyword").span.start;

        let mut values = Vec::new();

        loop {
            let value = self.parse_data_value()?;
            values.push(value);

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Data { values }, span))
    }

    /// Parses a single DATA value (string, number, or unquoted string).
    fn parse_data_value(&mut self) -> Result<DataValue, ()> {
        // Handle unary minus for negative numbers
        let negate = self.match_token(&TokenKind::Minus);

        let token = match self.peek() {
            Some(t) => t,
            None => {
                self.errors.push(ParseError::eof("DATA value"));
                return Err(());
            }
        };

        match &token.kind {
            TokenKind::IntegerLiteral => {
                let text = token.text.to_string();
                self.advance();
                // Parse integer, handling type suffixes (%, &, &&) and hex/octal/binary
                let clean = text.trim_end_matches(['%', '&', '~']);
                let mut value: i64 = if clean.starts_with("&H") || clean.starts_with("&h") {
                    i64::from_str_radix(&clean[2..], 16).unwrap_or(0)
                } else if clean.starts_with("&O") || clean.starts_with("&o") {
                    i64::from_str_radix(&clean[2..], 8).unwrap_or(0)
                } else if clean.starts_with("&B") || clean.starts_with("&b") {
                    i64::from_str_radix(&clean[2..], 2).unwrap_or(0)
                } else {
                    clean.parse().unwrap_or(0)
                };
                if negate {
                    value = -value;
                }
                Ok(DataValue::Integer(value))
            }
            TokenKind::FloatLiteral => {
                let text = token.text.to_string();
                self.advance();
                let mut value: f64 = text.replace(['D', 'd'], "e").parse().unwrap_or(0.0);
                if negate {
                    value = -value;
                }
                Ok(DataValue::Float(value))
            }
            TokenKind::StringLiteral => {
                if negate {
                    let span: Span = token.span;
                    self.errors.push(ParseError::syntax(
                        "cannot negate a string in DATA statement".to_string(),
                        span,
                    ));
                    return Err(());
                }
                let text = token.text.to_string();
                self.advance();
                // Remove quotes
                Ok(DataValue::String(text[1..text.len() - 1].to_string()))
            }
            _ => {
                // Try to parse as unquoted string (any text until comma or end of statement)
                if negate {
                    // If we had a minus sign, this is likely a malformed negative number
                    Ok(DataValue::String("-".to_string()))
                } else {
                    self.parse_data_unquoted_string()
                }
            }
        }
    }

    /// Parses an unquoted string in a DATA statement.
    /// Reads characters until comma, colon, or end of line.
    fn parse_data_unquoted_string(&mut self) -> Result<DataValue, ()> {
        let mut text = String::new();

        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Comma | TokenKind::Colon | TokenKind::Newline => break,
                _ => {
                    text.push_str(&token.text);
                    self.advance();
                }
            }
        }

        Ok(DataValue::String(text.trim().to_string()))
    }

    /// Parses a READ statement.
    ///
    /// Syntax: `READ variable1, variable2, ...`
    pub(in crate::parser) fn parse_read(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("READ keyword").span.start;

        let mut targets = Vec::new();

        loop {
            let target = self.parse_read_target()?;
            targets.push(target);

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Read { targets }, span))
    }

    /// Parses a single read target (variable, array element, or field access).
    fn parse_read_target(&mut self) -> Result<ReadTarget, ()> {
        let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
        let name = name_token.text.to_string();

        // Check for array subscript: name(indices)
        if self.match_token(&TokenKind::LeftParen) {
            let mut indices = Vec::new();
            loop {
                let idx = self.parse_expression()?;
                indices.push(idx);
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
            self.expect(&TokenKind::RightParen, ")")?;

            // Check for field access: .field
            if self.match_token(&TokenKind::Dot) {
                let field_token = self.expect(&TokenKind::Identifier, "field name")?;
                let field = field_token.text.to_string();
                Ok(ReadTarget::ArrayFieldElement {
                    name,
                    indices,
                    field,
                })
            } else {
                Ok(ReadTarget::ArrayElement { name, indices })
            }
        } else {
            // Simple variable
            Ok(ReadTarget::Variable(name))
        }
    }

    /// Parses a RESTORE statement.
    ///
    /// Syntax: `RESTORE [label]`
    pub(in crate::parser) fn parse_restore(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RESTORE keyword").span.start;

        // Optional label
        let label = if !self.is_at_statement_end() {
            Some(self.parse_label_target()?)
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Restore { label }, span))
    }

    // ==================== RANDOMIZE Statement ====================

    /// Parses a RANDOMIZE statement.
    ///
    /// Syntax: `RANDOMIZE [seed]` or `RANDOMIZE TIMER`
    pub(in crate::parser) fn parse_randomize(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("RANDOMIZE keyword").span.start;

        // Optional seed expression
        let seed = if !self.is_at_statement_end() {
            // Check for RANDOMIZE TIMER (use system timer as seed)
            if self.check(&TokenKind::Timer) {
                // Return None for seed, codegen will use time-based seed
                self.advance();
                None
            } else {
                Some(self.parse_expression()?)
            }
        } else {
            None
        };

        let span = self.span_from(start);
        Ok(Statement::new(StatementKind::Randomize { seed }, span))
    }

    // ==================== DEFtype Statements ====================

    /// Parses DEFtype statements (DEFINT, DEFLNG, DEFSNG, DEFDBL, DEFSTR).
    ///
    /// Syntax: `DEFINT A-Z` or `DEFINT A, B, C-F`
    pub(in crate::parser) fn parse_deftype(&mut self) -> Result<Statement, ()> {
        let token = self.advance().expect("DEFtype keyword");
        let start = token.span.start;
        let type_kind = match token.kind {
            TokenKind::DefInt => DefTypeKind::Integer,
            TokenKind::DefLng => DefTypeKind::Long,
            TokenKind::DefSng => DefTypeKind::Single,
            TokenKind::DefDbl => DefTypeKind::Double,
            TokenKind::DefStr => DefTypeKind::String,
            _ => unreachable!("parse_deftype called with non-DEF token"),
        };

        let ranges = self.parse_define_type()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::DefType { type_kind, ranges },
            span,
        ))
    }

    /// Parses _DEFINE statement (QB64 extended type definitions).
    ///
    /// Syntax: `_DEFINE A-Z AS type`
    pub(in crate::parser) fn parse_define(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("_DEFINE keyword").span.start;

        let ranges = self.parse_define_type()?;

        self.expect(&TokenKind::As, "AS")?;

        // Parse the full type specification as a string (including _UNSIGNED prefix)
        let type_spec = self.parse_define_type_spec()?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::Define { type_spec, ranges },
            span,
        ))
    }

    /// Parses the type specification for _DEFINE, returning the full type name as a string.
    ///
    /// Handles: INTEGER, LONG, SINGLE, DOUBLE, STRING, _BYTE, _INTEGER64, _FLOAT, _OFFSET,
    /// and _UNSIGNED variants.
    fn parse_define_type_spec(&mut self) -> Result<String, ()> {
        let mut type_name = String::new();

        // Check for _UNSIGNED prefix
        if self.match_token(&TokenKind::Unsigned) {
            type_name.push_str("_UNSIGNED ");
        }

        // Get the base type token
        let peeked = self.peek();
        let token = match peeked {
            Some(t) => t.clone(),
            None => {
                self.errors.push(ParseError::eof("type name"));
                return Err(());
            }
        };

        let base_type = match &token.kind {
            TokenKind::Integer => {
                self.advance();
                "INTEGER"
            }
            TokenKind::Long => {
                self.advance();
                "LONG"
            }
            TokenKind::Single => {
                self.advance();
                "SINGLE"
            }
            TokenKind::Double => {
                self.advance();
                "DOUBLE"
            }
            TokenKind::String_ => {
                self.advance();
                "STRING"
            }
            TokenKind::Byte => {
                self.advance();
                "_BYTE"
            }
            TokenKind::Integer64 => {
                self.advance();
                "_INTEGER64"
            }
            TokenKind::Float => {
                self.advance();
                "_FLOAT"
            }
            TokenKind::Offset => {
                self.advance();
                "_OFFSET"
            }
            TokenKind::BitType => {
                self.advance();
                "_BIT"
            }
            _ => {
                let span: Span = token.span;
                self.errors.push(ParseError::syntax(
                    format!("expected type name, found {:?}", token.kind),
                    span,
                ));
                return Err(());
            }
        };

        type_name.push_str(base_type);
        Ok(type_name)
    }

    /// Parses the letter ranges for DEFtype and _DEFINE statements.
    /// Returns a list of (start, end) character pairs representing letter ranges.
    pub(in crate::parser) fn parse_define_type(&mut self) -> Result<Vec<(char, char)>, ()> {
        let mut ranges = Vec::new();

        loop {
            // Expect a letter (as identifier)
            let start_token = self.expect(&TokenKind::Identifier, "letter")?;
            let start_text = start_token.text.to_uppercase();

            if start_text.len() != 1 || !start_text.chars().next().unwrap().is_ascii_alphabetic() {
                let span: Span = start_token.span;
                self.errors.push(ParseError::syntax(
                    "expected single letter for DEF range".to_string(),
                    span,
                ));
                return Err(());
            }

            let start_char = start_text.chars().next().unwrap();

            // Check for range: A-Z
            if self.match_token(&TokenKind::Minus) {
                let end_token = self.expect(&TokenKind::Identifier, "letter")?;
                let end_text = end_token.text.to_uppercase();

                if end_text.len() != 1 || !end_text.chars().next().unwrap().is_ascii_alphabetic() {
                    let span: Span = end_token.span;
                    self.errors.push(ParseError::syntax(
                        "expected single letter for DEF range end".to_string(),
                        span,
                    ));
                    return Err(());
                }

                let end_char = end_text.chars().next().unwrap();
                ranges.push((start_char, end_char));
            } else {
                // Single letter: 'A' becomes ('A', 'A')
                ranges.push((start_char, start_char));
            }

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(ranges)
    }

    // ==================== COMMON Statement ====================

    /// Parses a COMMON statement.
    ///
    /// Syntax: `COMMON [SHARED] variable [, variable]...`
    pub(in crate::parser) fn parse_common(&mut self) -> Result<Statement, ()> {
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
    pub(in crate::parser) fn parse_common_variable(&mut self) -> Result<CommonVariable, ()> {
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

    // ==================== SHARED Statement ====================

    /// Parses a SHARED statement (inside SUB/FUNCTION).
    ///
    /// Syntax: `SHARED variable, ...`
    /// This statement declares access to module-level shared variables.
    pub(in crate::parser) fn parse_shared_stmt(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SHARED keyword").span.start;

        let mut variables = Vec::new();

        loop {
            let name_token = self.expect(&TokenKind::Identifier, "variable name")?;
            let name = name_token.text.to_string();

            // Skip optional array indicator ()
            if self.match_token(&TokenKind::LeftParen) {
                self.expect(&TokenKind::RightParen, ")")?;
            }

            // Skip optional AS type (we just need the name)
            if self.match_token(&TokenKind::As) {
                let _ = self.parse_type_spec()?;
            }

            variables.push(name);

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::SharedStmt { variables },
            span,
        ))
    }
}
