//! Control flow statement parsing.
//!
//! This module handles parsing of control flow constructs:
//! - IF/THEN/ELSEIF/ELSE/END IF
//! - SELECT CASE/CASE/CASE ELSE/END SELECT
//! - FOR/NEXT loops
//! - WHILE/WEND loops
//! - DO/LOOP (with WHILE/UNTIL variants)

use crate::ast::{
    CaseClause, CaseCompareOp, CaseMatch, DoCondition, Span, Statement, StatementKind,
};
use crate::lexer::TokenKind;

use super::{ParseError, Parser};

impl<'a> Parser<'a> {
    // ==================== IF Statement ====================

    /// Parses an IF statement.
    pub(super) fn parse_if(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("IF keyword").span.start; // consume IF

        let condition = self.parse_expression()?;
        self.expect(&TokenKind::Then, "THEN")?;

        // Check if this is single-line IF
        if !self.check(&TokenKind::Newline) && !self.is_at_end() {
            // Single-line IF - handle line numbers as implicit GOTO
            let then_stmt = self.parse_single_line_if_statement()?;
            let else_branch = if self.match_token(&TokenKind::Else) {
                Some(vec![self.parse_single_line_if_statement()?])
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
            // Skip statement separators (newlines and colons) between statements
            self.skip_statement_separators();
            if self.check(&TokenKind::ElseIf) || self.check(&TokenKind::Else) || self.check_end_if()
            {
                break;
            }
            then_branch.push(self.parse_statement()?);
        }

        // Parse ELSEIF branches
        while self.match_token(&TokenKind::ElseIf) {
            let elseif_condition = self.parse_expression()?;
            self.expect(&TokenKind::Then, "THEN")?;

            let mut elseif_body = Vec::new();
            while !self.is_at_end() {
                self.skip_statement_separators();
                if self.check(&TokenKind::ElseIf)
                    || self.check(&TokenKind::Else)
                    || self.check_end_if()
                {
                    break;
                }
                elseif_body.push(self.parse_statement()?);
            }
            elseif_branches.push((elseif_condition, elseif_body));
        }

        // Parse ELSE branch
        if self.match_token(&TokenKind::Else) {
            let mut else_body = Vec::new();
            while !self.is_at_end() {
                self.skip_statement_separators();
                if self.check_end_if() {
                    break;
                }
                else_body.push(self.parse_statement()?);
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

    /// Parses a statement in a single-line IF context.
    ///
    /// In classic BASIC, a bare line number after THEN/ELSE is an implicit GOTO:
    /// `IF x > 0 THEN 100` means `IF x > 0 THEN GOTO 100`
    fn parse_single_line_if_statement(&mut self) -> Result<Statement, ()> {
        // Check for line number (implicit GOTO)
        if self.check(&TokenKind::IntegerLiteral) {
            let token = self.advance().expect("line number");
            let target = format!("_line_{}", token.text);
            let span: Span = token.span.clone().into();
            return Ok(Statement::new(StatementKind::Goto { target }, span));
        }

        // Otherwise parse a normal statement
        self.parse_statement()
    }

    /// Checks for END IF (handles both "END IF" and "ENDIF").
    pub(super) fn check_end_if(&self) -> bool {
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

    // ==================== SELECT CASE ====================

    /// Parses a SELECT CASE statement.
    pub(super) fn parse_select_case(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("SELECT keyword").span.start; // consume SELECT
        self.expect(&TokenKind::Case, "CASE")?;

        let test_expr = self.parse_expression()?;

        let mut cases = Vec::new();
        let mut case_else = None;

        while !self.is_at_end() {
            self.skip_statement_separators();

            if self.check(&TokenKind::End) {
                break;
            }

            if !self.match_token(&TokenKind::Case) {
                break;
            }

            // Check for CASE ELSE
            if self.match_token(&TokenKind::Else) {
                let mut body = Vec::new();
                while !self.is_at_end() {
                    self.skip_statement_separators();
                    if self.check(&TokenKind::End) || self.check(&TokenKind::Case) {
                        break;
                    }
                    body.push(self.parse_statement()?);
                }
                case_else = Some(body);
                break;
            }

            // Parse CASE matches
            let matches = self.parse_case_matches()?;

            let mut body = Vec::new();
            while !self.is_at_end() {
                self.skip_statement_separators();
                if self.check(&TokenKind::Case) || self.check(&TokenKind::End) {
                    break;
                }
                body.push(self.parse_statement()?);
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

    // ==================== FOR Loop ====================

    /// Parses a FOR loop.
    pub(super) fn parse_for(&mut self) -> Result<Statement, ()> {
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

        // Parse body until NEXT or leftover NEXT variable (from multi-variable NEXT)
        let mut body = Vec::new();
        while !self.is_at_end() {
            self.skip_statement_separators();
            if self.check(&TokenKind::Next) {
                break;
            }
            // Check for leftover NEXT variable pattern: identifier followed by comma or end-of-statement
            // This happens with "NEXT j%, i%" where inner loop consumed "NEXT j%, " leaving "i%"
            if self.check(&TokenKind::Identifier) {
                if let Some(next) = self.peek_ahead(1) {
                    // If identifier is followed by comma or statement end, it's a NEXT variable
                    if next.kind == TokenKind::Comma
                        || next.kind == TokenKind::Newline
                        || next.kind == TokenKind::Colon
                    {
                        break;
                    }
                } else {
                    // Identifier at end of file - treat as NEXT variable
                    break;
                }
            }
            body.push(self.parse_statement()?);
        }

        // Check if we're at NEXT or at an identifier (leftover from multi-variable NEXT)
        let next_variable = if self.check(&TokenKind::Next) {
            self.advance(); // consume NEXT

            // Optional variable name after NEXT (for validation in semantic analysis)
            if self.check(&TokenKind::Identifier) {
                let token = self.advance().expect("NEXT variable");
                let var_name = token.text.to_string();

                // If there's a comma, consume it - the remaining variables
                // are for outer loops
                if self.check(&TokenKind::Comma) {
                    self.advance(); // consume the comma
                }

                Some(var_name)
            } else {
                None
            }
        } else if self.check(&TokenKind::Identifier) {
            // This is a leftover variable from a multi-variable NEXT (e.g., "NEXT j%, i%")
            // The inner loop consumed "NEXT j%, " and left "i%" for us
            let token = self.advance().expect("NEXT variable");
            let var_name = token.text.to_string();

            // If there's a comma, consume it for the next outer loop
            if self.check(&TokenKind::Comma) {
                self.advance();
            }

            Some(var_name)
        } else {
            // No NEXT found - error
            self.errors.push(super::ParseError::unexpected(
                "NEXT",
                self.peek()
                    .map(|t| format!("{:?}", t.kind))
                    .unwrap_or("EOF".to_string()),
                self.peek()
                    .map(|t| t.span.clone().into())
                    .unwrap_or_else(|| crate::ast::Span { start: 0, end: 0 }),
            ));
            return Err(());
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

    // ==================== WHILE Loop ====================

    /// Parses a WHILE loop.
    pub(super) fn parse_while(&mut self) -> Result<Statement, ()> {
        let start = self.advance().expect("WHILE keyword").span.start; // consume WHILE

        let condition = self.parse_expression()?;

        let mut body = Vec::new();
        while !self.is_at_end() {
            self.skip_statement_separators();
            if self.check(&TokenKind::Wend) {
                break;
            }
            body.push(self.parse_statement()?);
        }

        self.expect(&TokenKind::Wend, "WEND")?;

        let span = self.span_from(start);
        Ok(Statement::new(
            StatementKind::While { condition, body },
            span,
        ))
    }

    // ==================== DO Loop ====================

    /// Parses a DO loop.
    pub(super) fn parse_do_loop(&mut self) -> Result<Statement, ()> {
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

        let mut body = Vec::new();
        while !self.is_at_end() {
            self.skip_statement_separators();
            if self.check(&TokenKind::Loop) {
                break;
            }
            body.push(self.parse_statement()?);
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
}
