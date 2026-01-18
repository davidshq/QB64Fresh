//! Control flow statement type checking.
//!
//! This module handles type checking for:
//! - IF/THEN/ELSEIF/ELSE statements
//! - SELECT CASE statements
//! - FOR/NEXT loops
//! - WHILE/WEND loops
//! - DO/LOOP variants
//! - GOTO/GOSUB/RETURN
//! - EXIT statements
//! - CALL statements

use crate::ast::{CaseCompareOp, CaseMatch, DoCondition, Expr, Statement};
use crate::semantic::{
    error::SemanticError,
    symbols::{Symbol, SymbolKind},
    typed_ir::*,
    types::BasicType,
};

use super::{ForLoopInfo, TypeChecker};

impl<'a> TypeChecker<'a> {
    // ========================================================================
    // IF Statement
    // ========================================================================

    /// Type checks an IF statement.
    pub(super) fn check_if(
        &mut self,
        condition: &Expr,
        then_branch: &[Statement],
        elseif_branches: &[(Expr, Vec<Statement>)],
        else_branch: &Option<Vec<Statement>>,
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_condition = self.check_expr(condition);

        if !typed_condition.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_condition.basic_type.to_string(),
                span: condition.span,
            });
        }

        let typed_then = self.check_statements(then_branch);

        let typed_elseifs: Vec<(TypedExpr, Vec<TypedStatement>)> = elseif_branches
            .iter()
            .map(|(cond, stmts)| {
                let tc = self.check_expr(cond);
                if !tc.basic_type.is_numeric() {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: "numeric".to_string(),
                        found: tc.basic_type.to_string(),
                        span: cond.span,
                    });
                }
                (tc, self.check_statements(stmts))
            })
            .collect();

        let typed_else = else_branch
            .as_ref()
            .map(|stmts| self.check_statements(stmts));

        TypedStatement::new(
            TypedStatementKind::If {
                condition: typed_condition,
                then_branch: typed_then,
                elseif_branches: typed_elseifs,
                else_branch: typed_else,
            },
            span,
        )
    }

    // ========================================================================
    // SELECT CASE Statement
    // ========================================================================

    /// Type checks a SELECT CASE statement.
    pub(super) fn check_select_case(
        &mut self,
        test_expr: &Expr,
        cases: &[crate::ast::CaseClause],
        case_else: &Option<Vec<Statement>>,
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_test = self.check_expr(test_expr);

        let typed_cases: Vec<TypedCaseClause> = cases
            .iter()
            .map(|case| TypedCaseClause {
                matches: case
                    .matches
                    .iter()
                    .map(|m| self.check_case_match(m, &typed_test.basic_type))
                    .collect(),
                body: self.check_statements(&case.body),
            })
            .collect();

        let typed_else = case_else.as_ref().map(|stmts| self.check_statements(stmts));

        TypedStatement::new(
            TypedStatementKind::SelectCase {
                test_expr: typed_test,
                cases: typed_cases,
                case_else: typed_else,
            },
            span,
        )
    }

    /// Type checks a CASE match.
    fn check_case_match(&mut self, m: &CaseMatch, test_type: &BasicType) -> TypedCaseMatch {
        match m {
            CaseMatch::Single(expr) => {
                let typed = self.check_expr(expr);
                if !self.types_comparable(&typed.basic_type, test_type) {
                    self.errors.push(SemanticError::TypeMismatch {
                        expected: test_type.to_string(),
                        found: typed.basic_type.to_string(),
                        span: expr.span,
                    });
                }
                TypedCaseMatch::Single(typed)
            }
            CaseMatch::Range { from, to } => {
                let typed_from = self.check_expr(from);
                let typed_to = self.check_expr(to);
                TypedCaseMatch::Range {
                    from: typed_from,
                    to: typed_to,
                }
            }
            CaseMatch::Comparison { op, value } => {
                let typed_value = self.check_expr(value);
                TypedCaseMatch::Comparison {
                    op: match op {
                        CaseCompareOp::Equal => TypedCaseCompareOp::Equal,
                        CaseCompareOp::NotEqual => TypedCaseCompareOp::NotEqual,
                        CaseCompareOp::LessThan => TypedCaseCompareOp::LessThan,
                        CaseCompareOp::LessEqual => TypedCaseCompareOp::LessEqual,
                        CaseCompareOp::GreaterThan => TypedCaseCompareOp::GreaterThan,
                        CaseCompareOp::GreaterEqual => TypedCaseCompareOp::GreaterEqual,
                    },
                    value: typed_value,
                }
            }
        }
    }

    // ========================================================================
    // FOR Loop
    // ========================================================================

    /// Type checks a FOR loop.
    pub(super) fn check_for(&mut self, info: ForLoopInfo<'_>) -> TypedStatement {
        let ForLoopInfo {
            variable,
            start,
            end,
            step,
            body,
            next_variable,
            span,
        } = info;

        // Validate NEXT variable matches FOR variable (case-insensitive)
        if let Some(next_var) = next_variable
            && !next_var.eq_ignore_ascii_case(variable)
        {
            self.errors.push(SemanticError::ForNextMismatch {
                expected: variable.to_string(),
                found: next_var.to_string(),
                for_span: span,
                next_span: span, // Ideally we'd have separate spans
            });
        }

        let typed_start = self.check_expr(start);
        let typed_end = self.check_expr(end);

        if !typed_start.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_start.basic_type.to_string(),
                span: start.span,
            });
        }

        if !typed_end.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_end.basic_type.to_string(),
                span: end.span,
            });
        }

        let typed_step = step.as_ref().map(|s| {
            let ts = self.check_expr(s);
            if !ts.basic_type.is_numeric() {
                self.errors.push(SemanticError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: ts.basic_type.to_string(),
                    span: s.span,
                });
            }
            ts
        });

        // Determine loop variable type
        let var_type = if let Some(sym) = self.symbols.lookup_symbol(variable) {
            sym.basic_type.clone()
        } else {
            let t = typed_start
                .basic_type
                .common_type(&typed_end.basic_type)
                .unwrap_or(BasicType::Single);
            let symbol = Symbol {
                name: variable.to_string(),
                kind: SymbolKind::Variable,
                basic_type: t.clone(),
                span,
                is_mutable: true,
            };
            let _ = self.symbols.define_symbol(symbol);
            t
        };

        // Check body with increased FOR depth
        self.loop_context.for_depth += 1;
        let typed_body = self.check_statements(body);
        self.loop_context.for_depth -= 1;

        TypedStatement::new(
            TypedStatementKind::For {
                variable: variable.to_string(),
                var_type,
                start: typed_start,
                end: typed_end,
                step: typed_step,
                body: typed_body,
            },
            span,
        )
    }

    // ========================================================================
    // WHILE Loop
    // ========================================================================

    /// Type checks a WHILE loop.
    pub(super) fn check_while(
        &mut self,
        condition: &Expr,
        body: &[Statement],
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_condition = self.check_expr(condition);

        if !typed_condition.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_condition.basic_type.to_string(),
                span: condition.span,
            });
        }

        self.loop_context.while_depth += 1;
        let typed_body = self.check_statements(body);
        self.loop_context.while_depth -= 1;

        TypedStatement::new(
            TypedStatementKind::While {
                condition: typed_condition,
                body: typed_body,
            },
            span,
        )
    }

    // ========================================================================
    // DO Loop
    // ========================================================================

    /// Type checks a DO loop.
    pub(super) fn check_do_loop(
        &mut self,
        pre_condition: &Option<DoCondition>,
        body: &[Statement],
        post_condition: &Option<DoCondition>,
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_pre = pre_condition.as_ref().map(|c| {
            let tc = self.check_expr(&c.condition);
            TypedDoCondition {
                is_while: c.is_while,
                condition: tc,
            }
        });

        self.loop_context.do_depth += 1;
        let typed_body = self.check_statements(body);
        self.loop_context.do_depth -= 1;

        let typed_post = post_condition.as_ref().map(|c| {
            let tc = self.check_expr(&c.condition);
            TypedDoCondition {
                is_while: c.is_while,
                condition: tc,
            }
        });

        TypedStatement::new(
            TypedStatementKind::DoLoop {
                pre_condition: typed_pre,
                body: typed_body,
                post_condition: typed_post,
            },
            span,
        )
    }

    // ========================================================================
    // Flow Control Statements
    // ========================================================================

    /// Type checks a GOTO statement.
    pub(super) fn check_goto(&mut self, target: &str, span: crate::ast::Span) -> TypedStatement {
        if self.symbols.lookup_label(target).is_none() {
            self.errors.push(SemanticError::UndefinedLabel {
                name: target.to_string(),
                span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Goto {
                target: target.to_string(),
            },
            span,
        )
    }

    /// Type checks a GOSUB statement.
    pub(super) fn check_gosub(&mut self, target: &str, span: crate::ast::Span) -> TypedStatement {
        if self.symbols.lookup_label(target).is_none() {
            self.errors.push(SemanticError::UndefinedLabel {
                name: target.to_string(),
                span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Gosub {
                target: target.to_string(),
            },
            span,
        )
    }

    /// Type checks a RETURN statement.
    pub(super) fn check_return(&mut self, span: crate::ast::Span) -> TypedStatement {
        // RETURN is valid after GOSUB or in a FUNCTION
        // We can't fully validate GOSUB context at compile time
        TypedStatement::new(TypedStatementKind::Return, span)
    }

    /// Type checks an EXIT statement.
    pub(super) fn check_exit(
        &mut self,
        exit_type: crate::ast::ExitType,
        span: crate::ast::Span,
    ) -> TypedStatement {
        use crate::ast::ExitType;

        let valid = match exit_type {
            ExitType::For => self.loop_context.for_depth > 0,
            ExitType::While => self.loop_context.while_depth > 0,
            ExitType::Do => self.loop_context.do_depth > 0,
            ExitType::Sub => self.in_sub,
            ExitType::Function => self.in_function,
        };

        if !valid {
            let exit_name = match exit_type {
                ExitType::For => "FOR",
                ExitType::While => "WHILE",
                ExitType::Do => "DO",
                ExitType::Sub => "SUB",
                ExitType::Function => "FUNCTION",
            };
            self.errors.push(SemanticError::ExitOutsideLoop {
                exit_type: exit_name.to_string(),
                span,
            });
        }

        TypedStatement::new(TypedStatementKind::Exit { exit_type }, span)
    }

    /// Type checks a CALL statement.
    pub(super) fn check_call(
        &mut self,
        name: &str,
        args: &[Expr],
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_args: Vec<TypedExpr> = args.iter().map(|a| self.check_expr(a)).collect();

        if let Some(proc) = self.symbols.lookup_procedure(name) {
            // Check argument count
            if args.len() != proc.params.len() {
                self.errors.push(SemanticError::ArgumentCountMismatch {
                    name: name.to_string(),
                    expected: proc.params.len(),
                    found: args.len(),
                    span,
                });
            }

            // Check argument types
            for (i, (arg, typed_arg)) in args.iter().zip(&typed_args).enumerate() {
                if i < proc.params.len() {
                    let param = &proc.params[i];
                    if !typed_arg.basic_type.is_convertible_to(&param.basic_type) {
                        self.errors.push(SemanticError::ArgumentTypeMismatch {
                            position: i + 1,
                            expected: param.basic_type.to_string(),
                            found: typed_arg.basic_type.to_string(),
                            span: arg.span,
                        });
                    }
                }
            }
        } else {
            self.errors.push(SemanticError::UndefinedProcedure {
                name: name.to_string(),
                span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Call {
                name: name.to_string(),
                args: typed_args,
            },
            span,
        )
    }
}
