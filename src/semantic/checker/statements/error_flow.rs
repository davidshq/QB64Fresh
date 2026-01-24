//! Error handling and computed control flow statement type checking.
//!
//! This module handles type checking for:
//! - ON ERROR GOTO/RESUME
//! - RESUME statements
//! - ERROR statement
//! - ON...GOTO/GOSUB computed branches
//! - DEF FN/DEF SEG

use crate::ast::{Expr, Parameter, ResumeTarget, Span, Statement};
use crate::semantic::{
    error::SemanticError,
    symbols::{ScopeKind, Symbol, SymbolKind},
    typed_ir::*,
    types::{BasicType, from_type_spec, type_from_suffix},
};

use super::super::TypeChecker;

impl<'a> TypeChecker<'a> {
    /// Type checks ON ERROR GOTO statement.
    pub(in crate::semantic::checker) fn check_on_error_goto(
        &mut self,
        target: &str,
        span: Span,
    ) -> TypedStatement {
        TypedStatement::new(
            TypedStatementKind::OnErrorGoto {
                target: target.to_string(),
            },
            span,
        )
    }

    /// Type checks ON ERROR RESUME NEXT statement.
    pub(in crate::semantic::checker) fn check_on_error_resume_next(
        &mut self,
        span: Span,
    ) -> TypedStatement {
        TypedStatement::new(TypedStatementKind::OnErrorResumeNext, span)
    }

    /// Type checks RESUME statement.
    pub(in crate::semantic::checker) fn check_resume_stmt(
        &mut self,
        target: &Option<ResumeTarget>,
        span: Span,
    ) -> TypedStatement {
        TypedStatement::new(
            TypedStatementKind::ResumeStmt {
                target: target.clone(),
            },
            span,
        )
    }

    /// Type checks ERROR statement.
    pub(in crate::semantic::checker) fn check_error_stmt(
        &mut self,
        code: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_code = self.check_expr(code);

        TypedStatement::new(TypedStatementKind::ErrorStmt { code: typed_code }, span)
    }

    /// Type checks ON...GOTO statement.
    pub(in crate::semantic::checker) fn check_on_goto(
        &mut self,
        selector: &Expr,
        targets: &[String],
        span: Span,
    ) -> TypedStatement {
        let typed_selector = self.check_expr(selector);

        // Selector should be numeric
        if !typed_selector.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_selector.basic_type.to_string(),
                span: typed_selector.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::OnGoto {
                selector: typed_selector,
                targets: targets.to_vec(),
            },
            span,
        )
    }

    /// Type checks ON...GOSUB statement.
    pub(in crate::semantic::checker) fn check_on_gosub(
        &mut self,
        selector: &Expr,
        targets: &[String],
        span: Span,
    ) -> TypedStatement {
        let typed_selector = self.check_expr(selector);

        if !typed_selector.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_selector.basic_type.to_string(),
                span: typed_selector.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::OnGosub {
                selector: typed_selector,
                targets: targets.to_vec(),
            },
            span,
        )
    }

    /// Type checks DEF FN (single-line function definition).
    pub(in crate::semantic::checker) fn check_def_fn(
        &mut self,
        name: &str,
        params: &[Parameter],
        body: &Expr,
        span: Span,
    ) -> TypedStatement {
        // Enter a new scope for the function
        self.symbols.enter_scope(ScopeKind::Function);

        // Define parameters in the scope
        let typed_params: Vec<TypedParameter> = params
            .iter()
            .map(|p| {
                let param_type = p
                    .type_spec
                    .as_ref()
                    .map(from_type_spec)
                    .or_else(|| type_from_suffix(&p.name))
                    .unwrap_or(BasicType::Single); // DEF FN defaults to Single
                let symbol = Symbol {
                    name: p.name.clone(),
                    kind: SymbolKind::Variable,
                    basic_type: param_type.clone(),
                    span,
                    is_mutable: !p.by_val,
                };
                let _ = self.symbols.define_symbol(symbol);

                TypedParameter {
                    name: p.name.clone(),
                    basic_type: param_type,
                    by_val: p.by_val,
                    is_array: false, // DEF FN doesn't support array params
                }
            })
            .collect();

        // Check the body expression
        let typed_body = self.check_expr(body);

        // Return type is inferred from the function name suffix or body
        let return_type = type_from_suffix(name).unwrap_or_else(|| typed_body.basic_type.clone());

        self.symbols.exit_scope();

        TypedStatement::new(
            TypedStatementKind::DefFn {
                name: name.to_string(),
                params: typed_params,
                return_type,
                body: typed_body,
            },
            span,
        )
    }

    /// Type checks DEF FN (multi-line function definition).
    pub(in crate::semantic::checker) fn check_def_fn_multi_line(
        &mut self,
        name: &str,
        params: &[Parameter],
        body: &[Statement],
        span: Span,
    ) -> TypedStatement {
        // Enter a new scope for the function
        self.symbols.enter_scope(ScopeKind::Function);

        // Define parameters in the scope
        let typed_params: Vec<TypedParameter> = params
            .iter()
            .map(|p| {
                let param_type = p
                    .type_spec
                    .as_ref()
                    .map(from_type_spec)
                    .or_else(|| type_from_suffix(&p.name))
                    .unwrap_or_else(|| self.symbols.default_type_for(&p.name));

                // Define parameter as a local variable
                let symbol = Symbol {
                    name: p.name.clone(),
                    kind: SymbolKind::Parameter { by_val: p.by_val },
                    basic_type: param_type.clone(),
                    span,
                    is_mutable: true,
                };
                let _ = self.symbols.define_symbol(symbol);

                TypedParameter {
                    name: p.name.clone(),
                    basic_type: param_type,
                    by_val: p.by_val,
                    is_array: p.is_array,
                }
            })
            .collect();

        // Check the body statements
        let typed_body: Vec<TypedStatement> =
            body.iter().map(|s| self.check_statement(s)).collect();

        // Return type is inferred from the function name suffix or defaults to SINGLE
        let return_type = type_from_suffix(name).unwrap_or(BasicType::Single);

        self.symbols.exit_scope();

        TypedStatement::new(
            TypedStatementKind::DefFnMultiLine {
                name: name.to_string(),
                params: typed_params,
                return_type,
                body: typed_body,
            },
            span,
        )
    }

    /// Type checks DEF SEG statement.
    pub(in crate::semantic::checker) fn check_def_seg(
        &mut self,
        segment: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_segment = segment.map(|e| self.check_expr(e));
        TypedStatement::new(
            TypedStatementKind::DefSeg {
                segment: typed_segment,
            },
            span,
        )
    }
}
