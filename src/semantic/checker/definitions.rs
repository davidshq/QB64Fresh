//! Definition statement type checking.
//!
//! This module handles type checking for:
//! - DIM statements (variable/array declarations)
//! - CONST statements (constant declarations)
//! - SUB definitions
//! - FUNCTION definitions

use crate::ast::{Expr, Statement};
use crate::semantic::{
    error::SemanticError,
    symbols::{ScopeKind, Symbol, SymbolKind},
    typed_ir::*,
    types::{from_type_spec, type_from_suffix},
};

use super::TypeChecker;

impl<'a> TypeChecker<'a> {
    // ========================================================================
    // DIM Statement
    // ========================================================================

    /// Type checks a DIM statement.
    pub(super) fn check_dim(
        &mut self,
        name: &str,
        dimensions: &[crate::ast::ArrayDimension],
        type_spec: &Option<crate::ast::TypeSpec>,
        shared: bool,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Determine type
        let basic_type = type_spec
            .as_ref()
            .map(from_type_spec)
            .or_else(|| type_from_suffix(name))
            .unwrap_or_else(|| self.symbols.default_type_for(name));

        // Handle SHARED
        if shared {
            if !self.symbols.in_procedure() {
                self.errors
                    .push(SemanticError::SharedOutsideProcedure { span });
            } else {
                self.symbols.add_shared_var(name.to_string());
            }
        }

        // Evaluate array dimensions - bounds must be constant expressions
        let typed_dims: Vec<TypedArrayDimension> = dimensions
            .iter()
            .map(|d| {
                // Evaluate lower bound (if provided)
                let lower = if let Some(lower_expr) = &d.lower {
                    let typed_lower = self.check_expr(lower_expr);
                    match self.try_evaluate_const_expr(&typed_lower) {
                        Some(crate::semantic::symbols::ConstValue::Integer(v)) => v,
                        Some(crate::semantic::symbols::ConstValue::Float(v)) => v as i64,
                        _ => {
                            self.errors.push(SemanticError::NonConstantExpression {
                                span: lower_expr.span,
                            });
                            0 // Default on error
                        }
                    }
                } else {
                    self.symbols.option_base()
                };

                // Evaluate upper bound (required)
                let typed_upper = self.check_expr(&d.upper);
                let upper = match self.try_evaluate_const_expr(&typed_upper) {
                    Some(crate::semantic::symbols::ConstValue::Integer(v)) => v,
                    Some(crate::semantic::symbols::ConstValue::Float(v)) => v as i64,
                    _ => {
                        self.errors
                            .push(SemanticError::NonConstantExpression { span: d.upper.span });
                        10 // Default on error
                    }
                };

                TypedArrayDimension { lower, upper }
            })
            .collect();

        // Define symbol
        let symbol_kind = if dimensions.is_empty() {
            SymbolKind::Variable
        } else {
            SymbolKind::ArrayVariable {
                dimensions: typed_dims
                    .iter()
                    .map(|d| crate::semantic::symbols::ArrayDimInfo {
                        lower_bound: d.lower,
                        upper_bound: d.upper,
                    })
                    .collect(),
            }
        };

        let symbol = Symbol {
            name: name.to_string(),
            kind: symbol_kind,
            basic_type: basic_type.clone(),
            span,
            is_mutable: true,
        };

        if let Err(err) = self.symbols.define_symbol(symbol) {
            let (existing, _) = *err;
            self.errors.push(SemanticError::DuplicateVariable {
                name: name.to_string(),
                original_span: existing.span,
                duplicate_span: span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Dim {
                name: name.to_string(),
                basic_type,
                dimensions: typed_dims,
                shared,
            },
            span,
        )
    }

    // ========================================================================
    // CONST Statement
    // ========================================================================

    /// Type checks a CONST statement.
    pub(super) fn check_const(
        &mut self,
        name: &str,
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_value = self.check_expr(value);
        let basic_type = typed_value.basic_type.clone();

        // Try to evaluate the expression as a compile-time constant
        let const_value = match self.try_evaluate_const_expr(&typed_value) {
            Some(cv) => cv,
            None => {
                self.errors
                    .push(SemanticError::NonConstantExpression { span: value.span });
                crate::semantic::symbols::ConstValue::Integer(0)
            }
        };

        let symbol = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Constant { value: const_value },
            basic_type: basic_type.clone(),
            span,
            is_mutable: false,
        };

        if let Err(err) = self.symbols.define_symbol(symbol) {
            let (existing, _) = *err;
            self.errors.push(SemanticError::DuplicateVariable {
                name: name.to_string(),
                original_span: existing.span,
                duplicate_span: span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Const {
                name: name.to_string(),
                value: typed_value,
                basic_type,
            },
            span,
        )
    }

    // ========================================================================
    // SUB Definition
    // ========================================================================

    /// Type checks a SUB definition.
    pub(super) fn check_sub_definition(
        &mut self,
        name: &str,
        params: &[crate::ast::Parameter],
        body: &[Statement],
        is_static: bool,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Enter SUB scope
        self.symbols.enter_scope(ScopeKind::Sub);
        self.in_sub = true;

        // Define parameters in local scope
        let typed_params: Vec<TypedParameter> = params
            .iter()
            .map(|p| {
                let basic_type = p
                    .type_spec
                    .as_ref()
                    .map(from_type_spec)
                    .or_else(|| type_from_suffix(&p.name))
                    .unwrap_or_else(|| self.symbols.default_type_for(&p.name));

                let symbol = Symbol {
                    name: p.name.clone(),
                    kind: SymbolKind::Parameter { by_val: p.by_val },
                    basic_type: basic_type.clone(),
                    span,
                    is_mutable: true,
                };
                let _ = self.symbols.define_symbol(symbol);

                TypedParameter {
                    name: p.name.clone(),
                    basic_type,
                    by_val: p.by_val,
                }
            })
            .collect();

        // Check body
        let typed_body = self.check_statements(body);

        // Exit scope
        self.in_sub = false;
        self.symbols.exit_scope();

        TypedStatement::new(
            TypedStatementKind::SubDefinition {
                name: name.to_string(),
                params: typed_params,
                body: typed_body,
                is_static,
            },
            span,
        )
    }

    // ========================================================================
    // FUNCTION Definition
    // ========================================================================

    /// Type checks a FUNCTION definition.
    pub(super) fn check_function_definition(
        &mut self,
        name: &str,
        params: &[crate::ast::Parameter],
        return_type: &Option<crate::ast::TypeSpec>,
        body: &[Statement],
        is_static: bool,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Determine return type
        let ret_type = return_type
            .as_ref()
            .map(from_type_spec)
            .or_else(|| type_from_suffix(name))
            .unwrap_or_else(|| self.symbols.default_type_for(name));

        // Enter FUNCTION scope
        self.symbols.enter_scope(ScopeKind::Function);
        self.in_function = true;
        self.current_function_name = Some(name.to_string());

        // Define function name as local variable for return value
        let return_var = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Variable,
            basic_type: ret_type.clone(),
            span,
            is_mutable: true,
        };
        let _ = self.symbols.define_symbol(return_var);

        // Define parameters
        let typed_params: Vec<TypedParameter> = params
            .iter()
            .map(|p| {
                let basic_type = p
                    .type_spec
                    .as_ref()
                    .map(from_type_spec)
                    .or_else(|| type_from_suffix(&p.name))
                    .unwrap_or_else(|| self.symbols.default_type_for(&p.name));

                let symbol = Symbol {
                    name: p.name.clone(),
                    kind: SymbolKind::Parameter { by_val: p.by_val },
                    basic_type: basic_type.clone(),
                    span,
                    is_mutable: true,
                };
                let _ = self.symbols.define_symbol(symbol);

                TypedParameter {
                    name: p.name.clone(),
                    basic_type,
                    by_val: p.by_val,
                }
            })
            .collect();

        // Check body
        let typed_body = self.check_statements(body);

        // Exit scope
        self.in_function = false;
        self.current_function_name = None;
        self.symbols.exit_scope();

        TypedStatement::new(
            TypedStatementKind::FunctionDefinition {
                name: name.to_string(),
                params: typed_params,
                return_type: ret_type,
                body: typed_body,
                is_static,
            },
            span,
        )
    }
}
