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

    /// Type checks a DIM statement (may declare multiple variables).
    pub(super) fn check_dim(
        &mut self,
        variables: &[crate::ast::DimVariable],
        shared: bool,
        span: crate::ast::Span,
    ) -> TypedStatement {
        use crate::semantic::typed_ir::TypedDimVariable;

        // DIM SHARED at module level is valid - it declares a shared variable that procedures
        // can access via the SHARED statement inside the procedure.
        // DIM SHARED inside a procedure makes that variable accessible to inner procedures
        // (though this is less common).

        let mut typed_variables = Vec::new();

        for var in variables {
            // Determine type
            let basic_type = var
                .type_spec
                .as_ref()
                .map(from_type_spec)
                .or_else(|| type_from_suffix(&var.name))
                .unwrap_or_else(|| self.symbols.default_type_for(&var.name));

            // Mark as shared if applicable
            if shared && self.symbols.in_procedure() {
                self.symbols.add_shared_var(var.name.clone());
            }

            // Evaluate array dimensions - bounds must be constant expressions
            let typed_dims: Vec<TypedArrayDimension> = var
                .dimensions
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
            let symbol_kind = if var.dimensions.is_empty() {
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
                name: var.name.clone(),
                kind: symbol_kind,
                basic_type: basic_type.clone(),
                span,
                is_mutable: true,
            };

            if let Err(err) = self.symbols.define_symbol(symbol) {
                let (existing, _) = *err;
                self.errors.push(SemanticError::DuplicateVariable {
                    name: var.name.clone(),
                    original_span: existing.span,
                    duplicate_span: span,
                });
            }

            typed_variables.push(TypedDimVariable {
                name: var.name.clone(),
                basic_type,
                dimensions: typed_dims,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Dim {
                variables: typed_variables,
                shared,
            },
            span,
        )
    }

    // ========================================================================
    // CONST Statement
    // ========================================================================

    /// Type checks a CONST statement (may define multiple constants).
    pub(super) fn check_const(
        &mut self,
        definitions: &[(String, Expr)],
        span: crate::ast::Span,
    ) -> TypedStatement {
        let mut typed_definitions = Vec::new();

        for (name, value) in definitions {
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

            typed_definitions.push((name.clone(), typed_value, basic_type));
        }

        TypedStatement::new(
            TypedStatementKind::Const {
                definitions: typed_definitions,
            },
            span,
        )
    }

    // ========================================================================
    // DEFxxx Statement
    // ========================================================================

    /// Type checks a DEFxxx statement (DEFINT, DEFLNG, DEFSNG, DEFDBL, DEFSTR).
    pub(super) fn check_deftype(
        &mut self,
        type_kind: &crate::ast::DefTypeKind,
        ranges: &[(char, char)],
        span: crate::ast::Span,
    ) -> TypedStatement {
        use crate::ast::DefTypeKind;
        use crate::semantic::types::BasicType;

        // Convert DefTypeKind to BasicType
        let basic_type = match type_kind {
            DefTypeKind::Integer => BasicType::Integer,
            DefTypeKind::Long => BasicType::Long,
            DefTypeKind::Single => BasicType::Single,
            DefTypeKind::Double => BasicType::Double,
            DefTypeKind::String => BasicType::String,
        };

        // Apply the type defaults to the symbol table
        for &(start, end) in ranges {
            self.symbols
                .set_default_type(start, end, basic_type.clone());
        }

        // The DEFxxx statement doesn't generate code - it only affects the symbol table
        TypedStatement::new(TypedStatementKind::DefType, span)
    }

    /// Type checks an OPTION BASE statement.
    ///
    /// OPTION BASE sets the default lower bound for array subscripts.
    /// It must appear before any array declarations and can only be 0 or 1.
    pub(super) fn check_option_base(
        &mut self,
        base: i64,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Validate that base is 0 or 1 (parser should catch this, but be defensive)
        if base != 0 && base != 1 {
            self.errors
                .push(SemanticError::InvalidOptionBase { value: base, span });
        }

        // Set the option base in the symbol table
        self.symbols.set_option_base(base);

        // OPTION BASE doesn't generate code - it only affects the symbol table
        TypedStatement::new(TypedStatementKind::OptionBase, span)
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

        // Collect labels from body for forward reference support (QB45 local GOSUB pattern)
        self.collect_labels_from_body(body);

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

        // Collect labels from body for forward reference support (QB45 local GOSUB pattern)
        self.collect_labels_from_body(body);

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
