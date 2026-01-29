//! DATA and READ statement type checking.
//!
//! This module handles type checking for data-related statements:
//! - DATA (literal values)
//! - READ (reading into variables)
//! - RESTORE (reset data pointer)
//! - RANDOMIZE (seed RNG)

use crate::ast::{DataValue, Expr, ReadTarget, Span};
use crate::semantic::{
    error::SemanticError,
    symbols::{Symbol, SymbolKind},
    typed_ir::*,
    types::{BasicType, type_from_suffix},
};

use super::super::TypeChecker;

impl<'a> TypeChecker<'a> {
    /// Type checks DATA statement.
    pub(in crate::semantic::checker) fn check_data(
        &mut self,
        values: &[DataValue],
        span: Span,
    ) -> TypedStatement {
        // Convert AST data values to typed data values
        let typed_values: Vec<TypedDataValue> = values
            .iter()
            .map(|v| match v {
                DataValue::Integer(n) => TypedDataValue::Integer(*n),
                DataValue::Float(f) => TypedDataValue::Float(*f),
                DataValue::String(s) => TypedDataValue::String(s.clone()),
            })
            .collect();

        TypedStatement::new(
            TypedStatementKind::Data {
                values: typed_values,
            },
            span,
        )
    }

    /// Type checks READ statement.
    pub(in crate::semantic::checker) fn check_read(
        &mut self,
        targets: &[ReadTarget],
        span: Span,
    ) -> TypedStatement {
        // Process each target (variable or array element)
        let typed_targets: Vec<TypedReadTarget> = targets
            .iter()
            .map(|target| self.check_read_target(target, span))
            .collect();

        TypedStatement::new(
            TypedStatementKind::Read {
                targets: typed_targets,
            },
            span,
        )
    }

    /// Helper to type check a READ target.
    fn check_read_target(&mut self, target: &ReadTarget, span: Span) -> TypedReadTarget {
        match target {
            ReadTarget::Variable(var_name) => {
                let var_type = if let Some(symbol) = self.symbols.lookup_symbol(var_name) {
                    // Variable exists - use existing type
                    symbol.basic_type.clone()
                } else {
                    // Infer type from suffix or default
                    let inferred = type_from_suffix(var_name)
                        .unwrap_or_else(|| self.symbols.default_type_for(var_name));

                    // Define the variable
                    let symbol = Symbol {
                        name: var_name.clone(),
                        kind: SymbolKind::Variable,
                        basic_type: inferred.clone(),
                        span,
                        is_mutable: true,
                    };
                    // Should never fail since we checked lookup_symbol, but handle it
                    if let Err(dup) = self.symbols.define_symbol(symbol) {
                        let (existing, _) = *dup;
                        // Type conflict - variable was defined elsewhere
                        self.errors.push(SemanticError::DuplicateVariable {
                            name: var_name.clone(),
                            original_span: existing.span,
                            duplicate_span: span,
                        });
                        // Use existing type to avoid cascading errors
                        existing.basic_type.clone()
                    } else {
                        inferred
                    }
                };
                TypedReadTarget::Variable {
                    name: var_name.clone(),
                    basic_type: var_type,
                }
            }
            ReadTarget::ArrayElement { name, indices } => {
                // Type check indices
                let typed_indices: Vec<_> = indices.iter().map(|e| self.check_expr(e)).collect();

                // Look up array and get element type
                let basic_type = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                    symbol.basic_type.clone()
                } else {
                    // Array not declared - error with suggestions
                    let candidates = self.symbols.collect_available_variable_names();
                    let suggestion =
                        crate::semantic::suggestions::find_best_match(name, &candidates, 0.6);
                    let suggestions = if suggestion.is_some() {
                        None
                    } else {
                        let similar = crate::semantic::suggestions::find_similar_names(
                            name,
                            &candidates,
                            0.4,
                            3,
                        );
                        if similar.is_empty() {
                            None
                        } else {
                            Some(similar)
                        }
                    };
                    self.errors
                        .push(SemanticError::undefined_variable_with_suggestions(
                            name.clone(),
                            span,
                            suggestion,
                            suggestions,
                        ));
                    BasicType::Single // Default on error
                };

                TypedReadTarget::ArrayElement {
                    name: name.clone(),
                    indices: typed_indices,
                    basic_type,
                }
            }
            ReadTarget::ArrayFieldElement {
                name,
                indices,
                field,
            } => {
                // Type check indices
                let typed_indices: Vec<_> = indices.iter().map(|e| self.check_expr(e)).collect();

                // Look up array and get element type, then resolve field type
                let basic_type = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                    // Get the element type of the array
                    let element_type = match &symbol.basic_type {
                        BasicType::Array { element_type, .. } => (**element_type).clone(),
                        other => other.clone(),
                    };

                    // Resolve the field type from the UDT
                    let field_type =
                        self.resolve_field_chain_type(&element_type, std::slice::from_ref(field));

                    if field_type == BasicType::Unknown {
                        // Fall back to suffix-based inference if UDT resolution fails
                        type_from_suffix(field)
                            .unwrap_or_else(|| self.symbols.default_type_for(field))
                    } else {
                        field_type
                    }
                } else {
                    // Array not declared - error with suggestions
                    let candidates = self.symbols.collect_available_variable_names();
                    let suggestion =
                        crate::semantic::suggestions::find_best_match(name, &candidates, 0.6);
                    let suggestions = if suggestion.is_some() {
                        None
                    } else {
                        let similar = crate::semantic::suggestions::find_similar_names(
                            name,
                            &candidates,
                            0.4,
                            3,
                        );
                        if similar.is_empty() {
                            None
                        } else {
                            Some(similar)
                        }
                    };
                    self.errors
                        .push(SemanticError::undefined_variable_with_suggestions(
                            name.clone(),
                            span,
                            suggestion,
                            suggestions,
                        ));
                    BasicType::Single // Default on error
                };

                TypedReadTarget::ArrayFieldElement {
                    name: name.clone(),
                    indices: typed_indices,
                    field: field.clone(),
                    basic_type,
                }
            }
        }
    }

    /// Type checks RESTORE statement.
    pub(in crate::semantic::checker) fn check_restore(
        &mut self,
        label: Option<&str>,
        span: Span,
    ) -> TypedStatement {
        TypedStatement::new(
            TypedStatementKind::Restore {
                label: label.map(|s| s.to_string()),
            },
            span,
        )
    }

    /// Type checks RANDOMIZE statement.
    pub(in crate::semantic::checker) fn check_randomize(
        &mut self,
        seed: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        // Type check the seed expression if provided
        let typed_seed = seed.map(|s| self.check_expr(s));

        // Seed should be a numeric type (but we'll allow any for flexibility)
        if let Some(ref typed) = typed_seed
            && typed.basic_type.is_string()
        {
            self.errors
                .push(SemanticError::type_mismatch("numeric", "STRING", span));
        }

        TypedStatement::new(TypedStatementKind::Randomize { seed: typed_seed }, span)
    }
}

/// Dispatch function for data statements.
pub(super) fn check_data_stmt(
    checker: &mut super::super::TypeChecker,
    kind: &crate::ast::StatementKind,
    span: crate::ast::Span,
) -> crate::semantic::typed_ir::TypedStatement {
    match kind {
        crate::ast::StatementKind::Data { values } => checker.check_data(values, span),
        crate::ast::StatementKind::Read { targets } => checker.check_read(targets, span),
        crate::ast::StatementKind::Restore { label } => {
            checker.check_restore(label.as_deref(), span)
        }
        crate::ast::StatementKind::Randomize { seed } => {
            checker.check_randomize(seed.as_ref(), span)
        }
        _ => unreachable!("Not a data statement"),
    }
}
