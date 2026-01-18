//! Assignment and I/O statement type checking.
//!
//! This module handles type checking for:
//! - Variable assignments (LET)
//! - Array element assignments
//! - PRINT statements
//! - INPUT statements
//! - LINE INPUT statements

use crate::ast::{Expr, PrintItem};
use crate::semantic::{
    error::SemanticError,
    symbols::{Symbol, SymbolKind},
    typed_ir::*,
    types::{BasicType, type_from_suffix},
};

use super::TypeChecker;

impl<'a> TypeChecker<'a> {
    // ========================================================================
    // Assignment Checking
    // ========================================================================

    /// Type checks an assignment statement.
    pub(super) fn check_assignment(
        &mut self,
        name: &str,
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Check if assigning to a constant
        if let Some(symbol) = self.symbols.lookup_symbol(name)
            && !symbol.is_mutable
        {
            self.errors.push(SemanticError::AssignmentToConst {
                name: name.to_string(),
                span,
            });
        }

        let typed_value = self.check_expr(value);

        // Determine target type
        let target_type = if let Some(symbol) = self.symbols.lookup_symbol(name) {
            symbol.basic_type.clone()
        } else {
            // New variable, infer from suffix or default
            let inferred =
                type_from_suffix(name).unwrap_or_else(|| self.symbols.default_type_for(name));

            // Define the new variable
            let symbol = Symbol {
                name: name.to_string(),
                kind: SymbolKind::Variable,
                basic_type: inferred.clone(),
                span,
                is_mutable: true,
            };
            let _ = self.symbols.define_symbol(symbol);

            inferred
        };

        // Check type compatibility
        if !typed_value.basic_type.is_convertible_to(&target_type) {
            self.errors.push(SemanticError::TypeMismatch {
                expected: target_type.to_string(),
                found: typed_value.basic_type.to_string(),
                span: value.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::Assignment {
                name: name.to_string(),
                value: typed_value,
                target_type,
            },
            span,
        )
    }

    /// Type checks an array element assignment.
    pub(super) fn check_array_assignment(
        &mut self,
        name: &str,
        indices: &[Expr],
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Look up the array
        let (element_type, dimensions) = if let Some(symbol) = self.symbols.lookup_symbol(name)
            && let SymbolKind::ArrayVariable { dimensions } = &symbol.kind
        {
            // Verify dimension count
            if indices.len() != dimensions.len() {
                self.errors.push(SemanticError::ArrayDimensionMismatch {
                    name: name.to_string(),
                    expected: dimensions.len(),
                    found: indices.len(),
                    span,
                });
            }

            let typed_dims: Vec<TypedArrayDimension> = dimensions
                .iter()
                .map(|d| TypedArrayDimension {
                    lower: d.lower_bound,
                    upper: d.upper_bound,
                })
                .collect();

            (symbol.basic_type.clone(), typed_dims)
        } else {
            self.errors.push(SemanticError::NotAnArray {
                name: name.to_string(),
                span,
            });
            (BasicType::Unknown, Vec::new())
        };

        // Check and type the indices
        let mut typed_indices = Vec::new();
        for idx in indices {
            let typed_idx = self.check_expr(idx);
            if !typed_idx.basic_type.is_numeric() {
                self.errors.push(SemanticError::NonNumericIndex {
                    found: typed_idx.basic_type.to_string(),
                    span: idx.span,
                });
            }
            typed_indices.push(typed_idx);
        }

        // Check the value
        let typed_value = self.check_expr(value);

        // Type compatibility check
        if !typed_value.basic_type.is_convertible_to(&element_type) {
            self.errors.push(SemanticError::TypeMismatch {
                expected: element_type.to_string(),
                found: typed_value.basic_type.to_string(),
                span: value.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::ArrayAssignment {
                name: name.to_string(),
                indices: typed_indices,
                value: typed_value,
                dimensions,
                element_type,
            },
            span,
        )
    }

    // ========================================================================
    // I/O Statement Checking
    // ========================================================================

    /// Type checks a PRINT statement.
    pub(super) fn check_print(
        &mut self,
        values: &[PrintItem],
        newline: bool,
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_items: Vec<TypedPrintItem> = values
            .iter()
            .map(|item| TypedPrintItem {
                expr: self.check_expr(&item.expr),
                separator: item.separator,
            })
            .collect();

        TypedStatement::new(
            TypedStatementKind::Print {
                items: typed_items,
                newline,
            },
            span,
        )
    }

    /// Type checks an INPUT statement.
    pub(super) fn check_input(
        &mut self,
        prompt: &Option<String>,
        show_question_mark: bool,
        variables: &[String],
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_vars: Vec<(String, BasicType)> = variables
            .iter()
            .map(|name| {
                let basic_type = if let Some(sym) = self.symbols.lookup_symbol(name) {
                    sym.basic_type.clone()
                } else {
                    // Implicitly declare
                    let t = type_from_suffix(name)
                        .unwrap_or_else(|| self.symbols.default_type_for(name));
                    let symbol = Symbol {
                        name: name.clone(),
                        kind: SymbolKind::Variable,
                        basic_type: t.clone(),
                        span,
                        is_mutable: true,
                    };
                    let _ = self.symbols.define_symbol(symbol);
                    t
                };
                (name.clone(), basic_type)
            })
            .collect();

        TypedStatement::new(
            TypedStatementKind::Input {
                prompt: prompt.clone(),
                show_question_mark,
                variables: typed_vars,
            },
            span,
        )
    }

    /// Type checks a LINE INPUT statement.
    pub(super) fn check_line_input(
        &mut self,
        prompt: &Option<String>,
        variable: &str,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // LINE INPUT always reads into a string
        if self.symbols.lookup_symbol(variable).is_none() {
            let symbol = Symbol {
                name: variable.to_string(),
                kind: SymbolKind::Variable,
                basic_type: BasicType::String,
                span,
                is_mutable: true,
            };
            let _ = self.symbols.define_symbol(symbol);
        }

        TypedStatement::new(
            TypedStatementKind::LineInput {
                prompt: prompt.clone(),
                variable: variable.to_string(),
            },
            span,
        )
    }
}
