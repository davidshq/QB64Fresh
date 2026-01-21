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
    symbols::{ArrayDimInfo, Symbol, SymbolKind},
    typed_ir::*,
    types::{BasicType, type_from_suffix},
};

use super::TypeChecker;

impl<'a> TypeChecker<'a> {
    // ========================================================================
    // Assignment Checking
    // ========================================================================

    /// Type checks an assignment statement.
    ///
    /// Handles both simple variable assignments and UDT field assignments.
    /// The lexer tokenizes `s.PERSON` as a single identifier (to support classic
    /// BASIC naming conventions like `player.move`), so we detect UDT field
    /// access by checking if the name contains a dot and the first part is
    /// a UDT variable.
    pub(super) fn check_assignment(
        &mut self,
        name: &str,
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Check if this is a UDT field assignment (e.g., "s.PERSON" or "s.nested.field")
        // The lexer includes dots in identifiers, so we need to split and check
        // if the first part is a UDT variable.
        if let Some(dot_pos) = name.find('.') {
            let object_name = &name[..dot_pos];
            let field_part = &name[dot_pos + 1..];

            // Check if the object is a UDT variable
            if let Some(symbol) = self.symbols.lookup_symbol(object_name)
                && let BasicType::UserDefined(_) = &symbol.basic_type
            {
                // This is a UDT field assignment
                return self.check_udt_field_assignment(object_name, field_part, value, span);
            }
        }

        // Regular variable assignment
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

    /// Type checks a UDT field assignment: `udt.field = value` or `udt.nested.field = value`
    fn check_udt_field_assignment(
        &mut self,
        object_name: &str,
        field_part: &str,
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        let typed_value = self.check_expr(value);

        // Split the field part into individual fields (for nested access like "nested.field")
        let fields: Vec<String> = field_part.split('.').map(|s| s.to_string()).collect();

        // Get the object's type
        let object_type = if let Some(symbol) = self.symbols.lookup_symbol(object_name) {
            symbol.basic_type.clone()
        } else {
            self.errors.push(SemanticError::UndefinedVariable {
                name: object_name.to_string(),
                span,
            });
            return TypedStatement::new(
                TypedStatementKind::FieldAssignment {
                    name: object_name.to_string(),
                    fields,
                    value: typed_value,
                    field_type: BasicType::Unknown,
                },
                span,
            );
        };

        // Resolve the field type by walking through the UDT definition chain
        let field_type = self.resolve_field_chain_type(&object_type, &fields);

        // Check type compatibility
        if field_type != BasicType::Unknown
            && !typed_value.basic_type.is_convertible_to(&field_type)
        {
            self.errors.push(SemanticError::TypeMismatch {
                expected: field_type.to_string(),
                found: typed_value.basic_type.to_string(),
                span: value.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::FieldAssignment {
                name: object_name.to_string(),
                fields,
                value: typed_value,
                field_type,
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
            // Verify dimension count - skip if dimensions are unknown (empty, for array params)
            if !dimensions.is_empty() && indices.len() != dimensions.len() {
                self.errors.push(SemanticError::ArrayDimensionMismatch {
                    name: name.to_string(),
                    expected: dimensions.len(),
                    found: indices.len(),
                    span,
                });
            }

            // For dynamic arrays (empty dimensions), create placeholder dimensions
            let typed_dims: Vec<TypedArrayDimension> = if dimensions.is_empty() {
                indices
                    .iter()
                    .map(|_| TypedArrayDimension { lower: 0, upper: 0 })
                    .collect()
            } else {
                dimensions
                    .iter()
                    .map(|d| TypedArrayDimension {
                        lower: d.lower_bound,
                        upper: d.upper_bound,
                    })
                    .collect()
            };

            (symbol.basic_type.clone(), typed_dims)
        } else if self.symbols.lookup_symbol(name).is_some() {
            // Scalar variable exists but is not an array
            self.errors.push(SemanticError::NotAnArray {
                name: name.to_string(),
                span,
            });
            (BasicType::Unknown, Vec::new())
        } else {
            // Classic BASIC: implicitly declare array on first use with default bounds (0-10)
            // Determine type from name suffix (e.g., A$ -> String, X% -> Integer)
            let element_type = type_from_suffix(name).unwrap_or(BasicType::Single);

            // Create dimensions with default bounds (0 TO 10) for each index
            let dim_info: Vec<ArrayDimInfo> = indices
                .iter()
                .map(|_| ArrayDimInfo {
                    lower_bound: 0,
                    upper_bound: 10,
                })
                .collect();

            // Define the implicit array
            let implicit_array = Symbol {
                name: name.to_string(),
                kind: SymbolKind::ArrayVariable {
                    dimensions: dim_info.clone(),
                },
                basic_type: element_type.clone(),
                span,
                is_mutable: true,
            };
            self.symbols.update_or_define_symbol(implicit_array);

            let typed_dims: Vec<TypedArrayDimension> = dim_info
                .iter()
                .map(|d| TypedArrayDimension {
                    lower: d.lower_bound,
                    upper: d.upper_bound,
                })
                .collect();

            (element_type, typed_dims)
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

    /// Type checks a field assignment statement: `variable.field = value`
    pub(super) fn check_field_assignment(
        &mut self,
        name: &str,
        fields: &[String],
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Look up the variable
        let var_type = if let Some(symbol) = self.symbols.lookup_symbol(name) {
            symbol.basic_type.clone()
        } else {
            // Implicit declaration with default type
            let default_type = self.symbols.default_type_for(name);
            self.symbols
                .define_symbol(Symbol {
                    name: name.to_string(),
                    kind: SymbolKind::Variable,
                    basic_type: default_type.clone(),
                    span,
                    is_mutable: true,
                })
                .ok();
            default_type
        };

        // Resolve field type by walking through the UDT definition
        let field_type = self.resolve_field_chain_type(&var_type, fields);

        // Check the value
        let typed_value = self.check_expr(value);

        // Type compatibility check
        if field_type != BasicType::Unknown
            && !typed_value.basic_type.is_convertible_to(&field_type)
        {
            self.errors.push(SemanticError::TypeMismatch {
                expected: field_type.to_string(),
                found: typed_value.basic_type.to_string(),
                span: value.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::FieldAssignment {
                name: name.to_string(),
                fields: fields.to_vec(),
                value: typed_value,
                field_type,
            },
            span,
        )
    }

    /// Type checks an array field assignment statement: `array(i).field = value`
    pub(super) fn check_array_field_assignment(
        &mut self,
        name: &str,
        indices: &[Expr],
        fields: &[String],
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Look up the array
        let (element_type, dimensions) = if let Some(symbol) = self.symbols.lookup_symbol(name)
            && let SymbolKind::ArrayVariable { dimensions } = &symbol.kind
        {
            // Verify dimension count - skip if dimensions are unknown (empty, for array params)
            if !dimensions.is_empty() && indices.len() != dimensions.len() {
                self.errors.push(SemanticError::ArrayDimensionMismatch {
                    name: name.to_string(),
                    expected: dimensions.len(),
                    found: indices.len(),
                    span,
                });
            }

            // For dynamic arrays (empty dimensions), create placeholder dimensions
            let typed_dims: Vec<TypedArrayDimension> = if dimensions.is_empty() {
                indices
                    .iter()
                    .map(|_| TypedArrayDimension { lower: 0, upper: 0 })
                    .collect()
            } else {
                dimensions
                    .iter()
                    .map(|d| TypedArrayDimension {
                        lower: d.lower_bound,
                        upper: d.upper_bound,
                    })
                    .collect()
            };

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

        // Resolve field type by walking through the UDT definition
        let field_type = self.resolve_field_chain_type(&element_type, fields);

        // Check the value
        let typed_value = self.check_expr(value);

        // Type compatibility check - now that we have proper field type resolution
        if field_type != BasicType::Unknown
            && !typed_value.basic_type.is_convertible_to(&field_type)
        {
            self.errors.push(SemanticError::TypeMismatch {
                expected: field_type.to_string(),
                found: typed_value.basic_type.to_string(),
                span: value.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::ArrayFieldAssignment {
                name: name.to_string(),
                indices: typed_indices,
                fields: fields.to_vec(),
                value: typed_value,
                dimensions,
                element_type,
                field_type,
            },
            span,
        )
    }

    // ========================================================================
    // MID$ Assignment
    // ========================================================================

    /// Type checks a MID$ assignment statement: `MID$(str$, start [, len]) = value$`
    ///
    /// This replaces a portion of the string in-place.
    /// The target can be a simple variable, array element, or UDT field.
    pub(super) fn check_mid_assignment(
        &mut self,
        target: &Expr,
        start: &Expr,
        length: Option<&Expr>,
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Type check the target expression - it must be a string lvalue
        let typed_target = self.check_expr(target);
        if typed_target.basic_type != BasicType::String
            && typed_target.basic_type != BasicType::Unknown
        {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "STRING".to_string(),
                found: typed_target.basic_type.to_string(),
                span: target.span,
            });
        }

        // Type check start position - must be numeric
        let typed_start = self.check_expr(start);
        if !typed_start.basic_type.is_numeric() && typed_start.basic_type != BasicType::Unknown {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_start.basic_type.to_string(),
                span: start.span,
            });
        }

        // Type check optional length - must be numeric
        let typed_length = length.map(|len| {
            let typed_len = self.check_expr(len);
            if !typed_len.basic_type.is_numeric() && typed_len.basic_type != BasicType::Unknown {
                self.errors.push(SemanticError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: typed_len.basic_type.to_string(),
                    span: len.span,
                });
            }
            typed_len
        });

        // Type check value - must be string
        let typed_value = self.check_expr(value);
        if typed_value.basic_type != BasicType::String
            && typed_value.basic_type != BasicType::Unknown
        {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "STRING".to_string(),
                found: typed_value.basic_type.to_string(),
                span: value.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::MidAssignment {
                target: typed_target,
                start: typed_start,
                length: typed_length,
                value: typed_value,
            },
            span,
        )
    }

    /// Type checks an ASC assignment statement: `ASC(str$, position) = value`
    ///
    /// This sets a character at a specific position in a string.
    /// The target can be a simple variable, array element, or UDT field.
    pub(super) fn check_asc_assignment(
        &mut self,
        target: &Expr,
        position: &Expr,
        value: &Expr,
        span: crate::ast::Span,
    ) -> TypedStatement {
        // Type check the target expression - it must be a string lvalue
        let typed_target = self.check_expr(target);
        if typed_target.basic_type != BasicType::String
            && typed_target.basic_type != BasicType::Unknown
        {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "STRING".to_string(),
                found: typed_target.basic_type.to_string(),
                span: target.span,
            });
        }

        // Type check position - must be numeric
        let typed_position = self.check_expr(position);
        if !typed_position.basic_type.is_numeric()
            && typed_position.basic_type != BasicType::Unknown
        {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_position.basic_type.to_string(),
                span: position.span,
            });
        }

        // Type check value - must be numeric (ASCII value 0-255)
        let typed_value = self.check_expr(value);
        if !typed_value.basic_type.is_numeric() && typed_value.basic_type != BasicType::Unknown {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_value.basic_type.to_string(),
                span: value.span,
            });
        }

        TypedStatement::new(
            TypedStatementKind::AscAssignment {
                target: typed_target,
                position: typed_position,
                value: typed_value,
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
        same_line: bool,
        targets: &[crate::ast::InputTarget],
        span: crate::ast::Span,
    ) -> TypedStatement {
        use crate::ast::InputTarget;
        use crate::semantic::typed_ir::TypedInputTarget;

        // Type-check each input target (same logic as FileInput)
        let typed_targets: Vec<TypedInputTarget> = targets
            .iter()
            .map(|target| match target {
                InputTarget::Variable(name) => {
                    let var_type = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                        symbol.basic_type.clone()
                    } else {
                        // Infer type from suffix or default
                        let inferred = type_from_suffix(name)
                            .unwrap_or_else(|| self.symbols.default_type_for(name));
                        let symbol = Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Variable,
                            basic_type: inferred.clone(),
                            span,
                            is_mutable: true,
                        };
                        let _ = self.symbols.define_symbol(symbol);
                        inferred
                    };
                    TypedInputTarget::Variable {
                        name: name.clone(),
                        basic_type: var_type,
                    }
                }
                InputTarget::ArrayElement { name, indices } => {
                    let typed_indices: Vec<_> =
                        indices.iter().map(|i| self.check_expr(i)).collect();
                    let element_type = if let Some(symbol) = self.symbols.lookup_symbol(name) {
                        if let BasicType::Array { element_type, .. } = &symbol.basic_type {
                            (**element_type).clone()
                        } else {
                            symbol.basic_type.clone()
                        }
                    } else {
                        type_from_suffix(name)
                            .unwrap_or_else(|| self.symbols.default_type_for(name))
                    };
                    TypedInputTarget::ArrayElement {
                        name: name.clone(),
                        indices: typed_indices,
                        element_type,
                    }
                }
                InputTarget::ArrayElementField {
                    name,
                    indices,
                    fields,
                } => {
                    let typed_indices: Vec<_> =
                        indices.iter().map(|i| self.check_expr(i)).collect();
                    let field_type = fields
                        .last()
                        .and_then(|f| type_from_suffix(f))
                        .unwrap_or(BasicType::Single);
                    TypedInputTarget::ArrayElementField {
                        name: name.clone(),
                        indices: typed_indices,
                        fields: fields.clone(),
                        field_type,
                    }
                }
                InputTarget::Field { name, fields } => {
                    let field_type = fields
                        .last()
                        .and_then(|f| type_from_suffix(f))
                        .unwrap_or(BasicType::Single);
                    TypedInputTarget::Field {
                        name: name.clone(),
                        fields: fields.clone(),
                        field_type,
                    }
                }
            })
            .collect();

        TypedStatement::new(
            TypedStatementKind::Input {
                prompt: prompt.clone(),
                show_question_mark,
                same_line,
                targets: typed_targets,
            },
            span,
        )
    }

    /// Resolves the type of a field chain (e.g., `.field1.field2`) starting from a base type.
    ///
    /// For example, given `UserDefined("Person")` and fields `["address", "city"]`:
    /// 1. Look up `address` in the `Person` type definition
    /// 2. If `address` is `UserDefined("Address")`, look up `city` in `Address`
    /// 3. Return the final field's type
    ///
    /// Returns `Unknown` if the type chain cannot be resolved (e.g., base type is not a UDT,
    /// or a field doesn't exist).
    pub(super) fn resolve_field_chain_type(
        &self,
        base_type: &BasicType,
        fields: &[String],
    ) -> BasicType {
        let mut current_type = base_type.clone();

        for field in fields {
            match &current_type {
                BasicType::UserDefined(type_name) => {
                    if let Some(field_type) = self.symbols.lookup_type_member(type_name, field) {
                        current_type = field_type;
                    } else {
                        // Field not found in this UDT - return Unknown
                        // (error reporting for undefined fields can be added later)
                        return BasicType::Unknown;
                    }
                }
                _ => {
                    // Trying to access a field on a non-UDT type
                    return BasicType::Unknown;
                }
            }
        }

        current_type
    }

    /// Type checks a LINE INPUT statement.
    pub(super) fn check_line_input(
        &mut self,
        prompt: &Option<String>,
        target: &crate::ast::InputTarget,
        span: crate::ast::Span,
    ) -> TypedStatement {
        use crate::ast::InputTarget;
        use crate::semantic::typed_ir::TypedInputTarget;

        // LINE INPUT always reads into a string
        let typed_target = match target {
            InputTarget::Variable(name) => {
                if self.symbols.lookup_symbol(name).is_none() {
                    let symbol = Symbol {
                        name: name.clone(),
                        kind: SymbolKind::Variable,
                        basic_type: BasicType::String,
                        span,
                        is_mutable: true,
                    };
                    let _ = self.symbols.define_symbol(symbol);
                }
                TypedInputTarget::Variable {
                    name: name.clone(),
                    basic_type: BasicType::String,
                }
            }
            InputTarget::ArrayElement { name, indices } => {
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
                TypedInputTarget::ArrayElement {
                    name: name.clone(),
                    indices: typed_indices,
                    element_type: BasicType::String,
                }
            }
            InputTarget::ArrayElementField {
                name,
                indices,
                fields,
            } => {
                let typed_indices: Vec<_> = indices.iter().map(|i| self.check_expr(i)).collect();
                TypedInputTarget::ArrayElementField {
                    name: name.clone(),
                    indices: typed_indices,
                    fields: fields.clone(),
                    field_type: BasicType::String,
                }
            }
            InputTarget::Field { name, fields } => TypedInputTarget::Field {
                name: name.clone(),
                fields: fields.clone(),
                field_type: BasicType::String,
            },
        };

        TypedStatement::new(
            TypedStatementKind::LineInput {
                prompt: prompt.clone(),
                target: typed_target,
            },
            span,
        )
    }
}
