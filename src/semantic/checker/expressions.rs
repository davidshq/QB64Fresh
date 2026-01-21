//! Expression type checking.
//!
//! This module handles type checking for all expression types:
//! - Literals (integer, float, string)
//! - Identifiers (variable references)
//! - Binary and unary operations
//! - Function calls
//! - Array access
//! - Field access (for user-defined types)

use crate::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use crate::semantic::{
    error::SemanticError,
    symbols::{ArrayDimInfo, ProcedureKind, Symbol, SymbolKind},
    typed_ir::*,
    types::{BasicType, type_from_suffix},
};

use super::TypeChecker;

impl<'a> TypeChecker<'a> {
    // ========================================================================
    // Expression Type Checking
    // ========================================================================

    /// Type checks an expression and returns the typed expression.
    ///
    /// On error, records the error and returns a placeholder expression.
    pub fn check_expr(&mut self, expr: &Expr) -> TypedExpr {
        match &expr.kind {
            ExprKind::IntegerLiteral(val) => TypedExpr::integer(*val, expr.span),

            ExprKind::FloatLiteral(val) => TypedExpr::float(*val, expr.span),

            ExprKind::StringLiteral(val) => TypedExpr::string(val.clone(), expr.span),

            ExprKind::Identifier(name) => self.check_identifier(name, expr.span),

            ExprKind::Binary { left, op, right } => self.check_binary(left, *op, right, expr.span),

            ExprKind::Unary { op, operand } => self.check_unary(*op, operand, expr.span),

            ExprKind::Grouped(inner) => {
                let typed_inner = self.check_expr(inner);
                TypedExpr::new(
                    TypedExprKind::Grouped(Box::new(typed_inner.clone())),
                    typed_inner.basic_type,
                    expr.span,
                )
            }

            ExprKind::FunctionCall { name, args } => {
                self.check_function_call(name, args, expr.span)
            }

            ExprKind::FieldAccess { object, field } => {
                self.check_field_access(object, field, expr.span)
            }

            ExprKind::ProcPtr { name } => {
                // _PROCPTR returns the address of a procedure as an _OFFSET
                // Look up the procedure to get its full name (including type suffix)
                if let Some(proc) = self.symbols.lookup_procedure(name) {
                    // Use the actual procedure name from the symbol table
                    // This includes any type suffix (e.g., "MyCompare&")
                    let actual_name = proc.name.clone();
                    // Generate wrapper function name for C callback
                    let wrapper_name = format!(
                        "qb_callback_{}",
                        actual_name
                            .to_lowercase()
                            .replace(['&', '%', '$', '!', '#'], "")
                    );
                    TypedExpr::new(
                        TypedExprKind::ProcPtr {
                            name: actual_name,
                            wrapper_name,
                        },
                        BasicType::Offset, // Function pointers are pointer-sized
                        expr.span,
                    )
                } else {
                    self.errors.push(SemanticError::UndefinedProcedure {
                        name: name.clone(),
                        span: expr.span,
                    });
                    // Return a placeholder expression on error
                    TypedExpr::new(
                        TypedExprKind::IntegerLiteral(0),
                        BasicType::Offset,
                        expr.span,
                    )
                }
            }
        }
    }

    /// Type checks an identifier (variable reference).
    fn check_identifier(&mut self, name: &str, span: crate::ast::Span) -> TypedExpr {
        // Check if it's an existing variable
        if let Some(symbol) = self.symbols.lookup_symbol(name) {
            return TypedExpr::new(
                TypedExprKind::Variable(name.to_string()),
                symbol.basic_type.clone(),
                span,
            );
        }

        // Check if it's a parameterless function call
        if let Some(proc) = self.symbols.lookup_procedure(name)
            && proc.params.is_empty()
            && proc.return_type.is_some()
        {
            return TypedExpr::new(
                TypedExprKind::FunctionCall {
                    name: name.to_string(),
                    args: vec![],
                },
                proc.return_type.clone().unwrap(),
                span,
            );
        }

        // Implicit variable declaration (BASIC allows undeclared variables)
        let basic_type =
            type_from_suffix(name).unwrap_or_else(|| self.symbols.default_type_for(name));

        let symbol = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Variable,
            basic_type: basic_type.clone(),
            span,
            is_mutable: true,
        };

        let _ = self.symbols.define_symbol(symbol);

        TypedExpr::new(TypedExprKind::Variable(name.to_string()), basic_type, span)
    }

    /// Type checks a field access expression (e.g., `person.name`).
    ///
    /// This validates that:
    /// - The object expression has a UserDefined type
    /// - The field exists in that type (when TYPE members are tracked)
    fn check_field_access(
        &mut self,
        object: &Expr,
        field: &str,
        span: crate::ast::Span,
    ) -> TypedExpr {
        let typed_object = self.check_expr(object);

        // Determine the field type based on the object's type
        let field_type = match &typed_object.basic_type {
            BasicType::UserDefined(type_name) => {
                // Look up the type definition to find the field's type
                if let Some(field_type) = self.lookup_type_field(type_name, field) {
                    field_type
                } else {
                    self.errors.push(SemanticError::UndefinedVariable {
                        name: format!("{}.{}", type_name, field),
                        span,
                    });
                    BasicType::Unknown
                }
            }
            // If object is Unknown, an error was already reported upstream - don't cascade
            BasicType::Unknown => BasicType::Unknown,
            _ => {
                self.errors.push(SemanticError::TypeMismatch {
                    expected: "user-defined type".to_string(),
                    found: typed_object.basic_type.to_string(),
                    span,
                });
                BasicType::Unknown
            }
        };

        TypedExpr::new(
            TypedExprKind::FieldAccess {
                object: Box::new(typed_object),
                field: field.to_string(),
            },
            field_type,
            span,
        )
    }

    /// Type checks a binary operation.
    pub(super) fn check_binary(
        &mut self,
        left: &Expr,
        op: BinaryOp,
        right: &Expr,
        span: crate::ast::Span,
    ) -> TypedExpr {
        let left_typed = self.check_expr(left);
        let right_typed = self.check_expr(right);

        let result_type = match op {
            // Comparison operators always return Integer (boolean in BASIC)
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::LessThan
            | BinaryOp::LessEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterEqual => {
                if !self.types_comparable(&left_typed.basic_type, &right_typed.basic_type) {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                }
                BasicType::Integer
            }

            // Logical operators (bitwise on integers)
            BinaryOp::And | BinaryOp::Or | BinaryOp::Xor | BinaryOp::Eqv | BinaryOp::Imp => {
                if !left_typed.basic_type.is_numeric() || !right_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                }
                left_typed
                    .basic_type
                    .common_type(&right_typed.basic_type)
                    .unwrap_or(BasicType::Long)
            }

            // String concatenation with +
            BinaryOp::Add
                if left_typed.basic_type.is_string() && right_typed.basic_type.is_string() =>
            {
                BasicType::String
            }

            // Arithmetic operators
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply => {
                if !left_typed.basic_type.is_numeric() || !right_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                    BasicType::Single
                } else {
                    left_typed
                        .basic_type
                        .common_type(&right_typed.basic_type)
                        .unwrap_or(BasicType::Single)
                }
            }

            // Division and Power return at least Single
            BinaryOp::Divide | BinaryOp::Power => {
                if !left_typed.basic_type.is_numeric() || !right_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                    BasicType::Single
                } else {
                    let common = left_typed
                        .basic_type
                        .common_type(&right_typed.basic_type)
                        .unwrap_or(BasicType::Single);
                    if common.is_integer() {
                        BasicType::Single
                    } else {
                        common
                    }
                }
            }

            // Integer division and modulo
            BinaryOp::IntDivide | BinaryOp::Modulo => {
                if !left_typed.basic_type.is_numeric() || !right_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidBinaryOp {
                        op: op.as_str().to_string(),
                        left_type: left_typed.basic_type.to_string(),
                        right_type: right_typed.basic_type.to_string(),
                        span,
                    });
                }
                BasicType::Long
            }
        };

        TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(left_typed),
                op,
                right: Box::new(right_typed),
            },
            result_type,
            span,
        )
    }

    /// Type checks a unary operation.
    fn check_unary(&mut self, op: UnaryOp, operand: &Expr, span: crate::ast::Span) -> TypedExpr {
        let operand_typed = self.check_expr(operand);

        match op {
            UnaryOp::Negate => {
                if !operand_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidUnaryOp {
                        op: "-".to_string(),
                        operand_type: operand_typed.basic_type.to_string(),
                        span,
                    });
                }
            }
            UnaryOp::Not => {
                if !operand_typed.basic_type.is_numeric() {
                    self.errors.push(SemanticError::InvalidUnaryOp {
                        op: "NOT".to_string(),
                        operand_type: operand_typed.basic_type.to_string(),
                        span,
                    });
                }
            }
        }

        TypedExpr::new(
            TypedExprKind::Unary {
                op,
                operand: Box::new(operand_typed.clone()),
            },
            operand_typed.basic_type,
            span,
        )
    }

    /// Type checks a function call or array access.
    pub(super) fn check_function_call(
        &mut self,
        name: &str,
        args: &[Expr],
        span: crate::ast::Span,
    ) -> TypedExpr {
        // First check if it's an array access
        if let Some(symbol) = self.symbols.lookup_symbol(name) {
            match &symbol.kind {
                SymbolKind::ArrayVariable { dimensions } => {
                    return self.check_array_access(
                        name,
                        args,
                        dimensions.clone(),
                        symbol.basic_type.clone(),
                        span,
                    );
                }
                SymbolKind::ExternalFunction {
                    c_name,
                    params,
                    return_type,
                } => {
                    // External function from DECLARE LIBRARY
                    return self.check_external_function_call(
                        name,
                        c_name.clone(),
                        args,
                        params.clone(),
                        return_type.clone(),
                        span,
                    );
                }
                _ => {}
            }
        }

        // Look up procedure
        let proc = match self.symbols.lookup_procedure(name) {
            Some(p) => p.clone(),
            None => {
                // Check if there's a scalar variable with this name (NotAnArray error)
                // Clone the type early to avoid borrow issues
                if let Some(basic_type) = self
                    .symbols
                    .lookup_symbol(name)
                    .map(|s| s.basic_type.clone())
                {
                    self.errors.push(SemanticError::NotAnArray {
                        name: name.to_string(),
                        span,
                    });
                    return TypedExpr::new(
                        TypedExprKind::FunctionCall {
                            name: name.to_string(),
                            args: args.iter().map(|a| self.check_expr(a)).collect(),
                        },
                        basic_type,
                        span,
                    );
                }

                // Classic BASIC: implicitly declare array on first use with default bounds (0-10)
                // Determine type from name suffix (e.g., A$ -> String, X% -> Integer)
                let element_type = type_from_suffix(name).unwrap_or(BasicType::Single);

                // Create dimensions with default bounds (0 TO 10) for each index
                let dimensions: Vec<ArrayDimInfo> = args
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
                        dimensions: dimensions.clone(),
                    },
                    basic_type: element_type.clone(),
                    span,
                    is_mutable: true,
                };
                // Use update_or_define to handle any race conditions
                self.symbols.update_or_define_symbol(implicit_array);

                // Now treat as array access
                return self.check_array_access(name, args, dimensions, element_type, span);
            }
        };

        // SUBs don't return values
        if proc.kind == ProcedureKind::Sub {
            self.errors.push(SemanticError::SubUsedAsFunction {
                name: name.to_string(),
                span,
            });
            return TypedExpr::new(
                TypedExprKind::FunctionCall {
                    name: name.to_string(),
                    args: args.iter().map(|a| self.check_expr(a)).collect(),
                },
                BasicType::Void,
                span,
            );
        }

        // Check argument count (considering optional parameters)
        let required_count = proc.required_param_count();
        let max_count = proc.params.len();
        if args.len() < required_count || args.len() > max_count {
            self.errors.push(SemanticError::ArgumentCountMismatch {
                name: name.to_string(),
                expected_min: required_count,
                expected_max: max_count,
                found: args.len(),
                span,
            });
        }

        // Check argument types, handling array arguments specially
        let mut typed_args = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            // Check if this argument is an array reference (arr() syntax)
            if i < proc.params.len()
                && proc.params[i].is_array
                && let Some(typed_arg) = self.try_check_array_ref(arg)
            {
                typed_args.push(typed_arg);
                continue;
            }

            let typed_arg = self.check_expr(arg);

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

            typed_args.push(typed_arg);
        }

        TypedExpr::new(
            TypedExprKind::FunctionCall {
                name: name.to_string(),
                args: typed_args,
            },
            proc.return_type.unwrap_or(BasicType::Void),
            span,
        )
    }

    /// Attempts to check an expression as an array reference (arr() syntax).
    ///
    /// In BASIC, `arr()` means "pass the entire array" to a procedure.
    /// This is recognized when the expression is a FunctionCall with empty args
    /// and the name matches a declared array.
    ///
    /// Returns `Some(TypedExpr)` with `ArrayRef` kind if this is an array reference,
    /// or `None` if the expression should be checked normally.
    pub(super) fn try_check_array_ref(&mut self, expr: &Expr) -> Option<TypedExpr> {
        // Array reference syntax: name with empty parentheses, e.g., arr()
        if let ExprKind::FunctionCall { name, args } = &expr.kind
            && args.is_empty()
        {
            // Check if this name refers to a declared array
            if let Some(symbol) = self.symbols.lookup_symbol(name)
                && let SymbolKind::ArrayVariable { dimensions } = &symbol.kind
            {
                let typed_dimensions: Vec<TypedArrayDimension> = dimensions
                    .iter()
                    .map(|d| TypedArrayDimension {
                        lower: d.lower_bound,
                        upper: d.upper_bound,
                    })
                    .collect();

                return Some(TypedExpr::new(
                    TypedExprKind::ArrayRef {
                        name: name.to_string(),
                        element_type: symbol.basic_type.clone(),
                        dimensions: typed_dimensions,
                    },
                    // The type of an array reference is a pointer/reference type,
                    // but for simplicity we use the element type since BASIC
                    // doesn't have explicit pointer types
                    symbol.basic_type.clone(),
                    expr.span,
                ));
            }
        }
        None
    }

    /// Type checks an array access.
    pub(super) fn check_array_access(
        &mut self,
        name: &str,
        indices: &[Expr],
        dim_info: Vec<ArrayDimInfo>,
        element_type: BasicType,
        span: crate::ast::Span,
    ) -> TypedExpr {
        // Only check dimension count if we know the dimensions (not empty).
        // Array parameters and REDIM arrays may have unknown dimensions at compile time.
        if !dim_info.is_empty() && indices.len() != dim_info.len() {
            self.errors.push(SemanticError::ArrayDimensionMismatch {
                name: name.to_string(),
                expected: dim_info.len(),
                found: indices.len(),
                span,
            });
        }

        let mut typed_indices = Vec::new();
        for index in indices {
            let typed_index = self.check_expr(index);
            if !typed_index.basic_type.is_numeric() {
                self.errors.push(SemanticError::NonNumericIndex {
                    found: typed_index.basic_type.to_string(),
                    span: index.span,
                });
            }
            typed_indices.push(typed_index);
        }

        // Convert ArrayDimInfo to TypedArrayDimension for code generation
        // For dynamic arrays (empty dim_info), use placeholder dimensions
        let typed_dimensions: Vec<TypedArrayDimension> = if dim_info.is_empty() {
            // Dynamic array - create placeholder dimensions based on indices
            indices
                .iter()
                .map(|_| TypedArrayDimension { lower: 0, upper: 0 })
                .collect()
        } else {
            dim_info
                .iter()
                .map(|d| TypedArrayDimension {
                    lower: d.lower_bound,
                    upper: d.upper_bound,
                })
                .collect()
        };

        TypedExpr::new(
            TypedExprKind::ArrayAccess {
                name: name.to_string(),
                indices: typed_indices,
                dimensions: typed_dimensions,
            },
            element_type,
            span,
        )
    }

    /// Type checks an external function call (from DECLARE LIBRARY).
    ///
    /// External functions need special handling because:
    /// - The C function name may differ from the BASIC name (ALIAS)
    /// - String arguments need marshalling (qb_string* -> char*)
    /// - BYVAL parameters pass by value
    fn check_external_function_call(
        &mut self,
        name: &str,
        c_name: String,
        args: &[Expr],
        params: Vec<BasicType>,
        return_type: BasicType,
        span: crate::ast::Span,
    ) -> TypedExpr {
        // Check argument count
        if args.len() != params.len() {
            self.errors.push(SemanticError::ArgumentCountMismatch {
                name: name.to_string(),
                expected_min: params.len(),
                expected_max: params.len(),
                found: args.len(),
                span,
            });
        }

        // Type-check arguments
        let mut typed_args = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let typed_arg = self.check_expr(arg);

            if i < params.len() {
                let expected_type = &params[i];
                if !typed_arg.basic_type.is_convertible_to(expected_type) {
                    self.errors.push(SemanticError::ArgumentTypeMismatch {
                        position: i + 1,
                        expected: expected_type.to_string(),
                        found: typed_arg.basic_type.to_string(),
                        span: arg.span,
                    });
                }
            }

            typed_args.push(typed_arg);
        }

        // Build parameter info for codegen marshalling
        // Note: We don't have is_byval info stored in SymbolKind::ExternalFunction
        // For now, assume all external params are BYVAL (required for C interop)
        let param_info: Vec<ExternalParamInfo> = params
            .iter()
            .map(|typ| ExternalParamInfo {
                typ: typ.clone(),
                is_byval: true, // External functions default to BYVAL for C compatibility
            })
            .collect();

        TypedExpr::new(
            TypedExprKind::ExternalFunctionCall {
                name: name.to_string(),
                c_name,
                args: typed_args,
                params: param_info,
            },
            return_type,
            span,
        )
    }
}
