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
        if let Some(symbol) = self.symbols.lookup_symbol(name)
            && let SymbolKind::ArrayVariable { dimensions } = &symbol.kind
        {
            return self.check_array_access(
                name,
                args,
                dimensions.clone(),
                symbol.basic_type.clone(),
                span,
            );
        }

        // Look up procedure
        let proc = match self.symbols.lookup_procedure(name) {
            Some(p) => p.clone(),
            None => {
                self.errors.push(SemanticError::UndefinedProcedure {
                    name: name.to_string(),
                    span,
                });
                // Return Unknown type to continue checking
                return TypedExpr::new(
                    TypedExprKind::FunctionCall {
                        name: name.to_string(),
                        args: args.iter().map(|a| self.check_expr(a)).collect(),
                    },
                    BasicType::Unknown,
                    span,
                );
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
        let mut typed_args = Vec::new();
        for (i, arg) in args.iter().enumerate() {
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

    /// Type checks an array access.
    pub(super) fn check_array_access(
        &mut self,
        name: &str,
        indices: &[Expr],
        dim_info: Vec<ArrayDimInfo>,
        element_type: BasicType,
        span: crate::ast::Span,
    ) -> TypedExpr {
        if indices.len() != dim_info.len() {
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
        let typed_dimensions: Vec<TypedArrayDimension> = dim_info
            .iter()
            .map(|d| TypedArrayDimension {
                lower: d.lower_bound,
                upper: d.upper_bound,
            })
            .collect();

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
}
