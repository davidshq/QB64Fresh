//! Compile-time constant expression evaluation.
//!
//! This module handles evaluation of constant expressions for:
//! - CONST statement values
//! - DIM array bounds
//! - Any other context requiring compile-time constants
//!
//! The evaluator supports:
//! - Literal values (integer, float, string)
//! - References to other constants
//! - Binary operations on constants
//! - Unary operations on constants
//! - Type conversions

use crate::ast::{BinaryOp, UnaryOp};
use crate::semantic::{
    symbols::{ConstValue, SymbolKind},
    typed_ir::{TypedExpr, TypedExprKind},
    types::BasicType,
};

use super::TypeChecker;

impl<'a> TypeChecker<'a> {
    /// Attempts to evaluate a typed expression as a compile-time constant.
    ///
    /// Returns `Some(ConstValue)` if the expression can be evaluated at compile time,
    /// `None` if it contains non-constant elements (like variable references).
    pub(super) fn try_evaluate_const_expr(&self, expr: &TypedExpr) -> Option<ConstValue> {
        match &expr.kind {
            // Direct literals
            TypedExprKind::IntegerLiteral(v) => Some(ConstValue::Integer(*v)),
            TypedExprKind::FloatLiteral(v) => Some(ConstValue::Float(*v)),
            TypedExprKind::StringLiteral(v) => Some(ConstValue::String(v.clone())),

            // Grouped expressions - just unwrap
            TypedExprKind::Grouped(inner) => self.try_evaluate_const_expr(inner),

            // Binary operations on constants
            TypedExprKind::Binary { left, op, right } => {
                let left_val = self.try_evaluate_const_expr(left)?;
                let right_val = self.try_evaluate_const_expr(right)?;
                self.evaluate_binary_const(*op, left_val, right_val)
            }

            // Unary operations on constants
            TypedExprKind::Unary { op, operand } => {
                let operand_val = self.try_evaluate_const_expr(operand)?;
                self.evaluate_unary_const(*op, operand_val)
            }

            // Variable references - check if it's a constant
            TypedExprKind::Variable(name) => {
                if let Some(symbol) = self.symbols.lookup_symbol(name)
                    && let SymbolKind::Constant { value } = &symbol.kind
                {
                    return Some(value.clone());
                }
                None // Not a constant
            }

            // Type conversions - evaluate inner and convert
            TypedExprKind::Convert {
                expr: inner,
                to_type,
            } => {
                let inner_val = self.try_evaluate_const_expr(inner)?;
                self.convert_const_value(inner_val, to_type)
            }

            // Function calls, array access, and field access are not constant
            TypedExprKind::FunctionCall { .. }
            | TypedExprKind::ArrayAccess { .. }
            | TypedExprKind::FieldAccess { .. } => None,
        }
    }

    /// Evaluates a binary operation on constant values.
    fn evaluate_binary_const(
        &self,
        op: BinaryOp,
        left: ConstValue,
        right: ConstValue,
    ) -> Option<ConstValue> {
        match (left, right) {
            // Integer operations
            (ConstValue::Integer(l), ConstValue::Integer(r)) => {
                let result = match op {
                    BinaryOp::Add => l.checked_add(r)?,
                    BinaryOp::Subtract => l.checked_sub(r)?,
                    BinaryOp::Multiply => l.checked_mul(r)?,
                    BinaryOp::Divide => l.checked_div(r)?,
                    BinaryOp::IntDivide => l.checked_div(r)?,
                    BinaryOp::Modulo => l.checked_rem(r)?,
                    BinaryOp::Power => l.checked_pow(r.try_into().ok()?)?,
                    BinaryOp::And => l & r,
                    BinaryOp::Or => l | r,
                    BinaryOp::Xor => l ^ r,
                    BinaryOp::Eqv => !(l ^ r),
                    BinaryOp::Imp => !l | r,
                    BinaryOp::Equal => {
                        if l == r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::NotEqual => {
                        if l != r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::LessThan => {
                        if l < r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::LessEqual => {
                        if l <= r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::GreaterThan => {
                        if l > r {
                            -1
                        } else {
                            0
                        }
                    }
                    BinaryOp::GreaterEqual => {
                        if l >= r {
                            -1
                        } else {
                            0
                        }
                    }
                };
                Some(ConstValue::Integer(result))
            }

            // Float operations
            (ConstValue::Float(l), ConstValue::Float(r)) => {
                let result = match op {
                    BinaryOp::Add => l + r,
                    BinaryOp::Subtract => l - r,
                    BinaryOp::Multiply => l * r,
                    BinaryOp::Divide => l / r,
                    BinaryOp::Power => l.powf(r),
                    BinaryOp::Equal => {
                        if l == r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::NotEqual => {
                        if l != r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::LessThan => {
                        if l < r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::LessEqual => {
                        if l <= r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::GreaterThan => {
                        if l > r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    BinaryOp::GreaterEqual => {
                        if l >= r {
                            -1.0
                        } else {
                            0.0
                        }
                    }
                    _ => return None, // Bitwise ops not valid on floats
                };
                Some(ConstValue::Float(result))
            }

            // Mixed int/float - promote to float
            (ConstValue::Integer(l), ConstValue::Float(r)) => {
                self.evaluate_binary_const(op, ConstValue::Float(l as f64), ConstValue::Float(r))
            }
            (ConstValue::Float(l), ConstValue::Integer(r)) => {
                self.evaluate_binary_const(op, ConstValue::Float(l), ConstValue::Float(r as f64))
            }

            // String concatenation
            (ConstValue::String(l), ConstValue::String(r)) => match op {
                BinaryOp::Add => Some(ConstValue::String(l + &r)),
                BinaryOp::Equal => Some(ConstValue::Integer(if l == r { -1 } else { 0 })),
                BinaryOp::NotEqual => Some(ConstValue::Integer(if l != r { -1 } else { 0 })),
                BinaryOp::LessThan => Some(ConstValue::Integer(if l < r { -1 } else { 0 })),
                BinaryOp::LessEqual => Some(ConstValue::Integer(if l <= r { -1 } else { 0 })),
                BinaryOp::GreaterThan => Some(ConstValue::Integer(if l > r { -1 } else { 0 })),
                BinaryOp::GreaterEqual => Some(ConstValue::Integer(if l >= r { -1 } else { 0 })),
                _ => None,
            },

            // String + non-string not allowed
            _ => None,
        }
    }

    /// Evaluates a unary operation on a constant value.
    fn evaluate_unary_const(&self, op: UnaryOp, operand: ConstValue) -> Option<ConstValue> {
        match (op, operand) {
            (UnaryOp::Negate, ConstValue::Integer(v)) => Some(ConstValue::Integer(-v)),
            (UnaryOp::Negate, ConstValue::Float(v)) => Some(ConstValue::Float(-v)),
            (UnaryOp::Not, ConstValue::Integer(v)) => Some(ConstValue::Integer(!v)),
            _ => None,
        }
    }

    /// Converts a constant value to a different type.
    fn convert_const_value(&self, value: ConstValue, to_type: &BasicType) -> Option<ConstValue> {
        match (value, to_type) {
            // Integer to float
            (ConstValue::Integer(v), BasicType::Single | BasicType::Double) => {
                Some(ConstValue::Float(v as f64))
            }
            // Float to integer (truncate)
            (ConstValue::Float(v), BasicType::Integer | BasicType::Long) => {
                Some(ConstValue::Integer(v as i64))
            }
            // Same type - no conversion needed
            (v @ ConstValue::Integer(_), BasicType::Integer | BasicType::Long) => Some(v),
            (v @ ConstValue::Float(_), BasicType::Single | BasicType::Double) => Some(v),
            (v @ ConstValue::String(_), BasicType::String) => Some(v),
            // Unsigned types
            (ConstValue::Integer(v), BasicType::UnsignedInteger | BasicType::UnsignedLong) => {
                Some(ConstValue::Integer(v))
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;

    // Helper to create a typed integer literal expression
    fn int_lit(v: i64) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::IntegerLiteral(v),
            basic_type: BasicType::Long,
            span: Span::new(0, 1),
        }
    }

    // Helper to create a typed float literal expression
    fn float_lit(v: f64) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::FloatLiteral(v),
            basic_type: BasicType::Double,
            span: Span::new(0, 1),
        }
    }

    // Helper to create a typed string literal expression
    fn string_lit(v: &str) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::StringLiteral(v.to_string()),
            basic_type: BasicType::String,
            span: Span::new(0, 1),
        }
    }

    // Helper to create a binary expression
    fn binary(left: TypedExpr, op: BinaryOp, right: TypedExpr) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            },
            basic_type: BasicType::Long,
            span: Span::new(0, 1),
        }
    }

    // Helper to create a unary expression
    fn unary(op: UnaryOp, operand: TypedExpr) -> TypedExpr {
        let basic_type = operand.basic_type.clone();
        TypedExpr {
            kind: TypedExprKind::Unary {
                op,
                operand: Box::new(operand),
            },
            basic_type,
            span: Span::new(0, 1),
        }
    }

    // Helper to create a type checker
    fn checker() -> TypeChecker<'static> {
        // Create a leaked mutable symbol table for testing
        let symbols = crate::semantic::symbols::SymbolTable::new();
        let symbols_box = Box::new(symbols);
        let symbols_ref: &'static mut _ = Box::leak(symbols_box);
        TypeChecker::new(symbols_ref)
    }

    #[test]
    fn test_evaluate_integer_literal() {
        let tc = checker();
        let expr = int_lit(42);
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(42)));
    }

    #[test]
    fn test_evaluate_float_literal() {
        let tc = checker();
        let expr = float_lit(3.14);
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Float(3.14)));
    }

    #[test]
    fn test_evaluate_string_literal() {
        let tc = checker();
        let expr = string_lit("hello");
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::String("hello".to_string())));
    }

    #[test]
    fn test_evaluate_integer_addition() {
        let tc = checker();
        let expr = binary(int_lit(10), BinaryOp::Add, int_lit(5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(15)));
    }

    #[test]
    fn test_evaluate_integer_subtraction() {
        let tc = checker();
        let expr = binary(int_lit(10), BinaryOp::Subtract, int_lit(3));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(7)));
    }

    #[test]
    fn test_evaluate_integer_multiplication() {
        let tc = checker();
        let expr = binary(int_lit(6), BinaryOp::Multiply, int_lit(7));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(42)));
    }

    #[test]
    fn test_evaluate_integer_division() {
        let tc = checker();
        let expr = binary(int_lit(20), BinaryOp::Divide, int_lit(4));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(5)));
    }

    #[test]
    fn test_evaluate_integer_modulo() {
        let tc = checker();
        let expr = binary(int_lit(17), BinaryOp::Modulo, int_lit(5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(2)));
    }

    #[test]
    fn test_evaluate_integer_power() {
        let tc = checker();
        let expr = binary(int_lit(2), BinaryOp::Power, int_lit(10));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(1024)));
    }

    #[test]
    fn test_evaluate_bitwise_and() {
        let tc = checker();
        let expr = binary(int_lit(0b1100), BinaryOp::And, int_lit(0b1010));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(0b1000)));
    }

    #[test]
    fn test_evaluate_bitwise_or() {
        let tc = checker();
        let expr = binary(int_lit(0b1100), BinaryOp::Or, int_lit(0b1010));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(0b1110)));
    }

    #[test]
    fn test_evaluate_bitwise_xor() {
        let tc = checker();
        let expr = binary(int_lit(0b1100), BinaryOp::Xor, int_lit(0b1010));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(0b0110)));
    }

    #[test]
    fn test_evaluate_comparison_equal() {
        let tc = checker();
        let expr = binary(int_lit(5), BinaryOp::Equal, int_lit(5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1))); // TRUE = -1 in BASIC
    }

    #[test]
    fn test_evaluate_comparison_not_equal() {
        let tc = checker();
        let expr = binary(int_lit(5), BinaryOp::NotEqual, int_lit(3));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1)));
    }

    #[test]
    fn test_evaluate_comparison_less_than() {
        let tc = checker();
        let expr = binary(int_lit(3), BinaryOp::LessThan, int_lit(5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1)));
    }

    #[test]
    fn test_evaluate_unary_negate() {
        let tc = checker();
        let expr = unary(UnaryOp::Negate, int_lit(42));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-42)));
    }

    #[test]
    fn test_evaluate_unary_not() {
        let tc = checker();
        let expr = unary(UnaryOp::Not, int_lit(0));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1))); // NOT 0 = -1
    }

    #[test]
    fn test_evaluate_string_concatenation() {
        let tc = checker();
        let expr = binary(string_lit("Hello"), BinaryOp::Add, string_lit(" World"));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::String("Hello World".to_string())));
    }

    #[test]
    fn test_evaluate_string_comparison() {
        let tc = checker();
        let expr = binary(string_lit("abc"), BinaryOp::LessThan, string_lit("abd"));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1)));
    }

    #[test]
    fn test_evaluate_float_addition() {
        let tc = checker();
        let expr = binary(float_lit(1.5), BinaryOp::Add, float_lit(2.5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Float(4.0)));
    }

    #[test]
    fn test_evaluate_mixed_int_float() {
        let tc = checker();
        let expr = binary(int_lit(10), BinaryOp::Add, float_lit(0.5));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Float(10.5)));
    }

    #[test]
    fn test_evaluate_grouped_expression() {
        let tc = checker();
        let inner = int_lit(42);
        let expr = TypedExpr {
            kind: TypedExprKind::Grouped(Box::new(inner)),
            basic_type: BasicType::Long,
            span: Span::new(0, 1),
        };
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(42)));
    }

    #[test]
    fn test_evaluate_variable_is_none() {
        let tc = checker();
        let expr = TypedExpr {
            kind: TypedExprKind::Variable("unknown_var".to_string()),
            basic_type: BasicType::Long,
            span: Span::new(0, 1),
        };
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, None); // Variables are not constants
    }

    #[test]
    fn test_evaluate_function_call_is_none() {
        let tc = checker();
        let expr = TypedExpr {
            kind: TypedExprKind::FunctionCall {
                name: "SIN".to_string(),
                args: vec![float_lit(0.5)],
            },
            basic_type: BasicType::Double,
            span: Span::new(0, 1),
        };
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, None); // Function calls are not constant
    }

    #[test]
    fn test_evaluate_division_by_zero() {
        let tc = checker();
        let expr = binary(int_lit(10), BinaryOp::Divide, int_lit(0));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, None); // Division by zero should return None
    }

    #[test]
    fn test_evaluate_eqv_operator() {
        let tc = checker();
        // EQV: A EQV B = NOT(A XOR B)
        let expr = binary(int_lit(-1), BinaryOp::Eqv, int_lit(-1));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1))); // TRUE EQV TRUE = TRUE
    }

    #[test]
    fn test_evaluate_imp_operator() {
        let tc = checker();
        // IMP: A IMP B = (NOT A) OR B
        let expr = binary(int_lit(0), BinaryOp::Imp, int_lit(0));
        let result = tc.try_evaluate_const_expr(&expr);
        assert_eq!(result, Some(ConstValue::Integer(-1))); // FALSE IMP FALSE = TRUE
    }
}
