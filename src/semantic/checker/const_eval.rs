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
