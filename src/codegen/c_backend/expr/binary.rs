//! Binary expression code generation.
//!
//! Handles arithmetic, comparison, string concatenation, and bitwise operators.

use crate::ast::BinaryOp;
use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::TypedExpr;
use crate::semantic::types::BasicType;

use super::ExprEmitCtx;

/// Emits a binary expression by delegating left/right to the context and combining with the operator.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_binary_expr(
    ctx: &ExprEmitCtx,
    left: &TypedExpr,
    op: &BinaryOp,
    right: &TypedExpr,
    wrap_string_temps: bool,
) -> Result<String, CodeGenError> {
    let left_code = ctx.emit(left, wrap_string_temps)?;
    let right_code = ctx.emit(right, wrap_string_temps)?;

    // Handle string concatenation specially
    if left.basic_type.is_string() && matches!(op, BinaryOp::Add) {
        let left_wrapped = if matches!(left.basic_type, BasicType::FixedString(_)) {
            if left_code.starts_with("qb_str_from_c(") {
                left_code
            } else {
                format!("qb_str_from_c({})", left_code)
            }
        } else {
            left_code
        };
        let right_wrapped = if matches!(right.basic_type, BasicType::FixedString(_)) {
            if right_code.starts_with("qb_str_from_c(") {
                right_code
            } else {
                format!("qb_str_from_c({})", right_code)
            }
        } else {
            right_code
        };
        let concat_code = format!("qb_string_concat({}, {})", left_wrapped, right_wrapped);
        if wrap_string_temps {
            return Ok(format!("qbs_tmp_register({})", concat_code));
        } else {
            return Ok(concat_code);
        }
    }

    // Handle string comparisons
    if left.basic_type.is_string() {
        let left_wrapped = if matches!(left.basic_type, BasicType::FixedString(_)) {
            if left_code.starts_with("qb_str_from_c(") {
                left_code
            } else {
                format!("qb_str_from_c({})", left_code)
            }
        } else {
            left_code
        };
        let right_wrapped = if matches!(right.basic_type, BasicType::FixedString(_)) {
            if right_code.starts_with("qb_str_from_c(") {
                right_code
            } else {
                format!("qb_str_from_c({})", right_code)
            }
        } else {
            right_code
        };
        return emit_string_comparison(&left_wrapped, &right_wrapped, op);
    }

    // Handle operators that can't be expressed as simple C binary operators
    match op {
        BinaryOp::Power => return Ok(format!("pow({}, {})", left_code, right_code)),
        BinaryOp::Modulo => {
            let needs_cast = left.basic_type.is_float()
                || right.basic_type.is_float()
                || matches!(left.basic_type, BasicType::Unknown)
                || matches!(right.basic_type, BasicType::Unknown);
            let (l, r) = if needs_cast {
                (
                    format!("(int64_t)({})", left_code),
                    format!("(int64_t)({})", right_code),
                )
            } else {
                (left_code, right_code)
            };
            return Ok(format!("({} % {})", l, r));
        }
        BinaryOp::Eqv => return Ok(format!("(~({} ^ {}))", left_code, right_code)),
        BinaryOp::Imp => return Ok(format!("((~{}) | {})", left_code, right_code)),
        _ => {}
    }

    let op_str = c_binary_op(op)?;

    if matches!(
        op,
        BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::LessThan
            | BinaryOp::LessEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterEqual
            | BinaryOp::AndAlso
            | BinaryOp::OrElse
    ) {
        return Ok(format!("-({} {} {})", left_code, op_str, right_code));
    }

    Ok(format!("({} {} {})", left_code, op_str, right_code))
}

/// Maps a binary operator to C syntax.
fn c_binary_op(op: &BinaryOp) -> Result<String, CodeGenError> {
    Ok(match op {
        BinaryOp::Add => "+".to_string(),
        BinaryOp::Subtract => "-".to_string(),
        BinaryOp::Multiply => "*".to_string(),
        BinaryOp::Divide => "/".to_string(),
        BinaryOp::IntDivide => "/".to_string(),
        BinaryOp::Modulo => "%".to_string(),
        BinaryOp::Power => unreachable!("Power operator should be handled in emit_binary_expr"),
        BinaryOp::Equal => "==".to_string(),
        BinaryOp::NotEqual => "!=".to_string(),
        BinaryOp::LessThan => "<".to_string(),
        BinaryOp::LessEqual => "<=".to_string(),
        BinaryOp::GreaterThan => ">".to_string(),
        BinaryOp::GreaterEqual => ">=".to_string(),
        BinaryOp::And => "&".to_string(),
        BinaryOp::Or => "|".to_string(),
        BinaryOp::Xor => "^".to_string(),
        BinaryOp::AndAlso => "&&".to_string(),
        BinaryOp::OrElse => "||".to_string(),
        BinaryOp::Eqv => unreachable!("EQV operator should be handled in emit_binary_expr"),
        BinaryOp::Imp => unreachable!("IMP operator should be handled in emit_binary_expr"),
    })
}

/// Emits string comparison code using qb_string_compare.
fn emit_string_comparison(left: &str, right: &str, op: &BinaryOp) -> Result<String, CodeGenError> {
    let cmp = format!("qb_string_compare({}, {})", left, right);
    let result = match op {
        BinaryOp::Equal => format!("-({} == 0)", cmp),
        BinaryOp::NotEqual => format!("-({} != 0)", cmp),
        BinaryOp::LessThan => format!("-({} < 0)", cmp),
        BinaryOp::LessEqual => format!("-({} <= 0)", cmp),
        BinaryOp::GreaterThan => format!("-({} > 0)", cmp),
        BinaryOp::GreaterEqual => format!("-({} >= 0)", cmp),
        _ => {
            return Err(CodeGenError::unsupported(format!(
                "operator {} on strings",
                op.as_str()
            )));
        }
    };
    Ok(result)
}
