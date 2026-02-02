//! Unary expression and grouped expression code generation.

use crate::ast::UnaryOp;

/// Formats a unary expression given the operand C code.
pub(super) fn format_unary(op: &UnaryOp, operand_code: &str) -> String {
    let op_str = match op {
        UnaryOp::Negate => "-",
        UnaryOp::Not => "~",
    };
    format!("({}{})", op_str, operand_code)
}

/// Formats a grouped (parenthesized) expression.
pub(super) fn format_grouped(inner_code: &str) -> String {
    format!("({})", inner_code)
}
