//! Expression code generation for QB64Fresh C backend.
//!
//! This module handles the emission of C code for all expression types,
//! including literals, binary/unary operations, function calls, and array access.
//!
//! # Special Handling
//!
//! Several BASIC operations require special treatment:
//! - **String concatenation**: Uses `qb_string_concat()` instead of `+`
//! - **String comparison**: Uses `qb_string_compare()` with appropriate operators
//! - **Power operator (`^`)**: Uses `pow()` from math.h
//! - **EQV/IMP operators**: Translated to bitwise expressions

use crate::ast::BinaryOp;
use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{TypedArrayDimension, TypedExpr, TypedExprKind};

use super::types::{c_identifier, c_type};

/// Emits C code for an expression.
///
/// This function recursively processes the expression tree, generating
/// appropriate C code for each node type.
pub(super) fn emit_expr(expr: &TypedExpr) -> Result<String, CodeGenError> {
    match &expr.kind {
        TypedExprKind::IntegerLiteral(n) => Ok(format!("{}LL", n)),

        TypedExprKind::FloatLiteral(n) => {
            // Handle special floating point values
            if n.is_nan() {
                Ok("(0.0/0.0)".to_string())
            } else if n.is_infinite() {
                if n.is_sign_positive() {
                    Ok("(1.0/0.0)".to_string())
                } else {
                    Ok("(-1.0/0.0)".to_string())
                }
            } else {
                Ok(format!("{:.17}", n))
            }
        }

        TypedExprKind::StringLiteral(s) => {
            let escaped = escape_string(s);
            Ok(format!("qb_string_new(\"{}\")", escaped))
        }

        TypedExprKind::Variable(name) => Ok(c_identifier(name)),

        TypedExprKind::Binary { left, op, right } => emit_binary_expr(left, op, right),

        TypedExprKind::Unary { op, operand } => {
            let operand_code = emit_expr(operand)?;
            let op_str = match op {
                crate::ast::UnaryOp::Negate => "-",
                crate::ast::UnaryOp::Not => "~", // Bitwise NOT for numeric types
            };
            Ok(format!("({}{})", op_str, operand_code))
        }

        TypedExprKind::Grouped(inner) => {
            let inner_code = emit_expr(inner)?;
            Ok(format!("({})", inner_code))
        }

        TypedExprKind::FunctionCall { name, args } => {
            let args_code: Result<Vec<_>, _> = args.iter().map(emit_expr).collect();
            let args_str = args_code?.join(", ");
            let c_name = c_function_name(name);
            Ok(format!("{}({})", c_name, args_str))
        }

        TypedExprKind::ArrayAccess {
            name,
            indices,
            dimensions,
        } => emit_array_access(name, indices, dimensions),

        TypedExprKind::Convert { expr, to_type } => {
            let inner_code = emit_expr(expr)?;
            let c_ty = c_type(to_type);

            // Handle string-to-string conversion (no-op)
            if expr.basic_type.is_string() && to_type.is_string() {
                return Ok(inner_code);
            }

            Ok(format!("(({})({}))", c_ty, inner_code))
        }

        TypedExprKind::FieldAccess { object, field } => {
            let obj_code = emit_expr(object)?;
            let c_field = c_identifier(field);
            Ok(format!("{}.{}", obj_code, c_field))
        }
    }
}

/// Emits a binary expression.
fn emit_binary_expr(
    left: &TypedExpr,
    op: &BinaryOp,
    right: &TypedExpr,
) -> Result<String, CodeGenError> {
    let left_code = emit_expr(left)?;
    let right_code = emit_expr(right)?;

    // Handle string concatenation specially
    if left.basic_type.is_string() && matches!(op, BinaryOp::Add) {
        return Ok(format!("qb_string_concat({}, {})", left_code, right_code));
    }

    // Handle string comparisons
    if left.basic_type.is_string() {
        return emit_string_comparison(&left_code, &right_code, op);
    }

    // Handle operators that can't be expressed as simple C binary operators
    match op {
        BinaryOp::Power => {
            // Use pow() from math.h for exponentiation
            return Ok(format!("pow({}, {})", left_code, right_code));
        }
        BinaryOp::Eqv => {
            // EQV (equivalence) = bitwise XNOR = ~(a ^ b)
            return Ok(format!("(~({} ^ {}))", left_code, right_code));
        }
        BinaryOp::Imp => {
            // IMP (implication) = ~a | b
            return Ok(format!("((~{}) | {})", left_code, right_code));
        }
        _ => {}
    }

    let op_str = c_binary_op(op)?;
    Ok(format!("({} {} {})", left_code, op_str, right_code))
}

/// Emits C code for array access.
fn emit_array_access(
    name: &str,
    indices: &[TypedExpr],
    dimensions: &[TypedArrayDimension],
) -> Result<String, CodeGenError> {
    let c_name = c_identifier(name);
    let indices_code: Result<Vec<_>, _> = indices.iter().map(emit_expr).collect();
    let indices_code = indices_code?;

    if dimensions.is_empty() || indices_code.len() == 1 {
        // 1D array - simple index (subtracting lower bound)
        if let Some(dim) = dimensions.first() {
            Ok(format!("{}[{} - {}]", c_name, indices_code[0], dim.lower))
        } else {
            // No dimension info, use index as-is (shouldn't happen)
            Ok(format!("{}[{}]", c_name, indices_code[0]))
        }
    } else {
        // Multi-dimensional array - calculate linear index
        // For 2D: arr(i, j) -> arr[(i - lower1) * size2 + (j - lower2)]
        // General formula: sum of (adjusted_index * stride)
        let mut linear_parts = Vec::new();

        for (i, (idx, dim)) in indices_code.iter().zip(dimensions.iter()).enumerate() {
            let adjusted = format!("({} - {})", idx, dim.lower);

            if i < dimensions.len() - 1 {
                // Calculate stride: product of all remaining dimension sizes
                let stride: i64 = dimensions[i + 1..]
                    .iter()
                    .map(|d| d.upper - d.lower + 1)
                    .product();
                linear_parts.push(format!("{} * {}", adjusted, stride));
            } else {
                // Last dimension, no stride multiplication
                linear_parts.push(adjusted);
            }
        }

        Ok(format!("{}[{}]", c_name, linear_parts.join(" + ")))
    }
}

/// Maps a binary operator to C syntax.
fn c_binary_op(op: &BinaryOp) -> Result<String, CodeGenError> {
    Ok(match op {
        BinaryOp::Add => "+".to_string(),
        BinaryOp::Subtract => "-".to_string(),
        BinaryOp::Multiply => "*".to_string(),
        BinaryOp::Divide => "/".to_string(),
        BinaryOp::IntDivide => "/".to_string(), // Integer division in C when both operands are int
        BinaryOp::Modulo => "%".to_string(),
        BinaryOp::Power => {
            // Handled specially in emit_binary_expr using pow()
            unreachable!("Power operator should be handled in emit_binary_expr")
        }
        BinaryOp::Equal => "==".to_string(),
        BinaryOp::NotEqual => "!=".to_string(),
        BinaryOp::LessThan => "<".to_string(),
        BinaryOp::LessEqual => "<=".to_string(),
        BinaryOp::GreaterThan => ">".to_string(),
        BinaryOp::GreaterEqual => ">=".to_string(),
        BinaryOp::And => "&".to_string(), // Bitwise AND in BASIC
        BinaryOp::Or => "|".to_string(),  // Bitwise OR
        BinaryOp::Xor => "^".to_string(), // Bitwise XOR
        BinaryOp::Eqv => {
            // Handled specially in emit_binary_expr as ~(a ^ b)
            unreachable!("EQV operator should be handled in emit_binary_expr")
        }
        BinaryOp::Imp => {
            // Handled specially in emit_binary_expr as (~a) | b
            unreachable!("IMP operator should be handled in emit_binary_expr")
        }
    })
}

/// Emits string comparison code.
///
/// BASIC string comparisons use lexicographic ordering, which is handled
/// by `qb_string_compare()` returning -1, 0, or 1 like strcmp.
fn emit_string_comparison(left: &str, right: &str, op: &BinaryOp) -> Result<String, CodeGenError> {
    let cmp = format!("qb_string_compare({}, {})", left, right);
    let result = match op {
        BinaryOp::Equal => format!("({} == 0)", cmp),
        BinaryOp::NotEqual => format!("({} != 0)", cmp),
        BinaryOp::LessThan => format!("({} < 0)", cmp),
        BinaryOp::LessEqual => format!("({} <= 0)", cmp),
        BinaryOp::GreaterThan => format!("({} > 0)", cmp),
        BinaryOp::GreaterEqual => format!("({} >= 0)", cmp),
        _ => {
            return Err(CodeGenError::unsupported(format!(
                "operator {} on strings",
                op.as_str()
            )));
        }
    };
    Ok(result)
}

/// Maps a BASIC function name to its C equivalent.
///
/// Built-in BASIC functions are mapped to corresponding runtime functions
/// or standard C math functions. User-defined functions are prefixed with `qb_`.
pub(super) fn c_function_name(name: &str) -> String {
    let upper = name.to_uppercase();
    match upper.as_str() {
        // Math functions
        "ABS" => "fabs".to_string(),
        "SIN" => "sin".to_string(),
        "COS" => "cos".to_string(),
        "TAN" => "tan".to_string(),
        "ATN" => "atan".to_string(),
        "SQR" => "sqrt".to_string(),
        "LOG" => "log".to_string(),
        "EXP" => "exp".to_string(),
        "INT" => "floor".to_string(),
        "SGN" => "qb_sgn".to_string(),
        "RND" => "qb_rnd".to_string(),

        // QB64 extended math functions
        "_PI" => "qb_pi".to_string(),
        "_ASIN" => "asin".to_string(),
        "_ACOS" => "acos".to_string(),
        "_ATAN2" => "atan2".to_string(),
        "_HYPOT" => "hypot".to_string(),
        "_CEIL" => "ceil".to_string(),
        "_ROUND" => "round".to_string(),
        "_MIN" => "fmin".to_string(),
        "_MAX" => "fmax".to_string(),
        "_CLAMP" => "qb_clamp".to_string(),

        // String functions
        "LEN" => "qb_len".to_string(),
        "CHR$" => "qb_chr".to_string(),
        "ASC" => "qb_asc".to_string(),
        "LEFT$" => "qb_left".to_string(),
        "RIGHT$" => "qb_right".to_string(),
        "MID$" => "qb_mid".to_string(),
        "INSTR" => "qb_instr".to_string(),
        "STR$" => "qb_str".to_string(),
        "VAL" => "qb_val".to_string(),
        "UCASE$" => "qb_ucase".to_string(),
        "LCASE$" => "qb_lcase".to_string(),
        "LTRIM$" => "qb_ltrim".to_string(),
        "RTRIM$" => "qb_rtrim".to_string(),
        "SPACE$" => "qb_space".to_string(),
        "STRING$" => "qb_string_fill".to_string(),

        // Default: prefix with qb_ for user functions
        _ => format!("qb_{}", c_identifier(name).to_lowercase()),
    }
}

/// Escapes a string for C string literal.
///
/// This function ensures the string is safe to embed in generated C code by
/// escaping special characters. Non-ASCII characters are escaped as `\xNN`
/// sequences for maximum C compiler portability.
pub(super) fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() * 2); // Worst case: all escapes
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            // Escape all control characters (ASCII and Unicode)
            c if c.is_control() => {
                // For ASCII control chars, use \xNN
                // For Unicode control chars, escape each UTF-8 byte
                for byte in c.to_string().as_bytes() {
                    result.push_str(&format!("\\x{:02x}", byte));
                }
            }
            // Keep printable ASCII as-is
            c if c.is_ascii() => result.push(c),
            // Escape non-ASCII characters as UTF-8 byte sequences
            // This ensures C compiler compatibility regardless of source encoding
            c => {
                for byte in c.to_string().as_bytes() {
                    result.push_str(&format!("\\x{:02x}", byte));
                }
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::semantic::types::BasicType;

    #[test]
    fn test_emit_integer_literal() {
        let expr = TypedExpr::integer(42, Span::new(0, 2));
        let result = emit_expr(&expr).unwrap();
        assert_eq!(result, "42LL");
    }

    #[test]
    fn test_emit_string_literal() {
        let expr = TypedExpr::string("Hello".to_string(), Span::new(0, 7));
        let result = emit_expr(&expr).unwrap();
        assert_eq!(result, "qb_string_new(\"Hello\")");
    }

    #[test]
    fn test_emit_power_operator() {
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(2, Span::new(0, 1))),
                op: BinaryOp::Power,
                right: Box::new(TypedExpr::integer(3, Span::new(4, 5))),
            },
            BasicType::Double,
            Span::new(0, 5),
        );
        let result = emit_expr(&expr).unwrap();
        assert_eq!(result, "pow(2LL, 3LL)");
    }

    #[test]
    fn test_emit_eqv_operator() {
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(5, Span::new(0, 1))),
                op: BinaryOp::Eqv,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7))),
            },
            BasicType::Long,
            Span::new(0, 7),
        );
        let result = emit_expr(&expr).unwrap();
        assert_eq!(result, "(~(5LL ^ 3LL))");
    }

    #[test]
    fn test_emit_imp_operator() {
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(5, Span::new(0, 1))),
                op: BinaryOp::Imp,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7))),
            },
            BasicType::Long,
            Span::new(0, 7),
        );
        let result = emit_expr(&expr).unwrap();
        assert_eq!(result, "((~5LL) | 3LL)");
    }

    #[test]
    fn test_escape_string() {
        // Basic escapes
        assert_eq!(escape_string("Hello"), "Hello");
        assert_eq!(escape_string("Say \"Hi\""), "Say \\\"Hi\\\"");
        assert_eq!(escape_string("path\\file"), "path\\\\file");
        assert_eq!(escape_string("line1\nline2"), "line1\\nline2");
        assert_eq!(escape_string("tab\there"), "tab\\there");

        // ASCII control characters
        assert_eq!(escape_string("\x00\x1f"), "\\x00\\x1f");

        // Non-ASCII characters are escaped as UTF-8 bytes for C portability
        // 'é' (U+00E9) is encoded as bytes [0xC3, 0xA9] in UTF-8
        assert_eq!(escape_string("é"), "\\xc3\\xa9");

        // Emoji: '😀' (U+1F600) is encoded as bytes [0xF0, 0x9F, 0x98, 0x80]
        assert_eq!(escape_string("😀"), "\\xf0\\x9f\\x98\\x80");
    }

    #[test]
    fn test_c_function_name() {
        assert_eq!(c_function_name("SIN"), "sin");
        assert_eq!(c_function_name("ABS"), "fabs");
        assert_eq!(c_function_name("LEN"), "qb_len");
        assert_eq!(c_function_name("CHR$"), "qb_chr");
        assert_eq!(c_function_name("myFunc"), "qb_myfunc");
    }
}
