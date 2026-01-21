//! Compile-time constant folding for code generation.
//!
//! This module evaluates constant expressions during code generation,
//! emitting literal values instead of expressions like `(10LL + 5LL)`.
//!
//! # Benefits
//!
//! - Simpler generated C code
//! - Smaller binary size (C compiler has less work)
//! - Better optimization opportunities
//!
//! # Supported Operations
//!
//! - Arithmetic: `+`, `-`, `*`, `/`, `\`, `MOD`, `^`
//! - Bitwise: `AND`, `OR`, `XOR`, `EQV`, `IMP`
//! - Comparison: `=`, `<>`, `<`, `<=`, `>`, `>=`
//! - Unary: `-`, `NOT`
//! - String concatenation
//! - Pure built-in functions: `ABS`, `INT`, `SGN`, `LEN`, etc.

use crate::ast::{BinaryOp, UnaryOp};
use crate::semantic::typed_ir::{TypedExpr, TypedExprKind};

/// Result of constant folding - either an integer, float, or string literal.
#[derive(Debug, Clone)]
pub enum FoldedValue {
    Integer(i64),
    Float(f64),
    String(String),
}

/// Attempts to fold a typed expression to a constant value.
///
/// Returns `Some(FoldedValue)` if the entire expression can be evaluated
/// at compile time, `None` if it contains runtime elements.
pub fn try_fold(expr: &TypedExpr) -> Option<FoldedValue> {
    match &expr.kind {
        // Direct literals
        TypedExprKind::IntegerLiteral(v) => Some(FoldedValue::Integer(*v)),
        TypedExprKind::FloatLiteral(v) => Some(FoldedValue::Float(*v)),
        TypedExprKind::StringLiteral(v) => Some(FoldedValue::String(v.clone())),

        // Grouped expressions - unwrap and fold
        TypedExprKind::Grouped(inner) => try_fold(inner),

        // Binary operations
        TypedExprKind::Binary { left, op, right } => {
            let left_val = try_fold(left)?;
            let right_val = try_fold(right)?;
            fold_binary(*op, left_val, right_val)
        }

        // Unary operations
        TypedExprKind::Unary { op, operand } => {
            let operand_val = try_fold(operand)?;
            fold_unary(*op, operand_val)
        }

        // Type conversions
        TypedExprKind::Convert { expr: inner, .. } => {
            // For codegen purposes, we can fold through conversions
            // The C compiler will handle the actual type conversion
            try_fold(inner)
        }

        // Function calls - check if it's a pure built-in we can fold
        TypedExprKind::FunctionCall { name, args } => try_fold_builtin(name, args),

        // Everything else requires runtime evaluation
        TypedExprKind::Variable(_)
        | TypedExprKind::ArrayAccess { .. }
        | TypedExprKind::ArrayRef { .. }
        | TypedExprKind::FieldAccess { .. }
        | TypedExprKind::ExternalFunctionCall { .. }
        | TypedExprKind::ProcPtr { .. }
        | TypedExprKind::CvFunc { .. }
        | TypedExprKind::MkDollarFunc { .. }
        | TypedExprKind::CastFunc { .. } => None,
    }
}

/// Folds a binary operation on constant values.
fn fold_binary(op: BinaryOp, left: FoldedValue, right: FoldedValue) -> Option<FoldedValue> {
    match (left, right) {
        // Integer operations
        (FoldedValue::Integer(l), FoldedValue::Integer(r)) => {
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
                BinaryOp::Equal => basic_bool(l == r),
                BinaryOp::NotEqual => basic_bool(l != r),
                BinaryOp::LessThan => basic_bool(l < r),
                BinaryOp::LessEqual => basic_bool(l <= r),
                BinaryOp::GreaterThan => basic_bool(l > r),
                BinaryOp::GreaterEqual => basic_bool(l >= r),
            };
            Some(FoldedValue::Integer(result))
        }

        // Float operations
        (FoldedValue::Float(l), FoldedValue::Float(r)) => {
            let result = match op {
                BinaryOp::Add => l + r,
                BinaryOp::Subtract => l - r,
                BinaryOp::Multiply => l * r,
                BinaryOp::Divide => l / r,
                BinaryOp::Power => l.powf(r),
                BinaryOp::Equal => return Some(FoldedValue::Integer(basic_bool(l == r))),
                BinaryOp::NotEqual => return Some(FoldedValue::Integer(basic_bool(l != r))),
                BinaryOp::LessThan => return Some(FoldedValue::Integer(basic_bool(l < r))),
                BinaryOp::LessEqual => return Some(FoldedValue::Integer(basic_bool(l <= r))),
                BinaryOp::GreaterThan => return Some(FoldedValue::Integer(basic_bool(l > r))),
                BinaryOp::GreaterEqual => return Some(FoldedValue::Integer(basic_bool(l >= r))),
                // Bitwise ops not valid on floats
                _ => return None,
            };
            Some(FoldedValue::Float(result))
        }

        // Mixed int/float - promote to float
        (FoldedValue::Integer(l), FoldedValue::Float(r)) => {
            fold_binary(op, FoldedValue::Float(l as f64), FoldedValue::Float(r))
        }
        (FoldedValue::Float(l), FoldedValue::Integer(r)) => {
            fold_binary(op, FoldedValue::Float(l), FoldedValue::Float(r as f64))
        }

        // String operations
        (FoldedValue::String(l), FoldedValue::String(r)) => match op {
            BinaryOp::Add => Some(FoldedValue::String(l + &r)),
            BinaryOp::Equal => Some(FoldedValue::Integer(basic_bool(l == r))),
            BinaryOp::NotEqual => Some(FoldedValue::Integer(basic_bool(l != r))),
            BinaryOp::LessThan => Some(FoldedValue::Integer(basic_bool(l < r))),
            BinaryOp::LessEqual => Some(FoldedValue::Integer(basic_bool(l <= r))),
            BinaryOp::GreaterThan => Some(FoldedValue::Integer(basic_bool(l > r))),
            BinaryOp::GreaterEqual => Some(FoldedValue::Integer(basic_bool(l >= r))),
            _ => None,
        },

        // String + non-string not allowed
        _ => None,
    }
}

/// Folds a unary operation on a constant value.
fn fold_unary(op: UnaryOp, operand: FoldedValue) -> Option<FoldedValue> {
    match (op, operand) {
        (UnaryOp::Negate, FoldedValue::Integer(v)) => Some(FoldedValue::Integer(-v)),
        (UnaryOp::Negate, FoldedValue::Float(v)) => Some(FoldedValue::Float(-v)),
        (UnaryOp::Not, FoldedValue::Integer(v)) => Some(FoldedValue::Integer(!v)),
        _ => None,
    }
}

/// Attempts to fold a built-in function call with constant arguments.
///
/// Only pure functions (no side effects, deterministic) are folded.
fn try_fold_builtin(name: &str, args: &[TypedExpr]) -> Option<FoldedValue> {
    let upper = name.to_uppercase();

    // Fold the arguments first
    let folded_args: Option<Vec<_>> = args.iter().map(try_fold).collect();
    let folded_args = folded_args?;

    match upper.as_str() {
        // Single-argument numeric functions
        "ABS" if folded_args.len() == 1 => match &folded_args[0] {
            FoldedValue::Integer(v) => Some(FoldedValue::Integer(v.abs())),
            FoldedValue::Float(v) => Some(FoldedValue::Float(v.abs())),
            _ => None,
        },

        "SGN" if folded_args.len() == 1 => match &folded_args[0] {
            FoldedValue::Integer(v) => Some(FoldedValue::Integer(v.signum())),
            FoldedValue::Float(v) => {
                let s = if *v > 0.0 {
                    1
                } else if *v < 0.0 {
                    -1
                } else {
                    0
                };
                Some(FoldedValue::Integer(s))
            }
            _ => None,
        },

        "INT" if folded_args.len() == 1 => match &folded_args[0] {
            FoldedValue::Integer(v) => Some(FoldedValue::Integer(*v)),
            FoldedValue::Float(v) => Some(FoldedValue::Float(v.floor())),
            _ => None,
        },

        "FIX" if folded_args.len() == 1 => match &folded_args[0] {
            FoldedValue::Integer(v) => Some(FoldedValue::Integer(*v)),
            FoldedValue::Float(v) => Some(FoldedValue::Float(v.trunc())),
            _ => None,
        },

        "_CEIL" if folded_args.len() == 1 => match &folded_args[0] {
            FoldedValue::Integer(v) => Some(FoldedValue::Integer(*v)),
            FoldedValue::Float(v) => Some(FoldedValue::Float(v.ceil())),
            _ => None,
        },

        "_ROUND" if folded_args.len() == 1 => match &folded_args[0] {
            FoldedValue::Integer(v) => Some(FoldedValue::Integer(*v)),
            FoldedValue::Float(v) => Some(FoldedValue::Float(v.round())),
            _ => None,
        },

        // Trigonometric functions (operate on floats)
        "SIN" if folded_args.len() == 1 => get_float(&folded_args[0]).map(|v| FoldedValue::Float(v.sin())),
        "COS" if folded_args.len() == 1 => get_float(&folded_args[0]).map(|v| FoldedValue::Float(v.cos())),
        "TAN" if folded_args.len() == 1 => get_float(&folded_args[0]).map(|v| FoldedValue::Float(v.tan())),
        "ATN" if folded_args.len() == 1 => get_float(&folded_args[0]).map(|v| FoldedValue::Float(v.atan())),
        "SQR" if folded_args.len() == 1 => {
            get_float(&folded_args[0]).and_then(|v| if v >= 0.0 { Some(FoldedValue::Float(v.sqrt())) } else { None })
        }
        "LOG" if folded_args.len() == 1 => {
            get_float(&folded_args[0]).and_then(|v| if v > 0.0 { Some(FoldedValue::Float(v.ln())) } else { None })
        }
        "EXP" if folded_args.len() == 1 => get_float(&folded_args[0]).map(|v| FoldedValue::Float(v.exp())),

        // QB64 extended trig
        "_ASIN" if folded_args.len() == 1 => {
            get_float(&folded_args[0]).and_then(|v| if (-1.0..=1.0).contains(&v) { Some(FoldedValue::Float(v.asin())) } else { None })
        }
        "_ACOS" if folded_args.len() == 1 => {
            get_float(&folded_args[0]).and_then(|v| if (-1.0..=1.0).contains(&v) { Some(FoldedValue::Float(v.acos())) } else { None })
        }
        "_SINH" if folded_args.len() == 1 => get_float(&folded_args[0]).map(|v| FoldedValue::Float(v.sinh())),
        "_COSH" if folded_args.len() == 1 => get_float(&folded_args[0]).map(|v| FoldedValue::Float(v.cosh())),
        "_TANH" if folded_args.len() == 1 => get_float(&folded_args[0]).map(|v| FoldedValue::Float(v.tanh())),

        // Two-argument functions
        "_ATAN2" if folded_args.len() == 2 => {
            let y = get_float(&folded_args[0])?;
            let x = get_float(&folded_args[1])?;
            Some(FoldedValue::Float(y.atan2(x)))
        }
        "_HYPOT" if folded_args.len() == 2 => {
            let x = get_float(&folded_args[0])?;
            let y = get_float(&folded_args[1])?;
            Some(FoldedValue::Float(x.hypot(y)))
        }
        "_MIN" if folded_args.len() == 2 => fold_min_max(&folded_args[0], &folded_args[1], true),
        "_MAX" if folded_args.len() == 2 => fold_min_max(&folded_args[0], &folded_args[1], false),

        // Angle conversions
        "_D2R" if folded_args.len() == 1 => {
            get_float(&folded_args[0]).map(|v| FoldedValue::Float(v.to_radians()))
        }
        "_R2D" if folded_args.len() == 1 => {
            get_float(&folded_args[0]).map(|v| FoldedValue::Float(v.to_degrees()))
        }

        // String functions
        "LEN" if folded_args.len() == 1 => {
            if let FoldedValue::String(s) = &folded_args[0] {
                Some(FoldedValue::Integer(s.len() as i64))
            } else {
                None
            }
        }

        "ASC" if folded_args.len() == 1 => {
            if let FoldedValue::String(s) = &folded_args[0] {
                s.chars().next().map(|c| FoldedValue::Integer(c as i64))
            } else {
                None
            }
        }

        "CHR$" if folded_args.len() == 1 => {
            if let FoldedValue::Integer(v) = &folded_args[0] {
                if *v >= 0 && *v <= 255 {
                    Some(FoldedValue::String((*v as u8 as char).to_string()))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "UCASE$" if folded_args.len() == 1 => {
            if let FoldedValue::String(s) = &folded_args[0] {
                Some(FoldedValue::String(s.to_uppercase()))
            } else {
                None
            }
        }

        "LCASE$" if folded_args.len() == 1 => {
            if let FoldedValue::String(s) = &folded_args[0] {
                Some(FoldedValue::String(s.to_lowercase()))
            } else {
                None
            }
        }

        "LTRIM$" if folded_args.len() == 1 => {
            if let FoldedValue::String(s) = &folded_args[0] {
                Some(FoldedValue::String(s.trim_start().to_string()))
            } else {
                None
            }
        }

        "RTRIM$" if folded_args.len() == 1 => {
            if let FoldedValue::String(s) = &folded_args[0] {
                Some(FoldedValue::String(s.trim_end().to_string()))
            } else {
                None
            }
        }

        "_TRIM$" | "TRIM$" if folded_args.len() == 1 => {
            if let FoldedValue::String(s) = &folded_args[0] {
                Some(FoldedValue::String(s.trim().to_string()))
            } else {
                None
            }
        }

        "SPACE$" if folded_args.len() == 1 => {
            if let FoldedValue::Integer(n) = &folded_args[0] {
                if *n >= 0 && *n <= 32767 {
                    Some(FoldedValue::String(" ".repeat(*n as usize)))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "STRING$" if folded_args.len() == 2 => {
            if let FoldedValue::Integer(n) = &folded_args[0] {
                if *n >= 0 && *n <= 32767 {
                    let ch = match &folded_args[1] {
                        FoldedValue::Integer(c) if *c >= 0 && *c <= 255 => *c as u8 as char,
                        FoldedValue::String(s) => s.chars().next()?,
                        _ => return None,
                    };
                    Some(FoldedValue::String(ch.to_string().repeat(*n as usize)))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "LEFT$" if folded_args.len() == 2 => {
            if let (FoldedValue::String(s), FoldedValue::Integer(n)) = (&folded_args[0], &folded_args[1]) {
                if *n >= 0 {
                    let n = (*n as usize).min(s.len());
                    Some(FoldedValue::String(s.chars().take(n).collect()))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "RIGHT$" if folded_args.len() == 2 => {
            if let (FoldedValue::String(s), FoldedValue::Integer(n)) = (&folded_args[0], &folded_args[1]) {
                if *n >= 0 {
                    let n = (*n as usize).min(s.len());
                    let skip = s.len().saturating_sub(n);
                    Some(FoldedValue::String(s.chars().skip(skip).collect()))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "INSTR" if folded_args.len() == 2 || folded_args.len() == 3 => {
            // INSTR([start,] string, search)
            let (start, haystack, needle) = if folded_args.len() == 2 {
                (1i64, &folded_args[0], &folded_args[1])
            } else {
                if let FoldedValue::Integer(s) = &folded_args[0] {
                    (*s, &folded_args[1], &folded_args[2])
                } else {
                    return None;
                }
            };

            if let (FoldedValue::String(h), FoldedValue::String(n)) = (haystack, needle) {
                if start < 1 {
                    return Some(FoldedValue::Integer(0));
                }
                let start_idx = (start as usize).saturating_sub(1);
                if start_idx >= h.len() {
                    return Some(FoldedValue::Integer(0));
                }
                if let Some(pos) = h[start_idx..].find(n.as_str()) {
                    Some(FoldedValue::Integer((start_idx + pos + 1) as i64))
                } else {
                    Some(FoldedValue::Integer(0))
                }
            } else {
                None
            }
        }

        // Type conversion functions
        "CINT" | "CLNG" | "CSNG" | "CDBL" | "CSGN" => {
            if folded_args.len() == 1 {
                match &folded_args[0] {
                    FoldedValue::Integer(v) => match upper.as_str() {
                        "CINT" => Some(FoldedValue::Integer(*v as i16 as i64)),
                        "CLNG" => Some(FoldedValue::Integer(*v as i32 as i64)),
                        "CSNG" | "CDBL" => Some(FoldedValue::Float(*v as f64)),
                        _ => None,
                    },
                    FoldedValue::Float(v) => match upper.as_str() {
                        "CINT" => Some(FoldedValue::Integer(*v as i16 as i64)),
                        "CLNG" => Some(FoldedValue::Integer(*v as i32 as i64)),
                        "CSNG" | "CDBL" => Some(FoldedValue::Float(*v)),
                        _ => None,
                    },
                    _ => None,
                }
            } else {
                None
            }
        }

        // Bit manipulation
        "_SHL" if folded_args.len() == 2 => {
            if let (FoldedValue::Integer(v), FoldedValue::Integer(bits)) = (&folded_args[0], &folded_args[1]) {
                if *bits >= 0 && *bits < 64 {
                    Some(FoldedValue::Integer(v << bits))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "_SHR" if folded_args.len() == 2 => {
            if let (FoldedValue::Integer(v), FoldedValue::Integer(bits)) = (&folded_args[0], &folded_args[1]) {
                if *bits >= 0 && *bits < 64 {
                    Some(FoldedValue::Integer(v >> bits))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "_READBIT" if folded_args.len() == 2 => {
            if let (FoldedValue::Integer(v), FoldedValue::Integer(bit)) = (&folded_args[0], &folded_args[1]) {
                if *bit >= 0 && *bit < 64 {
                    Some(FoldedValue::Integer((v >> bit) & 1))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "_SETBIT" if folded_args.len() == 2 => {
            if let (FoldedValue::Integer(v), FoldedValue::Integer(bit)) = (&folded_args[0], &folded_args[1]) {
                if *bit >= 0 && *bit < 64 {
                    Some(FoldedValue::Integer(v | (1 << bit)))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "_RESETBIT" if folded_args.len() == 2 => {
            if let (FoldedValue::Integer(v), FoldedValue::Integer(bit)) = (&folded_args[0], &folded_args[1]) {
                if *bit >= 0 && *bit < 64 {
                    Some(FoldedValue::Integer(v & !(1 << bit)))
                } else {
                    None
                }
            } else {
                None
            }
        }

        "_TOGGLEBIT" if folded_args.len() == 2 => {
            if let (FoldedValue::Integer(v), FoldedValue::Integer(bit)) = (&folded_args[0], &folded_args[1]) {
                if *bit >= 0 && *bit < 64 {
                    Some(FoldedValue::Integer(v ^ (1 << bit)))
                } else {
                    None
                }
            } else {
                None
            }
        }

        // VAL converts string to number
        "VAL" if folded_args.len() == 1 => {
            if let FoldedValue::String(s) = &folded_args[0] {
                let trimmed = s.trim();
                if let Ok(v) = trimmed.parse::<i64>() {
                    Some(FoldedValue::Integer(v))
                } else if let Ok(v) = trimmed.parse::<f64>() {
                    Some(FoldedValue::Float(v))
                } else {
                    Some(FoldedValue::Integer(0)) // BASIC VAL returns 0 for invalid strings
                }
            } else {
                None
            }
        }

        // STR$ converts number to string
        "STR$" if folded_args.len() == 1 => match &folded_args[0] {
            FoldedValue::Integer(v) => {
                // BASIC STR$ prepends a space for positive numbers
                if *v >= 0 {
                    Some(FoldedValue::String(format!(" {}", v)))
                } else {
                    Some(FoldedValue::String(format!("{}", v)))
                }
            }
            FoldedValue::Float(v) => {
                if *v >= 0.0 {
                    Some(FoldedValue::String(format!(" {}", v)))
                } else {
                    Some(FoldedValue::String(format!("{}", v)))
                }
            }
            _ => None,
        },

        // HEX$ and OCT$
        "HEX$" if folded_args.len() == 1 => {
            if let FoldedValue::Integer(v) = &folded_args[0] {
                Some(FoldedValue::String(format!("{:X}", v)))
            } else {
                None
            }
        }

        "OCT$" if folded_args.len() == 1 => {
            if let FoldedValue::Integer(v) = &folded_args[0] {
                Some(FoldedValue::String(format!("{:o}", v)))
            } else {
                None
            }
        }

        // Constants (zero-arg functions)
        "_PI" if folded_args.is_empty() => Some(FoldedValue::Float(std::f64::consts::PI)),
        "_TRUE" if folded_args.is_empty() => Some(FoldedValue::Integer(-1)),
        "_FALSE" if folded_args.is_empty() => Some(FoldedValue::Integer(0)),

        // Not a foldable function
        _ => None,
    }
}

/// Converts a FoldedValue to f64 if possible.
fn get_float(v: &FoldedValue) -> Option<f64> {
    match v {
        FoldedValue::Integer(i) => Some(*i as f64),
        FoldedValue::Float(f) => Some(*f),
        FoldedValue::String(_) => None,
    }
}

/// Folds MIN or MAX of two values.
fn fold_min_max(a: &FoldedValue, b: &FoldedValue, is_min: bool) -> Option<FoldedValue> {
    match (a, b) {
        (FoldedValue::Integer(l), FoldedValue::Integer(r)) => {
            if is_min {
                Some(FoldedValue::Integer(*l.min(r)))
            } else {
                Some(FoldedValue::Integer(*l.max(r)))
            }
        }
        _ => {
            let l = get_float(a)?;
            let r = get_float(b)?;
            if is_min {
                Some(FoldedValue::Float(l.min(r)))
            } else {
                Some(FoldedValue::Float(l.max(r)))
            }
        }
    }
}

/// Converts a boolean to BASIC convention: -1 for TRUE, 0 for FALSE.
#[inline]
fn basic_bool(b: bool) -> i64 {
    if b { -1 } else { 0 }
}

/// Emits C code for a folded constant value.
pub fn emit_folded(value: &FoldedValue) -> String {
    match value {
        FoldedValue::Integer(v) => format!("{}LL", v),
        FoldedValue::Float(v) => {
            if v.is_nan() {
                "(0.0/0.0)".to_string()
            } else if v.is_infinite() {
                if v.is_sign_positive() {
                    "(1.0/0.0)".to_string()
                } else {
                    "(-1.0/0.0)".to_string()
                }
            } else {
                format!("{:.17}", v)
            }
        }
        FoldedValue::String(s) => {
            let escaped = escape_string_for_c(s);
            format!("qb_string_new(\"{}\")", escaped)
        }
    }
}

/// Escapes a string for C code.
fn escape_string_for_c(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\0' => result.push_str("\\0"),
            c if c.is_ascii_control() => {
                result.push_str(&format!("\\x{:02x}", c as u8));
            }
            c => result.push(c),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::semantic::types::BasicType;

    fn int_lit(v: i64) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::IntegerLiteral(v),
            basic_type: BasicType::Long,
            span: Span::new(0, 1),
        }
    }

    #[allow(dead_code)]
    fn float_lit(v: f64) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::FloatLiteral(v),
            basic_type: BasicType::Double,
            span: Span::new(0, 1),
        }
    }

    fn string_lit(v: &str) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::StringLiteral(v.to_string()),
            basic_type: BasicType::String,
            span: Span::new(0, 1),
        }
    }

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

    fn func_call(name: &str, args: Vec<TypedExpr>) -> TypedExpr {
        TypedExpr {
            kind: TypedExprKind::FunctionCall {
                name: name.to_string(),
                args,
            },
            basic_type: BasicType::Long,
            span: Span::new(0, 1),
        }
    }

    #[test]
    fn test_fold_integer_arithmetic() {
        let expr = binary(int_lit(10), BinaryOp::Add, int_lit(5));
        assert!(matches!(try_fold(&expr), Some(FoldedValue::Integer(15))));

        let expr = binary(int_lit(10), BinaryOp::Multiply, int_lit(5));
        assert!(matches!(try_fold(&expr), Some(FoldedValue::Integer(50))));
    }

    #[test]
    fn test_fold_nested_arithmetic() {
        // (10 + 5) * 2 = 30
        let inner = binary(int_lit(10), BinaryOp::Add, int_lit(5));
        let expr = binary(inner, BinaryOp::Multiply, int_lit(2));
        assert!(matches!(try_fold(&expr), Some(FoldedValue::Integer(30))));
    }

    #[test]
    fn test_fold_string_concat() {
        let expr = binary(string_lit("Hello"), BinaryOp::Add, string_lit(" World"));
        match try_fold(&expr) {
            Some(FoldedValue::String(s)) => assert_eq!(s, "Hello World"),
            _ => panic!("Expected string concat"),
        }
    }

    #[test]
    fn test_fold_abs() {
        let expr = func_call("ABS", vec![int_lit(-42)]);
        assert!(matches!(try_fold(&expr), Some(FoldedValue::Integer(42))));
    }

    #[test]
    fn test_fold_len() {
        let expr = func_call("LEN", vec![string_lit("Hello")]);
        assert!(matches!(try_fold(&expr), Some(FoldedValue::Integer(5))));
    }

    #[test]
    fn test_fold_ucase() {
        let expr = func_call("UCASE$", vec![string_lit("hello")]);
        match try_fold(&expr) {
            Some(FoldedValue::String(s)) => assert_eq!(s, "HELLO"),
            _ => panic!("Expected uppercase string"),
        }
    }

    #[test]
    fn test_fold_chr_asc() {
        let expr = func_call("CHR$", vec![int_lit(65)]);
        match try_fold(&expr) {
            Some(FoldedValue::String(s)) => assert_eq!(s, "A"),
            _ => panic!("Expected character"),
        }

        let expr = func_call("ASC", vec![string_lit("A")]);
        assert!(matches!(try_fold(&expr), Some(FoldedValue::Integer(65))));
    }

    #[test]
    fn test_fold_comparison() {
        let expr = binary(int_lit(5), BinaryOp::GreaterThan, int_lit(3));
        assert!(matches!(try_fold(&expr), Some(FoldedValue::Integer(-1)))); // TRUE

        let expr = binary(int_lit(5), BinaryOp::LessThan, int_lit(3));
        assert!(matches!(try_fold(&expr), Some(FoldedValue::Integer(0)))); // FALSE
    }

    #[test]
    fn test_fold_bitwise() {
        let expr = binary(int_lit(0b1100), BinaryOp::And, int_lit(0b1010));
        assert!(matches!(try_fold(&expr), Some(FoldedValue::Integer(0b1000))));
    }

    #[test]
    fn test_emit_folded() {
        assert_eq!(emit_folded(&FoldedValue::Integer(42)), "42LL");
        assert_eq!(emit_folded(&FoldedValue::Float(3.14)), "3.14000000000000012");
        assert_eq!(emit_folded(&FoldedValue::String("test".to_string())), "qb_string_new(\"test\")");
    }
}
