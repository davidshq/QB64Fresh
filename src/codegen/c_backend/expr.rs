//! Expression code generation for QB64Fresh C backend.
//!
//! This module handles the emission of C code for all expression types,
//! including literals, binary/unary operations, function calls, and array access.
//!
//! # Constant Folding
//!
//! Before emitting C code, expressions are checked for constant folding
//! opportunities. Expressions like `10 + 5` are folded to `15` at compile
//! time, resulting in simpler generated code.
//!
//! # Special Handling
//!
//! Several BASIC operations require special treatment:
//! - **String concatenation**: Uses `qb_string_concat()` instead of `+`
//! - **String comparison**: Uses `qb_string_compare()` with appropriate operators
//! - **Power operator (`^`)**: Uses `pow()` from math.h
//! - **EQV/IMP operators**: Translated to bitwise expressions

use crate::ast::BinaryOp;
use crate::codegen::error::{CodeGenError, CodeGenErrorKind};
use crate::semantic::typed_ir::{TypedArrayDimension, TypedExpr, TypedExprKind};
use crate::semantic::types::BasicType;

use super::const_fold::{emit_folded, try_fold};
use super::types::{c_identifier, c_type};

/// Emits C code for an expression.
///
/// This function recursively processes the expression tree, generating
/// appropriate C code for each node type.
///
/// Before emitting, attempts to fold constant expressions. For example,
/// `10 + 5` will be emitted as `15LL` instead of `(10LL + 5LL)`.
///
/// When `no_shell` is true, `SHELL` and `_SHELLHIDE` (function form) are rejected with
/// `CodeGenErrorKind::ShellDisabled`. Statement forms are rejected in the statement emitter.
///
/// `param_names` is used to avoid incorrectly renaming parameter references when a local
/// variable shadows a parameter. If a variable name is both in `variable_renames` and
/// `param_names`, and it's used as a simple variable (not array access), we use the
/// parameter name instead of the renamed local variable.
///
/// `byref_scalar_names` contains BYREF scalar parameter names that need to be dereferenced
/// when accessed (they are pointers in the generated C code).
/// `byref_string_names` contains BYREF string parameter C names; we must not apply
/// variable_renames to them so we use the local name (e.g. `a_str`) that emit_byref_copies
/// created, not the global scalar name (e.g. `a_str_scalar`).
/// `byref_string_basic_names` is the parallel list of BASIC parameter names; used to match
/// abbreviated variable references (e.g. Variable "e" → param "elements" → use "elements_str").
/// Set of C names for external functions from DECLARE DYNAMIC LIBRARY.
/// When non-empty and `c_name` is in the set, calls use the function pointer `qb_dyn_<c_name>`.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_expr(
    expr: &TypedExpr,
    no_shell: bool,
    variable_renames: &std::collections::HashMap<String, String>,
    param_names: &std::collections::HashSet<String>,
    byref_scalar_names: &std::collections::HashSet<String>,
    byref_udt_names: &std::collections::HashSet<String>,
    byref_string_names: &[String],
    byref_string_basic_names: &[String],
    dynamic_external_c_names: &std::collections::HashSet<String>,
) -> Result<String, CodeGenError> {
    emit_expr_internal(
        expr,
        no_shell,
        variable_renames,
        param_names,
        byref_scalar_names,
        byref_udt_names,
        byref_string_names,
        byref_string_basic_names,
        dynamic_external_c_names,
        false,
    )
}

/// Emits C code for an expression, optionally wrapping string temporaries.
///
/// When `wrap_string_temps` is true (external runtime mode), string-returning
/// expressions (literals, function calls, concatenations) are wrapped with
/// `qbs_tmp_register()` to ensure proper cleanup.
///
/// This is needed for external runtime because the Rust library functions don't
/// internally register strings with the temp pool, unlike the inline C runtime.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_expr_external(
    expr: &TypedExpr,
    no_shell: bool,
    variable_renames: &std::collections::HashMap<String, String>,
    param_names: &std::collections::HashSet<String>,
    byref_scalar_names: &std::collections::HashSet<String>,
    byref_udt_names: &std::collections::HashSet<String>,
    byref_string_names: &[String],
    byref_string_basic_names: &[String],
    dynamic_external_c_names: &std::collections::HashSet<String>,
) -> Result<String, CodeGenError> {
    emit_expr_internal(
        expr,
        no_shell,
        variable_renames,
        param_names,
        byref_scalar_names,
        byref_udt_names,
        byref_string_names,
        byref_string_basic_names,
        dynamic_external_c_names,
        true,
    )
}

/// Internal implementation of emit_expr with wrap_string_temps flag.
#[allow(clippy::too_many_arguments)]
fn emit_expr_internal(
    expr: &TypedExpr,
    no_shell: bool,
    variable_renames: &std::collections::HashMap<String, String>,
    param_names: &std::collections::HashSet<String>,
    byref_scalar_names: &std::collections::HashSet<String>,
    byref_udt_names: &std::collections::HashSet<String>,
    byref_string_names: &[String],
    byref_string_basic_names: &[String],
    dynamic_external_c_names: &std::collections::HashSet<String>,
    wrap_string_temps: bool,
) -> Result<String, CodeGenError> {
    // Try to fold the expression to a constant first
    // Only fold complex expressions (binary, unary, function calls) to avoid
    // redundant work on already-literal values
    if matches!(
        &expr.kind,
        TypedExprKind::Binary { .. }
            | TypedExprKind::Unary { .. }
            | TypedExprKind::FunctionCall { .. }
            | TypedExprKind::Grouped(_)
    ) && let Some(folded) = try_fold(expr)
    {
        return Ok(emit_folded(&folded));
    }

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
            let code = format!("qb_string_new(\"{}\")", escaped);
            // For external runtime, wrap string literals with qbs_tmp_register
            if wrap_string_temps {
                Ok(format!("qbs_tmp_register({})", code))
            } else {
                Ok(code)
            }
        }

        TypedExprKind::Variable(name) => {
            let mut c_name = c_identifier(name);
            // If the variable could be a BYREF string param by abbreviation (e.g. "e" for "elements"),
            // use that param's C name so we emit the local (value), not the global scalar.
            // Only do this when the variable is actually string-typed; otherwise we would emit
            // e.g. n2_str (QbString*) where an integer is expected (FOR end value, array index).
            let mut resolved_byref_param = byref_string_names.contains(&c_name);
            if !resolved_byref_param
                && !byref_string_basic_names.is_empty()
                && byref_string_basic_names.len() == byref_string_names.len()
                && expr.basic_type.is_string()
                && let Some(i) = byref_string_basic_names.iter().position(|basic| {
                    basic.eq_ignore_ascii_case(name)
                        || (name.len() <= basic.len()
                            && basic.to_lowercase().starts_with(&name.to_lowercase()))
                })
            {
                c_name = byref_string_names[i].clone();
                resolved_byref_param = true;
            }
            // Apply variable renaming if this variable was renamed to avoid parameter shadowing.
            // However, if the variable name is also a parameter name, and this is a simple variable
            // reference (not array access), we should use the parameter name instead of the renamed
            // local variable. This handles cases like `countFunctionElements(args)` where `args`
            // is a parameter, not the local variable that shadows it.
            if let Some(renamed) = variable_renames.get(&c_name) {
                // Only apply rename if this is NOT a parameter name, and NOT a BYREF string
                // parameter. BYREF string params get a local (e.g. a_str) from emit_byref_copies;
                // we must use that name, not the global scalar name (e.g. a_str_scalar).
                if !param_names.contains(&c_name) && !resolved_byref_param {
                    c_name = renamed.clone();
                }
            }
            // Check if this is a BYREF scalar parameter - if so, dereference the pointer
            // to get the value. Exception: BYREF string parameters get a local that is
            // already the value (QbString* name = *name_ref or char* name = (*name_ref)),
            // so we must emit the variable name (value), not *name.
            let needs_deref = byref_scalar_names.contains(&c_name);
            let is_byref_string = needs_deref && expr.basic_type.is_string();
            // Fixed-length strings are char arrays in C, but need to be wrapped
            // when used in contexts expecting qb_string* (e.g., string concatenation)
            if matches!(expr.basic_type, BasicType::FixedString(_)) {
                // BYREF fixed-length: local is char* (value); never dereference
                Ok(format!("qb_str_from_c({})", c_name))
            } else if needs_deref && !is_byref_string {
                Ok(format!("*{}", c_name))
            } else {
                // BYREF string: local is QbString* (value); use as-is
                Ok(c_name)
            }
        }

        TypedExprKind::Binary { left, op, right } => emit_binary_expr(
            left,
            op,
            right,
            no_shell,
            variable_renames,
            param_names,
            byref_scalar_names,
            byref_udt_names,
            byref_string_names,
            byref_string_basic_names,
            dynamic_external_c_names,
            wrap_string_temps,
        ),

        TypedExprKind::Unary { op, operand } => {
            let operand_code = emit_expr_internal(
                operand,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
                wrap_string_temps,
            )?;
            let op_str = match op {
                crate::ast::UnaryOp::Negate => "-",
                crate::ast::UnaryOp::Not => "~", // Bitwise NOT for numeric types
            };
            Ok(format!("({}{})", op_str, operand_code))
        }

        TypedExprKind::Grouped(inner) => {
            let inner_code = emit_expr_internal(
                inner,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
                wrap_string_temps,
            )?;
            Ok(format!("({})", inner_code))
        }

        TypedExprKind::FunctionCall { name, args, params } => {
            let upper_name = name.to_uppercase();
            if no_shell && (upper_name == "SHELL" || upper_name == "_SHELLHIDE") {
                return Err(CodeGenError::new(CodeGenErrorKind::ShellDisabled).with_span(expr.span));
            }
            // Special case: _IIF is polymorphic - use appropriate variant based on return type
            if upper_name == "_IIF" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                let c_name = if expr.basic_type.is_string() {
                    "qb_iif_str".to_string()
                } else {
                    "qb_iif".to_string()
                };
                return Ok(format!("{}({})", c_name, args_str));
            }

            // Special case: MID$ with 2 arguments (no length) uses qb_mid2
            if upper_name == "MID$" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_mid2({})", args_str));
            }

            // Special case: INSTR with 2 arguments (no start) uses qb_instr2
            if upper_name == "INSTR" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_instr2({})", args_str));
            }

            // Special case: _INSTRREV with 3 arguments (start, source, search) uses qb_instrrev3
            // C function signature: qb_instrrev3(source, search, start) - arguments reordered
            if upper_name == "_INSTRREV" && args.len() == 3 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_vec = args_code?;
                // Reorder: BASIC (start, source, search) -> C (source, search, start)
                return Ok(format!(
                    "qb_instrrev3({}, {}, {})",
                    args_vec[1], args_vec[2], args_vec[0]
                ));
            }

            // Special case: LBOUND with 1 argument (array) uses qb_lbound
            // Don't convert fixed-length strings - pass array pointer directly
            if upper_name == "LBOUND" && args.len() == 1 {
                let arg_code = emit_expr(
                    &args[0],
                    no_shell,
                    variable_renames,
                    param_names,
                    byref_scalar_names,
                    byref_udt_names,
                    byref_string_names,
                    byref_string_basic_names,
                    dynamic_external_c_names,
                )?;
                // For fixed-length strings, unwrap qb_str_from_c() if present
                // LBOUND needs the raw array pointer, not a converted string
                let unwrapped = if arg_code.starts_with("qb_str_from_c(") {
                    unwrap_qb_str_from_c(&arg_code)
                } else {
                    arg_code
                };
                return Ok(format!("qb_lbound({})", unwrapped));
            }

            // Special case: LBOUND with 2 arguments (array, dimension) uses qb_lbound2
            if upper_name == "LBOUND" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_lbound2({})", args_str));
            }

            // Special case: UBOUND with 1 argument (array) uses qb_ubound
            // Don't convert fixed-length strings - pass array pointer directly
            if upper_name == "UBOUND" && args.len() == 1 {
                let arg_code = emit_expr(
                    &args[0],
                    no_shell,
                    variable_renames,
                    param_names,
                    byref_scalar_names,
                    byref_udt_names,
                    byref_string_names,
                    byref_string_basic_names,
                    dynamic_external_c_names,
                )?;
                // For fixed-length strings, unwrap qb_str_from_c() if present
                // UBOUND needs the raw array pointer, not a converted string
                let unwrapped = if arg_code.starts_with("qb_str_from_c(") {
                    unwrap_qb_str_from_c(&arg_code)
                } else {
                    arg_code
                };
                return Ok(format!("qb_ubound({})", unwrapped));
            }

            // Special case: UBOUND with 2 arguments (array, dimension) uses qb_ubound2
            if upper_name == "UBOUND" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_ubound2({})", args_str));
            }

            // Special case: LEN - use qb_len_str for strings, sizeof for numeric types
            if upper_name == "LEN" && args.len() == 1 {
                let arg_code = emit_expr(
                    &args[0],
                    no_shell,
                    variable_renames,
                    param_names,
                    byref_scalar_names,
                    byref_udt_names,
                    byref_string_names,
                    byref_string_basic_names,
                    dynamic_external_c_names,
                )?;
                if args[0].basic_type.is_string() {
                    return Ok(format!("qb_len_str({})", arg_code));
                } else {
                    // Numeric types: use sizeof to get byte size
                    return Ok(format!("(int32_t)sizeof({})", arg_code));
                }
            }

            // Special case: VARPTR - returns address of variable as integer
            // Must pass &variable, not the value
            if upper_name == "VARPTR" && args.len() == 1 {
                let arg = &args[0];
                // Check if this is an addressable lvalue
                let is_lvalue = matches!(
                    arg.kind,
                    TypedExprKind::Variable { .. }
                        | TypedExprKind::ArrayAccess { .. }
                        | TypedExprKind::FieldAccess { .. }
                );
                if is_lvalue {
                    let arg_code = emit_expr(
                        arg,
                        no_shell,
                        variable_renames,
                        param_names,
                        byref_scalar_names,
                        byref_udt_names,
                        byref_string_names,
                        byref_string_basic_names,
                        dynamic_external_c_names,
                    )?;
                    return Ok(format!("((int32_t)(intptr_t)&({}))", arg_code));
                } else {
                    // Non-lvalue - can't take address, return 0
                    return Ok("0".to_string());
                }
            }

            // Special case: VARPTR$ - returns binary string of variable's address
            if upper_name == "VARPTR$" && args.len() == 1 {
                let arg = &args[0];
                let is_lvalue = matches!(
                    arg.kind,
                    TypedExprKind::Variable { .. }
                        | TypedExprKind::ArrayAccess { .. }
                        | TypedExprKind::FieldAccess { .. }
                );
                if is_lvalue {
                    let arg_code = emit_expr(
                        arg,
                        no_shell,
                        variable_renames,
                        param_names,
                        byref_scalar_names,
                        byref_udt_names,
                        byref_string_names,
                        byref_string_basic_names,
                        dynamic_external_c_names,
                    )?;
                    return Ok(format!("qb_varptr_str(&({}))", arg_code));
                } else {
                    return Ok("qb_string_new(\"\")".to_string());
                }
            }

            // Special case: VARSEG - returns segment (always 0 in flat memory model)
            if upper_name == "VARSEG" && args.len() == 1 {
                // In modern flat memory model, segment is meaningless - return 0
                return Ok("0".to_string());
            }

            // Special case: SADD - returns address of string data
            if upper_name == "SADD" && args.len() == 1 {
                let arg_code = emit_expr(
                    &args[0],
                    no_shell,
                    variable_renames,
                    param_names,
                    byref_scalar_names,
                    byref_udt_names,
                    byref_string_names,
                    byref_string_basic_names,
                    dynamic_external_c_names,
                )?;
                return Ok(format!("((int32_t)(intptr_t)({}).data)", arg_code));
            }

            // Special case: _MESSAGEBOX with different argument counts
            if upper_name == "_MESSAGEBOX" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    1 => Ok(format!("qb_messagebox1({})", args_str)),
                    2 => Ok(format!("qb_messagebox2({})", args_str)),
                    3 => Ok(format!("qb_messagebox({})", args_str)),
                    4 => Ok(format!("qb_messagebox4({})", args_str)),
                    _ => Ok(format!("qb_messagebox({})", args_str)),
                };
            }

            // Special case: _LOADFONT with different argument counts
            if upper_name == "_LOADFONT" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    2 => Ok(format!("qb_loadfont({})", args_str)),
                    3 => Ok(format!("qb_loadfont3({})", args_str)),
                    4 => Ok(format!("qb_loadfont4({})", args_str)),
                    _ => Ok(format!("qb_loadfont({})", args_str)),
                };
            }

            // Special case: _WIDTH without arguments uses current destination
            if upper_name == "_WIDTH" && args.is_empty() {
                return Ok("qb_gfx_image_width(0)".to_string());
            }

            // Special case: _HEIGHT without arguments uses current destination
            if upper_name == "_HEIGHT" && args.is_empty() {
                return Ok("qb_gfx_image_height(0)".to_string());
            }

            // Special case: _RGB32 with different argument counts
            // 3 args: _RGB32(r, g, b) -> qb__rgb32
            // 4 args: _RGB32(r, g, b, a) or _RGB32(gray, gray, gray, a) -> qb__rgb32_4
            if upper_name == "_RGB32" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    3 => Ok(format!("qb__rgb32({})", args_str)),
                    4 => Ok(format!("qb__rgb32_4({})", args_str)),
                    _ => Ok(format!("qb__rgb32({})", args_str)),
                };
            }

            // Special case: SCREEN function (reads screen char/attr)
            // 2 args: SCREEN(row, col) -> qb_screen
            // 3 args: SCREEN(row, col, attr_flag) -> qb_screen3
            if upper_name == "SCREEN" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    2 => Ok(format!("qb_screen({})", args_str)),
                    3 => Ok(format!("qb_screen3({})", args_str)),
                    _ => Ok(format!("qb_screen({})", args_str)),
                };
            }

            // Special case: STRING$ with 2 args - use numeric variant if second arg is not a string
            // STRING$(n, code) fills with ASCII code, STRING$(n, c$) fills with first char of string
            // For external runtime, qb_string_fill takes int32_t, so we use qb_string_fill_str for string args
            if upper_name == "STRING$" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_vec = args_code?;
                if !args[1].basic_type.is_string() {
                    return Ok(format!(
                        "qb_string_fill_code({}, {})",
                        args_vec[0], args_vec[1]
                    ));
                } else {
                    // For string argument, use qb_string_fill_str which extracts first char
                    return Ok(format!(
                        "qb_string_fill_str({}, {})",
                        args_vec[0], args_vec[1]
                    ));
                }
            }

            // Special case: _SAVEFILEDIALOG$ expects const char* arguments, not QbString*
            if upper_name == "_SAVEFILEDIALOG$" {
                let mut args_codes = Vec::new();
                for arg in args.iter() {
                    let arg_code = emit_expr(
                        arg,
                        no_shell,
                        variable_renames,
                        param_names,
                        byref_scalar_names,
                        byref_udt_names,
                        byref_string_names,
                        byref_string_basic_names,
                        dynamic_external_c_names,
                    )?;
                    // Convert QbString* to const char* using qb_string_data()
                    let arg_data = if arg_code.starts_with("qb_str_from_c(") {
                        // Fixed-length string - unwrap and use directly (it's already const char*)
                        unwrap_qb_str_from_c(&arg_code)
                    } else {
                        // Dynamic string - use qb_string_data() to get const char*
                        format!("qb_string_data({})", arg_code)
                    };
                    args_codes.push(arg_data);
                }
                let args_str = args_codes.join(", ");
                // qb_savefiledialog takes 4 const char* arguments: title, initial_dir, default_name, filter
                return Ok(format!("qb_savefiledialog({})", args_str));
            }

            // Special case: _OPENFILEDIALOG$ with different argument counts
            if upper_name == "_OPENFILEDIALOG$" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    2 => Ok(format!("qb_openfiledialog({})", args_str)),
                    3 => Ok(format!("qb_openfiledialog3({})", args_str)),
                    4 => Ok(format!("qb_openfiledialog4({})", args_str)),
                    5 => Ok(format!("qb_openfiledialog5({})", args_str)),
                    _ => Ok(format!("qb_openfiledialog({})", args_str)),
                };
            }

            // Special case: _SELECTFOLDERDIALOG$ with different argument counts
            // External runtime: qb_selectfolderdialog(const char* title, const char* initial_dir)
            // Inline runtime: qb_selectfolderdialog(qb_string* title) or qb_selectfolderdialog2(qb_string* title, qb_string* initial_dir)
            if upper_name == "_SELECTFOLDERDIALOG$" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_vec = args_code?;
                return match args_vec.len() {
                    0 => Ok("qb_selectfolderdialog(NULL, NULL)".to_string()), // No args - use NULL for both
                    1 => Ok(format!(
                        "qb_selectfolderdialog(qb_string_data({}), NULL)",
                        args_vec[0]
                    )), // 1 arg - convert to const char* and pass NULL for initial_dir
                    2 => {
                        // 2 args - convert both to const char*
                        Ok(format!(
                            "qb_selectfolderdialog(qb_string_data({}), qb_string_data({}))",
                            args_vec[0], args_vec[1]
                        ))
                    }
                    _ => Ok(format!(
                        "qb_selectfolderdialog(qb_string_data({}), NULL)",
                        args_vec[0]
                    )), // Default to first arg
                };
            }

            // Special case: _SCREENIMAGE - provide default 0,0,0,0 for full screen capture
            if upper_name == "_SCREENIMAGE" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    0 => Ok("qb_screenimage(0, 0, 0, 0)".to_string()),
                    4 => Ok(format!("qb_screenimage({})", args_str)),
                    _ => Ok(format!("qb_screenimage({})", args_str)),
                };
            }

            // Special case: COMMAND$ with argument uses qb_command_n
            if upper_name == "COMMAND$" && !args.is_empty() {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_command_n({})", args_str));
            }

            // Special case: ASC with 2 arguments (position) uses qb_asc2
            if upper_name == "ASC" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_asc2({})", args_str));
            }

            // Special case: STRIG with 2 arguments (controller) uses qb_strig2
            // QB64 extension: STRIG(button, controller) overrides implicit controller
            if upper_name == "STRIG" && args.len() == 2 {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_strig2({})", args_str));
            }

            // Special case: TIMER with argument (accuracy) uses qb_timer_n
            if upper_name == "TIMER" && !args.is_empty() {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return Ok(format!("qb_timer_n({})", args_str));
            }

            // Special case: _CONSOLE - use qb_console_get() for no args, qb_console(mode) with args
            if upper_name == "_CONSOLE" {
                if args.is_empty() {
                    return Ok("qb_console_get()".to_string());
                } else {
                    let args_code: Result<Vec<_>, _> = args
                        .iter()
                        .map(|e| {
                            emit_expr(
                                e,
                                no_shell,
                                variable_renames,
                                param_names,
                                byref_scalar_names,
                                byref_udt_names,
                                byref_string_names,
                                byref_string_basic_names,
                                dynamic_external_c_names,
                            )
                        })
                        .collect();
                    let args_str = args_code?.join(", ");
                    return Ok(format!("qb_console({})", args_str));
                }
            }

            // Special case: _MAPUNICODE - use different functions based on arg count
            if upper_name == "_MAPUNICODE" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    1 => Ok(format!("qb__mapunicode1({})", args_str)),
                    2 => Ok(format!("qb__mapunicode2({})", args_str)),
                    3 => Ok(format!("qb__mapunicode({})", args_str)),
                    _ => Ok(format!("qb__mapunicode({})", args_str)),
                };
            }

            // Special case: _PALETTECOLOR function form (GET)
            // When used as function: _PALETTECOLOR(attr%) or _PALETTECOLOR(attr%, handle&)
            // Always returns the palette color at the given index
            if upper_name == "_PALETTECOLOR" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_vec = args_code?;
                return match args.len() {
                    1 => Ok(format!("qb_palettecolor_get({}, 0)", args_vec[0])),
                    2 => Ok(format!(
                        "qb_palettecolor_get({}, {})",
                        args_vec[0], args_vec[1]
                    )),
                    // 3 args in function context shouldn't happen, but handle gracefully
                    _ => Ok(format!(
                        "qb_palettecolor({}, {}, {})",
                        args_vec.first().map(String::as_str).unwrap_or("0"),
                        args_vec.get(1).map(String::as_str).unwrap_or("0"),
                        args_vec.get(2).map(String::as_str).unwrap_or("0")
                    )),
                };
            }

            // Special case: _ICON - use different functions based on arg count
            if upper_name == "_ICON" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    0 => Ok("qb_icon()".to_string()),
                    1 => Ok(format!("qb_icon1({})", args_str)),
                    2 => Ok(format!("qb_icon2({})", args_str)),
                    _ => Ok(format!("qb_icon({})", args_str)),
                };
            }

            // Special case: _ACCEPTFILEDROP - use different functions based on arg count
            if upper_name == "_ACCEPTFILEDROP" {
                let args_code: Result<Vec<_>, _> = args
                    .iter()
                    .map(|e| {
                        emit_expr(
                            e,
                            no_shell,
                            variable_renames,
                            param_names,
                            byref_scalar_names,
                            byref_udt_names,
                            byref_string_names,
                            byref_string_basic_names,
                            dynamic_external_c_names,
                        )
                    })
                    .collect();
                let args_str = args_code?.join(", ");
                return match args.len() {
                    0 => Ok("qb_acceptfiledrop()".to_string()),
                    1 => Ok(format!("qb_acceptfiledrop1({})", args_str)),
                    _ => Ok(format!("qb_acceptfiledrop({})", args_str)),
                };
            }

            // Special case: SHELL function expects const char*, not QbString*
            // _SHELLHIDE uses inline runtime which takes QbString*, so no conversion needed
            // Must be checked before c_function_name() is called
            if upper_name == "SHELL" {
                // SHELL always takes 1 string argument
                let cmd_code = emit_expr(
                    &args[0],
                    no_shell,
                    variable_renames,
                    param_names,
                    byref_scalar_names,
                    byref_udt_names,
                    byref_string_names,
                    byref_string_basic_names,
                    dynamic_external_c_names,
                )?;
                // Convert QbString* to const char* using qb_string_data()
                // qb_shell expects const char*, so we need to extract the data pointer
                let cmd_data = if cmd_code.starts_with("qb_str_from_c(") {
                    // Fixed-length string - unwrap and use directly (it's already const char*)
                    unwrap_qb_str_from_c(&cmd_code)
                } else {
                    // Dynamic string - use qb_string_data() to get const char*
                    format!("qb_string_data({})", cmd_code)
                };
                return Ok(format!("qb_shell({})", cmd_data));
            }

            // Special case: _OPENHOST expects const char* connection string, not QbString*
            if upper_name == "_OPENHOST" {
                // _OPENHOST takes 1 string argument (connection string like "TCP/IP:port")
                let conn_code = emit_expr(
                    &args[0],
                    no_shell,
                    variable_renames,
                    param_names,
                    byref_scalar_names,
                    byref_udt_names,
                    byref_string_names,
                    byref_string_basic_names,
                    dynamic_external_c_names,
                )?;
                // Convert QbString* to const char* using qb_string_data()
                let conn_data = if conn_code.starts_with("qb_str_from_c(") {
                    // Fixed-length string - unwrap and use directly (it's already const char*)
                    unwrap_qb_str_from_c(&conn_code)
                } else {
                    // Dynamic string - use qb_string_data() to get const char*
                    format!("qb_string_data({})", conn_code)
                };
                return Ok(format!("qb_net_openhost({})", conn_data));
            }

            // Special case: _OPENCLIENT expects const char* connection string, not QbString*
            if upper_name == "_OPENCLIENT" {
                // _OPENCLIENT takes 1 string argument (connection string like "TCP/IP:port:host")
                let conn_code = emit_expr(
                    &args[0],
                    no_shell,
                    variable_renames,
                    param_names,
                    byref_scalar_names,
                    byref_udt_names,
                    byref_string_names,
                    byref_string_basic_names,
                    dynamic_external_c_names,
                )?;
                // Convert QbString* to const char* using qb_string_data()
                let conn_data = if conn_code.starts_with("qb_str_from_c(") {
                    // Fixed-length string - unwrap and use directly (it's already const char*)
                    unwrap_qb_str_from_c(&conn_code)
                } else {
                    // Dynamic string - use qb_string_data() to get const char*
                    format!("qb_string_data({})", conn_code)
                };
                return Ok(format!("qb_net_openclient({})", conn_data));
            }

            // Special case: qb_removestringenclosingpair_str expects QbString** (pointer to pointer)
            // This is a QB64pe-specific runtime function that takes addresses of QbString* variables
            if c_function_name(name) == "qb_removestringenclosingpair_str" {
                let mut args_codes = Vec::new();
                for arg in args {
                    let arg_code = emit_expr(
                        arg,
                        no_shell,
                        variable_renames,
                        param_names,
                        byref_scalar_names,
                        byref_udt_names,
                        byref_string_names,
                        byref_string_basic_names,
                        dynamic_external_c_names,
                    )?;
                    // Check if expression is an lvalue (can take address of)
                    let is_lvalue = matches!(
                        arg.kind,
                        TypedExprKind::Variable { .. }
                            | TypedExprKind::ArrayAccess { .. }
                            | TypedExprKind::FieldAccess { .. }
                    );

                    if is_lvalue {
                        // It's a variable/array/field - take address: &variable
                        // For const variables, we need to cast away const: (QbString**)&variable
                        // Check if the variable name suggests it might be const (common pattern)
                        let needs_const_cast = if let TypedExprKind::Variable(var_name) = &arg.kind
                        {
                            arg_code.contains("const ")
                                || (var_name.contains("METACOMMAND")
                                    && var_name.contains("ENCLOSING"))
                        } else {
                            arg_code.contains("const ")
                        };

                        if needs_const_cast {
                            let arg_code_clean = arg_code.replace("const ", "");
                            args_codes.push(format!("(QbString**)&{}", arg_code_clean));
                        } else {
                            args_codes.push(format!("&{}", arg_code));
                        }
                    } else {
                        // It's a function call or expression - use statement-expression to create temporary
                        let temp_name = format!("_tmp_qbstr_{}", args_codes.len());
                        args_codes.push(format!(
                            "({{ QbString* {} = {}; &{}; }})",
                            temp_name, arg_code, temp_name
                        ));
                    }
                }
                let args_str = args_codes.join(", ");
                return Ok(format!("qb_removestringenclosingpair_str({})", args_str));
            }

            // Special case: qb_hasstringenclosingpair_int_int expects QbString** (pointer to pointer)
            // Same handling as qb_removestringenclosingpair_str
            if c_function_name(name) == "qb_hasstringenclosingpair_int_int" {
                let mut args_codes = Vec::new();
                for arg in args {
                    let arg_code = emit_expr(
                        arg,
                        no_shell,
                        variable_renames,
                        param_names,
                        byref_scalar_names,
                        byref_udt_names,
                        byref_string_names,
                        byref_string_basic_names,
                        dynamic_external_c_names,
                    )?;
                    // Check if expression is an lvalue (can take address of)
                    let is_lvalue = matches!(
                        arg.kind,
                        TypedExprKind::Variable { .. }
                            | TypedExprKind::ArrayAccess { .. }
                            | TypedExprKind::FieldAccess { .. }
                    );

                    if is_lvalue {
                        // It's a variable/array/field - take address: &variable
                        // For const variables, we need to cast away const: (QbString**)&variable
                        let needs_const_cast = if let TypedExprKind::Variable(var_name) = &arg.kind
                        {
                            arg_code.contains("const ")
                                || (var_name.contains("METACOMMAND")
                                    && var_name.contains("ENCLOSING"))
                        } else {
                            arg_code.contains("const ")
                        };

                        if needs_const_cast {
                            let arg_code_clean = arg_code.replace("const ", "");
                            args_codes.push(format!("(QbString**)&{}", arg_code_clean));
                        } else {
                            args_codes.push(format!("&{}", arg_code));
                        }
                    } else {
                        // It's a function call or expression - use statement-expression to create temporary
                        let temp_name = format!("_tmp_qbstr_{}", args_codes.len());
                        args_codes.push(format!(
                            "({{ QbString* {} = {}; &{}; }})",
                            temp_name, arg_code, temp_name
                        ));
                    }
                }
                let args_str = args_codes.join(", ");
                return Ok(format!("qb_hasstringenclosingpair_int_int({})", args_str));
            }

            let c_name = c_function_name(name);

            // Special case: RND without arguments defaults to RND(1)
            if name == "RND" && args.is_empty() {
                return Ok(format!("{}(1.0f)", c_name));
            }

            // For user-defined functions with BYREF parameters, add & for lvalue args
            if !params.is_empty() {
                let mut args_codes = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let arg_code = emit_expr_internal(
                        arg,
                        no_shell,
                        variable_renames,
                        param_names,
                        byref_scalar_names,
                        byref_udt_names,
                        byref_string_names,
                        byref_string_basic_names,
                        dynamic_external_c_names,
                        wrap_string_temps,
                    )?;

                    // Check if the argument is a UDT variable that's already a pointer
                    // (from being a BYREF UDT parameter in the current function)
                    let arg_is_udt_pointer = matches!(&arg.kind, TypedExprKind::Variable(name)
                    if {
                        let c_arg = c_identifier(name).to_lowercase();
                        byref_udt_names.iter().any(|s| s.to_lowercase() == c_arg)
                    });

                    // Check if this parameter is byref (and not an array)
                    // For UDT arguments that are already pointers, we don't need to add &
                    let is_byref = !arg_is_udt_pointer
                        && params
                            .get(i)
                            .map(|p| !p.by_val && !p.is_array)
                            .unwrap_or(false);

                    if is_byref {
                        // Check if expression is an lvalue (can take address of)
                        // Note: Built-in constants like _TRUE, _FALSE are Variables in the AST
                        // but expand to C macros, so they're not true lvalues
                        let is_builtin_const = matches!(&arg.kind, TypedExprKind::Variable(name)
                            if name.starts_with('_') && name.chars().all(|c| c.is_uppercase() || c == '_'));

                        // Fixed-length string variables are wrapped with qb_str_from_c(),
                        // making them function call results (not lvalues)
                        let is_fixed_string = matches!(arg.basic_type, BasicType::FixedString(_));

                        let is_lvalue = !is_builtin_const
                            && !is_fixed_string
                            && matches!(
                                arg.kind,
                                TypedExprKind::Variable { .. }
                                    | TypedExprKind::ArrayAccess { .. }
                                    | TypedExprKind::FieldAccess { .. }
                            );

                        // Check if argument type matches parameter type
                        // In BASIC, passing a LONG to a function expecting INTEGER% creates
                        // an implicit temporary. The function gets a pointer to the temp.
                        let param_type = params.get(i).map(|p| &p.basic_type);
                        let types_match =
                            param_type.map(|pt| pt == &arg.basic_type).unwrap_or(true);

                        if is_lvalue && types_match {
                            // For simple variable names, use &variable (no parentheses needed)
                            // For complex expressions, use &(expr)
                            // Special case: If the variable is const (like HASHFLAG_*, DEPENDENCY_*),
                            // we need to cast away const to match function signatures that expect non-const pointers
                            let needs_parens = arg_code.contains(' ')
                                || arg_code.contains('(')
                                || arg_code.contains('[');

                            // Check if this looks like a const variable (all uppercase with underscores)
                            // Common patterns: HASHFLAG_*, DEPENDENCY_*
                            let is_likely_const = !needs_parens
                                && arg_code
                                    .chars()
                                    .all(|c| c.is_uppercase() || c == '_' || c.is_ascii_digit())
                                && (arg_code.starts_with("HASHFLAG_")
                                    || arg_code.starts_with("DEPENDENCY_")
                                    || arg_code.contains("_FLAG")
                                    || arg_code.contains("_DEPENDENCY"));

                            if is_likely_const {
                                // Cast away const: (int32_t*)&CONSTANT
                                let c_ty = param_type
                                    .map(c_type)
                                    .unwrap_or_else(|| "int32_t".to_string());
                                args_codes.push(format!("({}*)&{}", c_ty, arg_code));
                            } else if needs_parens {
                                args_codes.push(format!("&({})", arg_code));
                            } else {
                                args_codes.push(format!("&{}", arg_code));
                            }
                        } else {
                            // Non-lvalue expression OR type mismatch - use C compound literal
                            // Format: &(type){expr} creates a temporary that can be addressed
                            let c_ty = param_type
                                .map(c_type)
                                .unwrap_or_else(|| "int32_t".to_string());

                            // Use C99 compound literals for all types including pointer types.
                            // Compound literals have block-scope lifetime, so they remain valid
                            // for the entire function call.
                            // Format: &(type){expr}
                            if c_ty.ends_with('*') {
                                // For pointer types (including QbString*), use compound literal
                                if needs_fixed_string_conversion(arg) {
                                    // Fixed-length string - convert first
                                    let mut inner_code = unwrap_qb_str_from_c(&arg_code);
                                    while inner_code.starts_with("qb_str_from_c(") {
                                        inner_code = unwrap_qb_str_from_c(&inner_code);
                                    }
                                    args_codes.push(format!(
                                        "&({}){{qb_str_from_c({})}}",
                                        c_ty, inner_code
                                    ));
                                } else {
                                    // Dynamic string or other pointer type
                                    args_codes.push(format!("&({}){{{}}}", c_ty, arg_code));
                                }
                            } else {
                                // Non-pointer type - can use compound literal
                                if needs_fixed_string_conversion(arg) {
                                    // Fixed-length string - convert first
                                    let mut inner_code = unwrap_qb_str_from_c(&arg_code);
                                    while inner_code.starts_with("qb_str_from_c(") {
                                        inner_code = unwrap_qb_str_from_c(&inner_code);
                                    }
                                    args_codes.push(format!("&({}){{{}}}", c_ty, inner_code));
                                } else {
                                    // Cast value to parameter type to handle type mismatches
                                    args_codes.push(format!("&({}){{{}}}", c_ty, arg_code));
                                }
                            }
                        }
                    } else {
                        // BYVAL parameter
                        // Fixed-length strings need conversion to qb_string*
                        if needs_fixed_string_conversion(arg) {
                            // Check if already wrapped to avoid double wrapping
                            // Unwrap multiple levels if needed
                            let mut inner_code = unwrap_qb_str_from_c(&arg_code);
                            while inner_code.starts_with("qb_str_from_c(") {
                                inner_code = unwrap_qb_str_from_c(&inner_code);
                            }
                            args_codes.push(format!("qb_str_from_c({})", inner_code));
                        } else {
                            args_codes.push(arg_code);
                        }
                    }
                }
                let args_str = args_codes.join(", ");
                let call_code = format!("{}({})", c_name, args_str);
                // For external runtime, wrap string-returning user-defined functions
                if wrap_string_temps && expr.basic_type.is_string() {
                    return Ok(format!("qbs_tmp_register({})", call_code));
                } else {
                    return Ok(call_code);
                }
            }

            // Built-in functions - all args are BYVAL
            // Fixed-length string arguments need conversion to qb_string*
            let args_code: Result<Vec<_>, _> = args
                .iter()
                .map(|arg| {
                    let code = emit_expr_internal(
                        arg,
                        no_shell,
                        variable_renames,
                        param_names,
                        byref_scalar_names,
                        byref_udt_names,
                        byref_string_names,
                        byref_string_basic_names,
                        dynamic_external_c_names,
                        wrap_string_temps,
                    )?;
                    // Check if this is a fixed-length string that needs conversion
                    // This includes FieldAccess of fixed-length string fields
                    if needs_fixed_string_conversion(arg) {
                        // Check if already wrapped to avoid double wrapping
                        let inner_code = unwrap_qb_str_from_c(&code);
                        Ok(format!("qb_str_from_c({})", inner_code))
                    } else {
                        Ok(code)
                    }
                })
                .collect();
            let args_str = args_code?.join(", ");

            let call_code = format!("{}({})", c_name, args_str);
            // For external runtime, wrap string-returning function calls with qbs_tmp_register
            if wrap_string_temps && expr.basic_type.is_string() {
                Ok(format!("qbs_tmp_register({})", call_code))
            } else {
                Ok(call_code)
            }
        }

        TypedExprKind::ArrayAccess {
            name,
            indices,
            dimensions,
        } => {
            let array_code = emit_array_access(
                name,
                indices,
                dimensions,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )?;
            // Fixed-length string array elements need conversion to qb_string*
            if matches!(expr.basic_type, BasicType::FixedString(_)) {
                Ok(format!("qb_str_from_c({})", array_code))
            } else {
                Ok(array_code)
            }
        }

        TypedExprKind::Convert { expr, to_type } => {
            let inner_code = emit_expr(
                expr,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )?;

            // Handle string-to-string conversion (no-op)
            if expr.basic_type.is_string() && to_type.is_string() {
                return Ok(inner_code);
            }

            // Handle conversion to fixed-length string
            // In C, we can't cast to array types. The actual copying happens
            // at the assignment point. In expression context, just pass the value.
            if matches!(to_type, BasicType::FixedString(_)) {
                return Ok(inner_code);
            }

            let c_ty = c_type(to_type);
            Ok(format!("(({})({}))", c_ty, inner_code))
        }

        TypedExprKind::FieldAccess { object, field } => {
            let obj_code = emit_expr(
                object,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )?;
            let c_field = c_identifier(field);
            // UDT parameters (BYREF/BYVAL) are pointers in C; use -> for field access
            let sep = if let TypedExprKind::Variable(name) = &object.kind {
                if byref_udt_names.contains(&c_identifier(name)) {
                    "->"
                } else {
                    "."
                }
            } else {
                "."
            };
            let field_access = format!("{}{}{}", obj_code, sep, c_field);
            // Fixed-length string fields need conversion to qb_string*
            if matches!(expr.basic_type, BasicType::FixedString(_)) {
                Ok(format!("qb_str_from_c({})", field_access))
            } else {
                Ok(field_access)
            }
        }

        TypedExprKind::ArrayRef { name, .. } => {
            // Array reference: pass the array pointer to a procedure.
            // In C, the array name decays to a pointer when passed.
            Ok(c_identifier(name))
        }

        TypedExprKind::ExternalFunctionCall {
            c_name,
            args,
            params,
            ..
        } => {
            // DECLARE DYNAMIC LIBRARY: call through function pointer qb_dyn_<c_name>
            let call_name = if dynamic_external_c_names.contains(c_name) {
                format!("qb_dyn_{}", c_name)
            } else {
                c_name.clone()
            };
            emit_external_function_call(
                &call_name,
                args,
                params,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )
        }

        TypedExprKind::ProcPtr { wrapper_name, .. } => {
            // Return the address of the C wrapper function as an intptr_t
            Ok(format!("((intptr_t)&{})", wrapper_name))
        }

        TypedExprKind::CvFunc { target_type, value } => {
            let value_code = emit_expr(
                value,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )?;
            // Use the appropriate qb_cv* function based on target type
            let func = match target_type {
                BasicType::Integer => "qb_cvi",
                BasicType::Long => "qb_cvl",
                BasicType::Single => "qb_cvs",
                BasicType::Double => "qb_cvd",
                BasicType::Integer64 => "qb_cvq",
                _ => "qb_cvi", // Default fallback
            };
            Ok(format!("{}({})", func, value_code))
        }

        TypedExprKind::MkDollarFunc { source_type, value } => {
            let value_code = emit_expr(
                value,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )?;
            // Use the appropriate qb_mk*$ function based on source type
            let func = match source_type {
                BasicType::Integer => "qb_mki",
                BasicType::Long => "qb_mkl",
                BasicType::Single => "qb_mks",
                BasicType::Double => "qb_mkd",
                BasicType::Integer64 => "qb_mkq",
                _ => "qb_mki", // Default fallback
            };
            Ok(format!("{}({})", func, value_code))
        }

        TypedExprKind::CastFunc { target_type, value } => {
            let value_code = emit_expr(
                value,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )?;
            // Explicit cast to the target C type
            let c_ty = c_type(target_type);
            Ok(format!("(({})({})", c_ty, value_code))
        }

        TypedExprKind::ValWithType { value, target_type } => {
            let value_code = emit_expr(
                value,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )?;
            // VAL with type specifier uses specific conversion functions
            // that parse the string and return the specified type
            let func = match target_type {
                BasicType::Integer64 => "qb_val_int64",
                BasicType::UnsignedInteger64 => "qb_val_uint64",
                BasicType::Long => "qb_val_long",
                BasicType::UnsignedLong => "qb_val_ulong",
                BasicType::Integer => "qb_val_int",
                BasicType::UnsignedInteger => "qb_val_uint",
                BasicType::Double => "qb_val_double",
                BasicType::Single => "qb_val_float",
                _ => "qb_val", // Default VAL function
            };
            Ok(format!("{}({})", func, value_code))
        }

        TypedExprKind::MemGetTyped {
            mem,
            offset,
            target_type,
        } => {
            let mem_code = emit_expr(
                mem,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )?;
            let offset_code = emit_expr(
                offset,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )?;
            // _MEMGET reads raw bytes from memory at the given offset
            // and interprets them as the specified type.
            // Generated code: *((type*)((char*)(mem).offset + (offset)))
            let c_ty = c_type(target_type);
            Ok(format!(
                "(*(({c_ty}*)((char*)({mem_code}).offset + ({offset_code}))))"
            ))
        }
    }
}

/// Emits an external function call with proper argument marshalling.
///
/// External functions (from DECLARE LIBRARY) need special handling:
/// - STRING arguments are converted to char* via qb_string_data()
/// - The C function name is used directly (not prefixed with qb_)
#[allow(clippy::too_many_arguments)]
fn emit_external_function_call(
    c_name: &str,
    args: &[TypedExpr],
    params: &[crate::semantic::typed_ir::ExternalParamInfo],
    no_shell: bool,
    variable_renames: &std::collections::HashMap<String, String>,
    param_names: &std::collections::HashSet<String>,
    byref_scalar_names: &std::collections::HashSet<String>,
    byref_udt_names: &std::collections::HashSet<String>,
    byref_string_names: &[String],
    byref_string_basic_names: &[String],
    dynamic_external_c_names: &std::collections::HashSet<String>,
) -> Result<String, CodeGenError> {
    let mut marshalled_args = Vec::new();

    for (i, arg) in args.iter().enumerate() {
        let arg_code = emit_expr(
            arg,
            no_shell,
            variable_renames,
            param_names,
            byref_scalar_names,
            byref_udt_names,
            byref_string_names,
            byref_string_basic_names,
            dynamic_external_c_names,
        )?;

        // Check if this argument needs string marshalling
        let needs_marshalling = if i < params.len() {
            // String arg to non-string C param needs marshalling
            arg.basic_type.is_string() && !params[i].typ.is_string()
        } else {
            false
        };

        if needs_marshalling || arg.basic_type.is_string() {
            // Convert qb_string* to const char* for C interop
            // qb_string_data() returns the internal char* buffer
            marshalled_args.push(format!("qb_string_data({})", arg_code));
        } else {
            marshalled_args.push(arg_code);
        }
    }

    Ok(format!("{}({})", c_name, marshalled_args.join(", ")))
}

/// Emits a binary expression.
#[allow(clippy::too_many_arguments)]
fn emit_binary_expr(
    left: &TypedExpr,
    op: &BinaryOp,
    right: &TypedExpr,
    no_shell: bool,
    variable_renames: &std::collections::HashMap<String, String>,
    param_names: &std::collections::HashSet<String>,
    byref_scalar_names: &std::collections::HashSet<String>,
    byref_udt_names: &std::collections::HashSet<String>,
    byref_string_names: &[String],
    byref_string_basic_names: &[String],
    dynamic_external_c_names: &std::collections::HashSet<String>,
    wrap_string_temps: bool,
) -> Result<String, CodeGenError> {
    let left_code = emit_expr_internal(
        left,
        no_shell,
        variable_renames,
        param_names,
        byref_scalar_names,
        byref_udt_names,
        byref_string_names,
        byref_string_basic_names,
        dynamic_external_c_names,
        wrap_string_temps,
    )?;
    let right_code = emit_expr_internal(
        right,
        no_shell,
        variable_renames,
        param_names,
        byref_scalar_names,
        byref_udt_names,
        byref_string_names,
        byref_string_basic_names,
        dynamic_external_c_names,
        wrap_string_temps,
    )?;

    // Handle string concatenation specially
    if left.basic_type.is_string() && matches!(op, BinaryOp::Add) {
        // Only wrap fixed-length strings (char arrays) for concatenation
        // Dynamic strings (qb_string*) are already the correct type
        let left_wrapped = if matches!(left.basic_type, BasicType::FixedString(_)) {
            // Check if already wrapped to avoid double wrapping
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
        // For external runtime, wrap concatenation result with qbs_tmp_register
        if wrap_string_temps {
            return Ok(format!("qbs_tmp_register({})", concat_code));
        } else {
            return Ok(concat_code);
        }
    }

    // Handle string comparisons
    if left.basic_type.is_string() {
        // Only wrap fixed-length strings (char arrays) for comparison
        // Dynamic strings (qb_string*) are already the correct type
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
        BinaryOp::Power => {
            // Use pow() from math.h for exponentiation
            return Ok(format!("pow({}, {})", left_code, right_code));
        }
        BinaryOp::Modulo => {
            // C's % is integer-only. BASIC MOD with floats truncates toward zero then mod.
            // Cast when either operand is float or Unknown (Unknown may resolve to float at runtime).
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

    // Comparison and short-circuit boolean operators in BASIC return -1 for TRUE and 0 for FALSE.
    // C comparison/logical operators return 1 for true and 0 for false.
    // The NOT operator in BASIC is bitwise (~), so:
    //   NOT TRUE = NOT -1 = 0 = FALSE  (works correctly)
    //   NOT FALSE = NOT 0 = -1 = TRUE  (works correctly)
    // But if we use C's 1 for true:
    //   NOT 1 = ~1 = -2 (which is truthy in C!) - WRONG!
    // We must convert boolean results: -(a > b) gives -1 or 0.
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
        // Negate to convert C bool (0/1) to BASIC bool (0/-1)
        return Ok(format!("-({} {} {})", left_code, op_str, right_code));
    }

    Ok(format!("({} {} {})", left_code, op_str, right_code))
}

/// Emits C code for array access.
#[allow(clippy::too_many_arguments)]
fn emit_array_access(
    name: &str,
    indices: &[TypedExpr],
    dimensions: &[TypedArrayDimension],
    no_shell: bool,
    variable_renames: &std::collections::HashMap<String, String>,
    param_names: &std::collections::HashSet<String>,
    byref_scalar_names: &std::collections::HashSet<String>,
    byref_udt_names: &std::collections::HashSet<String>,
    byref_string_names: &[String],
    byref_string_basic_names: &[String],
    dynamic_external_c_names: &std::collections::HashSet<String>,
) -> Result<String, CodeGenError> {
    let mut c_name = c_identifier(name);

    // UDT field array: "w.arr" -> emit w.arr[i-lower], not w_arr[i]
    let base_prefix = if name.contains('.') {
        let parts: Vec<&str> = name.splitn(2, '.').collect();
        if parts.len() == 2 {
            let base = parts[0];
            let field = parts[1];
            let base_c = c_identifier(base);
            let base_c = variable_renames.get(&base_c).cloned().unwrap_or(base_c);
            // Nested fields: "player.arr" -> "player.arr" in C (each part sanitized)
            let field_c: String = field
                .split('.')
                .map(c_identifier)
                .collect::<Vec<_>>()
                .join(".");
            format!("{}.{}", base_c, field_c)
        } else {
            c_name.clone()
        }
    } else {
        c_name.clone()
    };

    // Check if this array was renamed due to parameter shadowing.
    // When a local array shadows a function parameter (e.g., DIM args(5) AS ParseNum
    // when args is a parameter), the array is renamed (e.g., args -> args_local)
    // and the rename is stored in variable_renames.
    // Note: This is different from scalar/array dual namespace, where arrays keep
    // their original name and scalars are renamed. Here, the array itself is renamed.
    //
    // IMPORTANT: We must NOT apply renames that map array names to scalar names
    // (i.e., renames ending with "_scalar"). These are for scalar/array dual namespace
    // and should only be applied to scalar variable references, not array accesses.
    // Only apply renames for parameter shadowing (e.g., args -> args_local).
    if !name.contains('.')
        && let Some(renamed) = variable_renames.get(&c_name)
        && !renamed.ends_with("_scalar")
    {
        c_name = renamed.clone();
    }
    // Use base_prefix for UDT field arrays (w.arr), renamed c_name for global/local arrays
    let final_prefix = if name.contains('.') {
        base_prefix
    } else {
        c_name
    };

    // Collect index codes, casting to int64_t to ensure integer subscripts
    // (C requires integer array subscripts, but BASIC allows any numeric type)
    // We always cast to ensure safety, even for seemingly integer types, because
    // function calls like VAL() may return double even if wrapped in Convert
    let indices_code: Result<Vec<_>, _> = indices
        .iter()
        .map(|idx| {
            let code = emit_expr(
                idx,
                no_shell,
                variable_renames,
                param_names,
                byref_scalar_names,
                byref_udt_names,
                byref_string_names,
                byref_string_basic_names,
                dynamic_external_c_names,
            )?;
            // Cast to int64_t to ensure integer subscript
            // This handles VAL(), floating-point expressions, and implicit conversions
            Ok(format!("(int64_t)({})", code))
        })
        .collect();
    let indices_code = indices_code?;

    if dimensions.is_empty() || indices_code.len() == 1 {
        // 1D array - simple index (subtracting lower bound)
        if let Some(dim) = dimensions.first() {
            Ok(format!(
                "{}[{} - {}]",
                final_prefix, indices_code[0], dim.lower
            ))
        } else {
            // No dimension info, use index as-is (shouldn't happen)
            Ok(format!("{}[{}]", final_prefix, indices_code[0]))
        }
    } else if name.contains('.') {
        // Multi-dimensional UDT field: C has type arr[s0][s1], emit base.field[i0-l0][i1-l1]
        let brackets: String = dimensions
            .iter()
            .zip(indices_code.iter())
            .map(|(dim, idx)| format!("[{} - {}]", idx, dim.lower))
            .collect();
        Ok(format!("{}{}", final_prefix, brackets))
    } else {
        // Multi-dimensional global array - calculate linear index
        // For 2D: arr(i, j) -> arr[(i - lower1) * size2 + (j - lower2)]
        let mut linear_parts = Vec::new();

        for (i, (idx, dim)) in indices_code.iter().zip(dimensions.iter()).enumerate() {
            let adjusted = format!("({} - {})", idx, dim.lower);

            if i < dimensions.len() - 1 {
                let stride: i64 = dimensions[i + 1..]
                    .iter()
                    .map(|d| d.upper - d.lower + 1)
                    .product();
                linear_parts.push(format!("{} * {}", adjusted, stride));
            } else {
                linear_parts.push(adjusted);
            }
        }

        Ok(format!("{}[{}]", final_prefix, linear_parts.join(" + ")))
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
        BinaryOp::AndAlso => "&&".to_string(), // Short-circuit AND (QB64)
        BinaryOp::OrElse => "||".to_string(), // Short-circuit OR (QB64)
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
///
/// Note: We negate the result to convert C bool (0/1) to BASIC bool (0/-1).
/// This is critical for NOT operator compatibility (NOT in BASIC is bitwise ~).
///
/// The arguments should already be properly wrapped (fixed-length strings wrapped
/// with qb_str_from_c(), dynamic strings as qb_string*). This function just
/// generates the comparison code.
fn emit_string_comparison(left: &str, right: &str, op: &BinaryOp) -> Result<String, CodeGenError> {
    // Arguments are already properly typed (qb_string*), just use them directly
    let cmp = format!("qb_string_compare({}, {})", left, right);
    // Negate to convert C bool (0/1) to BASIC bool (0/-1)
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

        // Hyperbolic functions
        "_SINH" => "sinh".to_string(),
        "_COSH" => "cosh".to_string(),
        "_TANH" => "tanh".to_string(),
        "_ASINH" => "asinh".to_string(),
        "_ACOSH" => "acosh".to_string(),
        "_ATANH" => "atanh".to_string(),

        // Reciprocal trig functions
        "_SEC" => "qb_sec".to_string(),
        "_CSC" => "qb_csc".to_string(),
        "_COT" => "qb_cot".to_string(),

        // Hyperbolic reciprocals
        "_SECH" => "qb_sech".to_string(),
        "_CSCH" => "qb_csch".to_string(),
        "_COTH" => "qb_coth".to_string(),

        // Inverse reciprocal trig
        "_ARCSEC" => "qb_arcsec".to_string(),
        "_ARCCSC" => "qb_arccsc".to_string(),
        "_ARCCOT" => "qb_arccot".to_string(),

        // Inverse hyperbolic reciprocals
        "_ARCSECH" => "qb_arcsech".to_string(),
        "_ARCCSCH" => "qb_arccsch".to_string(),
        "_ARCCOTH" => "qb_arccoth".to_string(),

        // Angle conversions (degrees <-> radians)
        "_D2R" => "qb_d2r".to_string(),
        "_R2D" => "qb_r2d".to_string(),

        // Gradian conversions
        "_D2G" => "qb_d2g".to_string(),
        "_G2D" => "qb_g2d".to_string(),
        "_G2R" => "qb_g2r".to_string(),
        "_R2G" => "qb_r2g".to_string(),

        // Negate
        "_NEGATE" => "qb_negate".to_string(),

        // String comparison
        "_STRCMP" => "qb_strcmp".to_string(),
        "_STRICMP" => "qb_stricmp".to_string(),

        // Bitwise operations
        "_SHL" => "qb_shl".to_string(),
        "_SHR" => "qb_shr".to_string(),
        "_ROL" => "qb_rol".to_string(),
        "_ROR" => "qb_ror".to_string(),
        "_READBIT" => "qb_readbit".to_string(),
        "_SETBIT" => "qb_setbit".to_string(),
        "_RESETBIT" => "qb_resetbit".to_string(),
        "_TOGGLEBIT" => "qb_togglebit".to_string(),

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

        // File I/O functions
        "EOF" => "qb_eof".to_string(),
        "LOF" => "qb_lof".to_string(),
        "LOC" => "qb_loc".to_string(),
        "FREEFILE" => "qb_freefile".to_string(),

        // Keyboard input functions
        "INKEY$" => "qb_inkey".to_string(),
        "INPUT$" => "qb_input_chars".to_string(),

        // Error handling functions
        "ERR" => "qb_err_code".to_string(),
        "ERL" => "qb_err_line".to_string(),
        "_ERRORLINE" => "qb_errorline".to_string(),
        "_ERRORMESSAGE$" => "qb_errormessage".to_string(),

        // Version information
        "VERSION$" => "qb_version".to_string(),

        // Utility functions
        "_COMMANDCOUNT" => "qb_commandcount".to_string(),
        "_ENVIRONCOUNT" => "qb_environcount".to_string(),

        // Environment functions
        "ENVIRON$" => "qb_environ".to_string(),
        "COMMAND$" => "qb_command".to_string(),
        "_CWD$" => "qb_cwd".to_string(),
        "_OS$" => "qb_os".to_string(),
        "_STARTDIR$" => "qb_startdir".to_string(),

        // Classic BASIC date/time functions
        "TIMER" => "qb_timer".to_string(),
        "DATE$" => "qb_date".to_string(),
        "TIME$" => "qb_time".to_string(),
        // TRIM$ is a common BASIC extension (not in original QBasic).
        // Both TRIM$ and _TRIM$ map to the same implementation.
        "TRIM$" => "qb_trim".to_string(),

        // Print formatting functions
        "TAB" => "qb_tab".to_string(),
        "SPC" => "qb_spc".to_string(),
        "POS" => "qb_pos".to_string(),
        "CSRLIN" => "qb_csrlin".to_string(),

        // QB64 keyboard extensions
        "_KEYHIT" => "qb_keyhit".to_string(),
        "_KEYDOWN" => "qb_keydown".to_string(),
        "_CINP" => "qb_cinp".to_string(),

        // Lock key state functions
        "_CAPSLOCK" => "qb_capslock".to_string(),
        "_NUMLOCK" => "qb_numlock".to_string(),
        "_SCROLLLOCK" => "qb_scrolllock".to_string(),

        // Phase 2: String enhancements
        "_INSTRREV" => "qb_instrrev".to_string(),
        // _TRIM$ is the QB64-specific name; same as TRIM$ above
        "_TRIM$" => "qb_trim".to_string(),
        "MKI$" => "qb_mki".to_string(),
        "MKL$" => "qb_mkl".to_string(),
        "MKS$" => "qb_mks".to_string(),
        "MKD$" => "qb_mkd".to_string(),
        "CVI" => "qb_cvi".to_string(),
        "CVL" => "qb_cvl".to_string(),
        "CVS" => "qb_cvs".to_string(),
        "CVD" => "qb_cvd".to_string(),

        // Phase 2: QB64 Date/Time
        "_DATE$" => "qb_date64".to_string(),
        "_TIME$" => "qb_time64".to_string(),

        // Phase 2: Memory operations
        "_MEMNEW" => "qb_memnew".to_string(),
        "_MEMFREE" => "qb_memfree".to_string(),
        "_MEMGET" => "qb_memget".to_string(),
        "_MEMPUT" => "qb_memput".to_string(),
        "_MEMCOPY" => "qb_memcopy".to_string(),
        "_MEMFILL" => "qb_memfill".to_string(),
        "_OFFSET" => "qb_offset".to_string(),
        "_MEM" => "qb_mem".to_string(),

        // Phase 5: System Integration
        "_FILEEXISTS" => "qb_file_exists".to_string(),
        "_DIREXISTS" => "qb_dir_exists".to_string(),
        "_DIR$" => "qb_dir".to_string(),
        "_READFILE$" => "qb_readfile".to_string(),

        // Phase 5: Mouse Input
        "_MOUSEX" => "qb_mouse_x".to_string(),
        "_MOUSEY" => "qb_mouse_y".to_string(),
        "_MOUSEBUTTON" => "qb_mouse_button".to_string(),
        "_MOUSEINPUT" => "qb_mouse_input".to_string(),
        "_MOUSEMOVEMENTX" => "qb_mouse_movement_x".to_string(),
        "_MOUSEMOVEMENTY" => "qb_mouse_movement_y".to_string(),
        "_MOUSEWHEEL" => "qb_mouse_wheel".to_string(),

        // Phase 5: Clipboard
        "_CLIPBOARD$" => "qb_clipboard_get".to_string(),

        // Font support
        "_LOADFONT" => "qb_loadfont".to_string(),
        "_FONTHEIGHT" => "qb_fontheight".to_string(),
        "_FONTWIDTH" => "qb_fontwidth".to_string(),
        "_PRINTWIDTH" => "qb_printwidth".to_string(),
        "_FONT" => "qb_font".to_string(),
        "_FREEFONT" => "qb_freefont".to_string(),

        // Desktop/Window functions
        "_DESKTOPWIDTH" => "qb_desktopwidth".to_string(),
        "_DESKTOPHEIGHT" => "qb_desktopheight".to_string(),
        "_SCREENX" => "qb_screenx".to_string(),
        "_SCREENY" => "qb_screeny".to_string(),
        "_TITLE$" => "qb_title_get".to_string(),
        "_WINDOWHANDLE" => "qb_windowhandle".to_string(),
        "_WINDOWHASFOCUS" => "qb_windowhasfocus".to_string(),

        // Window control functions
        "_SCREENMOVE" => "qb_screenmove".to_string(),
        "_SCREENHIDE" => "qb_screenhide".to_string(),
        "_SCREENSHOW" => "qb_screenshow".to_string(),
        "_FULLSCREEN" => "qb_fullscreen".to_string(),
        "_SCREENCLICK" => "qb_screenclick".to_string(),
        "_SCREENPRINT" => "qb_screenprint".to_string(),
        "_SCREENIMAGE" => "qb_screenimage".to_string(),

        // Dialog boxes
        "_MESSAGEBOX" => "qb_messagebox".to_string(),
        "_INPUTBOX$" => "qb_inputbox".to_string(),
        "_OPENFILEDIALOG$" => "qb_openfiledialog".to_string(),
        "_SAVEFILEDIALOG$" => "qb_savefiledialog".to_string(),
        "_SELECTFOLDERDIALOG$" => "qb_selectfolderdialog".to_string(),

        // Binary/number-to-string conversion
        "HEX$" => "qb_hex".to_string(),
        "OCT$" => "qb_oct".to_string(),
        "_BIN$" => "qb_bin".to_string(),
        "_TOSTR$" => "qb_tostr".to_string(),

        // Inline conditional
        "_IIF" => "qb_iif".to_string(),
        "_IIF$" => "qb_iif_str".to_string(),

        // Phase 5: Networking
        "_OPENHOST" => "qb_net_openhost".to_string(),
        "_OPENCONNECTION" => "qb_net_openconnection".to_string(),
        "_OPENCLIENT" => "qb_net_openclient".to_string(),
        "_CONNECTED" => "qb_net_connected".to_string(),

        // Image buffer functions
        "_NEWIMAGE" => "qb_gfx_newimage".to_string(),
        "_LOADIMAGE" => "qb_gfx_loadimage".to_string(),
        "_COPYIMAGE" => "qb_gfx_copyimage".to_string(),
        "_WIDTH" => "qb_gfx_image_width".to_string(),
        "_HEIGHT" => "qb_gfx_image_height".to_string(),

        // Coordinate mapping and pixel query
        "PMAP" => "qb_gfx_pmap".to_string(),
        "POINT" => "qb_gfx_point".to_string(),

        // Event handling functions (QB4.5)
        "KEY" => "qb_key_status".to_string(), // KEY(n) function - check key trap status

        // Joystick functions (QB4.5)
        "STICK" => "qb_stick".to_string(),
        "STRIG" => "qb_strig".to_string(),

        // Memory functions (QB4.5)
        "FRE" => "qb_fre".to_string(),
        "PEEK" => "qb_peek".to_string(),

        // Port I/O functions (QB4.5)
        "INP" => "qb_inp".to_string(),

        // Light pen function (QB4.5 legacy)
        "PEN" => "qb_pen".to_string(),

        // Serial I/O functions (QB4.5)
        "ERDEV" => "qb_erdev".to_string(),
        "ERDEV$" => "qb_erdev_str".to_string(),
        "IOCTL$" => "qb_ioctl_str".to_string(),

        // Legacy BASIC functions
        "LPOS" => "qb_lpos".to_string(),
        "VARPTR" => "qb_varptr".to_string(),
        "VARPTR$" => "qb_varptr_str".to_string(),
        "VARSEG" => "qb_varseg".to_string(),
        "SADD" => "qb_sadd".to_string(),
        "FILEATTR" => "qb_fileattr".to_string(),

        // Microsoft Binary Format conversions
        "CVSMBF" => "qb_cvsmbf".to_string(),
        "CVDMBF" => "qb_cvdmbf".to_string(),
        "MKSMBF$" => "qb_mksmbf".to_string(),
        "MKDMBF$" => "qb_mkdmbf".to_string(),

        // QB64 Extension Functions (Session 031+)
        // Color component extraction
        "_RED" => "qb_red".to_string(),
        "_GREEN" => "qb_green".to_string(),
        "_BLUE" => "qb_blue".to_string(),
        "_ALPHA" => "qb_alpha".to_string(),
        "_RED32" => "qb_red32".to_string(),
        "_GREEN32" => "qb_green32".to_string(),
        "_BLUE32" => "qb_blue32".to_string(),
        "_ALPHA32" => "qb_alpha32".to_string(),

        // Screen/pixel info
        "_PIXELSIZE" => "qb_pixelsize".to_string(),
        "_SCREENEXISTS" => "qb_screenexists".to_string(),
        "_FPS" => "qb_fps".to_string(),

        // Path functions
        "_FULLPATH$" => "qb_fullpath".to_string(),

        // Hash and encoding functions
        "_CRC32" => "qb_crc32".to_string(),
        "_MD5$" => "qb_md5".to_string(),
        "_ADLER32" => "qb_adler32".to_string(),
        "_BASE64ENCODE$" => "qb_base64encode".to_string(),
        "_BASE64DECODE$" => "qb_base64decode".to_string(),
        "_ENCODEURL$" => "qb_encodeurl".to_string(),
        "_DECODEURL$" => "qb_decodeurl".to_string(),
        "_DEFLATE$" => "qb_deflate".to_string(),
        "_INFLATE$" => "qb_inflate".to_string(),

        // Memory extended
        "_MEMEXISTS" => "qb_memexists".to_string(),

        // Color defaults
        "_DEFAULTCOLOR" => "qb_defaultcolor".to_string(),
        "_BACKGROUNDCOLOR" => "qb_backgroundcolor".to_string(),

        // Exit state
        "_EXIT" => "qb_exit_state".to_string(),

        // Short-circuit operators
        "_ANDALSO" => "qb_andalso".to_string(),
        "_ORELSE" => "qb_orelse".to_string(),

        // Timer
        "_FREETIMER" => "qb_freetimer".to_string(),

        // Console
        "_CONSOLEINPUT" => "qb_consoleinput".to_string(),
        "_ECHO" => "qb_echo".to_string(),

        // Mouse extended
        "_MOUSEHIDDEN" => "qb_mousehidden".to_string(),

        // Clipboard extended
        "_CLIPBOARDIMAGE" => "qb_clipboardimage".to_string(),

        // Device input (gamepad/joystick)
        "_DEVICES" => "qb_devices".to_string(),
        "_DEVICE$" => "qb_device_name".to_string(),
        "_DEVICEINPUT" => "qb_deviceinput".to_string(),
        "_LASTAXIS" => "qb_lastaxis".to_string(),
        "_LASTBUTTON" => "qb_lastbutton".to_string(),
        "_LASTWHEEL" => "qb_lastwheel".to_string(),
        "_AXIS" => "qb_axis".to_string(),
        "_BUTTON" => "qb_button".to_string(),
        "_BUTTONCHANGE" => "qb_buttonchange".to_string(),
        "_WHEEL" => "qb_wheel".to_string(),

        // Drag and drop
        "_TOTALDROPPEDFILES" => "qb_totaldroppedfiles".to_string(),
        "_DROPPEDFILE" => "qb_droppedfile".to_string(),
        "_DROPPEDFILE$" => "qb_droppedfile_str".to_string(),

        // Resize events
        "_RESIZE" => "qb_resize".to_string(),
        "_RESIZEWIDTH" => "qb_resizewidth".to_string(),
        "_RESIZEHEIGHT" => "qb_resizeheight".to_string(),
        "_SCALEDWIDTH" => "qb_scaledwidth".to_string(),
        "_SCALEDHEIGHT" => "qb_scaledheight".to_string(),

        // Dialogs extended
        "_COLORCHOOSERDIALOG" => "qb_colorchooserdialog".to_string(),
        "_NOTIFYPOPUP" => "qb_notifypopup".to_string(),

        // Sound extended
        "_SNDRAWDONE" => "qb_sndrawdone".to_string(),
        "_SNDOPENRAW" => "qb_sndopenraw".to_string(),
        "_SNDRAWLEN" => "qb_sndrawlen".to_string(),

        // QB64 Extension Functions (Session 032+)
        // Error handling extended
        "_INCLERRORFILE$" => "qb_inclerrorfile".to_string(),
        "_INCLERRORLINE" => "qb_inclerrorline".to_string(),

        // Utility functions
        "_STATUSCODE" => "qb_statuscode".to_string(),

        // Networking extended
        "_CONNECTIONADDRESS" => "qb_connectionaddress".to_string(),
        "_CONNECTIONADDRESS$" => "qb_connectionaddress_str".to_string(),

        // HSB color functions
        "_HSB32" => "qb_hsb32".to_string(),
        "_HSBA32" => "qb_hsba32".to_string(),
        "_HUE32" => "qb_hue32".to_string(),
        "_SATURATION32" => "qb_saturation32".to_string(),
        "_BRIGHTNESS32" => "qb_brightness32".to_string(),

        // Memory extended
        "_MEMELEMENT" => "qb_memelement".to_string(),
        "_MEMIMAGE" => "qb_memimage".to_string(),
        "_MEMSOUND" => "qb_memsound".to_string(),

        // Sound extended
        "_SNDNEW" => "qb_sndnew".to_string(),

        // File I/O extended
        "_FILES$" => "qb_files_str".to_string(),

        // Device input extended
        "_LASTHANDLER" => "qb_lasthandler".to_string(),

        // Unicode font functions
        "_UCHARPOS" => "qb_ucharpos".to_string(),
        "_UFONTHEIGHT" => "qb_ufontheight".to_string(),
        "_ULINESPACING" => "qb_ulinespacing".to_string(),
        "_UPRINTWIDTH" => "qb_uprintwidth".to_string(),

        // QB64 Extension Functions (Session 033+)
        // File I/O extended
        "_EMBEDDED$" => "qb_embedded".to_string(),

        // Graphics rendering mode functions
        "_SMOOTH" => "qb_smooth".to_string(),
        "_SMOOTHSHRUNK" => "qb_smoothshrunk".to_string(),
        "_SMOOTHSTRETCHED" => "qb_smoothstretched".to_string(),
        "_HARDWARE" => "qb_hardware".to_string(),
        "_HARDWARE1" => "qb_hardware1".to_string(),
        "_SOFTWARE" => "qb_software".to_string(),

        // Graphics direction
        "_ANTICLOCKWISE" => "qb_anticlockwise".to_string(),
        "_CLOCKWISE" => "qb_clockwise".to_string(),

        // Print mode constants
        "_KEEPBACKGROUND" => "qb_keepbackground".to_string(),
        "_FILLBACKGROUND" => "qb_fillbackground".to_string(),
        "_ONLYBACKGROUND" => "qb_onlybackground".to_string(),

        // Alignment
        "_MIDDLE" => "qb_middle".to_string(),

        // Auto display
        "_AUTO" => "qb_auto".to_string(),

        // QB64 Extension Functions (Session 034+)
        // Graphics keyword constants
        "_CLIP" => "qb_clip".to_string(),
        "_STRETCH" => "qb_stretch".to_string(),
        "_SEAMLESS" => "qb_seamless".to_string(),
        "_SQUAREPIXELS" => "qb_squarepixels".to_string(),
        "_BEHIND" => "qb_behind".to_string(),

        // Type/mode keywords
        "_ALL" => "qb_all".to_string(),
        "_BLINK" => "qb_blink".to_string(),
        "_OFF" => "qb_off".to_string(),
        "_ONLY" => "qb_only".to_string(),

        // Sound/network keywords (Session 034)
        "_WAVE" => "qb_wave".to_string(),
        "_DONTWAIT" => "qb_dontwait".to_string(),

        // Console title (Session 034)
        "_CONSOLETITLE$" => "qb_consoletitle_str".to_string(),
        "_CONSOLE" => "qb_console".to_string(),

        // Environment functions (Session 034)
        "_SHELLHIDE" => "qb_shellhide".to_string(),

        // Graphics info functions (Session 034)
        "_GLCOMPAT" => "qb_glcompat".to_string(),

        // Debug/assert functions (Session 034)
        "_ASSERT" => "qb_assert".to_string(),
        "_ASSERTERROR$" => "qb_asserterror".to_string(),

        // Display extended functions (Session 034)
        "_FULLSCREENSMOOTH" => "qb_fullscreensmooth".to_string(),
        "_ALLOWFULLSCREEN" => "qb_allowfullscreen".to_string(),
        "_DISPLAYWIDTH" => "qb_displaywidth".to_string(),
        "_DISPLAYHEIGHT" => "qb_displayheight".to_string(),

        // QB64 Extension Functions (Session 035+)
        // Console extended
        "_SCREENBUFFER" => "qb_screenbuffer".to_string(),
        "_SCINKEY$" => "qb_scinkey".to_string(),

        // Date/time extended
        "_YEAR" => "qb_year".to_string(),
        "_MONTH" => "qb_month".to_string(),
        "_DAY" => "qb_day".to_string(),
        "_WEEKDAY" => "qb_weekday".to_string(),
        "_HOUR" => "qb_hour".to_string(),
        "_MINUTE" => "qb_minute".to_string(),
        "_SECOND" => "qb_second".to_string(),

        // Screen functions
        "_SCREENICON" => "qb_screenicon".to_string(),

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

/// Extracts the inner expression from a `qb_str_from_c()` wrapper.
///
/// If the code starts with `qb_str_from_c(`, finds the matching closing
/// parenthesis and returns the inner expression. Otherwise, returns the code unchanged.
///
/// # Arguments
///
/// * `code` - The C code that may be wrapped with `qb_str_from_c()`
///
/// # Returns
///
/// The inner expression without the `qb_str_from_c()` wrapper, or the original code if not wrapped.
pub(crate) fn unwrap_qb_str_from_c(code: &str) -> String {
    if let Some(stripped) = code.strip_prefix("qb_str_from_c(") {
        // Find the matching closing parenthesis
        let mut depth = 0;
        let mut end_pos = 0;
        for (i, ch) in stripped.char_indices() {
            match ch {
                '(' => depth += 1,
                ')' => {
                    if depth == 0 {
                        end_pos = i;
                        break;
                    }
                    depth -= 1;
                }
                _ => {}
            }
        }
        stripped[..end_pos].to_string()
    } else {
        code.to_string()
    }
}

/// Checks if an expression represents a fixed-length string field that needs
/// conversion to qb_string* for use with built-in string functions.
///
/// Returns true for:
/// - FieldAccess with FixedString type
/// - Convert from FixedString wrapping a FieldAccess
/// - ArrayAccess of fixed-length string elements
/// - Variable with FixedString type
pub(crate) fn needs_fixed_string_conversion(expr: &TypedExpr) -> bool {
    match &expr.kind {
        // Direct field access of a fixed-length string
        TypedExprKind::FieldAccess { .. } => {
            matches!(expr.basic_type, BasicType::FixedString(_))
        }
        // Array access of fixed-length string elements
        TypedExprKind::ArrayAccess { .. } => {
            matches!(expr.basic_type, BasicType::FixedString(_))
        }
        // Convert from FixedString - check the inner expression
        TypedExprKind::Convert { expr: inner, .. } => {
            matches!(inner.basic_type, BasicType::FixedString(_))
                && matches!(
                    inner.kind,
                    TypedExprKind::FieldAccess { .. } | TypedExprKind::ArrayAccess { .. }
                )
        }
        _ => false,
    }
}

/// Safely extracts `const char*` from a string expression for use with C functions
/// that expect `const char*` (like file operations, system calls, etc.).
///
/// This function handles both `QbString*` (from `emit_expr()`) and fixed-length
/// strings (char arrays) by ensuring proper conversion.
///
/// # Arguments
///
/// * `expr` - The string expression to extract data from
/// * `expr_code` - The C code for the expression (from `emit_expr()`)
/// * `runtime_mode` - Whether we're using inline or external runtime
///
/// # Returns
///
/// C code that evaluates to `const char*` pointing to the string data.
pub(super) fn emit_string_data_access(
    expr: &TypedExpr,
    expr_code: &str,
    runtime_mode: &super::RuntimeMode,
) -> String {
    // Check if this is a fixed-length string that needs conversion
    // Even though emit_expr() should convert them, we double-check here for safety
    let needs_conversion = matches!(expr.basic_type, BasicType::FixedString(_))
        && !expr_code.starts_with("qb_str_from_c(")
        && !expr_code.starts_with("qb_string_");

    let qb_string_expr = if needs_conversion {
        format!("qb_str_from_c({})", expr_code)
    } else {
        expr_code.to_string()
    };

    runtime_mode.string_data_access(&qb_string_expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::semantic::types::BasicType;
    use std::collections::{HashMap, HashSet};

    #[test]
    fn test_emit_integer_literal() {
        let expr = TypedExpr::integer(42, Span::new(0, 2, 1));
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting integer literal should succeed");
        assert_eq!(result, "42LL");
    }

    #[test]
    fn test_emit_string_literal() {
        let expr = TypedExpr::string("Hello".to_string(), Span::new(0, 7, 1));
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting string literal should succeed");
        assert_eq!(result, "qb_string_new(\"Hello\")");
    }

    #[test]
    fn test_emit_power_operator_constant_folded() {
        // When both operands are constant, the expression is folded
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(2, Span::new(0, 1, 1))),
                op: BinaryOp::Power,
                right: Box::new(TypedExpr::integer(3, Span::new(4, 5, 1))),
            },
            BasicType::Double,
            Span::new(0, 5, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting constant-folded power operator should succeed");
        assert_eq!(result, "8LL"); // 2^3 = 8, folded at compile time
    }

    #[test]
    fn test_emit_power_operator_with_variable() {
        // When an operand is a variable, pow() is used
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::new(
                    TypedExprKind::Variable("x".to_string()),
                    BasicType::Long,
                    Span::new(0, 1, 1),
                )),
                op: BinaryOp::Power,
                right: Box::new(TypedExpr::integer(3, Span::new(4, 5, 1))),
            },
            BasicType::Double,
            Span::new(0, 5, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting power operator with variable should succeed");
        assert_eq!(result, "pow(x, 3LL)");
    }

    #[test]
    fn test_emit_eqv_operator_constant_folded() {
        // When both operands are constant, the expression is folded
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(5, Span::new(0, 1, 1))),
                op: BinaryOp::Eqv,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7, 1))),
            },
            BasicType::Long,
            Span::new(0, 7, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting constant-folded EQV operator should succeed");
        // EQV: !(5 XOR 3) = !6 = -7 (bitwise NOT)
        assert_eq!(result, "-7LL");
    }

    #[test]
    fn test_emit_eqv_operator_with_variable() {
        // When an operand is a variable, the C expression is emitted
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::new(
                    TypedExprKind::Variable("x".to_string()),
                    BasicType::Long,
                    Span::new(0, 1, 1),
                )),
                op: BinaryOp::Eqv,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7, 1))),
            },
            BasicType::Long,
            Span::new(0, 7, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting EQV operator with variable should succeed");
        assert_eq!(result, "(~(x ^ 3LL))");
    }

    #[test]
    fn test_emit_imp_operator_constant_folded() {
        // When both operands are constant, the expression is folded
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::integer(5, Span::new(0, 1, 1))),
                op: BinaryOp::Imp,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7, 1))),
            },
            BasicType::Long,
            Span::new(0, 7, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting constant-folded IMP operator should succeed");
        // IMP: (!5) OR 3 = -6 OR 3 = -5
        assert_eq!(result, "-5LL");
    }

    #[test]
    fn test_emit_imp_operator_with_variable() {
        // When an operand is a variable, the C expression is emitted
        let expr = TypedExpr::new(
            TypedExprKind::Binary {
                left: Box::new(TypedExpr::new(
                    TypedExprKind::Variable("x".to_string()),
                    BasicType::Long,
                    Span::new(0, 1, 1),
                )),
                op: BinaryOp::Imp,
                right: Box::new(TypedExpr::integer(3, Span::new(6, 7, 1))),
            },
            BasicType::Long,
            Span::new(0, 7, 1),
        );
        let result = emit_expr(
            &expr,
            false,
            &HashMap::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &[] as &[String],
            &[] as &[String],
            &HashSet::new(),
        )
        .expect("emitting IMP operator with variable should succeed");
        assert_eq!(result, "((~x) | 3LL)");
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
