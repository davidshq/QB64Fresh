//! Literal and variable expression code generation.
//!
//! Handles integer, float, and string literals, and variable references
//! (including BYREF resolution and variable renaming).

use crate::codegen::error::CodeGenError;
use crate::semantic::types::BasicType;

use super::super::types::c_identifier;
use super::helpers::escape_string;

/// Emits C code for an integer literal.
pub(super) fn emit_integer_literal(n: i64) -> String {
    format!("{}LL", n)
}

/// Emits C code for a float literal, including NaN and infinity.
pub(super) fn emit_float_literal(n: f64) -> String {
    if n.is_nan() {
        "(0.0/0.0)".to_string()
    } else if n.is_infinite() {
        if n.is_sign_positive() {
            "(1.0/0.0)".to_string()
        } else {
            "(-1.0/0.0)".to_string()
        }
    } else {
        format!("{:.17}", n)
    }
}

/// Emits C code for a string literal, optionally wrapping with qbs_tmp_register.
pub(super) fn emit_string_literal(s: &str, wrap_string_temps: bool) -> String {
    let escaped = escape_string(s);
    let code = format!("qb_string_new(\"{}\")", escaped);
    if wrap_string_temps {
        format!("qbs_tmp_register({})", code)
    } else {
        code
    }
}

/// Emits C code for a variable reference.
///
/// Handles BYREF resolution, parameter shadowing, and fixed-length string wrapping.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_variable(
    name: &str,
    basic_type: &BasicType,
    variable_renames: &std::collections::HashMap<String, String>,
    param_names: &std::collections::HashSet<String>,
    byref_scalar_names: &std::collections::HashSet<String>,
    byref_string_names: &[String],
    byref_string_basic_names: &[String],
) -> Result<String, CodeGenError> {
    let mut c_name = c_identifier(name);
    let mut resolved_byref_param = byref_string_names.contains(&c_name);
    if !resolved_byref_param
        && !byref_string_basic_names.is_empty()
        && byref_string_basic_names.len() == byref_string_names.len()
        && basic_type.is_string()
    {
        if let Some(i) = byref_string_basic_names.iter().position(|basic| {
            basic.eq_ignore_ascii_case(name)
                || (name.len() <= basic.len()
                    && basic.to_lowercase().starts_with(&name.to_lowercase()))
        }) {
            c_name = byref_string_names[i].clone();
            resolved_byref_param = true;
        }
    }
    if let Some(renamed) = variable_renames.get(&c_name) {
        if !param_names.contains(&c_name) && !resolved_byref_param {
            c_name = renamed.clone();
        }
    }
    let needs_deref = byref_scalar_names.contains(&c_name);
    let is_byref_string = needs_deref && basic_type.is_string();
    if matches!(basic_type, BasicType::FixedString(_)) {
        Ok(format!("qb_str_from_c({})", c_name))
    } else if needs_deref && !is_byref_string {
        Ok(format!("*{}", c_name))
    } else {
        Ok(c_name)
    }
}
