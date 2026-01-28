//! Type mapping utilities for QB64Fresh C backend.
//!
//! This module provides functions for mapping BASIC types to their C equivalents
//! and handling BASIC identifiers that may contain special characters.
//!
//! # Type Mapping
//!
//! | BASIC Type       | C Type        |
//! |------------------|---------------|
//! | INTEGER          | int16_t       |
//! | LONG             | int32_t       |
//! | _INTEGER64       | int64_t       |
//! | SINGLE           | float         |
//! | DOUBLE           | double        |
//! | STRING           | qb_string*    |
//! | _UNSIGNED types  | uint*_t       |
//!
//! # Identifier Conversion
//!
//! BASIC allows type suffix characters in identifiers (`$`, `%`, `&`, etc.)
//! which are invalid in C. These are converted to descriptive suffixes.

use std::collections::{HashMap, HashSet};

use crate::semantic::types::BasicType;

/// Built-in constants and runtime variables that should never be redeclared.
///
/// These identifiers are either:
/// - Defined as `#define` macros in the runtime header
/// - Reserved for internal runtime use
/// - Used as dummy variables for `LEN()` type sizing
///
/// When collecting implicit variable declarations, these names must be skipped
/// to avoid redeclaration errors in the generated C code.
pub(super) const RESERVED_IDENTIFIERS: &[&str] = &[
    // Boolean constants (defined as macros)
    "_TRUE",
    "_FALSE",
    // Comparison result constants (defined as macros)
    "_EQUAL",
    "_GREATER",
    "_LESS",
    // String constant macros
    "_STR_EMPTY",
    "_STR_CRLF",
    "_STR_LF",
    "_STR_CR",
    "_CHR_QUOTE",
    "_CHR_HT",
    "_CHR_LF",
    // Dummy variables for LEN() type sizing (defined in runtime)
    "dummy",
    "dummy_int_int",
    "dummy_int",
    "dummy_lng_lng",
    "dummy_sng",
    "dummy_dbl",
    "dummy_dbl_dbl",
    "dummy_int_lng",
];

/// Adds all reserved identifiers to a HashSet.
///
/// This should be called when building the set of "already declared" variables
/// before collecting implicit declarations.
///
/// # Example
///
/// ```ignore
/// let mut declared_vars: HashSet<String> = HashSet::new();
/// add_reserved_identifiers(&mut declared_vars);
/// // Now declared_vars contains all reserved names
/// ```
pub(super) fn add_reserved_identifiers(set: &mut HashSet<String>) {
    for &name in RESERVED_IDENTIFIERS {
        set.insert(name.to_string());
    }
}

/// Declares a scalar variable if not already declared.
///
/// Returns `true` if the variable was newly declared, `false` if it already existed.
///
/// # Generated declarations
///
/// | Type | Declaration |
/// |------|-------------|
/// | `String` | `qb_string* name = NULL;` |
/// | `FixedString(N)` | `char name[N+1] = "";` |
/// | `UserDefined` | `type name = {0};` |
/// | Others | `type name = default_init;` |
pub(super) fn declare_scalar_var(
    name: &str,
    basic_type: &BasicType,
    declared_vars: &mut HashSet<String>,
    decls: &mut Vec<String>,
    variable_renames: Option<&mut HashMap<String, String>>,
    array_names: Option<&HashSet<String>>,
) -> bool {
    let c_name = c_identifier(name);
    // Check if this name is already declared
    // In BASIC's dual namespace, a scalar and array can coexist with the same name
    // If the name is already declared AND it's an array, we need to rename the scalar
    // If it's already a scalar, we shouldn't declare it again (return false)
    let final_name = if declared_vars.contains(&c_name) {
        // Check if this collision is with an array (which we should rename) or another scalar (which we shouldn't declare)
        if let Some(arrays) = array_names {
            if arrays.contains(&c_name) {
                // Collision with an array - rename the scalar
                let renamed = format!("{}_scalar", c_name);
                // Track the rename so variable references use the correct name
                if let Some(renames) = variable_renames {
                    renames.insert(c_name.clone(), renamed.clone());
                }
                renamed
            } else {
                // Collision with another scalar - don't declare again
                return false;
            }
        } else {
            // No array info available - assume it's an array collision and rename
            // (safer than assuming it's a scalar collision)
            let renamed = format!("{}_scalar", c_name);
            if let Some(renames) = variable_renames {
                renames.insert(c_name.clone(), renamed.clone());
            }
            renamed
        }
    } else {
        c_name.clone()
    };

    if declared_vars.contains(&final_name) {
        return false;
    }

    let decl = match basic_type {
        BasicType::FixedString(len) => format!("char {}[{}] = \"\";", final_name, len + 1),
        BasicType::String => format!("QbString* {} = NULL;", final_name),
        BasicType::UserDefined(_) => {
            let c_ty = c_type(basic_type);
            format!("{} {} = {{0}};", c_ty, final_name)
        }
        _ => {
            let c_ty = c_type(basic_type);
            let init = default_init(basic_type);
            format!("{} {} = {};", c_ty, final_name, init)
        }
    };

    decls.push(decl);
    declared_vars.insert(final_name);
    true
}

/// Declares an array pointer variable if not already declared.
///
/// Arrays in C are declared as pointers initialized to NULL.
/// They will be allocated with malloc/realloc at runtime.
///
/// The `is_global` parameter determines how the size tracking variable is declared:
/// - Global arrays: `size_t name_sz__ = 0;` (shared across all functions)
/// - Local arrays: `static size_t name_sz__ = 0;` (persists across function calls)
///
/// Returns `true` if the variable was newly declared, `false` if it already existed.
///
/// # Generated declarations
///
/// | Element Type | Declaration |
/// |--------------|-------------|
/// | `FixedString(N)` | `char (*name)[N+1] = NULL;` |
/// | Others | `type* name = NULL;` |
pub(super) fn declare_array_var(
    name: &str,
    element_type: &BasicType,
    declared_vars: &mut HashSet<String>,
    decls: &mut Vec<String>,
    is_global: bool,
) -> bool {
    let c_name = c_identifier(name);
    if declared_vars.contains(&c_name) {
        return false;
    }

    let decl = match element_type {
        BasicType::FixedString(len) => {
            // Array of fixed-length strings: char (*name)[len+1]
            format!("char (*{})[{}] = NULL;", c_name, len + 1)
        }
        _ => {
            let c_ty = c_type(element_type);
            format!("{}* {} = NULL;", c_ty, c_name)
        }
    };

    decls.push(decl);
    // Emit a size tracking variable for REDIM _PRESERVE support.
    // - For global arrays: must be global so all functions see the same size
    // - For local arrays: must be static so size persists across function calls
    if is_global {
        decls.push(format!("size_t {}_sz__ = 0;", c_name));
    } else {
        decls.push(format!("static size_t {}_sz__ = 0;", c_name));
    }
    declared_vars.insert(c_name);
    true
}

/// Maps a BASIC type to its C representation.
///
/// This function handles all BASIC types including unsigned variants,
/// fixed-length strings, user-defined types, and arrays.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(c_type(&BasicType::Integer), "int16_t");
/// assert_eq!(c_type(&BasicType::String), "QbString*");
/// assert_eq!(c_type(&BasicType::FixedString(10)), "char[11]");
/// ```
pub(super) fn c_type(basic_type: &BasicType) -> String {
    match basic_type {
        BasicType::Bit => "int8_t".to_string(),
        BasicType::Byte => "int8_t".to_string(),
        BasicType::Integer => "int16_t".to_string(),
        BasicType::Long => "int32_t".to_string(),
        BasicType::Integer64 => "int64_t".to_string(),
        BasicType::Offset => "intptr_t".to_string(),
        BasicType::Single => "float".to_string(),
        BasicType::Double => "double".to_string(),
        BasicType::Float => "long double".to_string(),
        BasicType::String => "QbString*".to_string(),
        BasicType::FixedString(n) => format!("char[{}]", n + 1), // +1 for null terminator
        BasicType::UnsignedBit => "uint8_t".to_string(),
        BasicType::UnsignedByte => "uint8_t".to_string(),
        BasicType::UnsignedInteger => "uint16_t".to_string(),
        BasicType::UnsignedLong => "uint32_t".to_string(),
        BasicType::UnsignedInteger64 => "uint64_t".to_string(),
        // Prefix with qbt_ to avoid collision with variable names
        BasicType::UserDefined(name) => format!("qbt_{}", name),
        BasicType::Array { element_type, .. } => {
            format!("{}*", c_type(element_type))
        }
        BasicType::Mem => "qb_mem".to_string(),
        BasicType::Void => "void".to_string(),
        BasicType::Unknown => "int32_t".to_string(), // Default to LONG
    }
}

/// Returns the default initializer for a BASIC type in C.
///
/// This ensures variables are properly initialized with type-appropriate
/// default values (empty strings, 0.0 for floats, 0 for integers).
///
/// # Examples
///
/// ```ignore
/// assert_eq!(default_init(&BasicType::String), "qb_string_new(\"\")");
/// assert_eq!(default_init(&BasicType::Double), "0.0");
/// assert_eq!(default_init(&BasicType::Integer), "0");
/// ```
pub(super) fn default_init(basic_type: &BasicType) -> String {
    match basic_type {
        BasicType::String => "qb_string_new(\"\")".to_string(),
        BasicType::FixedString(_) => "\"\"".to_string(),
        BasicType::Single | BasicType::Double | BasicType::Float => "0.0".to_string(),
        BasicType::Mem => "{0}".to_string(), // Zero-initialized struct
        BasicType::UserDefined(_) => "{0}".to_string(), // User-defined TYPE - zero-initialized
        _ => "0".to_string(),
    }
}

/// Converts a BASIC identifier to a valid C identifier.
///
/// BASIC allows type suffixes in variable names that are invalid in C:
/// - `$` (string) → `_str`
/// - `%` (integer) → `_int`
/// - `&` (long) → `_lng`
/// - `!` (single) → `_sng`
/// - `#` (double) → `_dbl`
/// - `` ` `` (bit) → `_bit`
///
/// # Examples
///
/// ```ignore
/// assert_eq!(c_identifier("name$"), "name_str");
/// assert_eq!(c_identifier("count%"), "count_int");
/// assert_eq!(c_identifier("myVar"), "myVar");
/// ```
pub(super) fn c_identifier(name: &str) -> String {
    let result = name
        .replace('$', "_str")
        .replace('%', "_int")
        .replace('&', "_lng")
        .replace('!', "_sng")
        .replace('#', "_dbl")
        .replace('`', "_bit")
        .replace('.', "_") // QB64 allows dots in variable names; C doesn't
        .replace('~', "_u"); // Unsigned type prefix

    // Handle C reserved words and standard library conflicts by appending underscore
    match result.to_lowercase().as_str() {
        // C keywords
        "default" | "switch" | "case" | "break" | "continue" | "return" | "void" | "int"
        | "char" | "float" | "double" | "long" | "short" | "unsigned" | "signed" | "const"
        | "static" | "extern" | "register" | "volatile" | "auto" | "struct" | "union" | "enum"
        | "typedef" | "sizeof" | "goto" | "if" | "else" | "for" | "while" | "do"
        // C standard library functions from <ctype.h>
        | "isalpha" | "isdigit" | "isalnum" | "isspace" | "isupper" | "islower" | "isprint"
        | "iscntrl" | "ispunct" | "isxdigit" | "isgraph" | "isblank" | "toupper" | "tolower"
        // C standard library functions from <stdlib.h>
        | "malloc" | "calloc" | "realloc" | "free" | "exit" | "abort" | "atoi" | "atol"
        | "atof" | "strtol" | "strtod" | "rand" | "srand" | "qsort" | "bsearch" | "abs"
        | "labs" | "div" | "ldiv" | "getenv" | "system"
        // C standard library functions from <string.h>
        | "memcpy" | "memmove" | "memset" | "memcmp" | "strlen" | "strcpy" | "strncpy"
        | "strcat" | "strncat" | "strcmp" | "strncmp" | "strchr" | "strrchr" | "strstr"
        | "strtok" | "sprintf" | "snprintf"
        // C standard library functions from <stdio.h>
        | "printf" | "fprintf" | "scanf" | "sscanf" | "fopen" | "fclose" | "fread" | "fwrite"
        | "fgets" | "fputs" | "fgetc" | "fputc" | "fseek" | "ftell" | "rewind" | "feof"
        | "ferror" | "clearerr" | "remove" | "rename" | "tmpfile" | "tmpnam"
        // C standard library functions from <math.h>
        | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "atan2" | "sinh" | "cosh"
        | "tanh" | "exp" | "log" | "log10" | "pow" | "sqrt" | "ceil" | "floor" | "fabs"
        | "fmod" | "modf" | "frexp" | "ldexp" => {
            format!("{}_", result)
        }
        _ => result,
    }
}

/// Infers BASIC type from variable name suffix.
///
/// In BASIC, variable names can end with a type suffix character that
/// determines the variable's type. This function extracts that information.
///
/// | Suffix | Type     |
/// |--------|----------|
/// | `$`    | String   |
/// | `%`    | Integer  |
/// | `&`    | Long     |
/// | `!`    | Single   |
/// | `#`    | Double   |
/// | `` ` `` | Bit     |
/// | (none) | Single   |
///
/// # Examples
///
/// ```ignore
/// assert_eq!(infer_type_from_suffix("name$"), BasicType::String);
/// assert_eq!(infer_type_from_suffix("count%"), BasicType::Integer);
/// assert_eq!(infer_type_from_suffix("myVar"), BasicType::Single); // default
/// ```
pub(super) fn infer_type_from_suffix(name: &str) -> BasicType {
    if name.ends_with('$') {
        BasicType::String
    } else if name.ends_with('%') {
        BasicType::Integer
    } else if name.ends_with('&') {
        BasicType::Long
    } else if name.ends_with('!') {
        BasicType::Single
    } else if name.ends_with('#') {
        BasicType::Double
    } else if name.ends_with('`') {
        BasicType::Bit
    } else {
        // Default to Single (QB64's default without DEFINT/etc.)
        BasicType::Single
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_c_type_mapping() {
        assert_eq!(c_type(&BasicType::Integer), "int16_t");
        assert_eq!(c_type(&BasicType::Long), "int32_t");
        assert_eq!(c_type(&BasicType::Integer64), "int64_t");
        assert_eq!(c_type(&BasicType::Double), "double");
        assert_eq!(c_type(&BasicType::String), "QbString*");
        assert_eq!(c_type(&BasicType::FixedString(10)), "char[11]");
        assert_eq!(c_type(&BasicType::UnsignedInteger), "uint16_t");
        assert_eq!(
            c_type(&BasicType::UserDefined("MyType".to_string())),
            "qbt_MyType"
        );
    }

    #[test]
    fn test_default_init() {
        assert_eq!(default_init(&BasicType::String), "qb_string_new(\"\")");
        assert_eq!(default_init(&BasicType::Double), "0.0");
        assert_eq!(default_init(&BasicType::Single), "0.0");
        assert_eq!(default_init(&BasicType::Integer), "0");
        assert_eq!(default_init(&BasicType::Long), "0");
    }

    #[test]
    fn test_c_identifier() {
        assert_eq!(c_identifier("name$"), "name_str");
        assert_eq!(c_identifier("count%"), "count_int");
        assert_eq!(c_identifier("total&"), "total_lng");
        assert_eq!(c_identifier("value!"), "value_sng");
        assert_eq!(c_identifier("amount#"), "amount_dbl");
        assert_eq!(c_identifier("flag`"), "flag_bit");
        assert_eq!(c_identifier("myVar"), "myVar");
    }

    #[test]
    fn test_infer_type_from_suffix() {
        assert_eq!(infer_type_from_suffix("name$"), BasicType::String);
        assert_eq!(infer_type_from_suffix("count%"), BasicType::Integer);
        assert_eq!(infer_type_from_suffix("total&"), BasicType::Long);
        assert_eq!(infer_type_from_suffix("value!"), BasicType::Single);
        assert_eq!(infer_type_from_suffix("amount#"), BasicType::Double);
        assert_eq!(infer_type_from_suffix("flag`"), BasicType::Bit);
        assert_eq!(infer_type_from_suffix("myVar"), BasicType::Single); // default
    }
}
