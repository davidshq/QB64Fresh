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

use crate::semantic::types::BasicType;

/// Maps a BASIC type to its C representation.
///
/// This function handles all BASIC types including unsigned variants,
/// fixed-length strings, user-defined types, and arrays.
///
/// # Examples
///
/// ```ignore
/// assert_eq!(c_type(&BasicType::Integer), "int16_t");
/// assert_eq!(c_type(&BasicType::String), "qb_string*");
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
        BasicType::String => "qb_string*".to_string(),
        BasicType::FixedString(n) => format!("char[{}]", n + 1), // +1 for null terminator
        BasicType::UnsignedBit => "uint8_t".to_string(),
        BasicType::UnsignedByte => "uint8_t".to_string(),
        BasicType::UnsignedInteger => "uint16_t".to_string(),
        BasicType::UnsignedLong => "uint32_t".to_string(),
        BasicType::UnsignedInteger64 => "uint64_t".to_string(),
        BasicType::UserDefined(name) => format!("struct {}", name),
        BasicType::Array { element_type, .. } => {
            format!("{}*", c_type(element_type))
        }
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
    name.replace('$', "_str")
        .replace('%', "_int")
        .replace('&', "_lng")
        .replace('!', "_sng")
        .replace('#', "_dbl")
        .replace('`', "_bit")
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
        assert_eq!(c_type(&BasicType::String), "qb_string*");
        assert_eq!(c_type(&BasicType::FixedString(10)), "char[11]");
        assert_eq!(c_type(&BasicType::UnsignedInteger), "uint16_t");
        assert_eq!(
            c_type(&BasicType::UserDefined("MyType".to_string())),
            "struct MyType"
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
}
