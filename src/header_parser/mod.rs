//! Simple C Header Parser for QB64Fresh
//!
//! This module provides basic parsing of C header files to automatically
//! extract function declarations for `DECLARE LIBRARY "header.h"` support.
//!
//! # Limitations
//!
//! This is a simplified parser that handles common cases:
//! - Simple function declarations (no complex macros)
//! - Basic C types (int, char, float, double, void, pointers)
//! - Ignores preprocessor directives, macros, and complex type definitions
//!
//! For complex headers, users should still manually declare functions.

mod lexer;
mod parser;

use crate::semantic::types::BasicType;

/// Information about a function extracted from a C header.
#[derive(Debug, Clone)]
pub struct CFunction {
    /// The function name.
    pub name: String,
    /// Return type converted to BASIC.
    pub return_type: BasicType,
    /// Parameter types converted to BASIC.
    pub params: Vec<CParam>,
}

/// A parameter from a C function declaration.
#[derive(Debug, Clone)]
pub struct CParam {
    /// Parameter name (if present, otherwise empty).
    pub name: String,
    /// Type converted to BASIC.
    pub typ: BasicType,
}

/// Parse a C header file and extract function declarations.
///
/// # Arguments
/// * `header_content` - The contents of the header file
///
/// # Returns
/// A vector of function declarations found in the header
pub fn parse_header(header_content: &str) -> Vec<CFunction> {
    let tokens = lexer::tokenize(header_content);
    parser::parse_functions(&tokens)
}

/// Map a C type to a BASIC type.
pub fn c_type_to_basic(c_type: &str, is_pointer: bool) -> BasicType {
    let base_type = c_type.trim();

    if is_pointer {
        // Char pointers are strings in BASIC
        if base_type == "char" || base_type == "const char" {
            return BasicType::String;
        }
        // Other pointers become _OFFSET
        return BasicType::Offset;
    }

    match base_type {
        // Signed integers
        "int" | "int32_t" | "long" => BasicType::Long,
        "short" | "int16_t" => BasicType::Integer,
        "char" | "int8_t" | "signed char" => BasicType::Byte,
        "long long" | "int64_t" => BasicType::Integer64,

        // Unsigned integers
        "unsigned int" | "uint32_t" | "unsigned long" | "unsigned" => BasicType::UnsignedLong,
        "unsigned short" | "uint16_t" => BasicType::UnsignedInteger,
        "unsigned char" | "uint8_t" => BasicType::UnsignedByte,
        "unsigned long long" | "uint64_t" => BasicType::UnsignedInteger64,

        // Floating point
        "float" => BasicType::Single,
        "double" => BasicType::Double,
        "long double" => BasicType::Float,

        // Void
        "void" => BasicType::Void,

        // Size types
        "size_t" | "ssize_t" | "intptr_t" | "uintptr_t" | "ptrdiff_t" => BasicType::Offset,

        // Unknown type - default to LONG
        _ => BasicType::Long,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_c_type_to_basic() {
        assert_eq!(c_type_to_basic("int", false), BasicType::Long);
        assert_eq!(c_type_to_basic("char", true), BasicType::String);
        assert_eq!(c_type_to_basic("double", false), BasicType::Double);
        assert_eq!(c_type_to_basic("void", true), BasicType::Offset);
    }
}
