//! Type-related runtime emissions for the C backend.
//!
//! This module contains functions that emit C code for type definitions
//! and type-related dummy variables used by the QB64 runtime system.

use std::fmt::Write;

/// Emits the qb_string type definition.
pub(super) fn emit_string_type(output: &mut String) {
    writeln!(output, "typedef struct qb_string {{").unwrap();
    writeln!(output, "    char* data;").unwrap();
    writeln!(output, "    size_t len;").unwrap();
    writeln!(output, "    size_t capacity;").unwrap();
    writeln!(output, "    int refcount;").unwrap();
    writeln!(output, "}} qb_string;").unwrap();
    writeln!(output).unwrap();
}

/// Emits dummy variables for the LEN() type-sizing pattern.
///
/// In BASIC, `LEN(dummy%%)` is used to get the size of a type by passing
/// a variable of that type. The code generator emits `sizeof()` for numeric
/// types and `qb_len_str()` for strings.
///
/// The dummy_* variables are explicitly defined here because BASIC code may
/// reference them implicitly (without DIM) in LEN() calls for type sizing.
pub(super) fn emit_type_size_dummies(output: &mut String) {
    // Dummy variables for LEN() type sizing pattern
    // BASIC uses implicit variables like LEN(dummy%%) to get type sizes
    // These must be global so they're visible from any function
    writeln!(
        output,
        "/* Dummy variables for LEN() type sizing pattern */"
    )
    .unwrap();
    writeln!(output, "int32_t dummy = 0;").unwrap(); // LONG (&)
    writeln!(output, "int8_t dummy_int_int = 0;").unwrap(); // BYTE (%%)
    writeln!(output, "int16_t dummy_int = 0;").unwrap(); // INTEGER (%)
    writeln!(output, "int64_t dummy_lng_lng = 0;").unwrap(); // _INTEGER64 (&&)
    writeln!(output, "float dummy_sng = 0.0f;").unwrap(); // SINGLE (!)
    writeln!(output, "double dummy_dbl = 0.0;").unwrap(); // DOUBLE (#)
    writeln!(output, "long double dummy_dbl_dbl = 0.0L;").unwrap(); // _FLOAT (##)
    writeln!(output, "intptr_t dummy_int_lng = 0;").unwrap(); // _OFFSET (%&)
    writeln!(output).unwrap();
}
