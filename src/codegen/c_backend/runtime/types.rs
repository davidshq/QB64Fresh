//! Type-related runtime emissions for the C backend.
//!
//! This module contains functions that emit C code for type definitions
//! and type-related dummy variables used by the QB64 runtime system.

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits the qb_string type definition.
pub(super) fn emit_string_type(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "typedef struct qb_string {{")?;
    writeln_code!(output, "    char* data;")?;
    writeln_code!(output, "    size_t len;")?;
    writeln_code!(output, "    size_t capacity;")?;
    writeln_code!(output, "    int refcount;")?;
    writeln_code!(output, "}} qb_string;")?;
    writeln_code!(output)?;
    Ok(())
}

/// Emits dummy variables for the LEN() type-sizing pattern.
///
/// In BASIC, `LEN(dummy%%)` is used to get the size of a type by passing
/// a variable of that type. The code generator emits `sizeof()` for numeric
/// types and `qb_len_str()` for strings.
///
/// The dummy_* variables are explicitly defined here because BASIC code may
/// reference them implicitly (without DIM) in LEN() calls for type sizing.
pub(super) fn emit_type_size_dummies(output: &mut String) -> Result<(), CodeGenError> {
    // Dummy variables for LEN() type sizing pattern
    // BASIC uses implicit variables like LEN(dummy%%) to get type sizes
    // These must be global so they're visible from any function
    writeln_code!(
        output,
        "/* Dummy variables for LEN() type sizing pattern */"
    )?;
    writeln_code!(output, "int32_t dummy = 0;")?; // LONG (&)
    writeln_code!(output, "int8_t dummy_int_int = 0;")?; // BYTE (%%)
    writeln_code!(output, "int16_t dummy_int = 0;")?; // INTEGER (%)
    writeln_code!(output, "int64_t dummy_lng_lng = 0;")?; // _INTEGER64 (&&)
    writeln_code!(output, "float dummy_sng = 0.0f;")?; // SINGLE (!)
    writeln_code!(output, "double dummy_dbl = 0.0;")?; // DOUBLE (#)
    writeln_code!(output, "long double dummy_dbl_dbl = 0.0L;")?; // _FLOAT (##)
    writeln_code!(output, "intptr_t dummy_int_lng = 0;")?; // _OFFSET (%&)
    writeln_code!(output)?;
    Ok(())
}
