//! Type-related runtime emissions for the C backend.
//!
//! This module contains functions that emit C code for type definitions
//! and type-related dummy variables used by the QB64 runtime system.
//!
//! ## Compilation-Time vs Runtime Distinction
//!
//! The `qb_string` struct definition is emitted in both inline and external runtime modes.
//! In external mode, it's provided as a "compilation-time detail" to allow generated code
//! to access struct members (e.g., in UDTs containing `qb_string*` fields). However, the
//! API functions in `qb64fresh_rt.h` (`qb_string_data()`, `qb_string_len()`, etc.) remain
//! the stable runtime contract. Direct struct member access is an implementation detail
//! that may change in future versions.

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

use crate::codegen::c_backend::type_registry::TypeRegistry;

/// Registers the qb_string type definitions in the type registry.
///
/// This registers both `QbString` (the struct) and `qb_string` (the typedef)
/// with proper dependency ordering.
pub(in crate::codegen::c_backend) fn register_string_types(
    registry: &mut TypeRegistry,
) -> Result<(), CodeGenError> {
    // Register QbString struct (no dependencies)
    registry.register_type("QbString", &[], |output| {
        writeln_code!(output, "struct QbString {{")?;
        writeln_code!(output, "    char* data;")?;
        writeln_code!(output, "    size_t len;")?;
        writeln_code!(output, "    size_t capacity;")?;
        writeln_code!(output, "    int refcount;")?;
        writeln_code!(output, "}};")?;
        Ok(())
    });

    // Register qb_string typedef (depends on QbString)
    registry.register_type("qb_string", &["QbString"], |output| {
        // The header's typedef struct QbString QbString; is now complete
        // Create qb_string alias - must be on separate line to avoid redefinition
        writeln_code!(output, "typedef QbString qb_string;")?;
        writeln_code!(output)?;
        Ok(())
    });

    Ok(())
}

/// Emits the qb_string type definition using the type registry.
///
/// This function ensures proper ordering: QbString struct is emitted before qb_string typedef.
/// It uses the type registry to handle dependencies automatically.
///
/// **Important**: While the struct definition is provided for compilation, the API
/// functions (`qb_string_data()`, `qb_string_len()`, `qb_string_release()`, etc.)
/// remain the stable runtime interface. Direct struct member access is an
/// implementation detail and may change in future versions.
///
/// In external runtime mode, we define the struct as `QbString` to match the header
/// file's forward declaration, then typedef it to `qb_string` for compatibility.
pub(super) fn emit_string_type(
    registry: &mut TypeRegistry,
    output: &mut String,
) -> Result<(), CodeGenError> {
    // Ensure qb_string is emitted (which will also emit QbString first due to dependency)
    registry.ensure_type_emitted("qb_string", output)?;
    Ok(())
}

/// Emits the qb_string type definition (legacy function for backward compatibility).
///
/// This is a wrapper that creates a temporary registry. New code should use
/// `emit_string_type` with a registry instead.
#[allow(dead_code)]
pub(super) fn emit_string_type_legacy(output: &mut String) -> Result<(), CodeGenError> {
    let mut registry = TypeRegistry::new();
    register_string_types(&mut registry)?;
    emit_string_type(&mut registry, output)
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
