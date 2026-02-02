//! Assignment-related code generation for QB64Fresh C backend.
//!
//! This module contains helper methods for emitting C code for various
//! assignment operations, including simple variable assignments, array
//! element assignments, and UDT field assignments.
//!
//! # Assignment Types
//!
//! - **Simple assignment:** `variable = value`
//! - **Array assignment:** `array(i, j) = value`
//! - **Array field assignment:** `array(i).field = value`
//! - **Field assignment:** `udt.field = value`
//! - **MID$ assignment:** `MID$(str$, start[, length]) = value`
//! - **ASC assignment:** `ASC(str$, pos) = value`
//!
//! All methods handle fixed-length string assignments specially, using
//! `strncpy` to copy string data into character arrays.
//!
//! The module also exports [`emit_assignments_stmt`] for the statement dispatcher.

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{TypedArrayDimension, TypedStatementKind};
use crate::semantic::types::BasicType;
use crate::writeln_code;

use super::super::expr::unwrap_qb_str_from_c;
use super::super::types::{c_identifier, c_type};

impl super::StmtEmitter {
    pub(crate) fn emit_assignment(
        &self,
        indent: &str,
        name: &str,
        value: &crate::semantic::typed_ir::TypedExpr,
        target_type: &BasicType,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let mut c_name = c_identifier(name);

        // Apply variable rename if this variable was renamed to avoid shadowing.
        // Scalar variables can shadow array variables, so check renames.
        // Exception: BYREF string parameters get a local (e.g. a_str) from emit_byref_copies;
        // we must use that name, not the global scalar name (e.g. a_str_scalar).
        if !self.procedure.current_func_byref_strings.contains(&c_name)
            && let Some(renamed) = self.procedure.variable_renames.get(&c_name)
        {
            c_name = renamed.clone();
        }

        // Check if this is a BYREF scalar parameter - if so, we need to write through the pointer
        let is_byref_scalar = self
            .procedure
            .current_func_byref_scalar_names
            .contains(&c_name);

        let value_code = self.emit_expr(value)?;

        // _CLIPBOARDIMAGE = handle — call runtime setter instead of variable assignment
        if name.eq_ignore_ascii_case("_CLIPBOARDIMAGE") {
            writeln_code!(output, "{}qb_clipboardimage_set({});", indent, value_code)?;
            return Ok(());
        }

        // Handle fixed-length string assignment specially
        if let BasicType::FixedString(len) = target_type {
            // For fixed-length strings, we need to copy the string content
            // The value is a QbString*, we need to copy its data into the char array
            // Note: Don't free _tmp here - it will be cleaned up by qbs_cleanup at statement end
            let data_access = self.config.runtime_mode.string_data_access("_tmp");
            if is_byref_scalar {
                // For BYREF fixed-length strings, write through the pointer
                writeln_code!(
                    output,
                    "{}{{ QbString* _tmp = {}; strncpy(*{}, _tmp ? {} : \"\", {}); (*{})[{}] = '\\0'; }}",
                    indent,
                    value_code,
                    c_name,
                    data_access,
                    len,
                    c_name,
                    len
                )?;
            } else {
                writeln_code!(
                    output,
                    "{}{{ QbString* _tmp = {}; strncpy({}, _tmp ? {} : \"\", {}); {}[{}] = '\\0'; }}",
                    indent,
                    value_code,
                    c_name,
                    data_access,
                    len,
                    c_name,
                    len
                )?;
            }
        } else if *target_type == BasicType::String {
            // For dynamic strings: release old, retain new
            // This ensures proper refcount management for temp string cleanup
            if is_byref_scalar {
                // For BYREF string parameters, write through the pointer
                writeln_code!(
                    output,
                    "{}{{ QbString* _new = {}; if (*{} != _new) {{ qb_string_release(*{}); *{} = qb_string_retain(_new); }} }}",
                    indent,
                    value_code,
                    c_name,
                    c_name,
                    c_name
                )?;
            } else {
                writeln_code!(
                    output,
                    "{}{{ QbString* _new = {}; if ({} != _new) {{ qb_string_release({}); {} = qb_string_retain(_new); }} }}",
                    indent,
                    value_code,
                    c_name,
                    c_name,
                    c_name
                )?;
            }
        } else if value.basic_type != *target_type {
            let c_ty = c_type(target_type);
            if is_byref_scalar {
                writeln_code!(
                    output,
                    "{}*{} = ({})({});",
                    indent,
                    c_name,
                    c_ty,
                    value_code
                )?;
            } else {
                writeln_code!(output, "{}{} = ({})({});", indent, c_name, c_ty, value_code)?;
            }
        } else if is_byref_scalar {
            writeln_code!(output, "{}*{} = {};", indent, c_name, value_code)?;
        } else {
            writeln_code!(output, "{}{} = {};", indent, c_name, value_code)?;
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit_array_assignment(
        &self,
        indent: &str,
        name: &str,
        indices: &[crate::semantic::typed_ir::TypedExpr],
        value: &crate::semantic::typed_ir::TypedExpr,
        dimensions: &[TypedArrayDimension],
        element_type: &BasicType,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let mut c_name = c_identifier(name);

        // UDT field array: "w.arr" -> emit w.arr[i-lower], not w_arr[i]
        let base_prefix = if name.contains('.') {
            let parts: Vec<&str> = name.splitn(2, '.').collect();
            if parts.len() == 2 {
                let base = parts[0];
                let field = parts[1];
                let base_c = c_identifier(base);
                let base_c = self
                    .procedure
                    .variable_renames
                    .get(&base_c)
                    .cloned()
                    .unwrap_or(base_c);
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

        // Array assignment always refers to the local array variable, not a parameter.
        // If the variable was renamed to avoid shadowing, use the renamed version.
        //
        // IMPORTANT: We must NOT apply renames that map array names to scalar names
        // (i.e., renames ending with "_scalar"). These are for scalar/array dual namespace
        // and should only be applied to scalar variable references, not array assignments.
        // Only apply renames for parameter shadowing (e.g., args -> args_local).
        if !name.contains('.')
            && let Some(renamed) = self.procedure.variable_renames.get(&c_name)
            && !renamed.ends_with("_scalar")
        {
            c_name = renamed.clone();
        }
        let value_code = self.emit_expr(value)?;

        // Cast indices to int64_t to ensure integer subscripts
        // (C requires integer array subscripts, but BASIC allows any numeric type)
        let indices_code: Result<Vec<_>, _> = indices
            .iter()
            .map(|idx| {
                let code = self.emit_expr(idx)?;
                Ok(format!("(int64_t)({})", code))
            })
            .collect();
        let indices_code = indices_code?;

        let index_expr = if dimensions.is_empty() || indices_code.len() == 1 {
            if let Some(dim) = dimensions.first() {
                format!("{} - {}", indices_code[0], dim.lower)
            } else {
                indices_code[0].clone()
            }
        } else {
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
            linear_parts.join(" + ")
        };

        let target = if name.contains('.') {
            &base_prefix
        } else {
            &c_name
        };

        // Multi-D UDT field: C has type arr[s0][s1], emit base.field[i0-l0][i1-l1] = value
        let lhs: String = if name.contains('.') && dimensions.len() > 1 {
            let brackets: String = dimensions
                .iter()
                .zip(indices_code.iter())
                .map(|(d, i)| format!("[{} - {}]", i, d.lower))
                .collect();
            format!("{}{}", target, brackets)
        } else {
            format!("{}[{}]", target, index_expr)
        };

        // Handle fixed-length string array elements specially
        if let BasicType::FixedString(len) = element_type {
            // For fixed-length strings, we need to copy the string content
            let data_access = self.config.runtime_mode.string_data_access("_tmp");
            writeln_code!(
                output,
                "{}{{ QbString* _tmp = {}; strncpy({}, _tmp ? {} : \"\", {}); {}[{}] = '\\0'; }}",
                indent,
                value_code,
                lhs,
                data_access,
                len,
                lhs,
                len
            )?;
        } else if *element_type == BasicType::String {
            writeln_code!(
                output,
                "{}{{ QbString* _new = {}; if ({} != _new) {{ qb_string_release({}); {} = qb_string_retain(_new); }} }}",
                indent,
                value_code,
                lhs,
                lhs,
                lhs
            )?;
        } else if value.basic_type != *element_type {
            let c_ty = c_type(element_type);
            writeln_code!(output, "{}{} = ({})({});", indent, lhs, c_ty, value_code)?;
        } else {
            writeln_code!(output, "{}{} = {};", indent, lhs, value_code)?;
        }
        Ok(())
    }

    /// Emits a UDT array field assignment: `array(i).field = value`
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn emit_array_field_assignment(
        &self,
        indent: &str,
        name: &str,
        indices: &[crate::semantic::typed_ir::TypedExpr],
        fields: &[String],
        value: &crate::semantic::typed_ir::TypedExpr,
        dimensions: &[TypedArrayDimension],
        field_type: &BasicType,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let mut c_name = c_identifier(name);

        // Array field assignment always refers to the local array variable, not a parameter.
        // If the variable was renamed to avoid shadowing, use the renamed version.
        if let Some(renamed) = self.procedure.variable_renames.get(&c_name) {
            c_name = renamed.clone();
        }
        let value_code = self.emit_expr(value)?;

        let indices_code: Result<Vec<_>, _> = indices.iter().map(|e| self.emit_expr(e)).collect();
        let indices_code = indices_code?;

        // Calculate linear index for multi-dimensional arrays
        let index_expr = if dimensions.is_empty() || indices_code.len() == 1 {
            if let Some(dim) = dimensions.first() {
                format!("{} - {}", indices_code[0], dim.lower)
            } else {
                indices_code[0].clone()
            }
        } else {
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
            linear_parts.join(" + ")
        };

        // Check if this is a BYREF UDT array parameter - if so, use -> instead of .
        // Note: Arrays are typically passed as pointers, so we check if the array name
        // is in byref_udt_names (though arrays of UDTs are less common)
        // Use the ORIGINAL name for the byref check, not the potentially renamed version
        let original_c_name = c_identifier(name);
        let is_byref_udt = self
            .procedure
            .current_func_byref_udt_names
            .contains(&original_c_name);

        // Build field access chain: first field uses -> if base is BYREF UDT pointer,
        // subsequent nested fields always use . because nested UDT members are embedded (not pointers)
        let field_chain: String = fields
            .iter()
            .enumerate()
            .map(|(i, f)| {
                let sep = if i == 0 && is_byref_udt { "->" } else { "." };
                format!("{}{}", sep, c_identifier(f))
            })
            .collect();

        // Handle fixed-length string fields specially
        if let BasicType::FixedString(len) = field_type {
            // For fixed-length strings, we need to copy the string content
            // The value is a QbString*, we need to copy its data into the char array
            let data_access = self.config.runtime_mode.string_data_access("_tmp");
            writeln_code!(
                output,
                "{}{{ QbString* _tmp = {}; strncpy({}[{}]{}, _tmp ? {} : \"\", {}); {}[{}]{}[{}] = '\\0'; }}",
                indent,
                value_code,
                c_name,
                index_expr,
                field_chain,
                data_access,
                len,
                c_name,
                index_expr,
                field_chain,
                len
            )?;
        } else if *field_type == BasicType::String {
            // For dynamic string UDT fields in arrays: release old, retain new
            writeln_code!(
                output,
                "{}{{ QbString* _new = {}; if ({}[{}]{} != _new) {{ qb_string_release({}[{}]{}); {}[{}]{} = qb_string_retain(_new); }} }}",
                indent,
                value_code,
                c_name,
                index_expr,
                field_chain,
                c_name,
                index_expr,
                field_chain,
                c_name,
                index_expr,
                field_chain
            )?;
        } else {
            writeln_code!(
                output,
                "{}{}[{}]{} = {};",
                indent,
                c_name,
                index_expr,
                field_chain,
                value_code
            )?;
        }

        Ok(())
    }

    /// Emits a simple UDT field assignment: `udt.field = value`
    pub(crate) fn emit_field_assignment(
        &self,
        indent: &str,
        name: &str,
        fields: &[String],
        value: &crate::semantic::typed_ir::TypedExpr,
        field_type: &BasicType,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        let value_code = self.emit_expr(value)?;

        // Check if this is a BYREF UDT parameter - if so, use -> instead of . for first field
        let is_byref_udt = self
            .procedure
            .current_func_byref_udt_names
            .contains(&c_name);

        // Build field access chain: first field uses -> if base is BYREF UDT pointer,
        // subsequent nested fields always use . because nested UDT members are embedded (not pointers)
        let field_chain: String = fields
            .iter()
            .enumerate()
            .map(|(i, f)| {
                let sep = if i == 0 && is_byref_udt { "->" } else { "." };
                format!("{}{}", sep, c_identifier(f))
            })
            .collect();

        // Handle fixed-length string fields specially
        if let BasicType::FixedString(len) = field_type {
            // For fixed-length strings, we need to copy the string content
            // The value is a QbString*, we need to copy its data into the char array
            let data_access = self.config.runtime_mode.string_data_access("_tmp");
            writeln_code!(
                output,
                "{}{{ QbString* _tmp = {}; strncpy({}{}, _tmp ? {} : \"\", {}); {}{}[{}] = '\\0'; }}",
                indent,
                value_code,
                c_name,
                field_chain,
                data_access,
                len,
                c_name,
                field_chain,
                len
            )?;
        } else if *field_type == BasicType::String {
            // For dynamic string UDT fields: release old, retain new
            writeln_code!(
                output,
                "{}{{ QbString* _new = {}; if ({}{} != _new) {{ qb_string_release({}{}); {}{} = qb_string_retain(_new); }} }}",
                indent,
                value_code,
                c_name,
                field_chain,
                c_name,
                field_chain,
                c_name,
                field_chain
            )?;
        } else {
            writeln_code!(
                output,
                "{}{}{} = {};",
                indent,
                c_name,
                field_chain,
                value_code
            )?;
        }

        Ok(())
    }

    /// Emits MID$(target, start[, length]) = value.
    pub(crate) fn emit_mid_assignment(
        &self,
        indent: &str,
        target: &crate::semantic::typed_ir::TypedExpr,
        start: &crate::semantic::typed_ir::TypedExpr,
        length: &Option<crate::semantic::typed_ir::TypedExpr>,
        value: &crate::semantic::typed_ir::TypedExpr,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let target_code_raw = self.emit_expr(target)?;
        let target_code = if matches!(
            target.basic_type,
            crate::semantic::types::BasicType::FixedString(_)
        ) {
            let mut unwrapped = unwrap_qb_str_from_c(&target_code_raw);
            while unwrapped.starts_with("qb_str_from_c(") {
                unwrapped = unwrap_qb_str_from_c(&unwrapped);
            }
            unwrapped
        } else {
            target_code_raw
        };
        let start_code = self.emit_expr(start)?;
        let value_code = self.emit_expr(value)?;

        if matches!(
            target.basic_type,
            crate::semantic::types::BasicType::FixedString(_)
        ) {
            let len_code = if let Some(len_expr) = length {
                self.emit_expr(len_expr)?
            } else {
                format!("(int32_t)(strlen({}) - ({} - 1))", target_code, start_code)
            };
            let data_access = self.config.runtime_mode.string_data_access("_mid_val");
            writeln_code!(
                output,
                "{} {{ QbString* _mid_val = {}; if (_mid_val) {{ int32_t _mid_start = {} - 1; int32_t _mid_len = {}; int32_t _mid_copy_len = _mid_len < (int32_t)strlen({}) ? _mid_len : (int32_t)strlen({}); if (_mid_start >= 0 && _mid_start < (int32_t)strlen({})) {{ strncpy({} + _mid_start, {}, _mid_copy_len); }} }} }}",
                indent,
                value_code,
                start_code,
                len_code,
                target_code,
                target_code,
                target_code,
                target_code,
                data_access
            )?;
        } else if let Some(len_expr) = length {
            let len_code = self.emit_expr(len_expr)?;
            writeln_code!(
                output,
                "{}qb_mid_assign(&({}), {}, {}, {});",
                indent,
                target_code,
                start_code,
                len_code,
                value_code
            )?;
        } else {
            writeln_code!(
                output,
                "{}qb_mid_assign(&({}), {}, -1, {});",
                indent,
                target_code,
                start_code,
                value_code
            )?;
        }
        Ok(())
    }

    /// Emits ASC(str$, pos) = value (single-character write).
    pub(crate) fn emit_asc_assignment(
        &self,
        indent: &str,
        target: &crate::semantic::typed_ir::TypedExpr,
        position: &crate::semantic::typed_ir::TypedExpr,
        value: &crate::semantic::typed_ir::TypedExpr,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let target_code = self.emit_expr(target)?;
        let position_code = self.emit_expr(position)?;
        let value_code = self.emit_expr(value)?;
        writeln_code!(
            output,
            "{}qb_asc_assign(&({}), {}, {});",
            indent,
            target_code,
            position_code,
            value_code
        )?;
        Ok(())
    }
}

/// Dispatcher for assignment-related statement kinds.
///
/// Matches on assignment variants and calls the appropriate `StmtEmitter` method.
pub(super) fn emit_assignments_stmt(
    emitter: &mut super::StmtEmitter,
    kind: &TypedStatementKind,
    indent: &str,
    output: &mut String,
) -> Result<(), CodeGenError> {
    match kind {
        TypedStatementKind::Assignment {
            name,
            value,
            target_type,
        } => emitter.emit_assignment(indent, name, value, target_type, output)?,

        TypedStatementKind::ArrayAssignment {
            name,
            indices,
            value,
            dimensions,
            element_type,
        } => emitter.emit_array_assignment(
            indent,
            name,
            indices,
            value,
            dimensions,
            element_type,
            output,
        )?,

        TypedStatementKind::ArrayFieldAssignment {
            name,
            indices,
            fields,
            value,
            dimensions,
            element_type: _,
            field_type,
        } => emitter.emit_array_field_assignment(
            indent,
            name,
            indices,
            fields,
            value,
            dimensions,
            field_type,
            output,
        )?,

        TypedStatementKind::FieldAssignment {
            name,
            fields,
            value,
            field_type,
        } => emitter.emit_field_assignment(indent, name, fields, value, field_type, output)?,

        TypedStatementKind::MidAssignment {
            target,
            start,
            length,
            value,
        } => emitter.emit_mid_assignment(indent, target, start, length, value, output)?,

        TypedStatementKind::AscAssignment {
            target,
            position,
            value,
        } => emitter.emit_asc_assignment(indent, target, position, value, output)?,

        _ => unreachable!("emit_assignments_stmt called with non-assignment kind"),
    }
    Ok(())
}
