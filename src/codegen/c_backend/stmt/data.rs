//! DATA/READ statement code generation for QB64Fresh C backend.
//!
//! This module contains helper methods for emitting C code related to
//! DATA and READ operations, extracted from the main statement emitter.
//!
//! # Supported Operations
//!
//! - `emit_read` - Handles READ statement code generation, including:
//!   - Simple variable targets: `READ x`
//!   - Array element targets: `READ arr(i, j)`
//!   - UDT array element fields: `READ arr(i).field`
//!   - Type-appropriate data reading (string vs numeric)
//!
//! - `emit_restore` - Handles RESTORE statement code generation:
//!   - Resetting data pointer to beginning
//!   - Restoring to specific labeled DATA statement

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::TypedReadTarget;
use crate::semantic::types::BasicType;
use crate::writeln_code;

use super::super::types::{c_identifier, c_type};

impl super::StmtEmitter {
    /// Emits code for a READ statement.
    ///
    /// Generates C code that reads values from the DATA array into the specified
    /// targets. Each target is processed in sequence, with type-appropriate
    /// handling for strings vs numeric types.
    ///
    /// For strings, the generated code:
    /// - Checks if the current DATA value is a string type ('s')
    /// - If so, converts the C string to a QB string
    /// - Otherwise, converts the numeric value to a string representation
    ///
    /// For numeric types, the generated code:
    /// - Checks if the current DATA value is numeric ('d')
    /// - Casts the double value to the appropriate target type
    ///
    /// # Arguments
    ///
    /// * `indent` - The indentation string to use for generated code
    /// * `targets` - The variables/array elements/fields to receive data values
    /// * `output` - The string buffer to write generated code to
    ///
    /// # Returns
    ///
    /// `Ok(())` on success, or `Err(CodeGenError)` if expression emission fails.
    pub(in crate::codegen::c_backend) fn emit_read(
        &self,
        indent: &str,
        targets: &[TypedReadTarget],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        for target in targets {
            let (c_target, var_type) = match target {
                TypedReadTarget::Variable { name, basic_type } => {
                    (c_identifier(name), basic_type.clone())
                }
                TypedReadTarget::ArrayElement {
                    name,
                    indices,
                    basic_type,
                } => {
                    let c_arr = c_identifier(name);
                    let idx_parts: Vec<_> = indices
                        .iter()
                        .map(|e| self.emit_expr(e))
                        .collect::<Result<Vec<_>, _>>()?;
                    let idx_str = idx_parts.join("][");
                    (format!("{}[{}]", c_arr, idx_str), basic_type.clone())
                }
                TypedReadTarget::ArrayFieldElement {
                    name,
                    indices,
                    field,
                    basic_type,
                } => {
                    let c_arr = c_identifier(name);
                    let c_field = c_identifier(field);
                    let idx_parts: Vec<_> = indices
                        .iter()
                        .map(|e| self.emit_expr(e))
                        .collect::<Result<Vec<_>, _>>()?;
                    let idx_str = idx_parts.join("][");
                    (
                        format!("{}[{}].{}", c_arr, idx_str, c_field),
                        basic_type.clone(),
                    )
                }
            };

            match var_type {
                BasicType::String | BasicType::FixedString(_) => {
                    writeln_code!(
                        output,
                        "{}if (_qb_data_ptr < _qb_data_count && _qb_data[_qb_data_ptr].type == 's') {{",
                        indent
                    )?;
                    writeln_code!(
                        output,
                        "{}    {} = qb_str_from_c(_qb_data[_qb_data_ptr].v.s);",
                        indent,
                        c_target
                    )?;
                    writeln_code!(
                        output,
                        "{}}} else if (_qb_data_ptr < _qb_data_count) {{",
                        indent
                    )?;
                    writeln_code!(
                        output,
                        "{}    {} = qb_str_float(_qb_data[_qb_data_ptr].v.n);",
                        indent,
                        c_target
                    )?;
                    writeln_code!(output, "{}}}", indent)?;
                    writeln_code!(output, "{}_qb_data_ptr++;", indent)?;
                }
                _ => {
                    let c_ty = c_type(&var_type);
                    writeln_code!(
                        output,
                        "{}if (_qb_data_ptr < _qb_data_count && _qb_data[_qb_data_ptr].type == 'd') {{",
                        indent
                    )?;
                    writeln_code!(
                        output,
                        "{}    {} = ({})_qb_data[_qb_data_ptr].v.n;",
                        indent,
                        c_target,
                        c_ty
                    )?;
                    writeln_code!(output, "{}}}", indent)?;
                    writeln_code!(output, "{}_qb_data_ptr++;", indent)?;
                }
            }
        }
        Ok(())
    }

    /// Emits code for a RESTORE statement.
    ///
    /// Generates C code that resets the DATA pointer to either the beginning
    /// of the DATA array or to a specific labeled DATA statement.
    ///
    /// # Arguments
    ///
    /// * `indent` - The indentation string to use for generated code
    /// * `label` - Optional label to restore to; if None, restores to beginning
    /// * `output` - The string buffer to write generated code to
    ///
    /// # Returns
    ///
    /// `Ok(())` on success. Currently always succeeds.
    ///
    /// # Notes
    ///
    /// If a label is specified but not found in the `data_label_indices` map,
    /// a warning comment is emitted and the pointer is reset to 0.
    pub(in crate::codegen::c_backend) fn emit_restore(
        &self,
        indent: &str,
        label: &Option<String>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        match label {
            None => {
                writeln_code!(output, "{}_qb_data_ptr = 0;", indent)?;
            }
            Some(lbl) => {
                let label_upper = lbl.to_uppercase();
                if let Some(&index) = self.data_label_indices.get(&label_upper) {
                    writeln_code!(
                        output,
                        "{}_qb_data_ptr = {}; /* RESTORE {} */",
                        indent,
                        index,
                        lbl
                    )?;
                } else {
                    writeln_code!(
                        output,
                        "{}/* Warning: RESTORE label '{}' not associated with DATA */",
                        indent,
                        lbl
                    )?;
                    writeln_code!(output, "{}_qb_data_ptr = 0;", indent)?;
                }
            }
        }
        Ok(())
    }
}
