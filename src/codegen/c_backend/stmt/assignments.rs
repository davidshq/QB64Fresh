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
//!
//! All methods handle fixed-length string assignments specially, using
//! `strncpy` to copy string data into character arrays.

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::TypedArrayDimension;
use crate::semantic::types::BasicType;
use crate::writeln_code;

use super::super::expr::emit_expr;
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
        let c_name = c_identifier(name);
        let value_code = self.emit_expr(value)?;

        // Handle fixed-length string assignment specially
        if let BasicType::FixedString(len) = target_type {
            // For fixed-length strings, we need to copy the string content
            // The value is a qb_string*, we need to copy its data into the char array
            // Note: Don't free _tmp here - it will be cleaned up by qbs_cleanup at statement end
            let data_access = match self.runtime_mode {
                super::super::RuntimeMode::External => "qb_string_data(_tmp)",
                super::super::RuntimeMode::Inline => "_tmp->data",
            };
            writeln_code!(
                output,
                "{}{{ qb_string* _tmp = {}; strncpy({}, _tmp ? {} : \"\", {}); {}[{}] = '\\0'; }}",
                indent,
                value_code,
                c_name,
                data_access,
                len,
                c_name,
                len
            )?;
        } else if *target_type == BasicType::String {
            // For dynamic strings: release old, retain new
            // This ensures proper refcount management for temp string cleanup
            writeln_code!(
                output,
                "{}{{ qb_string* _new = {}; if ({} != _new) {{ qb_string_release({}); {} = qb_string_retain(_new); }} }}",
                indent,
                value_code,
                c_name,
                c_name,
                c_name
            )?;
        } else if value.basic_type != *target_type {
            let c_ty = c_type(target_type);
            writeln_code!(output, "{}{} = ({})({});", indent, c_name, c_ty, value_code)?;
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
        let c_name = c_identifier(name);
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

        // Handle fixed-length string array elements specially
        if let BasicType::FixedString(len) = element_type {
            // For fixed-length strings, we need to copy the string content
            // The value is a qb_string*, we need to copy its data into the char array
            // Note: Don't free _tmp here - it will be cleaned up by qbs_cleanup at statement end
            let data_access = match self.runtime_mode {
                super::super::RuntimeMode::External => "qb_string_data(_tmp)",
                super::super::RuntimeMode::Inline => "_tmp->data",
            };
            writeln_code!(
                output,
                "{}{{ qb_string* _tmp = {}; strncpy({}[{}], _tmp ? {} : \"\", {}); {}[{}][{}] = '\\0'; }}",
                indent,
                value_code,
                c_name,
                index_expr,
                data_access,
                len,
                c_name,
                index_expr,
                len
            )?;
        } else if *element_type == BasicType::String {
            // For dynamic string arrays: release old, retain new
            // This ensures proper refcount management for temp string cleanup
            writeln_code!(
                output,
                "{}{{ qb_string* _new = {}; if ({}[{}] != _new) {{ qb_string_release({}[{}]); {}[{}] = qb_string_retain(_new); }} }}",
                indent,
                value_code,
                c_name,
                index_expr,
                c_name,
                index_expr,
                c_name,
                index_expr
            )?;
        } else if value.basic_type != *element_type {
            let c_ty = c_type(element_type);
            writeln_code!(
                output,
                "{}{}[{}] = ({})({});",
                indent,
                c_name,
                index_expr,
                c_ty,
                value_code
            )?;
        } else {
            writeln_code!(
                output,
                "{}{}[{}] = {};",
                indent,
                c_name,
                index_expr,
                value_code
            )?;
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
        let c_name = c_identifier(name);
        let value_code = self.emit_expr(value)?;

        let indices_code: Result<Vec<_>, _> = indices
            .iter()
            .map(|e| self.emit_expr(e))
            .collect();
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

        // Build field access chain: .field1.field2...
        let field_chain: String = fields
            .iter()
            .map(|f| format!(".{}", c_identifier(f)))
            .collect();

        // Handle fixed-length string fields specially
        if let BasicType::FixedString(len) = field_type {
            // For fixed-length strings, we need to copy the string content
            // The value is a qb_string*, we need to copy its data into the char array
            let data_access = match self.runtime_mode {
                super::super::RuntimeMode::External => "qb_string_data(_tmp)",
                super::super::RuntimeMode::Inline => "_tmp->data",
            };
            writeln_code!(
                output,
                "{}{{ qb_string* _tmp = {}; strncpy({}[{}]{}, _tmp ? {} : \"\", {}); {}[{}]{}[{}] = '\\0'; }}",
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
                "{}{{ qb_string* _new = {}; if ({}[{}]{} != _new) {{ qb_string_release({}[{}]{}); {}[{}]{} = qb_string_retain(_new); }} }}",
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

        // Build field access chain: .field1.field2...
        let field_chain: String = fields
            .iter()
            .map(|f| format!(".{}", c_identifier(f)))
            .collect();

        // Handle fixed-length string fields specially
        if let BasicType::FixedString(len) = field_type {
            // For fixed-length strings, we need to copy the string content
            // The value is a qb_string*, we need to copy its data into the char array
            let data_access = match self.runtime_mode {
                super::super::RuntimeMode::External => "qb_string_data(_tmp)",
                super::super::RuntimeMode::Inline => "_tmp->data",
            };
            writeln_code!(
                output,
                "{}{{ qb_string* _tmp = {}; strncpy({}{}, _tmp ? {} : \"\", {}); {}{}[{}] = '\\0'; }}",
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
                "{}{{ qb_string* _new = {}; if ({}{} != _new) {{ qb_string_release({}{}); {}{} = qb_string_retain(_new); }} }}",
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
}
