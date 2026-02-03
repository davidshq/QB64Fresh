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

use std::fmt::Write;

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::TypedArrayDimension;
use crate::semantic::types::BasicType;

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
        let value_code = emit_expr(value)?;

        // Handle fixed-length string assignment specially
        if let BasicType::FixedString(len) = target_type {
            // For fixed-length strings, we need to copy the string content
            // The value is a qb_string*, we need to copy its data into the char array
            writeln!(
                output,
                "{}{{ qb_string* _tmp = {}; strncpy({}, _tmp ? _tmp->data : \"\", {}); {}[{}] = '\\0'; qb_string_free(_tmp); }}",
                indent, value_code, c_name, len, c_name, len
            ).unwrap();
        } else if value.basic_type != *target_type {
            let c_ty = c_type(target_type);
            writeln!(output, "{}{} = ({})({});", indent, c_name, c_ty, value_code).unwrap();
        } else {
            writeln!(output, "{}{} = {};", indent, c_name, value_code).unwrap();
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
        let value_code = emit_expr(value)?;

        // Cast indices to int64_t to ensure integer subscripts
        // (C requires integer array subscripts, but BASIC allows any numeric type)
        let indices_code: Result<Vec<_>, _> = indices
            .iter()
            .map(|idx| {
                let code = emit_expr(idx)?;
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
            writeln!(
                output,
                "{}{{ qb_string* _tmp = {}; strncpy({}[{}], _tmp ? _tmp->data : \"\", {}); {}[{}][{}] = '\\0'; qb_string_free(_tmp); }}",
                indent, value_code, c_name, index_expr, len, c_name, index_expr, len
            ).unwrap();
        } else if value.basic_type != *element_type {
            let c_ty = c_type(element_type);
            writeln!(
                output,
                "{}{}[{}] = ({})({});",
                indent, c_name, index_expr, c_ty, value_code
            )
            .unwrap();
        } else {
            writeln!(
                output,
                "{}{}[{}] = {};",
                indent, c_name, index_expr, value_code
            )
            .unwrap();
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
        let value_code = emit_expr(value)?;

        let indices_code: Result<Vec<_>, _> = indices.iter().map(emit_expr).collect();
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
            writeln!(
                output,
                "{}{{ qb_string* _tmp = {}; strncpy({}[{}]{}, _tmp ? _tmp->data : \"\", {}); {}[{}]{}[{}] = '\\0'; qb_string_free(_tmp); }}",
                indent, value_code, c_name, index_expr, field_chain, len, c_name, index_expr, field_chain, len
            ).unwrap();
        } else {
            writeln!(
                output,
                "{}{}[{}]{} = {};",
                indent, c_name, index_expr, field_chain, value_code
            )
            .unwrap();
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
        let value_code = emit_expr(value)?;

        // Build field access chain: .field1.field2...
        let field_chain: String = fields
            .iter()
            .map(|f| format!(".{}", c_identifier(f)))
            .collect();

        // Handle fixed-length string fields specially
        if let BasicType::FixedString(len) = field_type {
            // For fixed-length strings, we need to copy the string content
            // The value is a qb_string*, we need to copy its data into the char array
            writeln!(
                output,
                "{}{{ qb_string* _tmp = {}; strncpy({}{}, _tmp ? _tmp->data : \"\", {}); {}{}[{}] = '\\0'; qb_string_free(_tmp); }}",
                indent, value_code, c_name, field_chain, len, c_name, field_chain, len
            ).unwrap();
        } else {
            writeln!(
                output,
                "{}{}{} = {};",
                indent, c_name, field_chain, value_code
            )
            .unwrap();
        }

        Ok(())
    }
}
