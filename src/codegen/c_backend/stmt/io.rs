//! I/O statement code generation for QB64Fresh C backend.
//!
//! This module contains helper methods for emitting C code related to
//! console I/O operations, extracted from the main statement emitter.
//!
//! # Supported Operations
//!
//! - `emit_input` - Handles INPUT statement code generation, including:
//!   - Optional prompts with question marks
//!   - Multiple input targets (variables, array elements, fields)
//!   - Type-appropriate input functions (string, float, integer)
//!
//! - `emit_print_item` - Handles individual PRINT item code generation:
//!   - Type-appropriate print functions
//!   - Print separators (comma for tab, semicolon for no space)

use crate::ast::PrintSeparator;
use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{TypedArrayDimension, TypedInputTarget, TypedPrintItem};
use crate::writeln_code;

use super::super::expr::escape_string;
use super::super::types::c_identifier;

impl super::StmtEmitter {
    /// Calculates a linear array index expression for multi-dimensional arrays.
    ///
    /// Given array indices and dimensions, calculates the linear (1D) index
    /// using row-major order, accounting for array lower bounds.
    ///
    /// # Arguments
    ///
    /// * `indices_code` - C code expressions for each dimension index
    /// * `dimensions` - Array dimension bounds (empty for 1D or unknown)
    ///
    /// # Returns
    ///
    /// C expression string for the linear index, or the first index if dimensions are unknown.
    ///
    /// # Panics
    ///
    /// Panics if `indices_code.len() != dimensions.len()` when both are non-empty.
    /// This should not happen in normal operation as the semantic analyzer ensures
    /// index count matches dimension count.
    /// Calculates a linear array index expression for multi-dimensional arrays.
    ///
    /// This is a helper method used by both console I/O and file I/O code generation.
    pub(crate) fn calculate_array_index(
        &self,
        indices_code: &[String],
        dimensions: &[TypedArrayDimension],
    ) -> String {
        if dimensions.is_empty() || indices_code.len() == 1 {
            // Single dimension or unknown dimensions - use first index directly
            if let Some(dim) = dimensions.first() {
                format!("({} - {})", indices_code[0], dim.lower)
            } else {
                indices_code[0].clone()
            }
        } else {
            // Multi-dimensional: calculate linear index using row-major order
            // Ensure indices and dimensions match (semantic analyzer should guarantee this)
            debug_assert_eq!(
                indices_code.len(),
                dimensions.len(),
                "Index count must match dimension count"
            );

            let mut linear_parts = Vec::new();
            for (i, (idx, dim)) in indices_code.iter().zip(dimensions.iter()).enumerate() {
                let adjusted = format!("({} - {})", idx, dim.lower);
                if i < dimensions.len() - 1 {
                    // Calculate stride for this dimension
                    let stride: i64 = dimensions[i + 1..]
                        .iter()
                        .map(|d| d.upper - d.lower + 1)
                        .product();
                    linear_parts.push(format!("{} * {}", adjusted, stride));
                } else {
                    // Last dimension - just add the adjusted index
                    linear_parts.push(adjusted);
                }
            }
            linear_parts.join(" + ")
        }
    }

    /// Emits code for an INPUT statement.
    ///
    /// Generates appropriate C function calls for each input target based on its type.
    /// The first target receives the prompt (if any), subsequent targets receive NULL
    /// as their prompt argument.
    ///
    /// # Arguments
    ///
    /// * `indent` - The indentation string to use for generated code
    /// * `prompt` - Optional prompt string to display before input
    /// * `show_question_mark` - Whether to append "? " to the prompt
    /// * `same_line` - Whether to keep cursor on same line after input (no newline printed)
    /// * `targets` - The variables/array elements to receive input values
    /// * `output` - The string buffer to write generated code to
    ///
    /// # Returns
    ///
    /// `Ok(())` on success, or `Err(CodeGenError)` if expression emission fails.
    pub(crate) fn emit_input(
        &self,
        indent: &str,
        prompt: &Option<String>,
        show_question_mark: bool,
        same_line: bool,
        targets: &[TypedInputTarget],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        use TypedInputTarget::*;

        // Convert same_line boolean to C int (1 = true, 0 = false)
        let same_line_int = if same_line { 1 } else { 0 };

        let full_prompt = match prompt {
            Some(p) => {
                if show_question_mark {
                    format!("{}? ", p)
                } else {
                    p.clone()
                }
            }
            None => {
                if show_question_mark {
                    "? ".to_string()
                } else {
                    String::new()
                }
            }
        };

        let num_targets = targets.len();
        for (i, target) in targets.iter().enumerate() {
            let (target_code, var_type) = match target {
                Variable { name, basic_type } => (c_identifier(name), basic_type.clone()),
                ArrayElement {
                    name,
                    indices,
                    element_type,
                    dimensions,
                } => {
                    let c_arr = c_identifier(name);
                    // Cast indices to int64_t to ensure integer subscripts
                    let idx_code: Vec<_> = indices
                        .iter()
                        .map(|e| {
                            let code = self.emit_expr(e)?;
                            Ok(format!("(int64_t)({})", code))
                        })
                        .collect::<Result<_, _>>()?;

                    // Calculate linear index for multi-dimensional arrays
                    let idx = self.calculate_array_index(&idx_code, dimensions);
                    (format!("{}[{}]", c_arr, idx), element_type.clone())
                }
                ArrayElementField {
                    name,
                    indices,
                    fields,
                    field_type,
                    dimensions,
                } => {
                    let c_arr = c_identifier(name);
                    // Cast indices to int64_t to ensure integer subscripts
                    let idx_code: Vec<_> = indices
                        .iter()
                        .map(|e| {
                            let code = self.emit_expr(e)?;
                            Ok(format!("(int64_t)({})", code))
                        })
                        .collect::<Result<_, _>>()?;

                    // Calculate linear index for multi-dimensional arrays
                    let idx = self.calculate_array_index(&idx_code, dimensions);
                    let field_chain = fields.join(".");
                    (
                        format!("{}[{}].{}", c_arr, idx, field_chain),
                        field_type.clone(),
                    )
                }
                Field {
                    name,
                    fields,
                    field_type,
                } => {
                    let c_var = c_identifier(name);
                    let field_chain = fields.join(".");
                    (format!("{}.{}", c_var, field_chain), field_type.clone())
                }
            };

            let prompt_arg = if i == 0 && !full_prompt.is_empty() {
                format!("\"{}\"", escape_string(&full_prompt))
            } else {
                "NULL".to_string()
            };

            // Only the last input should print a newline (if same_line is false).
            // All intermediate inputs should suppress newline (same_line = 1).
            let target_same_line = if i == num_targets - 1 {
                same_line_int
            } else {
                1 // Suppress newline for intermediate inputs
            };

            if var_type.is_string() {
                writeln_code!(
                    output,
                    "{}qb_input_string({}, &{}, {});",
                    indent,
                    prompt_arg,
                    target_code,
                    target_same_line
                )?;
            } else if var_type.is_float() {
                writeln_code!(
                    output,
                    "{}qb_input_float({}, &{}, {});",
                    indent,
                    prompt_arg,
                    target_code,
                    target_same_line
                )?;
            } else {
                writeln_code!(
                    output,
                    "{}qb_input_int({}, &{}, {});",
                    indent,
                    prompt_arg,
                    target_code,
                    target_same_line
                )?;
            }
        }
        Ok(())
    }

    /// Emits a PRINT item.
    ///
    /// Generates C code for a single PRINT item, selecting the appropriate
    /// runtime function based on the expression's type (string, float, or integer).
    /// Also handles print separators (comma for tab spacing, semicolon for adjacent).
    ///
    /// # Arguments
    ///
    /// * `item` - The typed print item containing the expression and optional separator
    /// * `output` - The string buffer to write generated code to
    ///
    /// # Returns
    ///
    /// `Ok(())` on success, or `Err(CodeGenError)` if expression emission fails.
    pub(crate) fn emit_print_item(
        &self,
        item: &TypedPrintItem,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let indent = self.indent_str();
        let expr_code = self.emit_expr(&item.expr)?;

        if item.expr.basic_type.is_string() {
            writeln_code!(output, "{}qb_print_string({});", indent, expr_code)?;
        } else if item.expr.basic_type.is_float() {
            writeln_code!(output, "{}qb_print_float({});", indent, expr_code)?;
        } else {
            writeln_code!(output, "{}qb_print_int({});", indent, expr_code)?;
        }

        if let Some(sep) = &item.separator {
            match sep {
                PrintSeparator::Comma => {
                    writeln_code!(output, "{}qb_print_tab();", indent)?;
                }
                PrintSeparator::Semicolon => {
                    // No separator - items print adjacent
                }
            }
        }

        Ok(())
    }
}
