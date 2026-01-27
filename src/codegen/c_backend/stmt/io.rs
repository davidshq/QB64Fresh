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
use crate::semantic::typed_ir::{TypedInputTarget, TypedPrintItem};
use crate::writeln_code;

use super::super::expr::{emit_expr, escape_string};
use super::super::types::c_identifier;

impl super::StmtEmitter {
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
                } => {
                    let c_arr = c_identifier(name);
                    let idx_code: Vec<_> = indices
                        .iter()
                        .map(|e| emit_expr(e, self.no_shell))
                        .collect::<Result<_, _>>()?;
                    // Use first index for 1D array syntax (TODO: handle multi-dim)
                    let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                    (format!("{}[{}]", c_arr, idx), element_type.clone())
                }
                ArrayElementField {
                    name,
                    indices,
                    fields,
                    field_type,
                } => {
                    let c_arr = c_identifier(name);
                    let idx_code: Vec<_> = indices
                        .iter()
                        .map(|e| emit_expr(e, self.no_shell))
                        .collect::<Result<_, _>>()?;
                    let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
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
        let expr_code = emit_expr(&item.expr, self.no_shell)?;

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
