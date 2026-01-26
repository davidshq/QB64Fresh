//! Statement code generation for QB64Fresh C backend.
//!
//! This module handles the emission of C code for all statement types,
//! including control flow, I/O operations, procedure definitions, and more.
//!
//! # Module Organization
//!
//! The statement emitter is split across multiple files for maintainability:
//!
//! - `mod.rs` (this file) - Core `StmtEmitter` struct and main `emit_stmt()` dispatcher
//! - `assignments.rs` - Assignment statement helpers
//! - `control_flow.rs` - IF, FOR, WHILE, DO, SELECT CASE
//! - `data.rs` - DATA/READ/RESTORE handling
//! - `def_fn.rs` - DEF FN single-line and multi-line functions
//! - `definitions.rs` - DIM, REDIM, SUB/FUNCTION definitions, DECLARE LIBRARY
//! - `error_jump.rs` - Error handling (ON ERROR) and computed jumps (ON...GOTO/GOSUB)
//! - `io.rs` - PRINT and INPUT helpers
//!
//! # Loop Handling
//!
//! Loops are tracked on a stack to support EXIT statements. Each loop
//! type (FOR, WHILE, DO) generates a break label that EXIT can target.

mod assignments;
mod control_flow;
mod data;
mod def_fn;
mod definitions;
mod error_jump;
mod io;

// Re-export standalone functions for use by parent module
pub(in crate::codegen::c_backend) use definitions::emit_params;

use std::collections::HashMap;

use crate::ast::{
    AllowFullScreenMode, EventControlMode, ExitType, FullScreenMode, ImageScaleMode, PrintSeparator,
};
use crate::codegen::error::{CodeGenError, CodeGenErrorKind};
use crate::semantic::typed_ir::{
    TypedExprKind, TypedInputTarget, TypedStatement, TypedStatementKind,
};
use crate::semantic::types::BasicType;
use crate::writeln_code;

use super::expr::{emit_expr, emit_string_data_access, escape_string};
use super::types::{c_identifier, c_type, default_init};

/// Context for the current loop (for EXIT statement handling).
#[derive(Clone)]
pub(super) struct LoopContext {
    /// Label to break to.
    pub break_label: String,
    /// Type of loop (For, While, Do).
    pub loop_type: ExitType,
}

/// State required for statement emission.
///
/// This is passed through recursive statement emission to track
/// indentation, loop context, and label generation.
pub(super) struct StmtEmitter {
    /// Counter for generating unique labels.
    pub label_counter: u32,
    /// Current indentation level.
    pub indent: usize,
    /// Stack of loop labels for EXIT statements.
    pub loop_stack: Vec<LoopContext>,
    /// Map of DATA labels to their indices (for RESTORE with label).
    pub data_label_indices: HashMap<String, usize>,
    /// Current procedure name (for unique label generation).
    pub current_proc: Option<String>,
    /// Current function's return variable (for EXIT FUNCTION).
    pub current_func_ret_var: Option<String>,
    /// Current function's byref STRING parameter names (for EXIT FUNCTION writebacks).
    /// These need to be written back before any return statement.
    pub current_func_byref_strings: Vec<String>,
    /// Global variable names (to avoid re-declaring as locals).
    pub global_var_names: std::collections::HashSet<String>,
    /// Global array variable names (arrays can't be implicitly declared as scalars).
    pub global_array_names: std::collections::HashSet<String>,
    /// DIM SHARED global variable names (accessible from all functions without local SHARED).
    pub shared_global_names: std::collections::HashSet<String>,
    /// Global CONST names (shouldn't be redeclared as local variables).
    pub global_const_names: std::collections::HashSet<String>,
    /// Counter for generating unique STRIG event IDs.
    pub strig_event_counter: u32,
    /// Registered STRIG event handlers: (event_id, target_label).
    pub strig_handlers: Vec<(u32, String)>,
    /// Debug mode enabled (emit qb_dbg_line calls).
    pub debug_enabled: bool,
    /// Source file name for debug tracking.
    pub debug_source_file: Option<String>,
    /// Disable SHELL / _SHELLHIDE (compile-time error if used). Set from CBackend via --no-shell.
    pub no_shell: bool,
    /// Labels already emitted (to skip duplicates from ambiguous parsing).
    pub emitted_labels: std::collections::HashSet<String>,
    /// Runtime mode (inline vs external) - affects how QbString data is accessed.
    pub runtime_mode: super::RuntimeMode,
}

impl StmtEmitter {
    /// Creates a new statement emitter.
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::with_runtime_mode(super::RuntimeMode::Inline)
    }

    /// Creates a new statement emitter with the specified runtime mode.
    pub fn with_runtime_mode(runtime_mode: super::RuntimeMode) -> Self {
        Self {
            label_counter: 0,
            indent: 0,
            loop_stack: Vec::new(),
            data_label_indices: HashMap::new(),
            current_proc: None,
            current_func_ret_var: None,
            current_func_byref_strings: Vec::new(),
            global_var_names: std::collections::HashSet::new(),
            global_array_names: std::collections::HashSet::new(),
            shared_global_names: std::collections::HashSet::new(),
            global_const_names: std::collections::HashSet::new(),
            strig_event_counter: 0,
            strig_handlers: Vec::new(),
            debug_enabled: false,
            debug_source_file: None,
            no_shell: false,
            emitted_labels: std::collections::HashSet::new(),
            runtime_mode,
        }
    }

    /// Generates a unique label name.
    pub fn next_label(&mut self, prefix: &str) -> String {
        let label = format!("_qb_{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    /// Converts a BASIC label to a C label, prefixing with procedure name if in a procedure.
    /// This ensures line number labels (e.g., _line_1) are unique per procedure.
    fn proc_label(&self, label: &str) -> String {
        let base_label = c_identifier(label);
        if let Some(ref proc) = self.current_proc {
            format!("{}_{}", proc, base_label)
        } else {
            base_label
        }
    }

    /// Returns the current indentation string.
    pub(crate) fn indent_str(&self) -> String {
        "    ".repeat(self.indent)
    }

    /// Determines if a statement is executable (should have debug hooks).
    ///
    /// Non-executable statements include:
    /// - Labels (just markers, no code)
    /// - DATA statements (compile-time data)
    /// - DIM/REDIM declarations (compile-time structure)
    /// - SUB/FUNCTION definitions (handled separately)
    /// - External declarations
    fn is_executable_statement(kind: &TypedStatementKind) -> bool {
        !matches!(
            kind,
            TypedStatementKind::Label { .. }
                | TypedStatementKind::Data { .. }
                | TypedStatementKind::SubDefinition { .. }
                | TypedStatementKind::FunctionDefinition { .. }
                | TypedStatementKind::DeclareLibrary { .. }
        )
    }

    /// Emits a debug line hook if debug mode is enabled.
    ///
    /// This calls `qb_dbg_line(line, file)` before executing the actual statement,
    /// allowing the debugger to check breakpoints and step mode.
    fn emit_debug_line(
        &self,
        stmt: &TypedStatement,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        if !self.debug_enabled {
            return Ok(());
        }

        // Extract line number from span
        // The span contains byte offsets; we need to get the source file name
        let line = stmt.span.start; // This is actually byte offset, but we'll use it for now
        let file = self
            .debug_source_file
            .as_deref()
            .unwrap_or("_qb_dbg_source_file");

        let indent = self.indent_str();

        // For now, use byte offset as line (the runtime can compute actual line number)
        // TODO: Store actual line numbers during parsing
        writeln_code!(
            output,
            "{}qb_dbg_line({}, {});",
            indent,
            line,
            if file == "_qb_dbg_source_file" {
                file.to_string()
            } else {
                format!("\"{}\"", file.replace('\\', "\\\\").replace('"', "\\\""))
            }
        )?;
        Ok(())
    }

    /// Emits a statement.
    pub fn emit_stmt(
        &mut self,
        stmt: &TypedStatement,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let indent = self.indent_str();

        // Emit debug line hook for executable statements
        // (skip labels, data, declarations that don't execute)
        if self.debug_enabled && Self::is_executable_statement(&stmt.kind) {
            self.emit_debug_line(stmt, output)?;
        }

        match &stmt.kind {
            TypedStatementKind::Assignment {
                name,
                value,
                target_type,
            } => {
                self.emit_assignment(&indent, name, value, target_type, output)?;
            }

            TypedStatementKind::ArrayAssignment {
                name,
                indices,
                value,
                dimensions,
                element_type,
            } => {
                self.emit_array_assignment(
                    &indent,
                    name,
                    indices,
                    value,
                    dimensions,
                    element_type,
                    output,
                )?;
            }

            TypedStatementKind::ArrayFieldAssignment {
                name,
                indices,
                fields,
                value,
                dimensions,
                element_type: _,
                field_type,
            } => {
                self.emit_array_field_assignment(
                    &indent, name, indices, fields, value, dimensions, field_type, output,
                )?;
            }

            TypedStatementKind::FieldAssignment {
                name,
                fields,
                value,
                field_type,
            } => {
                self.emit_field_assignment(&indent, name, fields, value, field_type, output)?;
            }

            TypedStatementKind::MidAssignment {
                target,
                start,
                length,
                value,
            } => {
                // Target is an lvalue (variable, array element, or field access)
                let target_code = emit_expr(target, self.no_shell)?;
                let start_code = emit_expr(start, self.no_shell)?;
                let value_code = emit_expr(value, self.no_shell)?;

                // Check if target is a fixed-length string (char array)
                // Fixed-length strings need manual character copying, not qb_mid_assign
                if matches!(
                    target.basic_type,
                    crate::semantic::types::BasicType::FixedString(_)
                ) {
                    // For fixed-length strings, manually copy characters
                    let len_code = if let Some(len_expr) = length {
                        emit_expr(len_expr, self.no_shell)?
                    } else {
                        // No length specified - replace rest of string
                        format!("(int32_t)(strlen({}) - ({} - 1))", target_code, start_code)
                    };
                    let data_access = match self.runtime_mode {
                        super::RuntimeMode::External => "qb_string_data(_mid_val)",
                        super::RuntimeMode::Inline => "_mid_val->data",
                    };
                    writeln_code!(
                        output,
                        "{} {{ qb_string* _mid_val = {}; if (_mid_val) {{ int32_t _mid_start = {} - 1; int32_t _mid_len = {}; int32_t _mid_copy_len = _mid_len < (int32_t)strlen({}) ? _mid_len : (int32_t)strlen({}); if (_mid_start >= 0 && _mid_start < (int32_t)strlen({})) {{ strncpy({} + _mid_start, {}, _mid_copy_len); }} }} }}",
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
                } else {
                    // For dynamic strings (QbString*), use qb_mid_assign
                    // We need to pass its address to qb_mid_assign
                    if let Some(len_expr) = length {
                        let len_code = emit_expr(len_expr, self.no_shell)?;
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
                        // No length specified - use -1 to indicate "rest of string"
                        writeln_code!(
                            output,
                            "{}qb_mid_assign(&({}), {}, -1, {});",
                            indent,
                            target_code,
                            start_code,
                            value_code
                        )?;
                    }
                }
            }

            TypedStatementKind::AscAssignment {
                target,
                position,
                value,
            } => {
                // ASC(str$, pos) = value sets a single character in a string
                // We need to pass the target's address to qb_asc_assign
                let target_code = emit_expr(target, self.no_shell)?;
                let position_code = emit_expr(position, self.no_shell)?;
                let value_code = emit_expr(value, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_asc_assign(&({}), {}, {});",
                    indent,
                    target_code,
                    position_code,
                    value_code
                )?;
            }

            TypedStatementKind::Print { items, newline } => {
                for item in items {
                    self.emit_print_item(item, output)?;
                }
                if *newline {
                    writeln_code!(output, "{}qb_print_newline();", indent)?;
                }
            }

            TypedStatementKind::PrintUsing {
                format,
                values,
                newline,
            } => {
                let format_code = emit_expr(format, self.no_shell)?;
                // Generate code to print each value using the format string
                // We use a runtime function that handles format string parsing
                if values.is_empty() {
                    // Just print the format string as-is if no values
                    writeln_code!(
                        output,
                        "{}qb_print_using({}, NULL, 0);",
                        indent,
                        format_code
                    )?;
                } else {
                    // Build array of values
                    writeln_code!(output, "{}{{", indent)?;
                    writeln_code!(output, "{}    QbPrintValue _pv[{}];", indent, values.len())?;
                    for (i, value) in values.iter().enumerate() {
                        let value_code = emit_expr(value, self.no_shell)?;
                        match &value.basic_type {
                            BasicType::String => {
                                writeln_code!(
                                    output,
                                    "{}    _pv[{}].type = QB_TYPE_STRING; _pv[{}].str_val = {};",
                                    indent,
                                    i,
                                    i,
                                    value_code
                                )?;
                            }
                            BasicType::Integer | BasicType::Long => {
                                writeln_code!(
                                    output,
                                    "{}    _pv[{}].type = QB_TYPE_INT; _pv[{}].int_val = (int64_t){};",
                                    indent,
                                    i,
                                    i,
                                    value_code
                                )?;
                            }
                            BasicType::Single | BasicType::Double => {
                                writeln_code!(
                                    output,
                                    "{}    _pv[{}].type = QB_TYPE_DOUBLE; _pv[{}].dbl_val = (double){};",
                                    indent,
                                    i,
                                    i,
                                    value_code
                                )?;
                            }
                            _ => {
                                // For other types, try to convert to double
                                writeln_code!(
                                    output,
                                    "{}    _pv[{}].type = QB_TYPE_DOUBLE; _pv[{}].dbl_val = (double){};",
                                    indent,
                                    i,
                                    i,
                                    value_code
                                )?;
                            }
                        }
                    }
                    writeln_code!(
                        output,
                        "{}    qb_print_using({}, _pv, {});",
                        indent,
                        format_code,
                        values.len()
                    )?;
                    writeln_code!(output, "{}}}", indent)?;
                }
                if *newline {
                    writeln_code!(output, "{}qb_print_newline();", indent)?;
                }
            }

            TypedStatementKind::Input {
                prompt,
                show_question_mark,
                same_line,
                targets,
            } => {
                self.emit_input(
                    &indent,
                    prompt,
                    *show_question_mark,
                    *same_line,
                    targets,
                    output,
                )?;
            }

            TypedStatementKind::LineInput { prompt, target } => {
                use TypedInputTarget::*;

                let target_code = match target {
                    Variable { name, .. } => c_identifier(name),
                    ArrayElement { name, indices, .. } => {
                        let c_arr = c_identifier(name);
                        let idx_code: Vec<_> = indices
                            .iter()
                            .map(|e| emit_expr(e, self.no_shell))
                            .collect::<Result<_, _>>()?;
                        let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                        format!("{}[{}]", c_arr, idx)
                    }
                    ArrayElementField {
                        name,
                        indices,
                        fields,
                        ..
                    } => {
                        let c_arr = c_identifier(name);
                        let idx_code: Vec<_> = indices
                            .iter()
                            .map(|e| emit_expr(e, self.no_shell))
                            .collect::<Result<_, _>>()?;
                        let idx = idx_code.first().map(|s| s.as_str()).unwrap_or("0");
                        let field_chain = fields.join(".");
                        format!("{}[{}].{}", c_arr, idx, field_chain)
                    }
                    Field { name, fields, .. } => {
                        let c_name = c_identifier(name);
                        let field_chain = fields.join(".");
                        format!("{}.{}", c_name, field_chain)
                    }
                };
                let prompt_arg = match prompt {
                    Some(p) => format!("\"{}\"", escape_string(p)),
                    None => "NULL".to_string(),
                };
                writeln_code!(
                    output,
                    "{}qb_input_string({}, &{});",
                    indent,
                    prompt_arg,
                    target_code
                )?;
            }

            TypedStatementKind::If {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                self.emit_if(
                    &indent,
                    condition,
                    then_branch,
                    elseif_branches,
                    else_branch,
                    output,
                )?;
            }

            TypedStatementKind::SelectCase {
                test_expr,
                cases,
                case_else,
            } => {
                self.emit_select_case(&indent, test_expr, cases, case_else, false, output)?;
            }

            TypedStatementKind::SelectEveryCase {
                test_expr,
                cases,
                case_else,
            } => {
                self.emit_select_case(&indent, test_expr, cases, case_else, true, output)?;
            }

            TypedStatementKind::For {
                variable,
                var_type,
                start,
                end,
                step,
                body,
            } => {
                self.emit_for(&indent, variable, var_type, start, end, step, body, output)?;
            }

            TypedStatementKind::While { condition, body } => {
                self.emit_while(&indent, condition, body, output)?;
            }

            TypedStatementKind::DoLoop {
                pre_condition,
                body,
                post_condition,
            } => {
                self.emit_do_loop(&indent, pre_condition, body, post_condition, output)?;
            }

            TypedStatementKind::Goto { target } => {
                let c_label = self.proc_label(target);
                writeln_code!(output, "{}goto {};", indent, c_label)?;
            }

            TypedStatementKind::Gosub { target } => {
                let c_label = self.proc_label(target);
                let return_label = self.next_label("gosub_ret");
                // Push return address onto stack and jump to subroutine
                writeln_code!(
                    output,
                    "{}_gosub_stack[_gosub_sp++] = &&{};",
                    indent,
                    return_label
                )?;
                writeln_code!(output, "{}goto {};", indent, c_label)?;
                writeln_code!(output, "{}{}:;", indent, return_label)?;
            }

            TypedStatementKind::Return => {
                // RETURN from GOSUB - pop return address from stack and jump
                writeln_code!(
                    output,
                    "{}if (_gosub_sp > 0) goto *_gosub_stack[--_gosub_sp];",
                    indent
                )?;
            }

            TypedStatementKind::Exit { exit_type } => {
                self.emit_exit(&indent, exit_type, output)?;
            }

            TypedStatementKind::End { exit_code } => {
                if let Some(code) = exit_code {
                    let code_expr = emit_expr(code, self.no_shell)?;
                    writeln_code!(output, "{}exit((int){});", indent, code_expr)?;
                } else {
                    writeln_code!(output, "{}exit(0);", indent)?;
                }
            }

            TypedStatementKind::Stop => {
                writeln_code!(output, "{}/* STOP */", indent)?;
                writeln_code!(output, "{}exit(1);", indent)?;
            }

            TypedStatementKind::System { exit_code } => {
                if let Some(code) = exit_code {
                    let code_expr = emit_expr(code, self.no_shell)?;
                    writeln_code!(output, "{}exit((int){});", indent, code_expr)?;
                } else {
                    writeln_code!(output, "{}exit(0);", indent)?;
                }
            }

            TypedStatementKind::Sleep { seconds } => {
                if let Some(secs) = seconds {
                    let secs_code = emit_expr(secs, self.no_shell)?;
                    writeln_code!(output, "{}qb_sleep((int){});", indent, secs_code)?;
                } else {
                    // No argument - wait for keypress
                    writeln_code!(output, "{}qb_sleep_keypress();", indent)?;
                }
            }

            TypedStatementKind::Wait {
                port,
                and_mask,
                xor_mask,
            } => {
                let port_code = emit_expr(port, self.no_shell)?;
                let and_code = emit_expr(and_mask, self.no_shell)?;
                if let Some(xor) = xor_mask {
                    let xor_code = emit_expr(xor, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}qb_wait((int){}, (int){}, (int){});",
                        indent,
                        port_code,
                        and_code,
                        xor_code
                    )?;
                } else {
                    writeln_code!(
                        output,
                        "{}qb_wait((int){}, (int){}, 0);",
                        indent,
                        port_code,
                        and_code
                    )?;
                }
            }

            TypedStatementKind::Delay { seconds } => {
                let secs_code = emit_expr(seconds, self.no_shell)?;
                writeln_code!(output, "{}qb_delay({});", indent, secs_code)?;
            }

            TypedStatementKind::Limit { fps } => {
                let fps_code = emit_expr(fps, self.no_shell)?;
                writeln_code!(output, "{}qb_limit((int){});", indent, fps_code)?;
                // STRIG event check after _LIMIT (common in game loops)
                let return_label = self.next_label("strig_ret");
                writeln_code!(output, "{}/* STRIG event check */", indent)?;
                writeln_code!(
                    output,
                    "{}_qb_strig_event_id = qb_strig_check_event();",
                    indent
                )?;
                writeln_code!(output, "{}if (_qb_strig_event_id) {{", indent)?;
                writeln_code!(
                    output,
                    "{}    _gosub_stack[_gosub_sp++] = &&{};",
                    indent,
                    return_label
                )?;
                writeln_code!(output, "{}    goto _qb_strig_dispatch;", indent)?;
                writeln_code!(output, "{}}}", indent)?;
                writeln_code!(output, "{}{}:;", indent, return_label)?;
                writeln_code!(output, "{}qb_strig_event_done();", indent)?;
            }

            TypedStatementKind::Erase { arrays } => {
                for array_name in arrays {
                    let c_name = c_identifier(array_name).to_lowercase();
                    writeln_code!(output, "{}qb_array_erase(&arr_{});", indent, c_name)?;
                }
            }

            TypedStatementKind::KeyClear => {
                writeln_code!(output, "{}qb_keyclear();", indent)?;
            }

            TypedStatementKind::Call { name, args, params } => {
                // Generate arguments, adding & for byref parameters
                // For non-lvalue expressions passed to byref, we need temp vars
                let mut args_codes = Vec::new();
                let mut temp_decls = Vec::new();
                let mut temp_counter = 0;

                for (i, arg) in args.iter().enumerate() {
                    let arg_code = emit_expr(arg, self.no_shell)?;
                    // Check if this parameter is byref (and not an array ref which decays to pointer)
                    let is_byref = params
                        .get(i)
                        .map(|p| !p.by_val && !p.is_array)
                        .unwrap_or(false);
                    if is_byref {
                        // For byref, we need to pass the address
                        // Check if expression is an lvalue (can take address of)
                        // Note: Built-in constants like _TRUE, _FALSE are Variables in the AST
                        // but expand to C macros, so they're not true lvalues
                        let is_builtin_const = matches!(&arg.kind, TypedExprKind::Variable(name)
                            if name.starts_with('_') && name.chars().all(|c| c.is_uppercase() || c == '_'));

                        let is_lvalue = !is_builtin_const
                            && matches!(
                                arg.kind,
                                TypedExprKind::Variable { .. }
                                    | TypedExprKind::ArrayAccess { .. }
                                    | TypedExprKind::FieldAccess { .. }
                            );

                        // Check if argument type matches parameter type
                        // In BASIC, passing a LONG to a function expecting INTEGER% creates
                        // an implicit temporary. The function gets a pointer to the temp, not
                        // to the original variable.
                        let param_type = params.get(i).map(|p| &p.basic_type);
                        let types_match =
                            param_type.map(|pt| pt == &arg.basic_type).unwrap_or(true);

                        if is_lvalue && types_match {
                            // Variable/array/field can be addressed directly
                            args_codes.push(format!("&({})", arg_code));
                        } else if is_lvalue && !types_match {
                            // Lvalue but types don't match - need temporary with correct type
                            // This handles cases like passing LONG to INTEGER% parameter
                            let c_ty = param_type
                                .map(c_type)
                                .unwrap_or_else(|| "int32_t".to_string());
                            let temp_name = format!("_tmp_arg_{}", temp_counter);
                            temp_counter += 1;
                            // Cast the value to parameter type
                            temp_decls.push(format!(
                                "{} {} = ({})({});",
                                c_ty, temp_name, c_ty, arg_code
                            ));
                            args_codes.push(format!("&{}", temp_name));
                        } else {
                            // Non-lvalue expression - need a temporary variable
                            let param_type = params.get(i).map(|p| &p.basic_type);
                            let c_ty = param_type
                                .map(c_type)
                                .unwrap_or_else(|| "int32_t".to_string());
                            let temp_name = format!("_tmp_arg_{}", temp_counter);
                            temp_counter += 1;
                            temp_decls.push(format!("{} {} = {};", c_ty, temp_name, arg_code));
                            args_codes.push(format!("&{}", temp_name));
                        }
                    } else {
                        args_codes.push(arg_code);
                    }
                }
                let args_str = args_codes.join(", ");
                let upper_name = name.to_uppercase();

                // Check for built-in SUBs with special C function names
                // Some have variable argument counts requiring different function names
                let c_name = match upper_name.as_str() {
                    "_ICON" => match args.len() {
                        0 => "qb_icon".to_string(),
                        1 => "qb_icon1".to_string(),
                        _ => "qb_icon2".to_string(),
                    },
                    "_ACCEPTFILEDROP" => match args.len() {
                        0 => "qb_acceptfiledrop".to_string(),
                        _ => "qb_acceptfiledrop1".to_string(),
                    },
                    "_WRITEFILE" => "qb_writefile".to_string(),
                    "_EXIT" => "qb_exit".to_string(),
                    "_FINISHDROP" => "qb_finishdrop".to_string(),
                    "_CONSOLECURSOR" => "qb_consolecursor".to_string(),
                    "_CONSOLEFONT" => "qb_consolefont".to_string(),
                    "_CONTROLCHR" => "qb_controlchr".to_string(),
                    "_SETALPHA" => "qb_setalpha".to_string(),
                    // _PALETTECOLOR statement: attr, color[, handle]
                    // 2 args = use current image (handle 0), 3 args = explicit handle
                    "_PALETTECOLOR" => {
                        match args.len() {
                            2 => {
                                // Add implicit 0 for handle
                                let full_args = format!("{}, 0", args_str);
                                writeln_code!(output, "{}qb_palettecolor({});", indent, full_args)?;
                                return Ok(());
                            }
                            _ => "qb_palettecolor".to_string(),
                        }
                    }
                    "_COPYPALETTE" => "qb_copypalette".to_string(),
                    "_BLEND" => "qb_blend".to_string(),
                    "_DONTBLEND" => "qb_dontblend".to_string(),
                    "_CLEARCOLOR" => "qb_clearcolor".to_string(),
                    "_DEPTHBUFFER" => "qb_depthbuffer".to_string(),
                    "_DISPLAYORDER" => "qb_displayorder".to_string(),
                    "_SNDLIMIT" => "qb_sndlimit".to_string(),
                    "_HIDE" => "qb_hide".to_string(),
                    "_SHOW" => "qb_show".to_string(),
                    "_ONTOP" => "qb_ontop".to_string(),
                    "_PRINTMODE" => "qb_printmode".to_string(),
                    // Session 032+ SUBs
                    "_SAVEIMAGE" => "qb_saveimage".to_string(),
                    "_SCREENPRINT" => "qb_screenprint".to_string(),
                    "_UPRINTSTRING" => "qb_uprintstring".to_string(),
                    "_MAPUNICODE" => "qb_mapunicode".to_string(),
                    "_LOGTRACE" => "qb_logtrace".to_string(),
                    "_LOGINFO" => "qb_loginfo".to_string(),
                    "_LOGWARN" => "qb_logwarn".to_string(),
                    "_LOGERROR" => "qb_logerror".to_string(),
                    "_LOGMINLEVEL" => "qb_logminlevel".to_string(),
                    "_SNDRAWBATCH" => "qb_sndrawbatch".to_string(),
                    "_MIDISOUNDBANK" => "qb_midisoundbank".to_string(),
                    "_NEWHANDLER" => "qb_newhandler".to_string(),
                    // Session 033+ SUBs
                    "_PRINTIMAGE" => "qb_printimage".to_string(),
                    "_CLEAR" => "qb_clear_resource".to_string(),
                    "_TOGGLE" => "qb_toggle".to_string(),
                    "_MAPTRIANGLE" => "qb_maptriangle".to_string(),
                    "_GLRENDER" => "qb_glrender".to_string(),
                    // Session 034+ SUBs (only non-duplicates)
                    "_ECHO" => "qb_echo".to_string(),
                    "_CONSOLETITLE" => "qb_consoletitle".to_string(),
                    "_CLIPBOARD" => "qb_clipboard_set".to_string(),
                    "_ANTIALIASING" => "qb_antialiasing".to_string(),
                    "_ALLOWFULLSCREEN" => "qb_allowfullscreen".to_string(),
                    "_FULLSCREEN" => "qb_fullscreen".to_string(),
                    "_SCREENMOVE" => "qb_screenmove".to_string(),
                    "_SCREENSHOW" => "qb_screenshow".to_string(),
                    "_SCREENHIDE" => "qb_screenhide".to_string(),
                    "_SCREENCLICK" => "qb_screenclick".to_string(),
                    "_SCREENIMAGE" => "qb_screenimage".to_string(),
                    "_DELAY" => "qb_delay".to_string(),
                    // Session 035+ SUBs (memory operations)
                    "_MEMPUT" => "qb_memput".to_string(),
                    "_MEMFILL" => "qb_memfill".to_string(),
                    "_MEMCOPY" => "qb_memcopy".to_string(),
                    "_MEMFREE" => "qb_memfree".to_string(),
                    "_SCREENICON" => "qb_screenicon".to_string(),
                    // Default: user-defined SUBs use qb_sub_ prefix
                    _ => format!("qb_sub_{}", c_identifier(name).to_lowercase()),
                };

                // If we have temp declarations, wrap in a block
                if temp_decls.is_empty() {
                    writeln_code!(output, "{}{}({});", indent, c_name, args_str)?;
                } else {
                    writeln_code!(output, "{}{{ ", indent)?;
                    for decl in temp_decls {
                        writeln_code!(output, "{}    {}", indent, decl)?;
                    }
                    writeln_code!(output, "{}    {}({});", indent, c_name, args_str)?;
                    writeln_code!(output, "{}}}", indent)?;
                }
            }

            TypedStatementKind::SubDefinition {
                name,
                params,
                body,
                is_static: _,
            } => {
                self.emit_sub_definition(&indent, name, params, body, output)?;
            }

            TypedStatementKind::FunctionDefinition {
                name,
                params,
                return_type,
                body,
                is_static: _,
            } => {
                self.emit_function_definition(&indent, name, params, return_type, body, output)?;
            }

            TypedStatementKind::Dim {
                variables,
                shared: _,
            } => {
                // Scalar DIMs are hoisted to function scope by collect_implicit_locals
                // Only emit arrays here (they need runtime allocation)
                for var in variables {
                    if !var.dimensions.is_empty() {
                        self.emit_dim(
                            &indent,
                            &var.name,
                            &var.basic_type,
                            &var.dimensions,
                            output,
                        )?;
                    }
                }
            }

            TypedStatementKind::Const { definitions } => {
                for (name, value, _basic_type) in definitions {
                    let c_name = c_identifier(name);
                    let value_code = emit_expr(value, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}const {} {} = {};",
                        indent,
                        c_type(&value.basic_type),
                        c_name,
                        value_code
                    )?;
                }
            }

            TypedStatementKind::DefType => {
                // DEFxxx statements affect type inference but generate no C code
            }

            TypedStatementKind::Define => {
                // _DEFINE affects type inference but generates no C code
            }

            TypedStatementKind::OptionBase => {
                // OPTION BASE affects array bounds but generates no C code
            }

            TypedStatementKind::OptionExplicit => {
                // OPTION _EXPLICIT affects semantic checking but generates no C code
            }

            TypedStatementKind::OptionExplicitArray => {
                // OPTION _EXPLICITARRAY affects semantic checking but generates no C code
            }

            TypedStatementKind::DefSeg { segment } => {
                // DEF SEG is a legacy statement for memory segment manipulation.
                // In modern QB64, this is largely a no-op, but we can emit a runtime call
                // for compatibility with PEEK/POKE/BLOAD/BSAVE.
                if let Some(seg_expr) = segment {
                    let seg_code = emit_expr(seg_expr, self.no_shell)?;
                    writeln_code!(output, "{}qb_def_seg((int32_t){});", indent, seg_code)?;
                } else {
                    // DEF SEG without argument resets to default segment
                    writeln_code!(output, "{}qb_def_seg(-1);", indent)?;
                }
            }

            TypedStatementKind::Poke { address, value } => {
                // POKE writes a byte to memory within the current segment.
                let addr_code = emit_expr(address, self.no_shell)?;
                let val_code = emit_expr(value, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_poke((int32_t){}, (uint8_t){});",
                    indent,
                    addr_code,
                    val_code
                )?;
            }

            TypedStatementKind::MemPutTyped {
                mem,
                offset,
                value,
                value_type,
            } => {
                // _MEMPUT writes a value to memory at the given offset,
                // interpreting the value as the specified type.
                // Generated code: *((type*)((char*)(mem).offset + (offset))) = (value);
                let mem_code = emit_expr(mem, self.no_shell)?;
                let offset_code = emit_expr(offset, self.no_shell)?;
                let value_code = emit_expr(value, self.no_shell)?;
                let c_ty = c_type(value_type);
                writeln_code!(
                    output,
                    "{indent}*(({c_ty}*)((char*)({mem_code}).offset + ({offset_code}))) = ({c_ty})({value_code});"
                )?;
            }

            TypedStatementKind::Label { name } => {
                let c_label = self.proc_label(name);
                // Skip duplicate labels (can occur from ambiguous parsing of
                // "SubName: AnotherSub" patterns that look like labels)
                if self.emitted_labels.insert(c_label.clone()) {
                    writeln_code!(output, "{}:", c_label)?;
                }
            }

            TypedStatementKind::Comment(text) => {
                writeln_code!(output, "{}/* {} */", indent, text)?;
            }

            TypedStatementKind::Expression(expr) => {
                let expr_code = emit_expr(expr, self.no_shell)?;
                writeln_code!(output, "{}{};", indent, expr_code)?;
            }

            TypedStatementKind::IncludeDirective { path } => {
                writeln_code!(output, "{}/* $INCLUDE: '{}' */", indent, path)?;
            }

            TypedStatementKind::ConditionalBlock {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                // Unevaluated conditional block - emit all branches as comments
                // (This case should be rare now that conditions are evaluated)
                writeln_code!(output, "{}/* $IF {} */", indent, condition)?;
                for s in then_branch {
                    self.emit_stmt(s, output)?;
                }
                for (elseif_cond, elseif_body) in elseif_branches {
                    writeln_code!(output, "{}/* $ELSEIF {} */", indent, elseif_cond)?;
                    for s in elseif_body {
                        self.emit_stmt(s, output)?;
                    }
                }
                if let Some(else_body) = else_branch {
                    writeln_code!(output, "{}/* $ELSE */", indent)?;
                    for s in else_body {
                        self.emit_stmt(s, output)?;
                    }
                }
                writeln_code!(output, "{}/* $END IF */", indent)?;
            }

            TypedStatementKind::ConditionalBlockResolved {
                original_condition,
                statements,
            } => {
                // Evaluated conditional block - only emit the selected branch
                if !statements.is_empty() {
                    writeln_code!(
                        output,
                        "{}/* Conditional compilation: {} */",
                        indent,
                        original_condition
                    )?;
                    for s in statements {
                        self.emit_stmt(s, output)?;
                    }
                }
                // If statements is empty, no code is emitted (condition was false
                // and there was no matching branch)
            }

            TypedStatementKind::MetaCommand { command, args } => {
                let args_str = args.as_deref().unwrap_or("");
                writeln_code!(output, "{}/* ${} {} */", indent, command, args_str)?;
            }

            TypedStatementKind::MetaLet { name, value } => {
                // Compile-time variable assignment - generates a comment
                writeln_code!(output, "{}/* $LET {} = {} */", indent, name, value)?;
            }

            TypedStatementKind::MetaChecking { enabled } => {
                // Compile-time bounds checking directive - generates a comment
                let state = if *enabled { "ON" } else { "OFF" };
                writeln_code!(output, "{}/* $CHECKING:{} */", indent, state)?;
            }

            TypedStatementKind::MetaConsole { only } => {
                // Console mode directive - affects program initialization
                // For now, generate a comment; actual console setup is runtime-dependent
                if *only {
                    writeln_code!(output, "{}/* $CONSOLE:ONLY - console-only mode */", indent)?;
                } else {
                    writeln_code!(output, "{}/* $CONSOLE - enable console window */", indent)?;
                }
            }

            TypedStatementKind::MetaScreenHide => {
                // Hide graphics window on startup
                writeln_code!(output, "{}/* $SCREENHIDE */", indent)?;
            }

            TypedStatementKind::MetaScreenShow => {
                // Show graphics window on startup (default)
                writeln_code!(output, "{}/* $SCREENSHOW */", indent)?;
            }

            TypedStatementKind::Swap { left, right } => {
                let left_code = emit_expr(left, self.no_shell)?;
                let right_code = emit_expr(right, self.no_shell)?;
                let temp_var = self.next_label("swap_temp");

                // Fixed-length strings need special handling (C arrays can't be assigned directly)
                if let BasicType::FixedString(n) = &left.basic_type {
                    // For fixed-length strings, use strcpy for the swap
                    writeln_code!(output, "{}{{ char {}[{}];", indent, temp_var, n + 1)?;
                    writeln_code!(output, "{}    strcpy({}, {});", indent, temp_var, left_code)?;
                    writeln_code!(
                        output,
                        "{}    strcpy({}, {});",
                        indent,
                        left_code,
                        right_code
                    )?;
                    writeln_code!(
                        output,
                        "{}    strcpy({}, {}); }}",
                        indent,
                        right_code,
                        temp_var
                    )?;
                } else {
                    let c_ty = c_type(&left.basic_type);
                    writeln_code!(output, "{}{} {} = {};", indent, c_ty, temp_var, left_code)?;
                    writeln_code!(output, "{}{} = {};", indent, left_code, right_code)?;
                    writeln_code!(output, "{}{} = {};", indent, right_code, temp_var)?;
                }
            }

            TypedStatementKind::Continue { continue_type } => {
                let _ = continue_type;
                writeln_code!(output, "{}continue;", indent)?;
            }

            TypedStatementKind::TypeDefinition { .. } => {
                // TYPE definitions are collected and emitted upfront by collect_type_definitions()
                // in mod.rs before global variables, so we skip them here.
            }

            TypedStatementKind::Data { .. } => {
                // DATA statements are handled in collect_data_values during global emission
            }

            TypedStatementKind::Read { targets } => {
                self.emit_read(&indent, targets, output)?;
            }

            TypedStatementKind::Restore { label } => {
                self.emit_restore(&indent, label, output)?;
            }

            TypedStatementKind::Randomize { seed } => {
                if let Some(seed_expr) = seed {
                    // RANDOMIZE expr - seed with specific value
                    // TIMER is just a function call in the expression, no special handling needed
                    let seed_code = emit_expr(seed_expr, self.no_shell)?;
                    writeln_code!(output, "{}qb_randomize((double)({}));", indent, seed_code)?;
                } else {
                    // RANDOMIZE without arguments - for compatibility, use timer
                    // (in original BASIC, this would prompt the user)
                    writeln_code!(output, "{}qb_randomize_timer();", indent)?;
                }
            }

            // ==================== File I/O Statements ====================
            TypedStatementKind::OpenFile {
                filename,
                mode,
                access,
                lock,
                file_num,
                record_len,
            } => {
                self.emit_open_file(
                    &indent,
                    filename,
                    *mode,
                    *access,
                    *lock,
                    file_num,
                    record_len.as_ref(),
                    output,
                )?;
            }

            TypedStatementKind::OpenFileLegacy {
                mode_expr,
                file_num,
                filename,
                record_len,
            } => {
                self.emit_open_file_legacy(
                    &indent,
                    mode_expr,
                    file_num,
                    filename,
                    record_len.as_ref(),
                    output,
                )?;
            }

            TypedStatementKind::CloseFile { file_nums } => {
                self.emit_close_file(&indent, file_nums, output)?;
            }

            TypedStatementKind::FilePrint {
                file_num,
                items,
                newline,
            } => {
                self.emit_file_print(&indent, file_num, items, *newline, output)?;
            }

            TypedStatementKind::FileWrite { file_num, values } => {
                self.emit_file_write(&indent, file_num, values, output)?;
            }

            TypedStatementKind::FileInput { file_num, targets } => {
                self.emit_file_input(&indent, file_num, targets, output)?;
            }

            TypedStatementKind::FileLineInput { file_num, target } => {
                self.emit_file_line_input(&indent, file_num, target, output)?;
            }

            TypedStatementKind::FileGet {
                file_num,
                position,
                target,
            } => {
                self.emit_file_get(&indent, file_num, position.as_ref(), target, output)?;
            }

            TypedStatementKind::FilePut {
                file_num,
                position,
                target,
            } => {
                self.emit_file_put(&indent, file_num, position.as_ref(), target, output)?;
            }

            TypedStatementKind::FileSeek { file_num, position } => {
                self.emit_file_seek(&indent, file_num, position, output)?;
            }

            // ==================== Error Handling Statements ====================
            TypedStatementKind::OnErrorGoto { target } => {
                self.emit_on_error_goto(&indent, target, output)?;
            }

            TypedStatementKind::OnErrorResumeNext => {
                self.emit_on_error_resume_next(&indent, output)?;
            }

            TypedStatementKind::ResumeStmt { target } => {
                self.emit_resume(&indent, target, output)?;
            }

            TypedStatementKind::ErrorStmt { code } => {
                self.emit_error_stmt(&indent, code, output)?;
            }

            // ==================== Computed Control Flow ====================
            TypedStatementKind::OnGoto { selector, targets } => {
                self.emit_on_goto(&indent, selector, targets, output)?;
            }

            TypedStatementKind::OnGosub { selector, targets } => {
                self.emit_on_gosub(&indent, selector, targets, output)?;
            }

            // ==================== DEF FN ====================
            TypedStatementKind::DefFn {
                name,
                params,
                return_type,
                body,
            } => {
                self.emit_def_fn(name, params, return_type, body, output)?;
            }

            TypedStatementKind::DefFnMultiLine {
                name,
                params,
                return_type,
                body,
            } => {
                self.emit_def_fn_multiline(name, params, return_type, body, output)?;
            }

            // ==================== Variable/Scope Statements ====================
            TypedStatementKind::CommonStmt { shared, variables } => {
                // COMMON is handled at program level, emit a comment here
                let _ = shared;
                let _ = variables;
                writeln_code!(
                    output,
                    "{}/* COMMON statement - handled at program level */",
                    indent
                )?;
            }

            TypedStatementKind::SharedStmt { variables } => {
                // SHARED inside SUB/FUNCTION declares access to module-level shared vars
                // In C, these are already global, so just emit a comment
                let _ = variables;
                writeln_code!(
                    output,
                    "{}/* SHARED statement - variables accessed from module level */",
                    indent
                )?;
            }

            TypedStatementKind::StaticStmt { variables } => {
                // STATIC inside SUB/FUNCTION declares static local variables
                // In C, these are declared with the `static` keyword
                for var in variables {
                    let c_name = c_identifier(&var.name);
                    let c_ty = c_type(&var.basic_type);

                    if var.dimensions.is_empty() {
                        // Simple static variable
                        // For strings, we must use NULL because function calls
                        // are not valid in static initializers in C
                        let init = match var.basic_type {
                            BasicType::String => "NULL".to_string(),
                            _ => default_init(&var.basic_type),
                        };
                        writeln_code!(output, "{}static {} {} = {};", indent, c_ty, c_name, init)?;
                    } else {
                        // Static array
                        let sizes: Vec<String> = var
                            .dimensions
                            .iter()
                            .map(|d| format!("{}", d.upper - d.lower + 1))
                            .collect();
                        let array_dims = sizes.join("][");
                        writeln_code!(
                            output,
                            "{}static {} {}[{}] = {{0}};",
                            indent,
                            c_ty,
                            c_name,
                            array_dims
                        )?;
                    }
                }
            }

            TypedStatementKind::Redim {
                preserve,
                shared: _shared,
                variables,
            } => {
                // Note: SHARED affects symbol visibility (handled in semantic analysis),
                // but the generated code is the same - the array is allocated dynamically.
                // For C codegen, SHARED arrays are just global variables that can be
                // redimensioned at runtime.
                for var in variables {
                    self.emit_redim(
                        &indent,
                        *preserve,
                        &var.name,
                        &var.element_type,
                        &var.dimensions,
                        output,
                    )?;
                }
            }

            // ==================== Graphics Statements ====================
            TypedStatementKind::Screen {
                mode,
                color_switch,
                active_page,
                visual_page,
            } => {
                // SCREEN [mode][,[colorswitch]][,[apage]][,[vpage]]
                let mode_code = mode
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let color_code = color_switch
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let apage_code = active_page
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let vpage_code = visual_page
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                writeln_code!(
                    output,
                    "{}qb_gfx_screen((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){});",
                    indent,
                    mode_code,
                    color_code,
                    apage_code,
                    vpage_code
                )?;
            }

            TypedStatementKind::Cls { mode } => {
                if let Some(mode_expr) = mode {
                    let mode_code = emit_expr(mode_expr, self.no_shell)?;
                    writeln_code!(output, "{}qb_gfx_cls_mode((int32_t){});", indent, mode_code)?;
                } else {
                    writeln_code!(output, "{}qb_gfx_cls();", indent)?;
                }
            }

            TypedStatementKind::Color {
                foreground,
                background,
                border,
            } => {
                // Use -1 as sentinel for "unchanged" - runtime will check this
                let fg_code = foreground
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let bg_code = background
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                // Border is ignored in modern systems (was CGA/EGA text mode only)
                // We accept it for compatibility but don't use it
                let _border_code = border
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?;
                writeln_code!(
                    output,
                    "{}qb_gfx_color((int32_t){}, (int32_t){});",
                    indent,
                    fg_code,
                    bg_code
                )?;
            }

            TypedStatementKind::Locate { row, col } => {
                // LOCATE with optional parameters - use -1 to indicate "unchanged"
                let row_code = row
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or("-1".to_string());
                let col_code = col
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or("-1".to_string());
                writeln_code!(
                    output,
                    "{}qb_gfx_locate((int32_t){}, (int32_t){});",
                    indent,
                    row_code,
                    col_code
                )?;
            }

            TypedStatementKind::Pset { step, x, y, color } => {
                let x_code = emit_expr(x, self.no_shell)?;
                let y_code = emit_expr(y, self.no_shell)?;
                let step_int = if *step { 1 } else { 0 };
                if let Some(c) = color {
                    let c_code = emit_expr(c, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}qb_gfx_pset_step((int32_t){}, (int32_t){}, (uint32_t){}, {});",
                        indent,
                        x_code,
                        y_code,
                        c_code,
                        step_int
                    )?;
                } else {
                    // Use current foreground color (pass -1 to signal "use current")
                    writeln_code!(
                        output,
                        "{}qb_gfx_pset_step((int32_t){}, (int32_t){}, 0xFFFFFFFF, {});",
                        indent,
                        x_code,
                        y_code,
                        step_int
                    )?;
                }
            }

            TypedStatementKind::Preset { step, x, y } => {
                let x_code = emit_expr(x, self.no_shell)?;
                let y_code = emit_expr(y, self.no_shell)?;
                let step_int = if *step { 1 } else { 0 };
                // PRESET plots in background color - pass 0 (black) by default
                writeln_code!(
                    output,
                    "{}qb_gfx_pset_step((int32_t){}, (int32_t){}, 0xFF000000, {});",
                    indent,
                    x_code,
                    y_code,
                    step_int
                )?;
            }

            TypedStatementKind::Line {
                x1,
                y1,
                x2,
                y2,
                step2,
                color,
                box_style,
                style: _, // TODO: implement line style pattern support
            } => {
                let x2_code = emit_expr(x2, self.no_shell)?;
                let y2_code = emit_expr(y2, self.no_shell)?;
                let color_code = if let Some(c) = color {
                    emit_expr(c, self.no_shell)?
                } else {
                    "0xFFFFFFFF".to_string() // Use current foreground
                };

                // Handle optional start coordinates (use 0,0 as default for now)
                let x1_code = if let Some(e) = x1 {
                    emit_expr(e, self.no_shell)?
                } else {
                    "0".to_string()
                };
                let y1_code = if let Some(e) = y1 {
                    emit_expr(e, self.no_shell)?
                } else {
                    "0".to_string()
                };

                // In LINE statement, STEP only applies to the endpoint (step2)
                // step1 = 0 (first point is absolute), step2 = whether endpoint is relative
                let step2_flag = if *step2 { "1" } else { "0" };

                match box_style {
                    None => {
                        // Plain line: qb_gfx_line_step(x1, y1, x2, y2, color, step1, step2)
                        writeln_code!(
                            output,
                            "{}qb_gfx_line_step((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (uint32_t){}, 0, {});",
                            indent,
                            x1_code,
                            y1_code,
                            x2_code,
                            y2_code,
                            color_code,
                            step2_flag
                        )?;
                    }
                    Some(false) => {
                        // Box (outline): qb_gfx_box_step(x1, y1, x2, y2, color, filled, step1, step2)
                        writeln_code!(
                            output,
                            "{}qb_gfx_box_step((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (uint32_t){}, 0, 0, {});",
                            indent,
                            x1_code,
                            y1_code,
                            x2_code,
                            y2_code,
                            color_code,
                            step2_flag
                        )?;
                    }
                    Some(true) => {
                        // Filled box: qb_gfx_box_step(x1, y1, x2, y2, color, filled, step1, step2)
                        writeln_code!(
                            output,
                            "{}qb_gfx_box_step((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (uint32_t){}, 1, 0, {});",
                            indent,
                            x1_code,
                            y1_code,
                            x2_code,
                            y2_code,
                            color_code,
                            step2_flag
                        )?;
                    }
                }
            }

            TypedStatementKind::Circle {
                step,
                x,
                y,
                radius,
                color,
                filled,
            } => {
                let x_code = emit_expr(x, self.no_shell)?;
                let y_code = emit_expr(y, self.no_shell)?;
                let r_code = emit_expr(radius, self.no_shell)?;
                let color_code = if let Some(c) = color {
                    emit_expr(c, self.no_shell)?
                } else {
                    "0xFFFFFFFF".to_string()
                };
                let filled_int = if *filled { 1 } else { 0 };
                let step_int = if *step { 1 } else { 0 };
                writeln_code!(
                    output,
                    "{}qb_gfx_circle_step((int32_t){}, (int32_t){}, (int32_t){}, (uint32_t){}, {}, {});",
                    indent,
                    x_code,
                    y_code,
                    r_code,
                    color_code,
                    filled_int,
                    step_int
                )?;
            }

            TypedStatementKind::Paint {
                step,
                x,
                y,
                color,
                border,
            } => {
                let x_code = emit_expr(x, self.no_shell)?;
                let y_code = emit_expr(y, self.no_shell)?;
                let color_code = if let Some(c) = color {
                    emit_expr(c, self.no_shell)?
                } else {
                    "0xFFFFFFFF".to_string()
                };
                let border_code = if let Some(b) = border {
                    emit_expr(b, self.no_shell)?
                } else {
                    color_code.clone() // Default border = fill color
                };
                let step_int = if *step { 1 } else { 0 };
                writeln_code!(
                    output,
                    "{}qb_gfx_paint_step((int32_t){}, (int32_t){}, (uint32_t){}, (uint32_t){}, {});",
                    indent,
                    x_code,
                    y_code,
                    color_code,
                    border_code,
                    step_int
                )?;
            }

            TypedStatementKind::GfxDisplay => {
                writeln_code!(output, "{}qb_gfx_display();", indent)?;
            }

            TypedStatementKind::ControlChr { enabled } => {
                writeln_code!(
                    output,
                    "{}qb_controlchr({});",
                    indent,
                    if *enabled { "1" } else { "0" }
                )?;
            }

            TypedStatementKind::MapUnicode {
                unicode_value,
                char_position,
            } => {
                let unicode_code = emit_expr(unicode_value, self.no_shell)?;
                let char_code = emit_expr(char_position, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_mapunicode((int32_t){}, (int32_t){});",
                    indent,
                    unicode_code,
                    char_code
                )?;
            }

            TypedStatementKind::GfxResize { enabled } => {
                writeln_code!(
                    output,
                    "{}qb_gfx_resize({});",
                    indent,
                    if *enabled { "1" } else { "0" }
                )?;
            }

            TypedStatementKind::Palette { attribute, color } => {
                match (attribute, color) {
                    (Some(attr), Some(col)) => {
                        let attr_code = emit_expr(attr, self.no_shell)?;
                        let col_code = emit_expr(col, self.no_shell)?;
                        writeln_code!(
                            output,
                            "{}qb_gfx_palette((int32_t){}, (uint32_t){});",
                            indent,
                            attr_code,
                            col_code
                        )?;
                    }
                    _ => {
                        // PALETTE without arguments - reset all palette entries
                        writeln_code!(output, "{}qb_gfx_palette_reset();", indent)?;
                    }
                }
            }

            TypedStatementKind::Pcopy { source, dest } => {
                let src_code = emit_expr(source, self.no_shell)?;
                let dst_code = emit_expr(dest, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_gfx_pcopy((int32_t){}, (int32_t){});",
                    indent,
                    src_code,
                    dst_code
                )?;
            }

            // ==================== Additional Graphics Statements ====================
            TypedStatementKind::Width { columns, rows } => {
                let cols_code = emit_expr(columns, self.no_shell)?;
                if let Some(r) = rows {
                    let rows_code = emit_expr(r, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}qb_gfx_set_width((uint32_t){}, (uint32_t){});",
                        indent,
                        cols_code,
                        rows_code
                    )?;
                } else {
                    writeln_code!(
                        output,
                        "{}qb_gfx_set_width((uint32_t){}, 0);",
                        indent,
                        cols_code
                    )?;
                }
            }

            TypedStatementKind::View {
                screen,
                coords,
                fill_color,
                border_color,
            } => {
                let screen_int = if *screen { 1 } else { 0 };
                if let Some(c) = coords {
                    let x1 = emit_expr(&c.x1, self.no_shell)?;
                    let y1 = emit_expr(&c.y1, self.no_shell)?;
                    let x2 = emit_expr(&c.x2, self.no_shell)?;
                    let y2 = emit_expr(&c.y2, self.no_shell)?;
                    let fill = fill_color
                        .as_ref()
                        .map(|e| emit_expr(e, self.no_shell))
                        .transpose()?
                        .unwrap_or_else(|| "-1".to_string());
                    let border = border_color
                        .as_ref()
                        .map(|e| emit_expr(e, self.no_shell))
                        .transpose()?
                        .unwrap_or_else(|| "-1".to_string());
                    writeln_code!(
                        output,
                        "{}qb_gfx_view({}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){});",
                        indent,
                        screen_int,
                        x1,
                        y1,
                        x2,
                        y2,
                        fill,
                        border
                    )?;
                } else {
                    // Reset viewport
                    writeln_code!(output, "{}qb_gfx_view_reset();", indent)?;
                }
            }

            TypedStatementKind::ViewPrint { top, bottom } => {
                if let (Some(t), Some(b)) = (top, bottom) {
                    let top_code = emit_expr(t, self.no_shell)?;
                    let bottom_code = emit_expr(b, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}qb_view_print((int32_t){}, (int32_t){});",
                        indent,
                        top_code,
                        bottom_code
                    )?;
                } else {
                    // Reset text viewport
                    writeln_code!(output, "{}qb_view_print_reset();", indent)?;
                }
            }

            TypedStatementKind::WindowCoords { screen, coords } => {
                let screen_int = if *screen { 1 } else { 0 };
                if let Some(c) = coords {
                    let x1 = emit_expr(&c.x1, self.no_shell)?;
                    let y1 = emit_expr(&c.y1, self.no_shell)?;
                    let x2 = emit_expr(&c.x2, self.no_shell)?;
                    let y2 = emit_expr(&c.y2, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}qb_gfx_window({}, (double){}, (double){}, (double){}, (double){});",
                        indent,
                        screen_int,
                        x1,
                        y1,
                        x2,
                        y2
                    )?;
                } else {
                    // Reset window coordinates
                    writeln_code!(output, "{}qb_gfx_window_reset();", indent)?;
                }
            }

            TypedStatementKind::DrawCmd { commands } => {
                let cmd_code = emit_expr(commands, self.no_shell)?;
                writeln_code!(output, "{}qb_gfx_draw({});", indent, cmd_code)?;
            }

            TypedStatementKind::GraphicsGet {
                step1,
                x1,
                y1,
                step2,
                x2,
                y2,
                array_name,
                array_indices,
            } => {
                let x1_code = emit_expr(x1, self.no_shell)?;
                let y1_code = emit_expr(y1, self.no_shell)?;
                let x2_code = emit_expr(x2, self.no_shell)?;
                let y2_code = emit_expr(y2, self.no_shell)?;
                let arr_name = c_identifier(array_name);

                // Calculate array pointer - either base or with offset
                let arr_ptr = if array_indices.is_empty() {
                    arr_name.clone()
                } else {
                    // For multi-dimensional arrays, generate index expression
                    let indices: Vec<String> = array_indices
                        .iter()
                        .map(|e| emit_expr(e, self.no_shell))
                        .collect::<Result<_, _>>()?;
                    format!("&{}[{}]", arr_name, indices.join("]["))
                };

                // Generate appropriate function call based on step flags
                // step1 affects (x1, y1), step2 affects (x2, y2)
                let func_name = match (*step1, *step2) {
                    (false, false) => "qb_gfx_get",
                    (false, true) => "qb_gfx_get_step2",
                    (true, false) => "qb_gfx_get_step1",
                    (true, true) => "qb_gfx_get_step_both",
                };
                writeln_code!(
                    output,
                    "{}{}((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {});",
                    indent,
                    func_name,
                    x1_code,
                    y1_code,
                    x2_code,
                    y2_code,
                    arr_ptr
                )?;
            }

            TypedStatementKind::GraphicsPut {
                x,
                y,
                step,
                array_name,
                array_indices,
                clip,
                action,
                transparent_color,
            } => {
                use crate::ast::PutAction;

                let x_code = emit_expr(x, self.no_shell)?;
                let y_code = emit_expr(y, self.no_shell)?;
                let arr_name = c_identifier(array_name);

                // Calculate array pointer
                let arr_ptr = if array_indices.is_empty() {
                    arr_name.clone()
                } else {
                    // For multi-dimensional arrays, generate index expression
                    let indices: Vec<String> = array_indices
                        .iter()
                        .map(|e| emit_expr(e, self.no_shell))
                        .collect::<Result<_, _>>()?;
                    format!("&{}[{}]", arr_name, indices.join("]["))
                };

                // Map action to C constant
                let action_code = match action {
                    PutAction::Xor => "QB_PUT_XOR",
                    PutAction::Pset => "QB_PUT_PSET",
                    PutAction::Preset => "QB_PUT_PRESET",
                    PutAction::And => "QB_PUT_AND",
                    PutAction::Or => "QB_PUT_OR",
                };

                // QB64 extension: _CLIP with optional transparent color
                let trans_code = if let Some(tc) = transparent_color {
                    emit_expr(tc, self.no_shell)?
                } else {
                    "-1".to_string() // No transparent color
                };

                let clip_flag = if *clip { "1" } else { "0" };

                if *step {
                    writeln_code!(
                        output,
                        "{}qb_gfx_put_step((int32_t){}, (int32_t){}, {}, {}, {}, (int32_t){});",
                        indent,
                        x_code,
                        y_code,
                        arr_ptr,
                        action_code,
                        clip_flag,
                        trans_code
                    )?;
                } else {
                    writeln_code!(
                        output,
                        "{}qb_gfx_put((int32_t){}, (int32_t){}, {}, {}, {}, (int32_t){});",
                        indent,
                        x_code,
                        y_code,
                        arr_ptr,
                        action_code,
                        clip_flag,
                        trans_code
                    )?;
                }
            }

            // ==================== QB64 Graphics Extensions ====================
            TypedStatementKind::FreeImage { handle } => {
                let h_code = emit_expr(handle, self.no_shell)?;
                writeln_code!(output, "{}qb_gfx_freeimage((int32_t){});", indent, h_code)?;
            }

            TypedStatementKind::PutImage {
                dest_coords,
                source,
                dest,
                source_coords,
                scale_mode,
            } => {
                // Generate _PUTIMAGE call with all optional parameters
                let src_handle = source
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());
                let dst_handle = dest
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "-1".to_string());

                // Scale mode: 0 = default, 1 = smooth (bilinear), 2 = stretch (nearest-neighbor)
                let scale_code = match scale_mode {
                    ImageScaleMode::Default => "0",
                    ImageScaleMode::Smooth => "1",
                    ImageScaleMode::Stretch => "2",
                };

                if let (Some(dc), Some(sc)) = (dest_coords, source_coords) {
                    let dx1 = emit_expr(&dc.x1, self.no_shell)?;
                    let dy1 = emit_expr(&dc.y1, self.no_shell)?;
                    let dx2 = emit_expr(&dc.x2, self.no_shell)?;
                    let dy2 = emit_expr(&dc.y2, self.no_shell)?;
                    let sx1 = emit_expr(&sc.x1, self.no_shell)?;
                    let sy1 = emit_expr(&sc.y1, self.no_shell)?;
                    let sx2 = emit_expr(&sc.x2, self.no_shell)?;
                    let sy2 = emit_expr(&sc.y2, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}qb_gfx_putimage_full((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {});",
                        indent,
                        dx1,
                        dy1,
                        dx2,
                        dy2,
                        src_handle,
                        dst_handle,
                        sx1,
                        sy1,
                        sx2,
                        sy2,
                        scale_code
                    )?;
                } else if let Some(dc) = dest_coords {
                    let dx1 = emit_expr(&dc.x1, self.no_shell)?;
                    let dy1 = emit_expr(&dc.y1, self.no_shell)?;
                    let dx2 = emit_expr(&dc.x2, self.no_shell)?;
                    let dy2 = emit_expr(&dc.y2, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}qb_gfx_putimage((int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, (int32_t){}, {});",
                        indent,
                        dx1,
                        dy1,
                        dx2,
                        dy2,
                        src_handle,
                        dst_handle,
                        scale_code
                    )?;
                } else {
                    writeln_code!(
                        output,
                        "{}qb_gfx_putimage_simple((int32_t){}, (int32_t){}, {});",
                        indent,
                        src_handle,
                        dst_handle,
                        scale_code
                    )?;
                }
            }

            TypedStatementKind::SourceImg { handle } => {
                let h_code = emit_expr(handle, self.no_shell)?;
                writeln_code!(output, "{}qb_gfx_source((int32_t){});", indent, h_code)?;
            }

            TypedStatementKind::DestImg { handle } => {
                let h_code = emit_expr(handle, self.no_shell)?;
                writeln_code!(output, "{}qb_gfx_dest((int32_t){});", indent, h_code)?;
            }

            TypedStatementKind::PrintStringStmt { x, y, text } => {
                let x_code = emit_expr(x, self.no_shell)?;
                let y_code = emit_expr(y, self.no_shell)?;
                let text_code = emit_expr(text, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_gfx_printstring((int32_t){}, (int32_t){}, {});",
                    indent,
                    x_code,
                    y_code,
                    text_code
                )?;
            }

            TypedStatementKind::AutoDisplay { enabled } => {
                let enable_int = if *enabled { 1 } else { 0 };
                writeln_code!(output, "{}qb_gfx_autodisplay({});", indent, enable_int)?;
            }

            // ==================== Audio Statements ====================
            TypedStatementKind::Beep => {
                writeln_code!(output, "{}qb_beep();", indent)?;
            }

            TypedStatementKind::SoundStmt {
                frequency,
                duration,
            } => {
                let freq_code = emit_expr(frequency, self.no_shell)?;
                let dur_code = emit_expr(duration, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_sound((double){}, (double){});",
                    indent,
                    freq_code,
                    dur_code
                )?;
            }

            TypedStatementKind::PlayStmt { commands } => {
                let cmd_code = emit_expr(commands, self.no_shell)?;
                writeln_code!(output, "{}qb_play({});", indent, cmd_code)?;
            }

            TypedStatementKind::SndClose { handle } => {
                let h_code = emit_expr(handle, self.no_shell)?;
                writeln_code!(output, "{}qb_sndclose((int32_t){});", indent, h_code)?;
            }

            TypedStatementKind::SndPlay { handle } => {
                let h_code = emit_expr(handle, self.no_shell)?;
                writeln_code!(output, "{}qb_sndplay((int32_t){});", indent, h_code)?;
            }

            TypedStatementKind::SndStop { handle } => {
                let h_code = emit_expr(handle, self.no_shell)?;
                writeln_code!(output, "{}qb_sndstop((int32_t){});", indent, h_code)?;
            }

            TypedStatementKind::SndPause { handle } => {
                let h_code = emit_expr(handle, self.no_shell)?;
                writeln_code!(output, "{}qb_sndpause((int32_t){});", indent, h_code)?;
            }

            TypedStatementKind::SndLoop { handle } => {
                let h_code = emit_expr(handle, self.no_shell)?;
                writeln_code!(output, "{}qb_sndloop((int32_t){});", indent, h_code)?;
            }

            TypedStatementKind::SndVol { handle, volume } => {
                let h_code = emit_expr(handle, self.no_shell)?;
                let vol_code = emit_expr(volume, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_sndvol((int32_t){}, (double){});",
                    indent,
                    h_code,
                    vol_code
                )?;
            }

            TypedStatementKind::SndBal {
                handle,
                x,
                y,
                z,
                channel,
            } => {
                let h_code = emit_expr(handle, self.no_shell)?;
                let x_code = match x {
                    Some(e) => emit_expr(e, self.no_shell)?,
                    None => "0.0".to_string(),
                };
                let y_code = match y {
                    Some(e) => emit_expr(e, self.no_shell)?,
                    None => "0.0".to_string(),
                };
                let z_code = match z {
                    Some(e) => emit_expr(e, self.no_shell)?,
                    None => "0.0".to_string(),
                };
                let ch_code = match channel {
                    Some(e) => emit_expr(e, self.no_shell)?,
                    None => "0".to_string(),
                };
                writeln_code!(
                    output,
                    "{}qb_sndbal((int32_t){}, (double){}, (double){}, (double){}, (int32_t){});",
                    indent,
                    h_code,
                    x_code,
                    y_code,
                    z_code,
                    ch_code
                )?;
            }

            TypedStatementKind::SndRaw { left, right } => {
                let left_code = emit_expr(left, self.no_shell)?;
                if let Some(r) = right {
                    let right_code = emit_expr(r, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}qb_sndraw_stereo((double){}, (double){});",
                        indent,
                        left_code,
                        right_code
                    )?;
                } else {
                    writeln_code!(output, "{}qb_sndraw((double){});", indent, left_code)?;
                }
            }

            TypedStatementKind::SndPlayFile {
                filename,
                volume,
                x,
                y,
                z,
            } => {
                let filename_code = emit_expr(filename, self.no_shell)?;
                let volume_code = volume
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "1.0".to_string());
                let x_code = x
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "0.0".to_string());
                let y_code = y
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "0.0".to_string());
                let z_code = z
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "0.0".to_string());
                let filename_access =
                    emit_string_data_access(filename, &filename_code, self.runtime_mode);
                writeln_code!(
                    output,
                    "{}qb_sndplayfile({}, (double){}, (double){}, (double){}, (double){});",
                    indent,
                    filename_access,
                    volume_code,
                    x_code,
                    y_code,
                    z_code
                )?;
            }

            TypedStatementKind::SndPlayCopy { handle, volume } => {
                let handle_code = emit_expr(handle, self.no_shell)?;
                let volume_code = volume
                    .as_ref()
                    .map(|e| emit_expr(e, self.no_shell))
                    .transpose()?
                    .unwrap_or_else(|| "1.0".to_string());
                writeln_code!(
                    output,
                    "{}qb_sndplaycopy((int32_t){}, (double){});",
                    indent,
                    handle_code,
                    volume_code
                )?;
            }

            TypedStatementKind::SndSetPos { handle, position } => {
                let handle_code = emit_expr(handle, self.no_shell)?;
                let position_code = emit_expr(position, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_sndsetpos((int32_t){}, (double){});",
                    indent,
                    handle_code,
                    position_code
                )?;
            }

            // ==================== System Integration Statements ====================
            TypedStatementKind::Kill { filename } => {
                let filename_code = emit_expr(filename, self.no_shell)?;
                let filename_access =
                    emit_string_data_access(filename, &filename_code, self.runtime_mode);
                writeln_code!(output, "{}qb_file_kill({});", indent, filename_access)?;
            }

            TypedStatementKind::Rename { old_name, new_name } => {
                let old_code = emit_expr(old_name, self.no_shell)?;
                let new_code = emit_expr(new_name, self.no_shell)?;
                let old_access = emit_string_data_access(old_name, &old_code, self.runtime_mode);
                let new_access = emit_string_data_access(new_name, &new_code, self.runtime_mode);
                writeln_code!(
                    output,
                    "{}qb_file_rename({}, {});",
                    indent,
                    old_access,
                    new_access
                )?;
            }

            TypedStatementKind::Mkdir { path } => {
                let path_code = emit_expr(path, self.no_shell)?;
                let path_access = emit_string_data_access(path, &path_code, self.runtime_mode);
                writeln_code!(output, "{}qb_mkdir({});", indent, path_access)?;
            }

            TypedStatementKind::Rmdir { path } => {
                let path_code = emit_expr(path, self.no_shell)?;
                let path_access = emit_string_data_access(path, &path_code, self.runtime_mode);
                writeln_code!(output, "{}qb_rmdir({});", indent, path_access)?;
            }

            TypedStatementKind::Chdir { path } => {
                let path_code = emit_expr(path, self.no_shell)?;
                let path_access = emit_string_data_access(path, &path_code, self.runtime_mode);
                writeln_code!(output, "{}qb_chdir({});", indent, path_access)?;
            }

            TypedStatementKind::Environ { env_string } => {
                let env_string_code = emit_expr(env_string, self.no_shell)?;
                writeln_code!(output, "{}qb_sub_environ({});", indent, env_string_code)?;
            }

            TypedStatementKind::ShellCmd { command } => {
                if self.no_shell {
                    return Err(
                        CodeGenError::new(CodeGenErrorKind::ShellDisabled).with_span(stmt.span)
                    );
                }
                if let Some(cmd) = command {
                    let cmd_code = emit_expr(cmd, self.no_shell)?;
                    writeln_code!(output, "{}qb_shell({});", indent, cmd_code)?;
                } else {
                    writeln_code!(output, "{}qb_shell(NULL);", indent)?;
                }
            }

            TypedStatementKind::ShellHide { command } => {
                if self.no_shell {
                    return Err(
                        CodeGenError::new(CodeGenErrorKind::ShellDisabled).with_span(stmt.span)
                    );
                }
                let cmd_code = emit_expr(command, self.no_shell)?;
                writeln_code!(output, "{}qb_shell_hide({});", indent, cmd_code)?;
            }

            TypedStatementKind::Bload { filename, address } => {
                let filename_code = emit_expr(filename, self.no_shell)?;
                let filename_access =
                    emit_string_data_access(filename, &filename_code, self.runtime_mode);
                if let Some(addr) = address {
                    let addr_code = emit_expr(addr, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}qb_bload({}, (void*)(intptr_t){});",
                        indent,
                        filename_access,
                        addr_code
                    )?;
                } else {
                    writeln_code!(output, "{}qb_bload({}, NULL);", indent, filename_access)?;
                }
            }

            TypedStatementKind::Bsave {
                filename,
                address,
                length,
            } => {
                let filename_code = emit_expr(filename, self.no_shell)?;
                let filename_access =
                    emit_string_data_access(filename, &filename_code, self.runtime_mode);
                let addr_code = emit_expr(address, self.no_shell)?;
                let len_code = emit_expr(length, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_bsave({}, (void*)(intptr_t){}, (size_t){});",
                    indent,
                    filename_access,
                    addr_code,
                    len_code
                )?;
            }

            TypedStatementKind::Setmem { bytes } => {
                // SETMEM is a no-op in modern systems - just evaluate the expression
                let bytes_code = emit_expr(bytes, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}(void){}; /* SETMEM: no-op in flat memory model */",
                    indent,
                    bytes_code
                )?;
            }

            TypedStatementKind::CallAbsolute { args: _, address } => {
                // CALL ABSOLUTE is a legacy statement that cannot be safely implemented
                let addr_code = emit_expr(address, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}fprintf(stderr, \"Warning: CALL ABSOLUTE at address %ld not supported in flat memory model\\n\", (long){});",
                    indent,
                    addr_code
                )?;
                writeln_code!(output, "{}fflush(stderr);", indent)?;
            }

            // ==================== Mouse Input Statements ====================
            TypedStatementKind::MouseHide => {
                writeln_code!(output, "{}qb_mouse_hide();", indent)?;
            }

            TypedStatementKind::MouseShow => {
                writeln_code!(output, "{}qb_mouse_show();", indent)?;
            }

            TypedStatementKind::MouseMoveStmt { x, y } => {
                let x_code = emit_expr(x, self.no_shell)?;
                let y_code = emit_expr(y, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_mouse_move((int32_t){}, (int32_t){});",
                    indent,
                    x_code,
                    y_code
                )?;
            }

            // ==================== Clipboard Statement ====================
            TypedStatementKind::ClipboardSet { text } => {
                let text_code = emit_expr(text, self.no_shell)?;
                let text_access = emit_string_data_access(text, &text_code, self.runtime_mode);
                writeln_code!(output, "{}qb_clipboard_set({});", indent, text_access)?;
            }

            // ==================== C Library Integration ====================
            TypedStatementKind::DeclareLibrary {
                library_name,
                is_dynamic,
                declarations,
            } => {
                // For static libraries, we just emit extern declarations
                // For dynamic libraries, we would need to emit dlopen/LoadLibrary code
                // at runtime, which is more complex and deferred for now.
                //
                // The actual function calls are handled in expression codegen
                // when the external function is called.
                if *is_dynamic {
                    writeln_code!(
                        output,
                        "{}// DECLARE DYNAMIC LIBRARY (runtime loading not yet implemented)",
                        indent
                    )?;
                    if let Some(lib) = library_name {
                        writeln_code!(output, "{}// Library: {}", indent, lib)?;
                    }
                } else {
                    writeln_code!(output, "{}// DECLARE LIBRARY - extern declarations", indent)?;
                    if let Some(lib) = library_name {
                        writeln_code!(output, "{}// Library: {}", indent, lib)?;
                    }
                }

                // Emit extern declarations for each function
                for decl in declarations {
                    self.emit_extern_declaration(&indent, decl, output)?;
                }
            }

            // Forward declarations - no code generated, just comments for documentation
            TypedStatementKind::DeclareSub { name } => {
                writeln_code!(output, "{}/* DECLARE SUB {} */", indent, name)?;
            }

            TypedStatementKind::DeclareFunction { name } => {
                writeln_code!(output, "{}/* DECLARE FUNCTION {} */", indent, name)?;
            }

            // ==================== Phase 7: Additional Statements ====================
            TypedStatementKind::Run { target } => {
                // RUN restarts the program or runs another - stub implementation
                if let Some(t) = target {
                    let target_code = emit_expr(t, self.no_shell)?;
                    writeln_code!(output, "{}qb_run({});", indent, target_code)?;
                } else {
                    writeln_code!(output, "{}qb_run(NULL);", indent)?;
                }
            }

            TypedStatementKind::Chain { filename } => {
                let filename_code = emit_expr(filename, self.no_shell)?;
                writeln_code!(output, "{}qb_chain({});", indent, filename_code)?;
            }

            TypedStatementKind::Tron => {
                writeln_code!(output, "{}qb_trace_on = 1;", indent)?;
            }

            TypedStatementKind::Troff => {
                writeln_code!(output, "{}qb_trace_on = 0;", indent)?;
            }

            TypedStatementKind::Lprint { values, newline } => {
                // Print to printer (LPT1) - similar to PRINT but to a different stream
                for item in values {
                    let expr_code = emit_expr(&item.expr, self.no_shell)?;
                    writeln_code!(output, "{}qb_lprint({});", indent, expr_code)?;
                    if item.separator == Some(PrintSeparator::Comma) {
                        writeln_code!(output, "{}qb_lprint_tab();", indent)?;
                    }
                }
                if *newline {
                    writeln_code!(output, "{}qb_lprint_newline();", indent)?;
                }
            }

            TypedStatementKind::FilesStmt { filespec } => {
                if let Some(spec) = filespec {
                    let spec_code = emit_expr(spec, self.no_shell)?;
                    writeln_code!(output, "{}qb_files({});", indent, spec_code)?;
                } else {
                    writeln_code!(output, "{}qb_files(NULL);", indent)?;
                }
            }

            TypedStatementKind::FieldStmt { file_num, fields } => {
                let file_num_code = emit_expr(file_num, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_field_start((int32_t)({}));",
                    indent,
                    file_num_code
                )?;
                for field in fields {
                    let width_code = emit_expr(&field.width, self.no_shell)?;
                    writeln_code!(
                        output,
                        "{}qb_field_add((int32_t)({}), &{});",
                        indent,
                        width_code,
                        field.variable
                    )?;
                }
            }

            TypedStatementKind::Lset { variable, value } => {
                let value_code = emit_expr(value, self.no_shell)?;
                let c_var = c_identifier(variable);
                writeln_code!(output, "{}qb_lset(&{}, {});", indent, c_var, value_code)?;
            }

            TypedStatementKind::Rset { variable, value } => {
                let value_code = emit_expr(value, self.no_shell)?;
                let c_var = c_identifier(variable);
                writeln_code!(output, "{}qb_rset(&{}, {});", indent, c_var, value_code)?;
            }

            TypedStatementKind::OnKey { key_num, target } => {
                let key_code = emit_expr(key_num, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_on_key((int32_t)({}), &&{});",
                    indent,
                    key_code,
                    target
                )?;
            }

            TypedStatementKind::KeyControl { key_num, mode } => {
                let key_code = emit_expr(key_num, self.no_shell)?;
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln_code!(
                    output,
                    "{}qb_key_control((int32_t)({}), {});",
                    indent,
                    key_code,
                    mode_code
                )?;
            }

            TypedStatementKind::OnTimer { interval, target } => {
                let interval_code = emit_expr(interval, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_on_timer({}, &&{});",
                    indent,
                    interval_code,
                    target
                )?;
            }

            TypedStatementKind::TimerControl { mode } => {
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln_code!(output, "{}qb_timer_control({});", indent, mode_code)?;
            }

            TypedStatementKind::StrigControl { button_num, mode } => {
                let btn_code = emit_expr(button_num, self.no_shell)?;
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln_code!(
                    output,
                    "{}qb_strig_control((int32_t)({}), {});",
                    indent,
                    btn_code,
                    mode_code
                )?;
            }

            TypedStatementKind::OnStrig { button_num, target } => {
                // Generate a unique event ID for this handler
                self.strig_event_counter += 1;
                let event_id = self.strig_event_counter;
                self.strig_handlers.push((event_id, target.clone()));

                let btn_code = emit_expr(button_num, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_on_strig((int32_t)({}), {});",
                    indent,
                    btn_code,
                    event_id
                )?;
            }

            TypedStatementKind::OnCom { port_num, target } => {
                let port_code = emit_expr(port_num, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_on_com((int32_t)({}), &&{});",
                    indent,
                    port_code,
                    target
                )?;
            }

            TypedStatementKind::ComControl { port_num, mode } => {
                let port_code = emit_expr(port_num, self.no_shell)?;
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln_code!(
                    output,
                    "{}qb_com_control((int32_t)({}), {});",
                    indent,
                    port_code,
                    mode_code
                )?;
            }

            TypedStatementKind::OnPen { target } => {
                writeln_code!(output, "{}qb_on_pen(&&{});", indent, target)?;
            }

            TypedStatementKind::PenControl { mode } => {
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln_code!(output, "{}qb_pen_control({});", indent, mode_code)?;
            }

            TypedStatementKind::OnUevent { target } => {
                writeln_code!(output, "{}qb_on_uevent(&&{});", indent, target)?;
            }

            TypedStatementKind::UeventControl { mode } => {
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln_code!(output, "{}qb_uevent_control({});", indent, mode_code)?;
            }

            TypedStatementKind::UeventTrigger => {
                writeln_code!(output, "{}qb_uevent_trigger();", indent)?;
            }

            TypedStatementKind::OnSignal { signal_num, target } => {
                let signal_code = emit_expr(signal_num, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_on_signal((int32_t)({}), &&{});",
                    indent,
                    signal_code,
                    target
                )?;
            }

            TypedStatementKind::SignalControl { signal_num, mode } => {
                let signal_code = emit_expr(signal_num, self.no_shell)?;
                let mode_code = match mode {
                    EventControlMode::On => "1",
                    EventControlMode::Off => "0",
                    EventControlMode::Stop => "2",
                };
                writeln_code!(
                    output,
                    "{}qb_signal_control((int32_t)({}), {});",
                    indent,
                    signal_code,
                    mode_code
                )?;
            }

            TypedStatementKind::OutPort { port, value } => {
                let port_code = emit_expr(port, self.no_shell)?;
                let value_code = emit_expr(value, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_out((int32_t)({}), (int32_t)({}));",
                    indent,
                    port_code,
                    value_code
                )?;
            }

            TypedStatementKind::InterruptStmt {
                int_num,
                in_regs,
                out_regs,
            } => {
                let int_code = emit_expr(int_num, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_interrupt((int32_t)({}), &{}, &{});",
                    indent,
                    int_code,
                    in_regs,
                    out_regs
                )?;
            }

            TypedStatementKind::InterruptXStmt {
                int_num,
                in_regs,
                out_regs,
            } => {
                let int_code = emit_expr(int_num, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_interruptx((int32_t)({}), &{}, &{});",
                    indent,
                    int_code,
                    in_regs,
                    out_regs
                )?;
            }

            TypedStatementKind::IoctlStmt {
                file_num,
                control_string,
            } => {
                let file_code = emit_expr(file_num, self.no_shell)?;
                let string_code = emit_expr(control_string, self.no_shell)?;
                writeln_code!(
                    output,
                    "{}qb_ioctl((int32_t)({}), {});",
                    indent,
                    file_code,
                    string_code
                )?;
            }

            TypedStatementKind::FreeStmt => {
                writeln_code!(output, "{}qb_free();", indent)?;
            }

            TypedStatementKind::ClearStmt { stack_size } => {
                if let Some(size) = stack_size {
                    let size_code = emit_expr(size, self.no_shell)?;
                    writeln_code!(output, "{}qb_clear((int32_t)({}));", indent, size_code)?;
                } else {
                    writeln_code!(output, "{}qb_clear(0);", indent)?;
                }
            }

            TypedStatementKind::ResetStmt => {
                writeln_code!(output, "{}qb_reset();", indent)?;
            }

            // Window/Desktop statements (QB64)
            TypedStatementKind::TitleStmt { title } => {
                let title_code = emit_expr(title, self.no_shell)?;
                writeln_code!(output, "{}qb_title({});", indent, title_code)?;
            }

            TypedStatementKind::ScreenMoveStmt { x, y, center } => {
                if *center {
                    writeln_code!(output, "{}qb_screenmove_center();", indent)?;
                } else {
                    let x_code = match x {
                        Some(e) => emit_expr(e, self.no_shell)?,
                        None => "0".to_string(),
                    };
                    let y_code = match y {
                        Some(e) => emit_expr(e, self.no_shell)?,
                        None => "0".to_string(),
                    };
                    writeln_code!(
                        output,
                        "{}qb_screenmove((int32_t)({}), (int32_t)({}));",
                        indent,
                        x_code,
                        y_code
                    )?;
                }
            }

            TypedStatementKind::FullScreenStmt { mode } => {
                let mode_code = match mode {
                    FullScreenMode::Stretch => "0",
                    FullScreenMode::SquarePixels => "1",
                    FullScreenMode::Off => "2",
                };
                writeln_code!(output, "{}qb_fullscreen({});", indent, mode_code)?;
            }

            TypedStatementKind::AllowFullScreenStmt { mode } => {
                let mode_code = match mode {
                    AllowFullScreenMode::Stretch => "0",
                    AllowFullScreenMode::SquarePixels => "1",
                    AllowFullScreenMode::All => "2",
                    AllowFullScreenMode::Off => "3",
                };
                writeln_code!(output, "{}qb_allowfullscreen({});", indent, mode_code)?;
            }

            TypedStatementKind::ScreenIconStmt => {
                writeln_code!(output, "{}qb_screenicon();", indent)?;
            }

            TypedStatementKind::IconStmt { handle } => {
                if let Some(h) = handle {
                    let handle_code = emit_expr(h, self.no_shell)?;
                    writeln_code!(output, "{}qb_icon((int32_t)({}));", indent, handle_code)?;
                } else {
                    writeln_code!(output, "{}qb_icon(0);", indent)?;
                }
            }

            TypedStatementKind::ScreenHideStmt => {
                writeln_code!(output, "{}qb_screenhide();", indent)?;
            }

            TypedStatementKind::ScreenShowStmt => {
                writeln_code!(output, "{}qb_screenshow();", indent)?;
            }

            TypedStatementKind::ConsoleTitleStmt { title } => {
                let title_code = emit_expr(title, self.no_shell)?;
                writeln_code!(output, "{}qb_consoletitle({});", indent, title_code)?;
            }

            TypedStatementKind::ConsoleStmt { visible } => {
                writeln_code!(
                    output,
                    "{}qb_console({});",
                    indent,
                    if *visible { "1" } else { "0" }
                )?;
            }

            TypedStatementKind::AssertStmt { condition, message } => {
                let cond_code = emit_expr(condition, self.no_shell)?;
                if let Some(msg) = message {
                    let msg_code = emit_expr(msg, self.no_shell)?;
                    writeln_code!(output, "{}qb_assert({}, {});", indent, cond_code, msg_code)?;
                } else {
                    writeln_code!(output, "{}qb_assert({}, NULL);", indent, cond_code)?;
                }
            }

            TypedStatementKind::MetaAsserts => {
                writeln_code!(output, "{}/* $ASSERTS */", indent)?;
            }

            TypedStatementKind::MetaNoPrefix => {
                writeln_code!(output, "{}/* $NOPREFIX */", indent)?;
            }

            TypedStatementKind::MetaColor { depth } => {
                if let Some(d) = depth {
                    writeln_code!(output, "{}/* $COLOR:{} */", indent, d)?;
                } else {
                    writeln_code!(output, "{}/* $COLOR:0 */", indent)?;
                }
            }

            TypedStatementKind::MetaResize { enabled } => {
                if *enabled {
                    writeln_code!(output, "{}/* $RESIZE:ON */", indent)?;
                } else {
                    writeln_code!(output, "{}/* $RESIZE:OFF */", indent)?;
                }
            }

            TypedStatementKind::MetaResizeStretch => {
                writeln_code!(output, "{}/* $RESIZE:STRETCH */", indent)?;
            }

            TypedStatementKind::MetaResizeSmooth => {
                writeln_code!(output, "{}/* $RESIZE:SMOOTH */", indent)?;
            }

            TypedStatementKind::MetaStatic => {
                writeln_code!(output, "{}/* $STATIC */", indent)?;
            }

            TypedStatementKind::MetaDynamic => {
                writeln_code!(output, "{}/* $DYNAMIC */", indent)?;
            }

            TypedStatementKind::MetaDebug => {
                writeln_code!(output, "{}/* $DEBUG */", indent)?;
            }

            TypedStatementKind::MetaIncludeOnce => {
                writeln_code!(output, "{}/* $INCLUDEONCE */", indent)?;
            }

            TypedStatementKind::MetaExeIcon { filename } => {
                writeln_code!(output, "{}/* $EXEICON:'{}' */", indent, filename)?;
            }

            TypedStatementKind::MetaVersionInfo { key, value } => {
                writeln_code!(output, "{}/* $VERSIONINFO:{}={} */", indent, key, value)?;
            }

            TypedStatementKind::MetaErrorDirective { message } => {
                // $ERROR should ideally stop compilation, but we'll emit a warning comment
                writeln_code!(output, "{}#error \"{}\"", indent, message)?;
            }

            TypedStatementKind::MetaEmbed { filename } => {
                // $EMBED embeds a file into the executable - emit as comment
                // Runtime function _EMBEDDED$ can retrieve embedded content
                writeln_code!(output, "{}/* $EMBED:'{}' */", indent, filename)?;
            }

            TypedStatementKind::MetaMidiSoundFont { filename } => {
                // $MIDISOUNDFONT sets the MIDI soundfont file for playback
                writeln_code!(output, "{}/* $MIDISOUNDFONT:'{}' */", indent, filename)?;
            }

            TypedStatementKind::MetaUnstable { feature } => {
                // $UNSTABLE enables an experimental feature
                writeln_code!(output, "{}/* $UNSTABLE:{} */", indent, feature)?;
            }

            TypedStatementKind::MetaFormat => {
                // $FORMAT is a no-op for code formatting (IDE support only)
                writeln_code!(output, "{}/* $FORMAT */", indent)?;
            }

            TypedStatementKind::MetaUseLibrary { library } => {
                // $USELIBRARY includes an external library
                writeln_code!(output, "{}/* $USELIBRARY:'{}' */", indent, library)?;
            }
        }

        Ok(())
    }
}
