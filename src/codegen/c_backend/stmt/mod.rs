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
//! - `audio.rs` - Audio statement code generation (BEEP, SOUND, PLAY, _SND*)
//! - `control_flow.rs` - IF, FOR, WHILE, DO, SELECT CASE
//! - `data.rs` - DATA/READ/RESTORE handling
//! - `def_fn.rs` - DEF FN single-line and multi-line functions
//! - `definitions.rs` - DIM, REDIM, SUB/FUNCTION definitions, DECLARE LIBRARY
//! - `error_jump.rs` - Error handling (ON ERROR) and computed jumps (ON...GOTO/GOSUB)
//! - `graphics.rs` - Graphics statement code generation (SCREEN, PSET, LINE, CIRCLE, etc.)
//! - `io.rs` - PRINT and INPUT helpers
//! - `meta.rs` - Meta directive code generation ($IF, $LET, $CHECKING, etc.)
//! - `misc.rs` - Miscellaneous statement code generation (SWAP, CONTINUE, RUN, etc.)
//! - `system.rs` - System integration statement code generation (KILL, RENAME, SHELL, etc.)
//!
//! # Loop Handling
//!
//! Loops are tracked on a stack to support EXIT statements. Each loop
//! type (FOR, WHILE, DO) generates a break label that EXIT can target.

mod assignments;
mod audio;
mod control_flow;
mod data;
mod def_fn;
mod definitions;
mod error_jump;
mod graphics;
mod io;
mod meta;
mod misc;
mod system;

// Re-export standalone functions for use by parent module
pub(in crate::codegen::c_backend) use definitions::{emit_dynamic_library_section, emit_params};

use std::collections::{HashMap, HashSet};

use crate::ast::{AllowFullScreenMode, EventControlMode, ExitType, FullScreenMode, PrintSeparator};
use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{
    TypedExprKind, TypedInputTarget, TypedStatement, TypedStatementKind,
};
use crate::semantic::types::BasicType;
use crate::writeln_code;

use super::expr::{escape_string, unwrap_qb_str_from_c};
use super::types::{c_identifier, c_type, default_init};

/// Context for the current loop (for EXIT statement handling).
#[derive(Clone)]
pub(super) struct LoopContext {
    /// Label to break to.
    pub break_label: String,
    /// Type of loop (For, While, Do).
    pub loop_type: ExitType,
}

/// Code generation state (labeling, indentation, emitted labels).
#[derive(Clone)]
pub(super) struct CodeGenState {
    /// Counter for generating unique labels.
    pub label_counter: u32,
    /// Current indentation level.
    pub indent: usize,
    /// Labels already emitted (to skip duplicates from ambiguous parsing).
    pub emitted_labels: std::collections::HashSet<String>,
}

impl CodeGenState {
    /// Creates a new code generation state.
    fn new() -> Self {
        Self {
            label_counter: 0,
            indent: 0,
            emitted_labels: std::collections::HashSet::new(),
        }
    }

    /// Generates a unique label name.
    pub fn next_label(&mut self, prefix: &str) -> String {
        let label = format!("_qb_{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    /// Returns the current indentation string.
    pub fn indent_str(&self) -> String {
        "    ".repeat(self.indent)
    }
}

/// Context for the current procedure/function being emitted.
#[derive(Clone)]
pub(super) struct ProcedureContext {
    /// Current procedure name (for unique label generation).
    pub current_proc: Option<String>,
    /// Current function's return variable (for EXIT FUNCTION).
    pub current_func_ret_var: Option<String>,
    /// Current function's byref STRING parameter names (for EXIT FUNCTION writebacks).
    /// These need to be written back before any return statement.
    pub current_func_byref_strings: Vec<String>,
    /// BYREF string parameter basic names (same order as `current_func_byref_strings`).
    /// Used to match abbreviated variable references (e.g. Variable "e" → param "elements").
    pub current_func_byref_string_basic_names: Vec<String>,
    /// Current function's parameter names (for detecting variable shadowing in DIM statements).
    /// In BASIC, local variables can shadow parameters, but in C this causes compilation errors.
    /// We need to rename local variables that shadow BYVAL parameters.
    pub current_func_param_names: std::collections::HashSet<String>,
    /// Current function's BYREF scalar parameter names (for pointer dereferencing).
    /// These are parameters that are passed by reference and need to be dereferenced when accessed.
    /// Excludes arrays (which are already pointers) and strings (which have special writeback logic).
    pub current_func_byref_scalar_names: std::collections::HashSet<String>,
    /// Current function's BYREF UDT parameter names and local pointer variables (for pointer field access).
    /// These are UDT parameters passed by reference or local variables that are pointers,
    /// so field access must use -> instead of .
    pub current_func_byref_udt_names: std::collections::HashSet<String>,
    /// Map of variable name renamings (original -> renamed) for variables that shadow parameters.
    /// When a local variable shadows a parameter, we rename it and track the mapping here.
    pub variable_renames: HashMap<String, String>,
}

impl ProcedureContext {
    /// Creates a new procedure context.
    fn new() -> Self {
        Self {
            current_proc: None,
            current_func_ret_var: None,
            current_func_byref_strings: Vec::new(),
            current_func_byref_string_basic_names: Vec::new(),
            current_func_param_names: std::collections::HashSet::new(),
            current_func_byref_scalar_names: std::collections::HashSet::new(),
            current_func_byref_udt_names: std::collections::HashSet::new(),
            variable_renames: HashMap::new(),
        }
    }

    /// Clears the procedure context (when exiting a procedure/function).
    pub fn clear(&mut self) {
        self.current_proc = None;
        self.current_func_ret_var = None;
        self.current_func_byref_strings.clear();
        self.current_func_byref_string_basic_names.clear();
        self.current_func_param_names.clear();
        self.current_func_byref_scalar_names.clear();
        self.current_func_byref_udt_names.clear();
        self.variable_renames.clear();
    }
}

/// Global symbol tracking (variables, arrays, constants).
#[derive(Clone)]
pub(super) struct GlobalSymbols {
    /// Global variable names (to avoid re-declaring as locals).
    pub var_names: std::collections::HashSet<String>,
    /// Global array variable names (arrays can't be implicitly declared as scalars).
    pub array_names: std::collections::HashSet<String>,
    /// DIM SHARED global variable names (accessible from all functions without local SHARED).
    pub shared_names: std::collections::HashSet<String>,
    /// Global CONST names (shouldn't be redeclared as local variables).
    pub const_names: std::collections::HashSet<String>,
}

impl GlobalSymbols {
    /// Creates a new global symbols tracker.
    fn new() -> Self {
        Self {
            var_names: std::collections::HashSet::new(),
            array_names: std::collections::HashSet::new(),
            shared_names: std::collections::HashSet::new(),
            const_names: std::collections::HashSet::new(),
        }
    }
}

/// Context for DATA statement handling.
#[derive(Clone)]
pub(super) struct DataContext {
    /// Map of DATA labels to their indices (for RESTORE with label).
    pub label_indices: HashMap<String, usize>,
}

impl DataContext {
    /// Creates a new data context.
    fn new() -> Self {
        Self {
            label_indices: HashMap::new(),
        }
    }
}

/// Context for event handling (STRIG events).
#[derive(Clone)]
pub(super) struct EventContext {
    /// Counter for generating unique STRIG event IDs.
    pub strig_event_counter: u32,
    /// Registered STRIG event handlers: (event_id, target_label).
    pub strig_handlers: Vec<(u32, String)>,
}

impl EventContext {
    /// Creates a new event context.
    fn new() -> Self {
        Self {
            strig_event_counter: 0,
            strig_handlers: Vec::new(),
        }
    }
}

/// Debug configuration.
#[derive(Clone)]
pub(super) struct DebugContext {
    /// Debug mode enabled (emit qb_dbg_line calls).
    pub enabled: bool,
    /// Source file name for debug tracking.
    pub source_file: Option<String>,
}

impl DebugContext {
    /// Creates a new debug context.
    fn new() -> Self {
        Self {
            enabled: false,
            source_file: None,
        }
    }
}

/// Compiler configuration options.
#[derive(Clone)]
pub(super) struct Config {
    /// Disable SHELL / _SHELLHIDE (compile-time error if used). Set from CBackend via --no-shell.
    pub no_shell: bool,
    /// Runtime mode (inline vs external) - affects how QbString data is accessed.
    pub runtime_mode: super::RuntimeMode,
}

impl Config {
    /// Creates a new config with the specified runtime mode.
    fn with_runtime_mode(runtime_mode: super::RuntimeMode) -> Self {
        Self {
            no_shell: false,
            runtime_mode,
        }
    }
}

/// State required for statement emission.
///
/// This is passed through recursive statement emission to track
/// indentation, loop context, and label generation.
///
/// The state is organized into focused context structs for better maintainability:
/// - `codegen`: Label generation, indentation, emitted labels
/// - `procedure`: Current procedure/function context and variable renamings
/// - `globals`: Global symbol tracking
/// - `data`: DATA statement handling
/// - `events`: Event handler tracking
/// - `debug`: Debug configuration
/// - `config`: Compiler configuration options
pub(super) struct StmtEmitter {
    /// Code generation state (labels, indentation).
    pub codegen: CodeGenState,
    /// Stack of loop labels for EXIT statements.
    pub loop_stack: Vec<LoopContext>,
    /// Procedure/function context.
    pub procedure: ProcedureContext,
    /// Global symbol tracking.
    pub globals: GlobalSymbols,
    /// DATA statement context.
    pub data: DataContext,
    /// Event handling context.
    pub events: EventContext,
    /// Debug configuration.
    pub debug: DebugContext,
    /// Compiler configuration.
    pub config: Config,
    /// C names of external functions from DECLARE DYNAMIC LIBRARY (call via qb_dyn_<c_name>).
    pub dynamic_external_c_names: HashSet<String>,
    /// Whether $SCREENHIDE directive was present in the program.
    pub screen_hide_requested: bool,
}

impl StmtEmitter {
    /// Creates a new statement emitter.
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::with_runtime_mode(super::RuntimeMode::inline())
    }

    /// Creates a new statement emitter with the specified runtime mode.
    pub fn with_runtime_mode(runtime_mode: super::RuntimeMode) -> Self {
        Self {
            codegen: CodeGenState::new(),
            loop_stack: Vec::new(),
            procedure: ProcedureContext::new(),
            globals: GlobalSymbols::new(),
            screen_hide_requested: false,
            data: DataContext::new(),
            events: EventContext::new(),
            debug: DebugContext::new(),
            config: Config::with_runtime_mode(runtime_mode),
            dynamic_external_c_names: HashSet::new(),
        }
    }

    /// Generates a unique label name.
    pub fn next_label(&mut self, prefix: &str) -> String {
        self.codegen.next_label(prefix)
    }

    /// Helper method to emit an expression with variable renamings applied.
    /// This wraps `emit_expr` and automatically passes the current variable renamings and parameter names.
    ///
    /// In external runtime mode, every string-returning expression (e.g. `qb_string_new`, `qb_string_concat`,
    /// built-ins) comes from the runtime library and is not auto-registered. We wrap such expressions in
    /// `qbs_tmp_register(...)` so the temp string pool can release them at scope cleanup, preventing
    /// unbounded memory growth (IDE GUI memory leak).
    pub(super) fn emit_expr(
        &self,
        expr: &crate::semantic::typed_ir::TypedExpr,
    ) -> Result<String, crate::codegen::error::CodeGenError> {
        // For external runtime, use emit_expr_external which wraps string temporaries
        // with qbs_tmp_register() at the point of creation (nested expressions too).
        // For inline runtime, use emit_expr which doesn't wrap (the inline C functions
        // call qbs_tmp_register internally).
        if self.config.runtime_mode.is_external() {
            super::expr::emit_expr_external(
                expr,
                self.config.no_shell,
                &self.procedure.variable_renames,
                &self.procedure.current_func_param_names,
                &self.procedure.current_func_byref_scalar_names,
                &self.procedure.current_func_byref_udt_names,
                &self.procedure.current_func_byref_strings,
                &self.procedure.current_func_byref_string_basic_names,
                &self.dynamic_external_c_names,
            )
        } else {
            super::expr::emit_expr(
                expr,
                self.config.no_shell,
                &self.procedure.variable_renames,
                &self.procedure.current_func_param_names,
                &self.procedure.current_func_byref_scalar_names,
                &self.procedure.current_func_byref_udt_names,
                &self.procedure.current_func_byref_strings,
                &self.procedure.current_func_byref_string_basic_names,
                &self.dynamic_external_c_names,
            )
        }
    }

    /// Converts a BASIC label to a C label, prefixing with procedure name if in a procedure.
    /// C identifiers cannot start with a digit, so labels whose first character is a digit
    /// (e.g. "0", "100") are prefixed with `qb_line_` to produce valid C (e.g. `qb_line_0`).
    fn proc_label(&self, label: &str) -> String {
        let base_label = if label.is_empty() {
            "qb_line_empty".to_string()
        } else if label.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            format!("qb_line_{}", label)
        } else {
            c_identifier(label)
        };
        if let Some(ref proc) = self.procedure.current_proc {
            format!("{}_{}", proc, base_label)
        } else {
            base_label
        }
    }

    /// Returns the current indentation string.
    pub(crate) fn indent_str(&self) -> String {
        self.codegen.indent_str()
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
        if !self.debug.enabled {
            return Ok(());
        }

        // Extract line number from span
        let line = stmt.span.line;
        let file = self
            .debug
            .source_file
            .as_deref()
            .unwrap_or("_qb_dbg_source_file");

        let indent = self.indent_str();

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
        if self.debug.enabled && Self::is_executable_statement(&stmt.kind) {
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
                let target_code_raw = self.emit_expr(target)?;
                // For fixed-length strings used with strlen/strncpy, we need the raw char array,
                // not the qb_str_from_c() wrapped version
                // Unwrap multiple levels if needed (e.g., qb_str_from_c(qb_str_from_c(...)))
                let target_code = if matches!(
                    target.basic_type,
                    crate::semantic::types::BasicType::FixedString(_)
                ) {
                    let mut unwrapped = unwrap_qb_str_from_c(&target_code_raw);
                    // Handle double-wrapping: qb_str_from_c(qb_str_from_c(...))
                    while unwrapped.starts_with("qb_str_from_c(") {
                        unwrapped = unwrap_qb_str_from_c(&unwrapped);
                    }
                    unwrapped
                } else {
                    target_code_raw
                };
                let start_code = self.emit_expr(start)?;
                let value_code = self.emit_expr(value)?;

                // Check if target is a fixed-length string (char array)
                // Fixed-length strings need manual character copying, not qb_mid_assign
                if matches!(
                    target.basic_type,
                    crate::semantic::types::BasicType::FixedString(_)
                ) {
                    // For fixed-length strings, manually copy characters
                    let len_code = if let Some(len_expr) = length {
                        self.emit_expr(len_expr)?
                    } else {
                        // No length specified - replace rest of string
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
                } else {
                    // For dynamic strings (QbString*), use qb_mid_assign
                    // We need to pass its address to qb_mid_assign
                    if let Some(len_expr) = length {
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
                let format_code = self.emit_expr(format)?;
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
                        let value_code = self.emit_expr(value)?;
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
                            .map(|e| self.emit_expr(e))
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
                            .map(|e| self.emit_expr(e))
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
                    "{}qb_input_string({}, &{}, 0);",
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
                    let code_expr = self.emit_expr(code)?;
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
                    let code_expr = self.emit_expr(code)?;
                    writeln_code!(output, "{}exit((int){});", indent, code_expr)?;
                } else {
                    writeln_code!(output, "{}exit(0);", indent)?;
                }
            }

            TypedStatementKind::Sleep { seconds } => {
                if let Some(secs) = seconds {
                    let secs_code = self.emit_expr(secs)?;
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
                let port_code = self.emit_expr(port)?;
                let and_code = self.emit_expr(and_mask)?;
                if let Some(xor) = xor_mask {
                    let xor_code = self.emit_expr(xor)?;
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
                let secs_code = self.emit_expr(seconds)?;
                writeln_code!(output, "{}qb_delay({});", indent, secs_code)?;
            }

            TypedStatementKind::Limit { fps } => {
                let fps_code = self.emit_expr(fps)?;
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
                    let mut c_name = c_identifier(array_name);
                    // Use same name as DIM/array access (including parameter-shadow renames)
                    if let Some(renamed) = self.procedure.variable_renames.get(&c_name)
                        && !renamed.ends_with("_scalar")
                    {
                        c_name = renamed.clone();
                    }
                    // Pass the array pointer (variable value), not its address; registry key is the pointer from qb_array_register
                    writeln_code!(output, "{}qb_array_erase({});", indent, c_name)?;
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
                    let arg_code = self.emit_expr(arg)?;
                    // If the argument is our local UDT pointer (byref_udt_names), always pass as-is (pointer), never &.
                    // BASIC is case-insensitive; compare lowercased C identifiers.
                    let arg_is_udt_pointer = matches!(&arg.kind, TypedExprKind::Variable(name)
                    if {
                        let c_arg = c_identifier(name).to_lowercase();
                        self.procedure.current_func_byref_udt_names.iter()
                            .any(|s| s.to_lowercase() == c_arg)
                    });
                    // Check if this parameter is byref (and not an array ref which decays to pointer).
                    // For UDT arguments that are already pointers (from BYREF UDT params in current function),
                    // arg_is_udt_pointer is true and we skip adding &.
                    // For regular UDT variables (not already pointers), we DO need to add & for BYREF params.
                    let is_byref = !arg_is_udt_pointer
                        && params
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
                            // Special case: If the variable is const (like HASHFLAG_*, DEPENDENCY_*),
                            // we need to cast away const to match function signatures that expect non-const pointers
                            let needs_parens = arg_code.contains(' ')
                                || arg_code.contains('(')
                                || arg_code.contains('[');

                            // Check if this looks like a const variable (all uppercase with underscores)
                            // Common patterns: HASHFLAG_*, DEPENDENCY_*
                            let is_likely_const = !needs_parens
                                && arg_code
                                    .chars()
                                    .all(|c| c.is_uppercase() || c == '_' || c.is_ascii_digit())
                                && (arg_code.starts_with("HASHFLAG_")
                                    || arg_code.starts_with("DEPENDENCY_")
                                    || arg_code.contains("_FLAG")
                                    || arg_code.contains("_DEPENDENCY"));

                            if is_likely_const {
                                // Cast away const: (int32_t*)&CONSTANT
                                let c_ty = param_type
                                    .map(c_type)
                                    .unwrap_or_else(|| "int32_t".to_string());
                                args_codes.push(format!("({}*)&{}", c_ty, arg_code));
                            } else {
                                args_codes.push(format!("&({})", arg_code));
                            }
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
                            var.is_static,
                            output,
                        )?;
                    }
                }
            }

            TypedStatementKind::Const { definitions } => {
                for (name, value, _basic_type) in definitions {
                    let c_name = c_identifier(name);
                    let value_code = self.emit_expr(value)?;
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
                    let seg_code = self.emit_expr(seg_expr)?;
                    writeln_code!(output, "{}qb_def_seg((int32_t){});", indent, seg_code)?;
                } else {
                    // DEF SEG without argument resets to default segment
                    writeln_code!(output, "{}qb_def_seg(-1);", indent)?;
                }
            }

            TypedStatementKind::Poke { address, value } => {
                // POKE writes a byte to memory within the current segment.
                let addr_code = self.emit_expr(address)?;
                let val_code = self.emit_expr(value)?;
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
                let mem_code = self.emit_expr(mem)?;
                let offset_code = self.emit_expr(offset)?;
                let value_code = self.emit_expr(value)?;
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
                if self.codegen.emitted_labels.insert(c_label.clone()) {
                    writeln_code!(output, "{}:", c_label)?;
                }
            }

            TypedStatementKind::Comment(text) => {
                writeln_code!(output, "{}/* {} */", indent, text)?;
            }

            TypedStatementKind::Expression(expr) => {
                let expr_code = self.emit_expr(expr)?;
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

            // Miscellaneous statements (part 1)
            k @ (TypedStatementKind::Swap { .. } | TypedStatementKind::Continue { .. }) => {
                misc::emit_misc_stmt(self, k, &indent, output)?;
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
                    let seed_code = self.emit_expr(seed_expr)?;
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

            TypedStatementKind::LockFile { file_num } => {
                self.emit_lock_file(&indent, file_num, output)?;
            }
            TypedStatementKind::UnlockFile { file_num } => {
                self.emit_unlock_file(&indent, file_num, output)?;
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
                // Arrays respect $STATIC/$DYNAMIC directive for allocation method
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
                        // Array: use static allocation if $STATIC is active, otherwise dynamic
                        if var.is_static {
                            // Static array (fixed-size C array)
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
                            // Register array bounds for UBOUND/LBOUND
                            if var.dimensions.len() == 1 {
                                writeln_code!(
                                    output,
                                    "{}qb_array_register({}, {}, {});",
                                    indent,
                                    c_name,
                                    var.dimensions[0].lower,
                                    var.dimensions[0].upper
                                )?;
                            } else {
                                let lowers: Vec<String> =
                                    var.dimensions.iter().map(|d| d.lower.to_string()).collect();
                                let uppers: Vec<String> =
                                    var.dimensions.iter().map(|d| d.upper.to_string()).collect();
                                writeln_code!(
                                    output,
                                    "{}{{ int32_t _lb[] = {{{}}}; int32_t _ub[] = {{{}}}; qb_array_register_md({}, {}, _lb, _ub); }}",
                                    indent,
                                    lowers.join(", "),
                                    uppers.join(", "),
                                    c_name,
                                    var.dimensions.len()
                                )?;
                            }
                        } else {
                            // Dynamic array (pointer + malloc)
                            // Use emit_dim helper for consistency
                            self.emit_dim(
                                &indent,
                                &var.name,
                                &var.basic_type,
                                &var.dimensions,
                                false, // Not static (dynamic allocation)
                                output,
                            )?;
                            // Add static keyword to the pointer declaration
                            // (the variable persists between calls, but array is dynamically allocated)
                            // Note: emit_dim already emitted the declaration, so we need to modify it
                            // Actually, for STATIC statement with dynamic arrays, we want:
                            // static type* name = malloc(...);
                            // But emit_dim doesn't add static. Let's handle it specially here.
                            // Actually, let's just use emit_dim and then we'll need to track if it's static.
                            // For now, let's emit it manually for STATIC statement dynamic arrays:
                            let sizes: Vec<String> = var
                                .dimensions
                                .iter()
                                .map(|d| format!("({})", d.upper - d.lower + 1))
                                .collect();
                            let size_expr = sizes.join(" * ");
                            let alloc_fn = if var.basic_type == BasicType::String {
                                "calloc"
                            } else {
                                "malloc"
                            };
                            if var.basic_type == BasicType::String {
                                writeln_code!(
                                    output,
                                    "{}static {}* {} = {}({}, sizeof({}));",
                                    indent,
                                    c_ty,
                                    c_name,
                                    alloc_fn,
                                    size_expr,
                                    c_ty
                                )?;
                            } else {
                                writeln_code!(
                                    output,
                                    "{}static {}* {} = {}(sizeof({}) * {});",
                                    indent,
                                    c_ty,
                                    c_name,
                                    alloc_fn,
                                    c_ty,
                                    size_expr
                                )?;
                            }
                            // Register array bounds
                            if var.dimensions.len() == 1 {
                                writeln_code!(
                                    output,
                                    "{}qb_array_register({}, {}, {});",
                                    indent,
                                    c_name,
                                    var.dimensions[0].lower,
                                    var.dimensions[0].upper
                                )?;
                            } else {
                                let lowers: Vec<String> =
                                    var.dimensions.iter().map(|d| d.lower.to_string()).collect();
                                let uppers: Vec<String> =
                                    var.dimensions.iter().map(|d| d.upper.to_string()).collect();
                                writeln_code!(
                                    output,
                                    "{}{{ int32_t _lb[] = {{{}}}; int32_t _ub[] = {{{}}}; qb_array_register_md({}, {}, _lb, _ub); }}",
                                    indent,
                                    lowers.join(", "),
                                    uppers.join(", "),
                                    c_name,
                                    var.dimensions.len()
                                )?;
                            }
                        }
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

            // Graphics statements
            k @ (TypedStatementKind::Screen { .. }
            | TypedStatementKind::Cls { .. }
            | TypedStatementKind::Color { .. }
            | TypedStatementKind::Locate { .. }
            | TypedStatementKind::Pset { .. }
            | TypedStatementKind::Preset { .. }
            | TypedStatementKind::Line { .. }
            | TypedStatementKind::Circle { .. }
            | TypedStatementKind::Paint { .. }
            | TypedStatementKind::GfxDisplay
            | TypedStatementKind::ControlChr { .. }
            | TypedStatementKind::MapUnicode { .. }
            | TypedStatementKind::GfxResize { .. }
            | TypedStatementKind::Palette { .. }
            | TypedStatementKind::Pcopy { .. }
            | TypedStatementKind::Width { .. }
            | TypedStatementKind::View { .. }
            | TypedStatementKind::ViewPrint { .. }
            | TypedStatementKind::WindowCoords { .. }
            | TypedStatementKind::DrawCmd { .. }
            | TypedStatementKind::GraphicsGet { .. }
            | TypedStatementKind::GraphicsPut { .. }
            | TypedStatementKind::FreeImage { .. }
            | TypedStatementKind::PutImage { .. }
            | TypedStatementKind::SourceImg { .. }
            | TypedStatementKind::DestImg { .. }
            | TypedStatementKind::PrintStringStmt { .. }
            | TypedStatementKind::AutoDisplay { .. }) => {
                graphics::emit_graphics_stmt(self, k, &indent, output)?;
            }

            // Audio statements
            k @ (TypedStatementKind::Beep
            | TypedStatementKind::SoundStmt { .. }
            | TypedStatementKind::PlayStmt { .. }
            | TypedStatementKind::SndClose { .. }
            | TypedStatementKind::SndPlay { .. }
            | TypedStatementKind::SndStop { .. }
            | TypedStatementKind::SndPause { .. }
            | TypedStatementKind::SndLoop { .. }
            | TypedStatementKind::SndVol { .. }
            | TypedStatementKind::SndBal { .. }
            | TypedStatementKind::SndRaw { .. }
            | TypedStatementKind::SndPlayFile { .. }
            | TypedStatementKind::SndPlayCopy { .. }
            | TypedStatementKind::SndSetPos { .. }) => {
                audio::emit_audio_stmt(self, k, &indent, output)?;
            }

            // System integration statements
            k @ (TypedStatementKind::Kill { .. }
            | TypedStatementKind::Rename { .. }
            | TypedStatementKind::Mkdir { .. }
            | TypedStatementKind::Rmdir { .. }
            | TypedStatementKind::Chdir { .. }
            | TypedStatementKind::Environ { .. }
            | TypedStatementKind::ShellCmd { .. }
            | TypedStatementKind::ShellHide { .. }
            | TypedStatementKind::Bload { .. }
            | TypedStatementKind::Bsave { .. }
            | TypedStatementKind::Setmem { .. }
            | TypedStatementKind::CallAbsolute { .. }
            | TypedStatementKind::MouseHide
            | TypedStatementKind::MouseShow
            | TypedStatementKind::MouseMoveStmt { .. }
            | TypedStatementKind::ClipboardSet { .. }
            | TypedStatementKind::DeclareLibrary { .. }
            | TypedStatementKind::DeclareSub { .. }
            | TypedStatementKind::DeclareFunction { .. }) => {
                system::emit_system_stmt(self, k, &indent, output)?;
            }

            // ==================== Phase 7: Additional Statements ====================
            TypedStatementKind::Run { target } => {
                // RUN: NULL = no-op (restart); non-NULL = run program then exit (minimal implementation)
                if let Some(t) = target {
                    let target_code = self.emit_expr(t)?;
                    writeln_code!(output, "{}qb_run({});", indent, target_code)?;
                } else {
                    writeln_code!(output, "{}qb_run(NULL);", indent)?;
                }
            }

            TypedStatementKind::Chain { filename } => {
                let filename_code = self.emit_expr(filename)?;
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
                    let expr_code = self.emit_expr(&item.expr)?;
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
                    let spec_code = self.emit_expr(spec)?;
                    writeln_code!(output, "{}qb_files({});", indent, spec_code)?;
                } else {
                    writeln_code!(output, "{}qb_files(NULL);", indent)?;
                }
            }

            TypedStatementKind::FieldStmt { file_num, fields } => {
                let file_num_code = self.emit_expr(file_num)?;
                writeln_code!(
                    output,
                    "{}qb_field_start((int32_t)({}));",
                    indent,
                    file_num_code
                )?;
                for field in fields {
                    let width_code = self.emit_expr(&field.width)?;
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
                let value_code = self.emit_expr(value)?;
                let c_var = c_identifier(variable);
                writeln_code!(output, "{}qb_lset(&{}, {});", indent, c_var, value_code)?;
            }

            TypedStatementKind::Rset { variable, value } => {
                let value_code = self.emit_expr(value)?;
                let c_var = c_identifier(variable);
                writeln_code!(output, "{}qb_rset(&{}, {});", indent, c_var, value_code)?;
            }

            TypedStatementKind::OnKey { key_num, target } => {
                let key_code = self.emit_expr(key_num)?;
                writeln_code!(
                    output,
                    "{}qb_on_key((int32_t)({}), &&{});",
                    indent,
                    key_code,
                    target
                )?;
            }

            TypedStatementKind::KeyControl { key_num, mode } => {
                let key_code = self.emit_expr(key_num)?;
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
                let interval_code = self.emit_expr(interval)?;
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
                let btn_code = self.emit_expr(button_num)?;
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
                self.events.strig_event_counter += 1;
                let event_id = self.events.strig_event_counter;
                self.events.strig_handlers.push((event_id, target.clone()));

                let btn_code = self.emit_expr(button_num)?;
                writeln_code!(
                    output,
                    "{}qb_on_strig((int32_t)({}), {});",
                    indent,
                    btn_code,
                    event_id
                )?;
            }

            TypedStatementKind::OnCom { port_num, target } => {
                let port_code = self.emit_expr(port_num)?;
                writeln_code!(
                    output,
                    "{}qb_on_com((int32_t)({}), &&{});",
                    indent,
                    port_code,
                    target
                )?;
            }

            TypedStatementKind::ComControl { port_num, mode } => {
                let port_code = self.emit_expr(port_num)?;
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
                let signal_code = self.emit_expr(signal_num)?;
                writeln_code!(
                    output,
                    "{}qb_on_signal((int32_t)({}), &&{});",
                    indent,
                    signal_code,
                    target
                )?;
            }

            TypedStatementKind::SignalControl { signal_num, mode } => {
                let signal_code = self.emit_expr(signal_num)?;
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
                let port_code = self.emit_expr(port)?;
                let value_code = self.emit_expr(value)?;
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
                let int_code = self.emit_expr(int_num)?;
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
                let int_code = self.emit_expr(int_num)?;
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
                let file_code = self.emit_expr(file_num)?;
                let string_code = self.emit_expr(control_string)?;
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
                    let size_code = self.emit_expr(size)?;
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
                let title_code = self.emit_expr(title)?;
                writeln_code!(output, "{}qb_title({});", indent, title_code)?;
            }

            TypedStatementKind::ScreenMoveStmt { x, y, center } => {
                if *center {
                    writeln_code!(output, "{}qb_screenmove_center();", indent)?;
                } else {
                    let x_code = match x {
                        Some(e) => self.emit_expr(e)?,
                        None => "0".to_string(),
                    };
                    let y_code = match y {
                        Some(e) => self.emit_expr(e)?,
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
                    let handle_code = self.emit_expr(h)?;
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
                let title_code = self.emit_expr(title)?;
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
                let cond_code = self.emit_expr(condition)?;
                if let Some(msg) = message {
                    let msg_code = self.emit_expr(msg)?;
                    writeln_code!(output, "{}qb_assert({}, {});", indent, cond_code, msg_code)?;
                } else {
                    writeln_code!(output, "{}qb_assert({}, NULL);", indent, cond_code)?;
                }
            }

            // Meta directives (part 2)
            k @ (TypedStatementKind::MetaAsserts { .. }
            | TypedStatementKind::MetaNoPrefix
            | TypedStatementKind::MetaColor { .. }
            | TypedStatementKind::MetaResize { .. }
            | TypedStatementKind::MetaResizeStretch
            | TypedStatementKind::MetaResizeSmooth
            | TypedStatementKind::MetaStatic
            | TypedStatementKind::MetaDynamic
            | TypedStatementKind::MetaDebug
            | TypedStatementKind::MetaIncludeOnce
            | TypedStatementKind::MetaExeIcon { .. }
            | TypedStatementKind::MetaVersionInfo { .. }
            | TypedStatementKind::MetaErrorDirective { .. }
            | TypedStatementKind::MetaEmbed { .. }
            | TypedStatementKind::MetaMidiSoundFont { .. }
            | TypedStatementKind::MetaUnstable { .. }
            | TypedStatementKind::MetaFormat
            | TypedStatementKind::MetaUseLibrary { .. }) => {
                meta::emit_meta_stmt(self, k, &indent, output)?;
            }
        }

        Ok(())
    }
}
