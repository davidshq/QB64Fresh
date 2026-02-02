//! Statement emitter state and helper methods.
//!
//! This module holds the context structs and [`StmtEmitter`] definition used by
//! the statement code generator. The main [`emit_stmt`](super::StmtEmitter::emit_stmt)
//! dispatcher lives in `mod.rs` and delegates to submodules; helpers such as
//! [`emit_expr`](StmtEmitter::emit_expr) and [`proc_label`](StmtEmitter::proc_label)
//! are implemented here.

use std::collections::{HashMap, HashSet};

use crate::ast::ExitType;
use crate::codegen::error::CodeGenError;
use crate::codegen::RuntimeMode;
use crate::semantic::typed_ir::{TypedStatement, TypedStatementKind};
use crate::writeln_code;

use crate::codegen::c_backend::types::c_identifier;

/// Context for the current loop (for EXIT statement handling).
#[derive(Clone)]
pub(in crate::codegen::c_backend) struct LoopContext {
    /// Label to break to.
    pub break_label: String,
    /// Type of loop (For, While, Do).
    pub loop_type: ExitType,
}

/// Code generation state (labeling, indentation, emitted labels).
#[derive(Clone)]
pub(in crate::codegen::c_backend) struct CodeGenState {
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
pub(in crate::codegen::c_backend) struct ProcedureContext {
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
pub(in crate::codegen::c_backend) struct GlobalSymbols {
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
pub(in crate::codegen::c_backend) struct DataContext {
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
pub(in crate::codegen::c_backend) struct EventContext {
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
pub(in crate::codegen::c_backend) struct DebugContext {
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
pub(in crate::codegen::c_backend) struct Config {
    /// Disable SHELL / _SHELLHIDE (compile-time error if used). Set from CBackend via --no-shell.
    pub no_shell: bool,
    /// Runtime mode (inline vs external) - affects how QbString data is accessed.
    pub runtime_mode: RuntimeMode,
}

impl Config {
    /// Creates a new config with the specified runtime mode.
    fn with_runtime_mode(runtime_mode: RuntimeMode) -> Self {
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
pub(in crate::codegen::c_backend) struct StmtEmitter {
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
    /// Whether the program uses OpenGL (SUB _GL or _GL*); when true, emit _GL* calls under #ifdef QB64FRESH_OPENGL.
    pub uses_opengl: bool,
}

impl StmtEmitter {
    /// Creates a new statement emitter.
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::with_runtime_mode(RuntimeMode::inline())
    }

    /// Creates a new statement emitter with the specified runtime mode.
    pub fn with_runtime_mode(runtime_mode: RuntimeMode) -> Self {
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
            uses_opengl: false,
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
    pub(crate) fn emit_expr(
        &self,
        expr: &crate::semantic::typed_ir::TypedExpr,
    ) -> Result<String, crate::codegen::error::CodeGenError> {
        // For external runtime, use emit_expr_external which wraps string temporaries
        // with qbs_tmp_register() at the point of creation (nested expressions too).
        // For inline runtime, use emit_expr which doesn't wrap (the inline C functions
        // call qbs_tmp_register internally).
        if self.config.runtime_mode.is_external() {
            crate::codegen::c_backend::expr::emit_expr_external(
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
            crate::codegen::c_backend::expr::emit_expr(
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
    pub(super) fn proc_label(&self, label: &str) -> String {
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
    pub(super) fn is_executable_statement(kind: &TypedStatementKind) -> bool {
        !matches!(
            kind,
            TypedStatementKind::Label { .. }
                | TypedStatementKind::Data { .. }
                | TypedStatementKind::SubDefinition { .. }
                | TypedStatementKind::FunctionDefinition { .. }
                | TypedStatementKind::DeclareLibrary { .. }
        )
    }

    /// Returns the C expression for the current source file (for qb_dbg_line and qb_evnt).
    pub(super) fn debug_file_expr(&self) -> String {
        let file = self
            .debug
            .source_file
            .as_deref()
            .unwrap_or("_qb_dbg_source_file");
        if file == "_qb_dbg_source_file" {
            file.to_string()
        } else {
            format!("\"{}\"", file.replace('\\', "\\\\").replace('"', "\\\""))
        }
    }

    /// Emits a debug line hook if debug mode is enabled.
    ///
    /// This calls `qb_dbg_line(line, file)` before executing the actual statement,
    /// allowing the debugger to check breakpoints and step mode.
    pub(super) fn emit_debug_line(
        &self,
        stmt: &TypedStatement,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        if !self.debug.enabled {
            return Ok(());
        }

        let line = stmt.span.line;
        let file = self.debug_file_expr();
        let indent = self.indent_str();

        writeln_code!(output, "{}qb_dbg_line({}, {});", indent, line, file)?;
        Ok(())
    }
}
