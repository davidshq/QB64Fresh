//! C code generation backend for QB64Fresh.
//!
//! This module generates portable C99 code from the typed intermediate representation.
//! The generated code uses `<stdint.h>` for predictable type sizes and a minimal
//! runtime library for I/O operations.
//!
//! # Module Structure
//!
//! The C backend is organized into focused sub-modules:
//!
//! - [`types`] - BASIC-to-C type mapping utilities
//! - [`runtime`] - Inline runtime library code generation
//! - [`expr`] - Expression emission
//! - [`stmt`] - Statement emission (main dispatcher + core helpers)
//! - [`file_io`] - File I/O statement helpers (OPEN, CLOSE, GET, PUT, etc.)
//! - [`analysis`] - Program analysis (globals, DATA collection)
//! - [`const_fold`] - Compile-time constant folding optimization
//!
//! # Generated Code Structure
//!
//! ```c
//! // Includes and runtime declarations
//! #include <stdio.h>
//! #include <stdint.h>
//! #include "qb_runtime.h"
//!
//! // Global variables
//! int32_t my_global;
//!
//! // Forward declarations for SUBs/FUNCTIONs
//! void sub_mysub(void);
//!
//! // SUB/FUNCTION definitions
//! void sub_mysub(void) { ... }
//!
//! // Main program
//! int main(int argc, char** argv) {
//!     // Main program statements
//!     return 0;
//! }
//! ```
//!
//! # Type Mapping
//!
//! | BASIC Type    | C Type        |
//! |---------------|---------------|
//! | INTEGER       | int16_t       |
//! | LONG          | int32_t       |
//! | _INTEGER64    | int64_t       |
//! | SINGLE        | float         |
//! | DOUBLE        | double        |
//! | STRING        | qb_string*    |

mod analysis;
mod const_fold;
mod expr;
mod file_io;
mod implicit_vars;
mod runtime;
mod stmt;
mod types;

use std::collections::HashSet;
use std::fmt::Write;

use crate::codegen::error::CodeGenError;
use crate::codegen::{CodeGenerator, GeneratedOutput};
use crate::semantic::typed_ir::{TypedProgram, TypedStatement, TypedStatementKind};

use self::analysis::{collect_callback_wrappers, collect_data_values, collect_type_definitions};
use self::implicit_vars::collect_implicit_locals;
use self::runtime::emit_header_with_debug;
use self::stmt::{StmtEmitter, emit_params};
use self::types::c_identifier;

/// Runtime mode for code generation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RuntimeMode {
    /// Inline runtime code in generated output (no external dependencies).
    #[default]
    Inline,
    /// Use external runtime library (requires libqb64fresh_rt.a).
    External,
}

/// C code generation backend.
///
/// Produces portable C99 code that compiles with gcc, clang, or MSVC.
///
/// # Example
///
/// ```ignore
/// use qb64fresh::codegen::{CodeGenerator, CBackend};
///
/// let backend = CBackend::new();
/// let output = backend.generate(&typed_program)?;
/// println!("{}", output.code);
/// ```
pub struct CBackend {
    /// Runtime mode.
    runtime_mode: RuntimeMode,
    /// Enable debug hooks for breakpoints, stepping, etc.
    debug_enabled: bool,
    /// Source file name (for debug tracking).
    source_file: Option<String>,
}

impl Default for CBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl CBackend {
    /// Creates a new C backend with inline runtime.
    pub fn new() -> Self {
        Self {
            runtime_mode: RuntimeMode::Inline,
            debug_enabled: false,
            source_file: None,
        }
    }

    /// Creates a new C backend with the specified runtime mode.
    pub fn with_runtime_mode(runtime_mode: RuntimeMode) -> Self {
        Self {
            runtime_mode,
            debug_enabled: false,
            source_file: None,
        }
    }

    /// Enables debug mode for breakpoints, stepping, and debugger integration.
    ///
    /// When enabled, the generated code includes:
    /// - `qb_dbg_line()` calls before each statement
    /// - `qb_dbg_enter_proc()`/`qb_dbg_exit_proc()` for SUB/FUNCTION tracking
    /// - Debug IPC for communication with the debugger
    pub fn with_debug(mut self, enabled: bool) -> Self {
        self.debug_enabled = enabled;
        self
    }

    /// Sets the source file name for debug tracking.
    pub fn with_source_file(mut self, source_file: &str) -> Self {
        self.source_file = Some(source_file.to_string());
        self
    }

    /// Emits a callback wrapper function for _PROCPTR.
    ///
    /// Generates a C function with the signature matching the BASIC procedure
    /// that can be passed to C library functions expecting callbacks.
    fn emit_callback_wrapper(&self, output: &mut String, wrapper: &analysis::CallbackWrapperInfo) {
        use std::fmt::Write;
        use types::c_type;

        // Determine return type
        let return_type_str = match &wrapper.return_type {
            Some(bt) => c_type(bt),
            None => "void".to_string(),
        };

        // Build parameter list for wrapper signature
        let mut wrapper_params = Vec::new();
        let mut call_args = Vec::new();

        for (i, param) in wrapper.params.iter().enumerate() {
            let param_name = format!("p{}", i);
            let c_ty = c_type(&param.basic_type);

            if param.by_val {
                // BYVAL: pass value directly
                wrapper_params.push(format!("{} {}", c_ty, param_name));
                call_args.push(param_name);
            } else {
                // BYREF: pass pointer, BASIC function expects pointer
                wrapper_params.push(format!("{}* {}", c_ty, param_name));
                call_args.push(param_name);
            }
        }

        let params_str = if wrapper_params.is_empty() {
            "void".to_string()
        } else {
            wrapper_params.join(", ")
        };

        let args_str = call_args.join(", ");

        // Emit wrapper function
        writeln!(
            output,
            "static {} {}({}) {{",
            return_type_str, wrapper.wrapper_name, params_str
        )
        .unwrap();

        if wrapper.return_type.is_some() {
            writeln!(output, "    return {}({});", wrapper.c_func_name, args_str).unwrap();
        } else {
            writeln!(output, "    {}({});", wrapper.c_func_name, args_str).unwrap();
        }

        writeln!(output, "}}").unwrap();
        writeln!(output).unwrap();
    }
}

impl CodeGenerator for CBackend {
    fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, CodeGenError> {
        let mut emitter = StmtEmitter::new();
        emitter.debug_enabled = self.debug_enabled;
        emitter.debug_source_file = self.source_file.clone();
        let mut output = String::new();

        // Header (with optional debug support)
        emit_header_with_debug(
            &mut output,
            self.runtime_mode,
            self.debug_enabled,
            self.source_file.as_deref(),
        );

        // TYPE definitions (must come before global variables that use those types)
        let type_defs = collect_type_definitions(program);
        if !type_defs.is_empty() {
            writeln!(output, "/* User-Defined Types */").unwrap();
            for def in type_defs {
                write!(output, "{}", def).unwrap();
            }
            writeln!(output).unwrap();
        }

        // Collect globals, forward declarations, and string constant initializations
        let (globals, forward_decls, string_const_inits) =
            analysis::collect_globals(program, emit_params);

        // Global variables
        if !globals.is_empty() {
            writeln!(output, "/* Global Variables */").unwrap();
            for decl in &globals {
                writeln!(output, "{}", decl).unwrap();
            }
            writeln!(output).unwrap();
        }

        // Forward declarations
        if !forward_decls.is_empty() {
            writeln!(output, "/* Forward Declarations */").unwrap();
            for decl in forward_decls {
                writeln!(output, "{}", decl).unwrap();
            }
            writeln!(output).unwrap();
        }

        // Collect and emit DATA pool
        let data_pool = collect_data_values(program);
        // Store label indices for RESTORE statement emission
        emitter.data_label_indices = data_pool.label_indices;

        if !data_pool.values.is_empty() {
            writeln!(output, "/* DATA Pool */").unwrap();
            writeln!(output, "typedef struct {{ char type; union {{ double n; const char* s; }} v; }} _qb_data_item;").unwrap();
            write!(output, "static _qb_data_item _qb_data[] = {{").unwrap();
            for (i, (val, type_tag)) in data_pool.values.iter().enumerate() {
                if i > 0 {
                    write!(output, ",").unwrap();
                }
                if *type_tag == "d" {
                    write!(output, " {{'d', {{.n = {}}}}}", val).unwrap();
                } else {
                    write!(output, " {{'s', {{.s = {}}}}}", val).unwrap();
                }
            }
            writeln!(output, " }};").unwrap();
            writeln!(output, "static int _qb_data_ptr = 0;").unwrap();
            writeln!(
                output,
                "static const int _qb_data_count = {};",
                data_pool.values.len()
            )
            .unwrap();
            writeln!(output).unwrap();
        }

        // Collect names of DIM SHARED variables - these are accessible from all functions
        // without needing local SHARED statements, so they shouldn't be shadowed by implicit locals
        let mut shared_global_names: HashSet<String> = HashSet::new();
        for stmt in &program.statements {
            if let TypedStatementKind::Dim { variables, shared } = &stmt.kind
                && *shared
            {
                for var in variables {
                    shared_global_names.insert(c_identifier(&var.name));
                }
            }
        }

        // Collect names of global CONST values - these shouldn't be redeclared as local variables
        let mut global_const_names: HashSet<String> = HashSet::new();
        for stmt in &program.statements {
            if let TypedStatementKind::Const { definitions } = &stmt.kind {
                for (name, _, _) in definitions {
                    global_const_names.insert(c_identifier(name));
                }
            }
        }

        // Build sets of global variable names for use by SUB/FUNCTION implicit local detection
        // Global declarations look like "type name = init;" or "const type name = init;"
        // Arrays are identified by: pointer types (type*, type**) or static arrays (name[N])
        let mut global_var_names: HashSet<String> = HashSet::new();
        let mut global_array_names: HashSet<String> = HashSet::new();

        for decl in &globals {
            // Parse various declaration forms:
            // "type name = init;" -> parts[1] is name
            // "type name[N];" -> parts[1] is name (static array)
            // "type* name = NULL;" -> parts[1] is name (dynamic array)
            // "type** name = NULL;" -> parts[1] is name (2D dynamic array)
            // "const type name = init;" -> parts[2] is name
            // "type (*name)[N]" -> fixed-length string arrays
            let decl_trimmed = decl.trim_end_matches(';');
            let parts: Vec<&str> = decl_trimmed.split_whitespace().collect();

            // Handle "const type name" form (const is parts[0])
            let (type_idx, name_idx) = if parts.first() == Some(&"const") {
                (1, 2)
            } else {
                (0, 1)
            };

            if parts.len() > name_idx {
                let type_part = parts.get(type_idx).unwrap_or(&"");
                let raw_name = parts[name_idx];

                // Determine if this is an array:
                // - Type contains * (pointer = dynamic array)
                // - Name contains [ (static array)
                // - Pattern (*name) (fixed-length string array)
                let is_array =
                    type_part.contains('*') || raw_name.contains('[') || raw_name.starts_with("(*");

                // Handle fixed-length string array: "char (*name)[N]"
                // Pattern: (*name) or (*name)[N] - extract name from parens
                if raw_name.starts_with("(*") {
                    if let Some(end_paren) = raw_name.find(')') {
                        let name = &raw_name[2..end_paren];
                        if !name.is_empty() {
                            global_var_names.insert(name.to_string());
                            if is_array {
                                global_array_names.insert(name.to_string());
                            }
                        }
                    }
                    continue;
                }

                // Get the name part (might have [N] or = suffix)
                if let Some(name) = raw_name.split('[').next()
                    && let Some(name) = name.split('=').next()
                {
                    let name = name.trim();
                    if !name.is_empty() && !name.starts_with('(') {
                        global_var_names.insert(name.to_string());
                        if is_array {
                            global_array_names.insert(name.to_string());
                        }
                    }
                }
            }
        }

        // Set global variable names on emitter for SUB/FUNCTION implicit local detection
        emitter.global_var_names = global_var_names.clone();
        emitter.global_array_names = global_array_names.clone();
        emitter.shared_global_names = shared_global_names.clone();
        emitter.global_const_names = global_const_names.clone();

        // SUB/FUNCTION definitions (emit before main)
        for stmt in &program.statements {
            match &stmt.kind {
                TypedStatementKind::SubDefinition { .. }
                | TypedStatementKind::FunctionDefinition { .. } => {
                    emitter.emit_stmt(stmt, &mut output)?;
                }
                _ => {}
            }
        }

        // Callback wrappers for _PROCPTR (emit after SUB/FUNCTION definitions)
        let callback_wrappers = collect_callback_wrappers(program);
        if !callback_wrappers.is_empty() {
            writeln!(output, "/* Callback Wrappers for _PROCPTR */").unwrap();
            for wrapper in &callback_wrappers {
                // Generate a wrapper function with the actual procedure signature
                self.emit_callback_wrapper(&mut output, wrapper);
            }
        }

        // Main function
        writeln!(output, "int main(int argc, char** argv) {{").unwrap();
        // Initialize command-line argument access for COMMAND$ and _COMMANDCOUNT
        writeln!(output, "    qb_init_args(argc, argv);").unwrap();
        // Initialize start directory for _STARTDIR$
        writeln!(output, "    qb_init_startdir();").unwrap();
        // Initialize VGA palette for INP/OUT port emulation
        writeln!(output, "    _qb_init_palette();").unwrap();
        // STRIG event dispatch global (stores event ID for dispatch switch)
        writeln!(output, "    static uint32_t _qb_strig_event_id = 0;").unwrap();
        writeln!(output).unwrap();

        // Debug initialization (if debug mode enabled)
        if self.debug_enabled {
            writeln!(output, "    /* Initialize debugger connection */").unwrap();
            writeln!(
                output,
                "    const char* _qb_dbg_pipe_env = getenv(\"QB64FRESH_DEBUG_PIPE\");"
            )
            .unwrap();
            writeln!(output, "    if (_qb_dbg_pipe_env) {{").unwrap();
            writeln!(output, "        qb_dbg_init(_qb_dbg_pipe_env);").unwrap();
            writeln!(output, "    }}").unwrap();
            writeln!(output).unwrap();
        }

        // Initialize string constants (can't be done at global scope in C)
        // These are NOT cleaned up as they need to persist for the program lifetime
        if !string_const_inits.is_empty() {
            writeln!(output, "    /* Initialize string constants */").unwrap();
            for init in &string_const_inits {
                writeln!(output, "    {};", init).unwrap();
            }
            writeln!(output).unwrap();
        }

        // Save temp pool base for main program AFTER string constant initialization
        // Strings created before this point (globals) won't be cleaned up
        writeln!(output, "    uint64_t _qbs_main_base = qbs_tmp_base_get();").unwrap();
        writeln!(output).unwrap();

        // Collect main-level statements (excluding SUB/FUNCTION definitions)
        let main_stmts: Vec<&TypedStatement> = program
            .statements
            .iter()
            .filter(|s| {
                !matches!(
                    s.kind,
                    TypedStatementKind::SubDefinition { .. }
                        | TypedStatementKind::FunctionDefinition { .. }
                )
            })
            .collect();

        // Collect implicit local declarations for main
        // Convert Vec<&TypedStatement> to slice for collect_implicit_locals
        let main_stmts_owned: Vec<TypedStatement> =
            main_stmts.iter().map(|s| (*s).clone()).collect();
        // Pass is_main_program=true so arrays with existing globals use the global
        // (for cross-function sharing) instead of creating shadowing locals
        // Pass empty always_exclude: main has no return variable
        // Pass shared_global_names: DIM SHARED vars shouldn't be re-declared
        // Pass global_const_names: CONST values must never be shadowed by locals
        let implicit_locals = collect_implicit_locals(
            &main_stmts_owned,
            &[],
            &global_var_names,
            &HashSet::new(),
            &global_array_names,
            &shared_global_names,
            &global_const_names,
            true,
        );

        // Emit implicit local declarations
        if !implicit_locals.is_empty() {
            writeln!(output, "    /* Implicit local variables */").unwrap();
            for decl in &implicit_locals {
                writeln!(output, "    {}", decl).unwrap();
            }
            writeln!(output).unwrap();
        }

        emitter.indent = 1;

        // Emit main program statements (excluding SUB/FUNCTION definitions)
        // Each statement cleans up its temp strings to prevent memory growth
        for stmt in &program.statements {
            match &stmt.kind {
                TypedStatementKind::SubDefinition { .. }
                | TypedStatementKind::FunctionDefinition { .. } => {
                    // Already emitted above
                }
                _ => {
                    emitter.emit_stmt(stmt, &mut output)?;
                    // Clean up temp strings after each statement
                    writeln!(output, "    qbs_cleanup(_qbs_main_base, 0);").unwrap();
                }
            }
        }

        // Generate STRIG event dispatch code
        // This is always generated because loop code references _qb_strig_dispatch
        // The dispatch uses _qb_strig_event_id which was set at the check point
        writeln!(output).unwrap();
        writeln!(output, "    /* STRIG Event Dispatch */").unwrap();
        writeln!(output, "    goto _qb_strig_dispatch_end;").unwrap();
        writeln!(output, "_qb_strig_dispatch:").unwrap();
        if emitter.strig_handlers.is_empty() {
            // No handlers registered - just return to caller
            writeln!(output, "    goto *_gosub_stack[--_gosub_sp];").unwrap();
        } else {
            writeln!(output, "    switch (_qb_strig_event_id) {{").unwrap();
            for (event_id, label) in &emitter.strig_handlers {
                writeln!(output, "        case {}: goto {};", event_id, label).unwrap();
            }
            writeln!(output, "        default: goto *_gosub_stack[--_gosub_sp];").unwrap();
            writeln!(output, "    }}").unwrap();
        }
        writeln!(output, "_qb_strig_dispatch_end:").unwrap();

        writeln!(output).unwrap();
        // Debug shutdown before exit
        if self.debug_enabled {
            writeln!(output, "    qb_dbg_shutdown();").unwrap();
        }
        writeln!(output, "    return 0;").unwrap();
        writeln!(output, "}}").unwrap();

        Ok(GeneratedOutput::c_code(output))
    }

    fn backend_name(&self) -> &str {
        "C"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::semantic::typed_ir::{
        TypedExpr, TypedPrintItem, TypedStatement, TypedStatementKind,
    };
    use crate::semantic::types::BasicType;

    #[test]
    fn test_generate_empty_program() {
        let program = TypedProgram::new(vec![]);
        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(result.code.contains("int main("));
        assert!(result.code.contains("return 0;"));
    }

    #[test]
    fn test_generate_print_statement() {
        let program = TypedProgram::new(vec![TypedStatement::new(
            TypedStatementKind::Print {
                items: vec![TypedPrintItem {
                    expr: TypedExpr::string("Hello".to_string(), Span::new(6, 13)),
                    separator: None,
                }],
                newline: true,
            },
            Span::new(0, 13),
        )]);

        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(result.code.contains("qb_print_string"));
        assert!(result.code.contains("qb_print_newline"));
    }

    #[test]
    fn test_generate_assignment() {
        let program = TypedProgram::new(vec![TypedStatement::new(
            TypedStatementKind::Assignment {
                name: "x".to_string(),
                value: TypedExpr::integer(42, Span::new(4, 6)),
                target_type: BasicType::Long,
            },
            Span::new(0, 6),
        )]);

        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(result.code.contains("x = 42LL"));
    }

    #[test]
    fn test_restore_with_label_codegen() {
        let program = TypedProgram::new(vec![
            TypedStatement::new(
                TypedStatementKind::Label {
                    name: "testLabel".to_string(),
                },
                Span::new(0, 10),
            ),
            TypedStatement::new(
                TypedStatementKind::Data {
                    values: vec![
                        crate::semantic::typed_ir::TypedDataValue::Integer(1),
                        crate::semantic::typed_ir::TypedDataValue::Integer(2),
                    ],
                },
                Span::new(11, 20),
            ),
            TypedStatement::new(
                TypedStatementKind::Restore {
                    label: Some("testLabel".to_string()),
                },
                Span::new(21, 36),
            ),
        ]);

        let backend = CBackend::new();
        let result = backend.generate(&program).unwrap();

        assert!(
            result
                .code
                .contains("_qb_data_ptr = 0; /* RESTORE testLabel */")
        );
    }
}
