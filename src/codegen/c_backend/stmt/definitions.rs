//! Definition helper methods for statement code generation.
//!
//! This module contains methods for emitting definitions in C code:
//! - External declarations (`DECLARE LIBRARY`)
//! - Subroutine definitions (`SUB`)
//! - Function definitions (`FUNCTION`)
//! - Variable declarations (`DIM`)
//! - Array redimensioning (`REDIM`)
//!
//! These methods are part of [`StmtEmitter`](super::StmtEmitter) and handle
//! the generation of C code for BASIC definition statements.

use std::collections::HashSet;

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{
    TypedArrayDimension, TypedExternalDeclaration, TypedParameter, TypedRedimDimension,
    TypedStatement,
};
use crate::semantic::types::BasicType;
use crate::writeln_code;

use crate::codegen::c_backend::analysis::DynamicLibInfo;
use crate::codegen::c_backend::expr::c_function_name;
use crate::codegen::c_backend::implicit_vars::collect_implicit_locals;
use crate::codegen::c_backend::types::{c_identifier, c_type, default_init};

/// Returns (return_type, params_str) for an external declaration (for extern or function pointer).
fn extern_decl_signature(decl: &TypedExternalDeclaration) -> (String, String) {
    let return_type = if decl.return_type == BasicType::String {
        "char*".to_string()
    } else {
        c_type(&decl.return_type)
    };
    let params: Vec<String> = decl
        .params
        .iter()
        .map(|p| {
            let param_type = if p.typ == BasicType::String {
                if p.is_byval {
                    "const char*".to_string()
                } else {
                    "qb_string**".to_string()
                }
            } else {
                c_type(&p.typ)
            };
            format!("{} {}", param_type, p.name)
        })
        .collect();
    let params_str = if params.is_empty() {
        "void".to_string()
    } else {
        params.join(", ")
    };
    (return_type, params_str)
}

/// Returns (return_type, params_types_only) for a function pointer typedef (no param names).
fn extern_decl_signature_types_only(decl: &TypedExternalDeclaration) -> (String, String) {
    let return_type = if decl.return_type == BasicType::String {
        "char*".to_string()
    } else {
        c_type(&decl.return_type)
    };
    let param_types: Vec<String> = decl
        .params
        .iter()
        .map(|p| {
            if p.typ == BasicType::String {
                if p.is_byval {
                    "const char*".to_string()
                } else {
                    "qb_string**".to_string()
                }
            } else {
                c_type(&p.typ)
            }
        })
        .collect();
    let params_str = if param_types.is_empty() {
        "void".to_string()
    } else {
        param_types.join(", ")
    };
    (return_type, params_str)
}

/// Emits the dynamic library section: handle variables, function pointers, and qb_init_dynamic_libs().
///
/// Uses dlopen/dlsym on Unix and LoadLibrary/GetProcAddress on Windows. Call qb_init_dynamic_libs()
/// at the start of main.
///
/// Each function pointer is emitted once per unique c_name across all dynamic libs (later lib
/// overwrites if same symbol appears in multiple libs). Empty library path is skipped (no load,
/// pointers for that lib stay NULL).
pub(in crate::codegen::c_backend) fn emit_dynamic_library_section(
    libs: &[DynamicLibInfo],
    output: &mut String,
) -> Result<(), CodeGenError> {
    if libs.is_empty() {
        return Ok(());
    }
    writeln_code!(output, "/* DECLARE DYNAMIC LIBRARY - runtime loading */")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "#include <windows.h>")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "#include <dlfcn.h>")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // Emit one handle per lib
    for lib in libs {
        let handle_name = format!("qb_dll_{}", lib.handle_id);
        writeln_code!(output, "static void* {} = NULL;", handle_name)?;
    }
    writeln_code!(output)?;

    // Emit one typedef + one function pointer per unique c_name (first occurrence wins signature)
    let mut seen_c_names: HashSet<String> = HashSet::new();
    for lib in libs {
        for decl in &lib.declarations {
            if seen_c_names.insert(decl.c_name.clone()) {
                let (ret, params) = extern_decl_signature_types_only(decl);
                let ptr_name = format!("qb_dyn_{}", decl.c_name);
                let typedef_name = format!("{}_t", ptr_name);
                writeln_code!(output, "typedef {} (*{})({});", ret, typedef_name, params)?;
                writeln_code!(output, "static {} {} = NULL;", typedef_name, ptr_name)?;
            }
        }
    }
    writeln_code!(output)?;

    writeln_code!(output, "static void qb_init_dynamic_libs(void) {{")?;
    for lib in libs {
        let handle_name = format!("qb_dll_{}", lib.handle_id);
        let lib_path = lib
            .library_name
            .as_deref()
            .unwrap_or("")
            .replace('\\', "\\\\")
            .replace('"', "\\\"");
        // Skip load when path is empty (invalid; would fail at runtime)
        if !lib_path.is_empty() {
            writeln_code!(output, "#ifdef _WIN32")?;
            writeln_code!(
                output,
                "    {} = (void*)LoadLibrary(\"{}\");",
                handle_name,
                lib_path
            )?;
            writeln_code!(output, "#else")?;
            writeln_code!(
                output,
                "    {} = dlopen(\"{}\", RTLD_LAZY);",
                handle_name,
                lib_path
            )?;
            writeln_code!(output, "#endif")?;
        }
        for decl in &lib.declarations {
            let ptr_name = format!("qb_dyn_{}", decl.c_name);
            let (ret, params) = extern_decl_signature_types_only(decl);
            let cast_type = format!("{} (*)({})", ret, params);
            writeln_code!(output, "#ifdef _WIN32")?;
            writeln_code!(
                output,
                "    if ({}) {} = ({})GetProcAddress((HMODULE){}, \"{}\");",
                handle_name,
                ptr_name,
                cast_type,
                handle_name,
                decl.c_name
            )?;
            writeln_code!(output, "#else")?;
            writeln_code!(
                output,
                "    if ({}) {} = ({})dlsym({}, \"{}\");",
                handle_name,
                ptr_name,
                cast_type,
                handle_name,
                decl.c_name
            )?;
            writeln_code!(output, "#endif")?;
        }
    }
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}

impl super::StmtEmitter {
    /// Emits an external function/sub declaration.
    ///
    /// Generates a C `extern` declaration for functions declared via
    /// `DECLARE LIBRARY`. Handles special cases for:
    /// - STRING parameters (BYVAL → `const char*`, BYREF → `qb_string**`)
    /// - STRING return types → `char*`
    /// - System functions that conflict with C standard library
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `decl` - The external declaration to emit
    /// * `output` - Output buffer to write to
    pub(in crate::codegen::c_backend) fn emit_extern_declaration(
        &self,
        indent: &str,
        decl: &TypedExternalDeclaration,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let (return_type, params_str) = extern_decl_signature(decl);

        // Skip functions that are already declared in C standard library headers
        // These would conflict with the system declarations
        let skip_system_functions = [
            "getpid",
            "getppid",
            "getuid",
            "getgid",
            "geteuid",
            "getegid",
            "fork",
            "exec",
            "execl",
            "execv",
            "execle",
            "execve",
            "exit",
            "abort",
            "_exit",
            "sleep",
            "usleep",
            "nanosleep",
            "malloc",
            "calloc",
            "realloc",
            "free",
            "printf",
            "fprintf",
            "sprintf",
            "snprintf",
            "scanf",
            "fscanf",
            "sscanf",
            "fopen",
            "fclose",
            "fread",
            "fwrite",
            "fseek",
            "ftell",
            "strlen",
            "strcpy",
            "strncpy",
            "strcat",
            "strncat",
            "strcmp",
            "strncmp",
            "memcpy",
            "memmove",
            "memset",
            "memcmp",
            "sin",
            "cos",
            "tan",
            "asin",
            "acos",
            "atan",
            "atan2",
            "sinh",
            "cosh",
            "tanh",
            "exp",
            "log",
            "log10",
            "pow",
            "sqrt",
            "ceil",
            "floor",
            "fabs",
            "fmod",
            "time",
            "clock",
            "difftime",
            "mktime",
            "localtime",
            "gmtime",
            "rand",
            "srand",
            "abs",
            "labs",
            "atoi",
            "atol",
            "atof",
        ];

        if skip_system_functions.contains(&decl.c_name.as_str()) {
            writeln_code!(
                output,
                "{}// extern {} {}({}); // already declared in system headers",
                indent,
                return_type,
                decl.c_name,
                params_str
            )?;
        } else {
            writeln_code!(
                output,
                "{}extern {} {}({});",
                indent,
                return_type,
                decl.c_name,
                params_str
            )?;
        }
        Ok(())
    }

    /// Emits a SUB definition.
    ///
    /// Generates a C function with void return type for a BASIC SUB.
    /// Handles:
    /// - Parameter generation with byref handling
    /// - Local copies of byref parameters
    /// - Implicit local variable declarations
    /// - Unique label generation via `current_proc`
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `name` - SUB name
    /// * `params` - Parameter list
    /// * `body` - SUB body statements
    /// * `output` - Output buffer to write to
    pub(in crate::codegen::c_backend) fn emit_sub_definition(
        &mut self,
        indent: &str,
        name: &str,
        params: &[TypedParameter],
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = format!("qb_sub_{}", c_identifier(name).to_lowercase());
        let params_str = emit_params(params);

        writeln_code!(output, "{}void {}({}) {{", indent, c_name, params_str)?;

        // Emit debug entry hook
        if self.debug.enabled {
            // Estimate line number from first statement's span if available
            let entry_line = body.first().map(|s| s.span.start).unwrap_or(0);
            writeln_code!(
                output,
                "    qb_dbg_enter_proc(\"{}\", {});",
                name,
                entry_line
            )?;
        }

        // Create local copies of byref parameters
        self.emit_byref_copies(params, output)?;

        // Collect and emit implicit local variables
        // Use global_var_names to avoid re-declaring globals as locals
        // Pass is_main_program=false: SUB creates locals even if globals with same name exist
        // Pass empty always_exclude: SUB has no return variable
        // Pass global_array_names: arrays can't be shadowed by implicit scalars
        // Pass shared_global_names: DIM SHARED vars are accessible without local SHARED
        // Pass global_const_names: CONST values must never be shadowed by local vars
        let implicit_locals = collect_implicit_locals(
            body,
            params,
            &self.globals.var_names,
            &HashSet::new(),
            &self.globals.array_names,
            &self.globals.shared_names,
            &self.globals.const_names,
            false,
            &mut self.procedure.variable_renames,
        );
        for decl in &implicit_locals {
            writeln_code!(output, "    {}", decl)?;
        }
        if !implicit_locals.is_empty() || params.iter().any(|p| !p.by_val) {
            writeln_code!(output)?;
        }

        // Set current procedure name for unique label generation
        self.procedure.current_proc = Some(c_name.clone());
        // Track byref STRING parameters for EXIT SUB writebacks
        self.procedure.current_func_byref_strings = params
            .iter()
            .filter(|p| !p.by_val && p.basic_type == BasicType::String && !p.is_array)
            .map(|p| c_identifier(&p.name))
            .collect();
        // Track BYREF scalar parameter names (for pointer dereferencing in SUB body)
        // Excludes arrays (which are already pointers) and strings (which have special writeback logic)
        self.procedure.current_func_byref_scalar_names = params
            .iter()
            .filter(|p| {
                !p.by_val
                    && !p.is_array
                    && p.basic_type != BasicType::String
                    && !matches!(p.basic_type, BasicType::FixedString(_))
                    && !matches!(p.basic_type, BasicType::UserDefined(_))
            })
            .map(|p| c_identifier(&p.name))
            .collect();
        // Track BYREF UDT parameter names (for pointer field access -> instead of .)
        self.procedure.current_func_byref_udt_names = params
            .iter()
            .filter(|p| {
                !p.by_val && !p.is_array && matches!(p.basic_type, BasicType::UserDefined(_))
            })
            .map(|p| c_identifier(&p.name))
            .collect();
        // Track all parameter names (BYVAL parameters don't get _ref suffix, so they can be shadowed)
        // We need to rename local variables that shadow BYVAL parameters to avoid C compilation errors
        self.procedure.current_func_param_names =
            params.iter().map(|p| c_identifier(&p.name)).collect();

        // Save temp pool base for this procedure - cleanup after each statement
        writeln_code!(output, "    uint64_t _qbs_proc_base = qbs_tmp_base_get();")?;
        writeln_code!(output)?;

        self.codegen.indent += 1;
        for stmt in body {
            self.emit_stmt(stmt, output)?;
            // Clean up temp strings after each statement
            writeln_code!(output, "    qbs_cleanup(_qbs_proc_base, 0);")?;
        }
        self.codegen.indent -= 1;

        self.procedure.clear();

        // Write back STRING byref parameters to caller's variables
        // Only strings need writeback - they use reference counting and local copies
        // Numeric parameters often have constants passed, which can't be written to
        emit_string_writebacks(params, output)?;

        // Emit STRIG dispatch label (required because loops emit goto _qb_strig_dispatch)
        // This is a no-op stub - actual dispatch happens in main() only
        writeln_code!(output, "    goto _qb_strig_dispatch_end;")?;
        writeln_code!(output, "_qb_strig_dispatch:")?;
        writeln_code!(output, "_qb_strig_dispatch_end:")?;

        // Emit debug exit hook
        if self.debug.enabled {
            writeln_code!(output, "    qb_dbg_exit_proc(\"{}\");", name)?;
        }

        writeln_code!(output, "{}}}", indent)?;
        writeln_code!(output)?;
        Ok(())
    }

    /// Emits a FUNCTION definition.
    ///
    /// Generates a C function with the appropriate return type for a BASIC FUNCTION.
    /// Handles:
    /// - Parameter generation with byref handling
    /// - Return variable initialization
    /// - Local copies of byref parameters
    /// - Implicit local variable declarations
    /// - Unique label generation via `current_proc`
    /// - EXIT FUNCTION support via `current_func_ret_var`
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `name` - FUNCTION name
    /// * `params` - Parameter list
    /// * `return_type` - Return type of the function
    /// * `body` - FUNCTION body statements
    /// * `output` - Output buffer to write to
    pub(in crate::codegen::c_backend) fn emit_function_definition(
        &mut self,
        indent: &str,
        name: &str,
        params: &[TypedParameter],
        return_type: &BasicType,
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_function_name(name);
        let c_ret_type = c_type(return_type);
        let params_str = emit_params(params);

        writeln_code!(
            output,
            "{}{} {}({}) {{",
            indent,
            c_ret_type,
            c_name,
            params_str
        )?;

        // Emit debug entry hook
        if self.debug.enabled {
            let entry_line = body.first().map(|s| s.span.start).unwrap_or(0);
            writeln_code!(
                output,
                "    qb_dbg_enter_proc(\"{}\", {});",
                name,
                entry_line
            )?;
        }

        let ret_var = c_identifier(name);
        writeln_code!(
            output,
            "    {} {} = {};",
            c_ret_type,
            ret_var,
            default_init(return_type)
        )?;

        // Create local copies of byref parameters
        self.emit_byref_copies(params, output)?;

        // Collect and emit implicit local variables
        // Pass is_main_program=false: FUNCTION creates locals even if globals with same name exist
        // Pass return variable in always_exclude to prevent redeclaration
        // Pass global_array_names: arrays can't be shadowed by implicit scalars
        // Pass shared_global_names: DIM SHARED vars are accessible without local SHARED
        // Pass global_const_names: CONST values must never be shadowed by local vars
        let mut always_exclude = HashSet::new();
        always_exclude.insert(ret_var.clone());
        let implicit_locals = collect_implicit_locals(
            body,
            params,
            &self.globals.var_names,
            &always_exclude,
            &self.globals.array_names,
            &self.globals.shared_names,
            &self.globals.const_names,
            false,
            &mut self.procedure.variable_renames,
        );
        for decl in &implicit_locals {
            writeln_code!(output, "    {}", decl)?;
        }
        if !implicit_locals.is_empty() || params.iter().any(|p| !p.by_val) {
            writeln_code!(output)?;
        }

        // Set current procedure name for unique label generation
        self.procedure.current_proc = Some(c_name.clone());
        // Set return variable for EXIT FUNCTION
        self.procedure.current_func_ret_var = Some(ret_var.clone());
        // Track byref STRING parameters for EXIT FUNCTION writebacks
        self.procedure.current_func_byref_strings = params
            .iter()
            .filter(|p| !p.by_val && p.basic_type == BasicType::String && !p.is_array)
            .map(|p| c_identifier(&p.name))
            .collect();
        // Track BYREF scalar parameter names (for pointer dereferencing in function body)
        // Excludes arrays (which are already pointers) and strings (which have special writeback logic)
        self.procedure.current_func_byref_scalar_names = params
            .iter()
            .filter(|p| {
                !p.by_val
                    && !p.is_array
                    && p.basic_type != BasicType::String
                    && !matches!(p.basic_type, BasicType::FixedString(_))
                    && !matches!(p.basic_type, BasicType::UserDefined(_))
            })
            .map(|p| c_identifier(&p.name))
            .collect();
        // Track BYREF UDT parameter names (for pointer field access -> instead of .)
        self.procedure.current_func_byref_udt_names = params
            .iter()
            .filter(|p| {
                !p.by_val && !p.is_array && matches!(p.basic_type, BasicType::UserDefined(_))
            })
            .map(|p| c_identifier(&p.name))
            .collect();
        // Track all parameter names (BYVAL parameters don't get _ref suffix, so they can be shadowed)
        // We need to rename local variables that shadow BYVAL parameters to avoid C compilation errors
        self.procedure.current_func_param_names =
            params.iter().map(|p| c_identifier(&p.name)).collect();

        // Save temp pool base for this function - cleanup after each statement
        writeln_code!(output, "    uint64_t _qbs_proc_base = qbs_tmp_base_get();")?;
        writeln_code!(output)?;

        self.codegen.indent += 1;
        for stmt in body {
            self.emit_stmt(stmt, output)?;
            // Clean up temp strings after each statement
            writeln_code!(output, "    qbs_cleanup(_qbs_proc_base, 0);")?;
        }
        self.codegen.indent -= 1;

        // Clear procedure context (ret_var is a local variable, not from the context)
        self.procedure.clear();

        // Write back STRING byref parameters to caller's variables
        // Only strings need writeback - they use reference counting and local copies
        // Numeric parameters often have constants passed, which can't be written to
        emit_string_writebacks(params, output)?;

        // Emit STRIG dispatch label (required because loops emit goto _qb_strig_dispatch)
        // This is a no-op stub - actual dispatch happens in main() only
        writeln_code!(output, "    goto _qb_strig_dispatch_end;")?;
        writeln_code!(output, "_qb_strig_dispatch:")?;
        writeln_code!(output, "_qb_strig_dispatch_end:")?;

        // Emit debug exit hook
        if self.debug.enabled {
            writeln_code!(output, "    qb_dbg_exit_proc(\"{}\");", name)?;
        }

        writeln_code!(output, "    return {};", ret_var)?;
        writeln_code!(output, "{}}}", indent)?;
        writeln_code!(output)?;
        Ok(())
    }

    /// Emits a DIM statement.
    ///
    /// Generates C variable declarations for BASIC DIM statements.
    /// Handles:
    /// - Simple variables (scalar types)
    /// - Fixed-length strings (`char name[N]`)
    /// - Arrays (statically or dynamically allocated based on `is_static`)
    /// - Array bounds registration for UBOUND/LBOUND
    /// - Global vs local scope handling
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `name` - Variable name
    /// * `basic_type` - Variable type
    /// * `dimensions` - Array dimensions (empty for scalar)
    /// * `is_static` - Whether to allocate as static array (fixed-size C array)
    /// * `output` - Output buffer to write to
    pub(in crate::codegen::c_backend) fn emit_dim(
        &mut self,
        indent: &str,
        name: &str,
        basic_type: &BasicType,
        dimensions: &[TypedArrayDimension],
        is_static: bool,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let mut c_name = c_identifier(name);
        let original_c_name = c_name.clone();

        // Check if this variable shadows a function parameter
        // In BASIC, local variables can shadow parameters, but in C this causes compilation errors
        // Rename the local variable to avoid the collision
        if self.procedure.current_func_param_names.contains(&c_name) {
            c_name = format!("{}_local", c_name);
            // Track the renaming so we can update all references to this variable
            self.procedure
                .variable_renames
                .insert(original_c_name.clone(), c_name.clone());
        }

        if dimensions.is_empty() {
            // Handle fixed-length strings specially: char name[N] = "";
            if let BasicType::FixedString(len) = basic_type {
                writeln_code!(output, "{}char {}[{}] = \"\";", indent, c_name, len + 1)?;
            } else {
                let c_ty = c_type(basic_type);
                let init = default_init(basic_type);
                writeln_code!(output, "{}{} {} = {};", indent, c_ty, c_name, init)?;
            }
        } else {
            // Determine if we should use an existing global:
            // - In main (current_proc is None): use global if it exists (for arrays shared across functions)
            // - In SUB/FUNCTION: always create local (DIM inside procedure = local scope)
            let use_global =
                self.procedure.current_proc.is_none() && self.globals.var_names.contains(&c_name);

            // Static arrays: emit fixed-size C arrays
            // For global static arrays, they're already declared at global scope, so just register bounds
            if is_static {
                if use_global {
                    // Global static array already declared - just register bounds
                    if dimensions.len() == 1 {
                        writeln_code!(
                            output,
                            "{}qb_array_register({}, {}, {});",
                            indent,
                            c_name,
                            dimensions[0].lower,
                            dimensions[0].upper
                        )?;
                    } else {
                        let lowers: Vec<String> =
                            dimensions.iter().map(|d| d.lower.to_string()).collect();
                        let uppers: Vec<String> =
                            dimensions.iter().map(|d| d.upper.to_string()).collect();
                        writeln_code!(
                            output,
                            "{}{{ int32_t _lb[] = {{{}}}; int32_t _ub[] = {{{}}}; qb_array_register_md({}, {}, _lb, _ub); }}",
                            indent,
                            lowers.join(", "),
                            uppers.join(", "),
                            c_name,
                            dimensions.len()
                        )?;
                    }
                    return Ok(());
                }
                // Static arrays require compile-time constant sizes
                // Calculate sizes for each dimension
                let sizes: Vec<String> = dimensions
                    .iter()
                    .map(|d| {
                        let size = d.upper - d.lower + 1;
                        size.to_string()
                    })
                    .collect();
                let array_dims = sizes.join("][");

                // Handle fixed-length string arrays specially
                if let BasicType::FixedString(len) = basic_type {
                    if use_global {
                        writeln_code!(
                            output,
                            "{}char {}[{}][{}] = {{0}};",
                            indent,
                            c_name,
                            array_dims,
                            len + 1
                        )?;
                    } else {
                        // Local static array - use static keyword to persist between calls
                        writeln_code!(
                            output,
                            "{}static char {}[{}][{}] = {{0}};",
                            indent,
                            c_name,
                            array_dims,
                            len + 1
                        )?;
                    }
                } else {
                    let c_ty = c_type(basic_type);
                    if use_global {
                        // Global static array
                        writeln_code!(
                            output,
                            "{}{} {}[{}] = {{0}};",
                            indent,
                            c_ty,
                            c_name,
                            array_dims
                        )?;
                    } else {
                        // Local static array - use static keyword to persist between calls
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

                // Register array bounds for UBOUND/LBOUND (same as dynamic arrays)
                if dimensions.len() == 1 {
                    writeln_code!(
                        output,
                        "{}qb_array_register({}, {}, {});",
                        indent,
                        c_name,
                        dimensions[0].lower,
                        dimensions[0].upper
                    )?;
                } else {
                    let lowers: Vec<String> =
                        dimensions.iter().map(|d| d.lower.to_string()).collect();
                    let uppers: Vec<String> =
                        dimensions.iter().map(|d| d.upper.to_string()).collect();
                    writeln_code!(
                        output,
                        "{}{{ int32_t _lb[] = {{{}}}; int32_t _ub[] = {{{}}}; qb_array_register_md({}, {}, _lb, _ub); }}",
                        indent,
                        lowers.join(", "),
                        uppers.join(", "),
                        c_name,
                        dimensions.len()
                    )?;
                }
                return Ok(());
            }

            // Dynamic arrays - handle fixed-length string arrays specially
            if let BasicType::FixedString(len) = basic_type {
                let sizes: Vec<String> = dimensions
                    .iter()
                    .map(|d| format!("({})", d.upper - d.lower + 1))
                    .collect();
                let size_expr = sizes.join(" * ");
                // Array of char arrays: char (*name)[len+1] = calloc(...)
                if use_global {
                    writeln_code!(
                        output,
                        "{}{} = calloc({}, sizeof(char[{}]));",
                        indent,
                        c_name,
                        size_expr,
                        len + 1
                    )?;
                } else {
                    writeln_code!(
                        output,
                        "{}char (*{})[{}] = calloc({}, sizeof(char[{}]));",
                        indent,
                        c_name,
                        len + 1,
                        size_expr,
                        len + 1
                    )?;
                }
            } else {
                let c_ty = c_type(basic_type);
                let sizes: Vec<String> = dimensions
                    .iter()
                    .map(|d| format!("({})", d.upper - d.lower + 1))
                    .collect();
                let size_expr = sizes.join(" * ");

                // Use calloc for string arrays to ensure NULL initialization
                // (uninitialized string pointers would cause crashes in string operations)
                let _alloc_fn = if *basic_type == BasicType::String {
                    "calloc"
                } else {
                    "malloc"
                };

                if use_global {
                    // In main with existing global: allocate to global (don't create shadowing local)
                    // This is critical for arrays used by subroutines - they access the global
                    if *basic_type == BasicType::String {
                        writeln_code!(
                            output,
                            "{}{} = calloc({}, sizeof({}));",
                            indent,
                            c_name,
                            size_expr,
                            c_ty
                        )?;
                    } else {
                        writeln_code!(
                            output,
                            "{}{} = malloc(sizeof({}) * {});",
                            indent,
                            c_name,
                            c_ty,
                            size_expr
                        )?;
                    }
                } else {
                    // In SUB/FUNCTION or no global exists: create local array
                    if *basic_type == BasicType::String {
                        writeln_code!(
                            output,
                            "{}{}* {} = calloc({}, sizeof({}));",
                            indent,
                            c_ty,
                            c_name,
                            size_expr,
                            c_ty
                        )?;
                    } else {
                        writeln_code!(
                            output,
                            "{}{}* {} = malloc(sizeof({}) * {});",
                            indent,
                            c_ty,
                            c_name,
                            c_ty,
                            size_expr
                        )?;
                    }
                }
            }

            // Register array bounds for UBOUND/LBOUND
            if dimensions.len() == 1 {
                // Single dimension: use simple register function
                writeln_code!(
                    output,
                    "{}qb_array_register({}, {}, {});",
                    indent,
                    c_name,
                    dimensions[0].lower,
                    dimensions[0].upper
                )?;
            } else {
                // Multi-dimensional: use qb_array_register_md
                let lowers: Vec<String> = dimensions.iter().map(|d| d.lower.to_string()).collect();
                let uppers: Vec<String> = dimensions.iter().map(|d| d.upper.to_string()).collect();
                writeln_code!(
                    output,
                    "{}{{ int32_t _lb[] = {{{}}}; int32_t _ub[] = {{{}}}; qb_array_register_md({}, {}, _lb, _ub); }}",
                    indent,
                    lowers.join(", "),
                    uppers.join(", "),
                    c_name,
                    dimensions.len()
                )?;
            }
        }
        Ok(())
    }

    /// Emits a REDIM statement.
    ///
    /// Generates C code to resize a dynamic array. Handles:
    /// - Size calculation from dimension expressions
    /// - `REDIM _PRESERVE` to keep existing values
    /// - Regular `REDIM` to zero the entire array
    /// - Array bounds registration for UBOUND/LBOUND
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `preserve` - Whether to preserve existing values (`_PRESERVE`)
    /// * `name` - Array name
    /// * `element_type` - Array element type
    /// * `dimensions` - New array dimensions
    /// * `output` - Output buffer to write to
    pub(in crate::codegen::c_backend) fn emit_redim(
        &self,
        indent: &str,
        preserve: bool,
        name: &str,
        element_type: &BasicType,
        dimensions: &[TypedRedimDimension],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let c_name = c_identifier(name);
        let c_elem_type = c_type(element_type);
        let size_var = format!("{}_sz__", c_name);

        // Calculate total size
        if dimensions.is_empty() {
            writeln_code!(output, "{}/* REDIM {} - no dimensions */", indent, name)?;
            return Ok(());
        }

        // Calculate new size expression - emit actual expressions for runtime bounds
        let size_expr = dimensions
            .iter()
            .map(|d| {
                let upper_code = self.emit_expr(&d.upper)?;
                let lower_code = d
                    .lower
                    .as_ref()
                    .map(|e| self.emit_expr(e))
                    .transpose()?
                    .unwrap_or_else(|| "0".to_string());
                Ok(format!("({} - {} + 1)", upper_code, lower_code))
            })
            .collect::<Result<Vec<_>, CodeGenError>>()?
            .join(" * ");

        // Generate bounds registration code for after realloc
        let bounds_reg = if dimensions.len() == 1 {
            // Single dimension: use simple register function
            let lower_code = dimensions[0]
                .lower
                .as_ref()
                .map(|e| self.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "0".to_string());
            let upper_code = self.emit_expr(&dimensions[0].upper)?;
            format!(
                "    qb_array_register({}, {}, {});",
                c_name, lower_code, upper_code
            )
        } else {
            // Multi-dimensional: use qb_array_register_md
            let lowers: Result<Vec<String>, CodeGenError> = dimensions
                .iter()
                .map(|d| {
                    d.lower
                        .as_ref()
                        .map(|e| self.emit_expr(e))
                        .transpose()
                        .map(|opt| opt.unwrap_or_else(|| "0".to_string()))
                })
                .collect();
            let uppers: Result<Vec<String>, CodeGenError> = dimensions
                .iter()
                .map(|d| self.emit_expr(&d.upper))
                .collect();
            let lowers = lowers?;
            let uppers = uppers?;
            format!(
                "    {{ int32_t _lb[] = {{{}}}; int32_t _ub[] = {{{}}}; qb_array_register_md({}, {}, _lb, _ub); }}",
                lowers.join(", "),
                uppers.join(", "),
                c_name,
                dimensions.len()
            )
        };

        if preserve {
            // REDIM _PRESERVE: Keep existing values, zero only new elements
            // The size tracking variable is global (declared alongside the array)
            // to support REDIM from multiple functions sharing the same array.
            writeln_code!(output, "{}{{", indent)?;
            writeln_code!(
                output,
                "{}    size_t new_sz__ = sizeof({}) * ({});",
                indent,
                c_elem_type,
                size_expr
            )?;
            writeln_code!(
                output,
                "{}    {} = realloc({}, new_sz__);",
                indent,
                c_name,
                c_name
            )?;
            // Zero only the new portion if array grew
            writeln_code!(
                output,
                "{}    if (new_sz__ > {}) memset((char*){} + {}, 0, new_sz__ - {});",
                indent,
                size_var,
                c_name,
                size_var,
                size_var
            )?;
            writeln_code!(output, "{}    {} = new_sz__;", indent, size_var)?;
            // Register new bounds
            writeln_code!(output, "{}{}", indent, bounds_reg)?;
            writeln_code!(output, "{}}}", indent)?;
        } else {
            // Regular REDIM: Reallocate and zero entire array
            writeln_code!(output, "{}{{", indent)?;
            writeln_code!(
                output,
                "{}    size_t new_sz__ = sizeof({}) * ({});",
                indent,
                c_elem_type,
                size_expr
            )?;
            writeln_code!(
                output,
                "{}    {} = realloc({}, new_sz__);",
                indent,
                c_name,
                c_name
            )?;
            writeln_code!(output, "{}    memset({}, 0, new_sz__);", indent, c_name)?;
            // Update size tracking variable
            writeln_code!(output, "{}    {} = new_sz__;", indent, size_var)?;
            // Register new bounds
            writeln_code!(output, "{}{}", indent, bounds_reg)?;
            writeln_code!(output, "{}}}", indent)?;
        }

        Ok(())
    }

    /// Emits local copies for byref parameters and BYVAL UDT parameters.
    ///
    /// This allows the function body to use the parameter names directly without dereferencing.
    /// For each byref parameter, generates a local variable that dereferences the pointer.
    /// For BYVAL UDT parameters (passed as pointers for efficiency), creates a pointer alias.
    ///
    /// # Arguments
    ///
    /// * `params` - The parameter list
    /// * `output` - Output buffer to write to
    fn emit_byref_copies(
        &mut self,
        params: &[TypedParameter],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        for p in params {
            // Process BYREF parameters and BYVAL UDT parameters (both are passed as pointers)
            if !p.by_val || matches!(p.basic_type, BasicType::UserDefined(_)) {
                let c_name = c_identifier(&p.name);

                // Array parameters are passed as pointers - keep as pointer, don't dereference
                if p.is_array {
                    // For arrays of fixed-length strings, use pointer to array type
                    if let BasicType::FixedString(n) = p.basic_type {
                        writeln_code!(
                            output,
                            "    char (*{})[{}] = {}_ref;",
                            c_name,
                            n + 1,
                            c_name
                        )?;
                    } else {
                        let c_ty = c_type(&p.basic_type);
                        // Array remains as pointer: int32_t* arr = arr_ref;
                        writeln_code!(output, "    {}* {} = {}_ref;", c_ty, c_name, c_name)?;
                    }
                    // Emit size tracking variable for REDIM _PRESERVE support
                    // Must be static so it persists across function calls
                    writeln_code!(output, "    static size_t {}_sz__ = 0;", c_name)?;
                } else if let BasicType::FixedString(n) = p.basic_type {
                    // Fixed-length strings need special handling - use a pointer alias
                    // instead of copying (arrays can't be assigned directly in C)
                    // Create pointer alias: char* name = (*name_ref);
                    // This allows direct access to the array contents
                    writeln_code!(output, "    char* {} = (*{}_ref);", c_name, c_name)?;
                    let _ = n; // Silence unused warning
                } else if p.basic_type == BasicType::String {
                    // BYREF STRING parameters: QbString** name_ref → QbString* name = *name_ref;
                    // We need to dereference the pointer-to-pointer to get the actual string pointer
                    writeln_code!(output, "    QbString* {} = *{}_ref;", c_name, c_name)?;
                } else if matches!(p.basic_type, BasicType::UserDefined(_)) {
                    // UDT parameters (both BYREF and BYVAL): passed as pointers
                    // BYREF: qbt_Type** name_ref → qbt_Type* name = name_ref;
                    // BYVAL: qbt_Type* name_ref → qbt_Type* name = name_ref;
                    // The local variable is a pointer, so field accesses must use -> instead of .
                    let c_ty = c_type(&p.basic_type);
                    writeln_code!(output, "    {}* {} = {}_ref;", c_ty, c_name, c_name)?;
                    // Track this local variable name as a pointer for field access
                    // The parameter name is name_ref, but the local variable is name, so we need to track 'name'
                    self.procedure
                        .current_func_byref_udt_names
                        .insert(c_name.clone());
                } else {
                    let c_ty = c_type(&p.basic_type);
                    // Create pointer alias for BYREF scalar parameters: int32_t* x = x_ref;
                    // This allows modifications to write through to the caller's variable.
                    // The parameter will be dereferenced when accessed in the function body.
                    writeln_code!(output, "    {}* {} = {}_ref;", c_ty, c_name, c_name)?;
                }
            }
        }
        Ok(())
    }
}

/// Emits function/sub parameters.
///
/// Generates C parameter declarations for a BASIC SUB or FUNCTION.
/// For byref parameters, appends `_ref` to the name so the function body can
/// create a local copy with the original name.
///
/// # Arguments
///
/// * `params` - The parameter list
///
/// # Returns
///
/// A comma-separated parameter string, or "void" if no parameters.
pub(in crate::codegen::c_backend) fn emit_params(params: &[TypedParameter]) -> String {
    if params.is_empty() {
        return "void".to_string();
    }

    params
        .iter()
        .map(|p| {
            let c_name = c_identifier(&p.name);

            // Array parameters are passed as pointers to the element type
            if p.is_array {
                // For arrays of fixed-length strings, element type is char[N]
                // which can't be written as char[N]*, so use char (*name_ref)[N]
                if let BasicType::FixedString(n) = p.basic_type {
                    format!("char (*{}_ref)[{}]", c_name, n + 1)
                } else {
                    let c_ty = c_type(&p.basic_type);
                    // Array parameter: int32_t* arr_ref (pointer to array data)
                    format!("{}* {}_ref", c_ty, c_name)
                }
            } else if let BasicType::FixedString(n) = p.basic_type {
                // Fixed-length strings need special handling for array type in C
                if p.by_val {
                    // Pass by value: char name[N] (array decays to pointer)
                    format!("char {}[{}]", c_name, n + 1)
                } else {
                    // Pass by reference: pointer to array - char (*name_ref)[N]
                    format!("char (*{}_ref)[{}]", c_name, n + 1)
                }
            } else {
                let c_ty = c_type(&p.basic_type);
                if p.by_val {
                    // For BYVAL UDT parameters, pass as pointer for efficiency (avoid copying large structs)
                    // The local variable will be a pointer copy, so field access needs ->
                    if matches!(p.basic_type, BasicType::UserDefined(_)) {
                        format!("{}* {}_ref", c_ty, c_name)
                    } else {
                        format!("{} {}", c_ty, c_name)
                    }
                } else {
                    // Byref parameters get _ref suffix; we'll create a local copy with the original name
                    format!("{}* {}_ref", c_ty, c_name)
                }
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Emits writebacks for STRING byref parameters at function exit.
///
/// In BASIC, parameters are passed by reference by default, meaning the caller
/// expects to see any modifications made within the function. For STRING parameters,
/// this is critical because the function may assign a new string value.
///
/// We ONLY write back STRING parameters, not numeric ones, because:
/// 1. Numeric parameters often have constants or literals passed by reference
/// 2. Writing to constant addresses causes segfaults (read-only memory)
/// 3. String parameters are the ones that need writeback for proper semantics
///
/// # Arguments
///
/// * `params` - The parameter list
/// * `output` - Output buffer to write to
fn emit_string_writebacks(
    params: &[TypedParameter],
    output: &mut String,
) -> Result<(), CodeGenError> {
    for p in params {
        // Only process non-BYVAL STRING parameters
        if !p.by_val && p.basic_type == BasicType::String && !p.is_array {
            let c_name = c_identifier(&p.name);
            // Write back the local string pointer to the caller's variable
            // *name_str_ref = name_str;
            writeln_code!(output, "    *{}_ref = {};", c_name, c_name)?;
        }
    }
    Ok(())
}
