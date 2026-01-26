//! Program analysis for QB64Fresh C backend.
//!
//! This module provides functions for collecting program-wide information
//! needed for code generation, including:
//!
//! - TYPE definitions (must be emitted before global variables)
//! - Global variable declarations
//! - Forward declarations for SUBs and FUNCTIONs
//! - DATA pool values and label indices for RESTORE
//!
//! These analysis passes run before code generation to gather all the
//! information needed for the C output's structure.

use std::collections::HashMap;
use std::fmt::Write;

use crate::semantic::typed_ir::{
    TypedDataValue, TypedParameter, TypedProgram, TypedStatement, TypedStatementKind,
};
use crate::semantic::types::BasicType;

use super::expr::{c_function_name, escape_string};
use super::types::{
    add_reserved_identifiers, c_identifier, c_type, declare_array_var, declare_scalar_var,
    infer_type_from_suffix,
};

/// Information collected from DATA statements for code generation.
///
/// This includes both the data values and a mapping of labels to their
/// positions in the data pool (for RESTORE with label support).
pub(super) struct DataPoolInfo {
    /// The data values as (value_string, type_tag) tuples.
    /// - type_tag "d" for numeric (double)
    /// - type_tag "s" for string
    pub values: Vec<(String, &'static str)>,
    /// Map of uppercase label names to their DATA pool index.
    ///
    /// When a label appears before a DATA statement, it maps to the index
    /// of the first value in that DATA statement.
    pub label_indices: HashMap<String, usize>,
}

impl DataPoolInfo {
    /// Creates a new empty DataPoolInfo.
    fn new() -> Self {
        Self {
            values: Vec::new(),
            label_indices: HashMap::new(),
        }
    }
}

/// Collects TYPE definitions from the program (user-defined types).
///
/// TYPE definitions must be emitted before global variables that use those types.
/// This function recursively scans all statements including those inside
/// SUB/FUNCTION bodies (since TYPEs can be defined in $INCLUDE files that
/// are processed at any point in the program).
///
/// Returns a vector of C typedef strings in definition order.
pub(super) fn collect_type_definitions(program: &TypedProgram) -> Vec<String> {
    use std::collections::HashSet;

    let mut type_defs = Vec::new();
    let mut defined_types: HashSet<String> = HashSet::new();

    // Recursively collect TYPE definitions from all statements
    fn collect_from_stmt(
        stmt: &TypedStatement,
        type_defs: &mut Vec<String>,
        defined_types: &mut HashSet<String>,
    ) {
        match &stmt.kind {
            TypedStatementKind::TypeDefinition {
                name,
                members,
                custom_type,
            } => {
                let base_name = c_identifier(name);
                // Prefix with qbt_ to avoid collision with variable names
                // (QB64 allows TYPE and DIM to use the same name)
                let c_name = format!("qbt_{}", base_name);
                if !defined_types.contains(&c_name) {
                    let mut def = String::new();

                    // CUSTOMTYPE modifier indicates C-compatible (packed) memory layout
                    if *custom_type {
                        writeln!(def, "#pragma pack(push, 1)").unwrap();
                    }

                    writeln!(def, "typedef struct {} {{", c_name).unwrap();

                    for member in members {
                        let c_member_name = c_identifier(&member.name);
                        if let BasicType::FixedString(len) = &member.basic_type {
                            writeln!(def, "    char {}[{}];", c_member_name, len + 1).unwrap();
                        } else {
                            let c_member_type = c_type(&member.basic_type);
                            writeln!(def, "    {} {};", c_member_type, c_member_name).unwrap();
                        }
                    }

                    writeln!(def, "}} {};", c_name).unwrap();

                    if *custom_type {
                        writeln!(def, "#pragma pack(pop)").unwrap();
                    }

                    type_defs.push(def);
                    defined_types.insert(c_name);
                }
            }

            // Recurse into SUB/FUNCTION bodies since $INCLUDE files may define TYPEs there
            TypedStatementKind::SubDefinition { body, .. }
            | TypedStatementKind::FunctionDefinition { body, .. } => {
                for s in body {
                    collect_from_stmt(s, type_defs, defined_types);
                }
            }

            // Recurse into control flow bodies
            TypedStatementKind::If {
                then_branch,
                elseif_branches,
                else_branch,
                ..
            } => {
                for s in then_branch {
                    collect_from_stmt(s, type_defs, defined_types);
                }
                for (_, branch_body) in elseif_branches {
                    for s in branch_body {
                        collect_from_stmt(s, type_defs, defined_types);
                    }
                }
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        collect_from_stmt(s, type_defs, defined_types);
                    }
                }
            }

            TypedStatementKind::For { body, .. }
            | TypedStatementKind::While { body, .. }
            | TypedStatementKind::DoLoop { body, .. } => {
                for s in body {
                    collect_from_stmt(s, type_defs, defined_types);
                }
            }

            TypedStatementKind::SelectCase { cases, .. } => {
                for case in cases {
                    for s in &case.body {
                        collect_from_stmt(s, type_defs, defined_types);
                    }
                }
            }

            _ => {}
        }
    }

    // Collect from all top-level statements
    for stmt in &program.statements {
        collect_from_stmt(stmt, &mut type_defs, &mut defined_types);
    }

    type_defs
}

/// Collects global variables and procedure forward declarations from the program.
///
/// Returns a tuple of:
/// - Global variable declarations (for variables declared at module scope)
/// - Forward declarations for SUBs and FUNCTIONs
///
/// This enables the generated C code to have proper ordering: globals first,
/// then forward declarations, then the actual procedure definitions.
///
/// # Implicit Variables
///
/// In BASIC, variables can be used without explicit DIM declaration. These
/// implicitly-declared variables are also collected by scanning Assignment
/// and FOR loop statements to ensure all variables are properly declared
/// in the generated C code.
/// Returns (globals, forward_decls, string_const_inits)
/// - globals: Global variable declarations
/// - forward_decls: Forward declarations for SUBs/FUNCTIONs
/// - string_const_inits: String constant initializations to run at program start
pub(super) fn collect_globals(
    program: &TypedProgram,
    emit_params_fn: impl Fn(&[TypedParameter]) -> String,
) -> (Vec<String>, Vec<String>, Vec<String>) {
    use crate::semantic::types::BasicType;
    use std::collections::HashSet;

    let mut globals = Vec::new();
    let mut forward_decls = Vec::new();
    let mut string_const_inits = Vec::new();
    // Track already-declared variable names to avoid duplicates
    let mut declared_vars: HashSet<String> = HashSet::new();

    // Add built-in constants and runtime variables that should not be redeclared
    add_reserved_identifiers(&mut declared_vars);

    // First pass: collect explicit DIM declarations and SUB/FUNCTION forward decls
    for stmt in &program.statements {
        match &stmt.kind {
            TypedStatementKind::Dim {
                variables,
                shared: _,
            } => {
                for var in variables {
                    if var.dimensions.is_empty() {
                        // Simple global variables (non-array)
                        declare_scalar_var(
                            &var.name,
                            &var.basic_type,
                            &mut declared_vars,
                            &mut globals,
                        );
                    } else {
                        // Global arrays (declared as pointers)
                        declare_array_var(
                            &var.name,
                            &var.basic_type,
                            &mut declared_vars,
                            &mut globals,
                            true, // is_global
                        );
                    }
                }
            }

            TypedStatementKind::Const { definitions } => {
                // Emit CONST definitions as global constants
                for (name, value_expr, _basic_type) in definitions {
                    let c_name = c_identifier(name);
                    if !declared_vars.contains(&c_name) {
                        let c_ty = c_type(&value_expr.basic_type);
                        // Try to evaluate as a constant expression
                        let value_code = super::expr::emit_expr(value_expr, false)
                            .unwrap_or_else(|_| "0".to_string());

                        // For string constants, we can't use function calls as initializers
                        // in C. Declare without initializer and add initialization to run at start.
                        if matches!(value_expr.basic_type, BasicType::String) {
                            globals.push(format!("qb_string* {} = NULL;", c_name));
                            string_const_inits.push(format!("{} = {};", c_name, value_code));
                        } else {
                            globals.push(format!("const {} {} = {};", c_ty, c_name, value_code));
                        }
                        declared_vars.insert(c_name);
                    }
                }
            }

            TypedStatementKind::SubDefinition { name, params, .. } => {
                let c_name = format!("qb_sub_{}", c_identifier(name).to_lowercase());
                let params_str = emit_params_fn(params);
                forward_decls.push(format!("void {}({});", c_name, params_str));
            }

            TypedStatementKind::FunctionDefinition {
                name,
                params,
                return_type,
                ..
            } => {
                let c_name = c_function_name(name);
                let c_ret_type = c_type(return_type);
                let params_str = emit_params_fn(params);
                forward_decls.push(format!("{} {}({});", c_ret_type, c_name, params_str));
            }

            _ => {}
        }
    }

    // Additional pass: collect REDIM SHARED arrays from within SUB/FUNCTION bodies
    // In QB64, REDIM SHARED inside a procedure creates a global array
    fn collect_redim_shared(
        stmt: &TypedStatement,
        declared_vars: &mut HashSet<String>,
        globals: &mut Vec<String>,
    ) {
        match &stmt.kind {
            TypedStatementKind::Redim {
                variables, shared, ..
            } if *shared => {
                for var in variables {
                    // REDIM SHARED creates a global array pointer
                    declare_array_var(&var.name, &var.element_type, declared_vars, globals, true);
                }
            }
            TypedStatementKind::SubDefinition { body, .. }
            | TypedStatementKind::FunctionDefinition { body, .. } => {
                // Recurse into procedure bodies to find REDIM SHARED
                for s in body {
                    collect_redim_shared(s, declared_vars, globals);
                }
            }
            TypedStatementKind::If {
                then_branch,
                elseif_branches,
                else_branch,
                ..
            } => {
                for s in then_branch {
                    collect_redim_shared(s, declared_vars, globals);
                }
                for (_, branch) in elseif_branches {
                    for s in branch {
                        collect_redim_shared(s, declared_vars, globals);
                    }
                }
                if let Some(branch) = else_branch {
                    for s in branch {
                        collect_redim_shared(s, declared_vars, globals);
                    }
                }
            }
            TypedStatementKind::For { body, .. }
            | TypedStatementKind::While { body, .. }
            | TypedStatementKind::DoLoop { body, .. } => {
                for s in body {
                    collect_redim_shared(s, declared_vars, globals);
                }
            }
            TypedStatementKind::SelectCase { cases, .. } => {
                for case in cases {
                    for s in &case.body {
                        collect_redim_shared(s, declared_vars, globals);
                    }
                }
            }
            _ => {}
        }
    }

    for stmt in &program.statements {
        collect_redim_shared(stmt, &mut declared_vars, &mut globals);
    }

    // Additional pass: collect SHARED variables from within SUB/FUNCTION bodies
    // In BASIC, SHARED inside a procedure declares access to a module-level variable.
    // If the variable doesn't exist at module level, it's implicitly created.

    fn collect_shared_vars(
        stmt: &TypedStatement,
        declared_vars: &mut HashSet<String>,
        globals: &mut Vec<String>,
    ) {
        match &stmt.kind {
            TypedStatementKind::SharedStmt { variables } => {
                for var_name in variables {
                    // Implicitly create module-level variable
                    // Infer type from the variable name suffix
                    let basic_type = infer_type_from_suffix(var_name);
                    declare_scalar_var(var_name, &basic_type, declared_vars, globals);
                }
            }
            TypedStatementKind::SubDefinition { body, .. }
            | TypedStatementKind::FunctionDefinition { body, .. } => {
                for s in body {
                    collect_shared_vars(s, declared_vars, globals);
                }
            }
            TypedStatementKind::If {
                then_branch,
                elseif_branches,
                else_branch,
                ..
            } => {
                for s in then_branch {
                    collect_shared_vars(s, declared_vars, globals);
                }
                for (_, branch) in elseif_branches {
                    for s in branch {
                        collect_shared_vars(s, declared_vars, globals);
                    }
                }
                if let Some(branch) = else_branch {
                    for s in branch {
                        collect_shared_vars(s, declared_vars, globals);
                    }
                }
            }
            TypedStatementKind::For { body, .. }
            | TypedStatementKind::While { body, .. }
            | TypedStatementKind::DoLoop { body, .. } => {
                for s in body {
                    collect_shared_vars(s, declared_vars, globals);
                }
            }
            TypedStatementKind::SelectCase { cases, .. } => {
                for case in cases {
                    for s in &case.body {
                        collect_shared_vars(s, declared_vars, globals);
                    }
                }
            }
            _ => {}
        }
    }

    for stmt in &program.statements {
        collect_shared_vars(stmt, &mut declared_vars, &mut globals);
    }

    // Second pass: collect implicit variables from assignments and FOR loops
    // (only at module level, not inside SUB/FUNCTION definitions)
    for stmt in &program.statements {
        collect_implicit_vars_from_stmt(stmt, &mut declared_vars, &mut globals, false);
    }

    // Add initialization for all global string variables
    // In BASIC, uninitialized strings are empty (""), not null.
    // We can't initialize qb_string* at global scope in C, so we do it at program start.
    for decl in &globals {
        // Match declarations like "qb_string* name = NULL;" or "qb_string* name_str = NULL;"
        if decl.starts_with("qb_string* ") && decl.ends_with(" = NULL;") {
            // Extract variable name: "qb_string* foo = NULL;" -> "foo"
            let after_type = &decl["qb_string* ".len()..];
            if let Some(name) = after_type.strip_suffix(" = NULL;") {
                // Add initialization: foo = qb_string_new("");
                string_const_inits.push(format!("{} = qb_string_new(\"\");", name));
            }
        }
    }

    (globals, forward_decls, string_const_inits)
}

/// Recursively collects implicit variable declarations from a statement.
///
/// This finds variables that are used (via assignment or FOR loop) without
/// an explicit DIM declaration, which is valid in BASIC.
fn collect_implicit_vars_from_stmt(
    stmt: &TypedStatement,
    declared_vars: &mut std::collections::HashSet<String>,
    globals: &mut Vec<String>,
    inside_procedure: bool,
) {
    match &stmt.kind {
        // Skip SUB/FUNCTION bodies - local variables don't need global declarations
        TypedStatementKind::SubDefinition { .. }
        | TypedStatementKind::FunctionDefinition { .. } => {
            // Don't recurse into procedures - their variables are local
        }

        // Assignment introduces an implicit variable at module level
        TypedStatementKind::Assignment {
            name, target_type, ..
        } => {
            if !inside_procedure {
                declare_scalar_var(name, target_type, declared_vars, globals);
            }
        }

        // FOR loop counter variable
        TypedStatementKind::For {
            variable,
            var_type,
            body,
            ..
        } => {
            if !inside_procedure {
                declare_scalar_var(variable, var_type, declared_vars, globals);
            }
            // Recurse into body (still at module level if we're at module level)
            for s in body {
                collect_implicit_vars_from_stmt(s, declared_vars, globals, inside_procedure);
            }
        }

        // Recurse into compound statements
        TypedStatementKind::If {
            then_branch,
            elseif_branches,
            else_branch,
            ..
        } => {
            for s in then_branch {
                collect_implicit_vars_from_stmt(s, declared_vars, globals, inside_procedure);
            }
            for (_, branch) in elseif_branches {
                for s in branch {
                    collect_implicit_vars_from_stmt(s, declared_vars, globals, inside_procedure);
                }
            }
            if let Some(else_stmts) = else_branch {
                for s in else_stmts {
                    collect_implicit_vars_from_stmt(s, declared_vars, globals, inside_procedure);
                }
            }
        }

        TypedStatementKind::While { body, .. } | TypedStatementKind::DoLoop { body, .. } => {
            for s in body {
                collect_implicit_vars_from_stmt(s, declared_vars, globals, inside_procedure);
            }
        }

        TypedStatementKind::SelectCase {
            cases, case_else, ..
        }
        | TypedStatementKind::SelectEveryCase {
            cases, case_else, ..
        } => {
            for case in cases {
                for s in &case.body {
                    collect_implicit_vars_from_stmt(s, declared_vars, globals, inside_procedure);
                }
            }
            if let Some(else_stmts) = case_else {
                for s in else_stmts {
                    collect_implicit_vars_from_stmt(s, declared_vars, globals, inside_procedure);
                }
            }
        }

        // Print statement - scan for variables in expressions
        TypedStatementKind::Print { items, .. } => {
            if !inside_procedure {
                for item in items {
                    collect_vars_from_expr(&item.expr, declared_vars, globals);
                }
            }
        }

        // FilePrint statement - scan for variables in expressions
        TypedStatementKind::FilePrint { items, .. } => {
            if !inside_procedure {
                for item in items {
                    collect_vars_from_expr(&item.expr, declared_vars, globals);
                }
            }
        }

        _ => {}
    }
}

/// Recursively collect variables from an expression and add as globals if not declared.
fn collect_vars_from_expr(
    expr: &crate::semantic::typed_ir::TypedExpr,
    declared_vars: &mut std::collections::HashSet<String>,
    globals: &mut Vec<String>,
) {
    use crate::semantic::typed_ir::TypedExprKind;

    match &expr.kind {
        TypedExprKind::Variable(name) => {
            // Skip variables starting with _ (built-in constants)
            if !name.starts_with('_') {
                declare_scalar_var(name, &expr.basic_type, declared_vars, globals);
            }
        }
        TypedExprKind::Binary { left, right, .. } => {
            collect_vars_from_expr(left, declared_vars, globals);
            collect_vars_from_expr(right, declared_vars, globals);
        }
        TypedExprKind::Unary { operand, .. } => {
            collect_vars_from_expr(operand, declared_vars, globals);
        }
        TypedExprKind::Grouped(inner) => {
            collect_vars_from_expr(inner, declared_vars, globals);
        }
        TypedExprKind::FunctionCall { args, .. } => {
            for arg in args {
                collect_vars_from_expr(arg, declared_vars, globals);
            }
        }
        TypedExprKind::ArrayAccess { indices, .. } => {
            for idx in indices {
                collect_vars_from_expr(idx, declared_vars, globals);
            }
        }
        _ => {}
    }
}

/// Collects all DATA values from the program into a flat array.
///
/// Also tracks label positions for RESTORE with label support.
/// Labels immediately before DATA statements map to that DATA's index.
///
/// # DATA Pool Structure
///
/// In BASIC, DATA statements define a global pool of values that can be
/// read sequentially using READ statements. RESTORE resets or repositions
/// the read pointer.
///
/// ```basic
/// MyData:
///     DATA 1, 2, 3
///     DATA "Hello", "World"
///
/// READ a, b, c        ' Reads 1, 2, 3
/// READ s1$, s2$       ' Reads "Hello", "World"
/// RESTORE MyData      ' Reset to start of MyData
/// ```
pub(super) fn collect_data_values(program: &TypedProgram) -> DataPoolInfo {
    let mut info = DataPoolInfo::new();
    // Pending labels: labels seen that haven't been assigned to a DATA index yet
    let mut pending_labels: Vec<String> = Vec::new();

    for stmt in &program.statements {
        collect_data_from_stmt(stmt, &mut info, &mut pending_labels);
    }

    info
}

/// Recursively collects DATA values from a statement and its children.
///
/// Tracks labels that precede DATA statements so RESTORE can jump to them.
fn collect_data_from_stmt(
    stmt: &TypedStatement,
    info: &mut DataPoolInfo,
    pending_labels: &mut Vec<String>,
) {
    match &stmt.kind {
        TypedStatementKind::Label { name } => {
            // Record this label as pending - it will be associated with the
            // next DATA statement's starting index
            pending_labels.push(name.to_uppercase());
        }

        TypedStatementKind::Data {
            values: data_values,
        } => {
            // Associate any pending labels with the current DATA index
            let current_index = info.values.len();
            for label in pending_labels.drain(..) {
                info.label_indices.insert(label, current_index);
            }

            // Collect the data values
            for val in data_values {
                let (val_str, type_tag) = match val {
                    TypedDataValue::Integer(n) => (format!("{}.0", n), "d"),
                    TypedDataValue::Float(f) => (format!("{}", f), "d"),
                    TypedDataValue::String(s) => {
                        // Escape the string for C
                        let escaped = escape_string(s);
                        (format!("\"{}\"", escaped), "s")
                    }
                };
                info.values.push((val_str, type_tag));
            }
        }

        // Recurse into compound statements
        TypedStatementKind::If {
            then_branch,
            elseif_branches,
            else_branch,
            ..
        } => {
            for s in then_branch {
                collect_data_from_stmt(s, info, pending_labels);
            }
            for (_, branch) in elseif_branches {
                for s in branch {
                    collect_data_from_stmt(s, info, pending_labels);
                }
            }
            if let Some(else_stmts) = else_branch {
                for s in else_stmts {
                    collect_data_from_stmt(s, info, pending_labels);
                }
            }
        }

        TypedStatementKind::For { body, .. }
        | TypedStatementKind::While { body, .. }
        | TypedStatementKind::DoLoop { body, .. } => {
            for s in body {
                collect_data_from_stmt(s, info, pending_labels);
            }
        }

        TypedStatementKind::SelectCase {
            cases, case_else, ..
        }
        | TypedStatementKind::SelectEveryCase {
            cases, case_else, ..
        } => {
            for case in cases {
                for s in &case.body {
                    collect_data_from_stmt(s, info, pending_labels);
                }
            }
            if let Some(else_stmts) = case_else {
                for s in else_stmts {
                    collect_data_from_stmt(s, info, pending_labels);
                }
            }
        }

        TypedStatementKind::SubDefinition { body, .. }
        | TypedStatementKind::FunctionDefinition { body, .. } => {
            for s in body {
                collect_data_from_stmt(s, info, pending_labels);
            }
        }

        _ => {}
    }
}

/// Information about a procedure that needs a callback wrapper.
#[derive(Debug, Clone)]
pub(super) struct CallbackWrapperInfo {
    /// The C wrapper function name (e.g., "qb_callback_mycompare").
    pub wrapper_name: String,
    /// The C function name of the BASIC procedure (e.g., "qb_mycompare_lng").
    pub c_func_name: String,
    /// Parameter types for the callback signature.
    pub params: Vec<crate::semantic::typed_ir::CallbackParam>,
    /// Return type (None for SUB, Some for FUNCTION).
    pub return_type: Option<crate::semantic::types::BasicType>,
}

/// Collects all procedures that need callback wrappers (used with _PROCPTR).
///
/// Scans the program for ProcPtr expressions and returns information needed
/// to generate C wrapper functions that can be passed to C libraries.
pub(super) fn collect_callback_wrappers(program: &TypedProgram) -> Vec<CallbackWrapperInfo> {
    use crate::semantic::typed_ir::TypedExprKind;
    use std::collections::HashSet;

    let mut wrappers = Vec::new();
    let mut seen = HashSet::new();

    // Scan all expressions for ProcPtr usages
    fn scan_expr(
        expr: &crate::semantic::typed_ir::TypedExpr,
        wrappers: &mut Vec<CallbackWrapperInfo>,
        seen: &mut HashSet<String>,
    ) {
        match &expr.kind {
            TypedExprKind::ProcPtr {
                name,
                wrapper_name,
                params,
                return_type,
            } => {
                if !seen.contains(name) {
                    seen.insert(name.clone());
                    // Generate the C function name for the BASIC procedure
                    let c_func_name = c_function_name(name);
                    wrappers.push(CallbackWrapperInfo {
                        wrapper_name: wrapper_name.clone(),
                        c_func_name,
                        params: params.clone(),
                        return_type: return_type.clone(),
                    });
                }
            }
            TypedExprKind::Binary { left, right, .. } => {
                scan_expr(left, wrappers, seen);
                scan_expr(right, wrappers, seen);
            }
            TypedExprKind::Unary { operand, .. } => {
                scan_expr(operand, wrappers, seen);
            }
            TypedExprKind::Grouped(inner) => {
                scan_expr(inner, wrappers, seen);
            }
            TypedExprKind::FunctionCall { args, .. } => {
                for arg in args {
                    scan_expr(arg, wrappers, seen);
                }
            }
            TypedExprKind::ExternalFunctionCall { args, .. } => {
                for arg in args {
                    scan_expr(arg, wrappers, seen);
                }
            }
            TypedExprKind::ArrayAccess { indices, .. } => {
                for idx in indices {
                    scan_expr(idx, wrappers, seen);
                }
            }
            TypedExprKind::Convert { expr, .. } => {
                scan_expr(expr, wrappers, seen);
            }
            TypedExprKind::FieldAccess { object, .. } => {
                scan_expr(object, wrappers, seen);
            }
            _ => {}
        }
    }

    fn scan_stmt(
        stmt: &TypedStatement,
        wrappers: &mut Vec<CallbackWrapperInfo>,
        seen: &mut HashSet<String>,
    ) {
        use crate::semantic::typed_ir::TypedStatementKind;

        match &stmt.kind {
            TypedStatementKind::Assignment { value, .. } => {
                scan_expr(value, wrappers, seen);
            }
            TypedStatementKind::ArrayAssignment { indices, value, .. } => {
                for idx in indices {
                    scan_expr(idx, wrappers, seen);
                }
                scan_expr(value, wrappers, seen);
            }
            TypedStatementKind::Print { items, .. } => {
                for item in items {
                    scan_expr(&item.expr, wrappers, seen);
                }
            }
            TypedStatementKind::If {
                condition,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                scan_expr(condition, wrappers, seen);
                for s in then_branch {
                    scan_stmt(s, wrappers, seen);
                }
                for (cond, branch) in elseif_branches {
                    scan_expr(cond, wrappers, seen);
                    for s in branch {
                        scan_stmt(s, wrappers, seen);
                    }
                }
                if let Some(else_stmts) = else_branch {
                    for s in else_stmts {
                        scan_stmt(s, wrappers, seen);
                    }
                }
            }
            TypedStatementKind::For {
                start,
                end,
                step,
                body,
                ..
            } => {
                scan_expr(start, wrappers, seen);
                scan_expr(end, wrappers, seen);
                if let Some(s) = step {
                    scan_expr(s, wrappers, seen);
                }
                for s in body {
                    scan_stmt(s, wrappers, seen);
                }
            }
            TypedStatementKind::While { condition, body } => {
                scan_expr(condition, wrappers, seen);
                for s in body {
                    scan_stmt(s, wrappers, seen);
                }
            }
            TypedStatementKind::DoLoop {
                pre_condition,
                body,
                post_condition,
            } => {
                if let Some(cond) = pre_condition {
                    scan_expr(&cond.condition, wrappers, seen);
                }
                for s in body {
                    scan_stmt(s, wrappers, seen);
                }
                if let Some(cond) = post_condition {
                    scan_expr(&cond.condition, wrappers, seen);
                }
            }
            TypedStatementKind::SubDefinition { body, .. }
            | TypedStatementKind::FunctionDefinition { body, .. } => {
                for s in body {
                    scan_stmt(s, wrappers, seen);
                }
            }
            TypedStatementKind::Call { args, .. } => {
                for arg in args {
                    scan_expr(arg, wrappers, seen);
                }
            }
            _ => {}
        }
    }

    for stmt in &program.statements {
        scan_stmt(stmt, &mut wrappers, &mut seen);
    }

    wrappers
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;

    #[test]
    fn test_collect_data_with_labels() {
        // Test that labels before DATA statements are correctly tracked
        let program = TypedProgram::new(vec![
            TypedStatement::new(
                TypedStatementKind::Label {
                    name: "myLabel".to_string(),
                },
                Span::new(0, 8),
            ),
            TypedStatement::new(
                TypedStatementKind::Data {
                    values: vec![
                        TypedDataValue::Integer(1),
                        TypedDataValue::Integer(2),
                        TypedDataValue::Integer(3),
                    ],
                },
                Span::new(9, 20),
            ),
            TypedStatement::new(
                TypedStatementKind::Data {
                    values: vec![TypedDataValue::Integer(4), TypedDataValue::Integer(5)],
                },
                Span::new(21, 30),
            ),
            TypedStatement::new(
                TypedStatementKind::Label {
                    name: "anotherLabel".to_string(),
                },
                Span::new(31, 44),
            ),
            TypedStatement::new(
                TypedStatementKind::Data {
                    values: vec![TypedDataValue::Integer(6)],
                },
                Span::new(45, 51),
            ),
        ]);

        let data_pool = collect_data_values(&program);

        // Should have 6 data values total
        assert_eq!(data_pool.values.len(), 6);

        // myLabel should point to index 0 (first DATA)
        assert_eq!(data_pool.label_indices.get("MYLABEL"), Some(&0));

        // anotherLabel should point to index 5 (after the first 5 values)
        assert_eq!(data_pool.label_indices.get("ANOTHERLABEL"), Some(&5));
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_collect_data_string_values() {
        let program = TypedProgram::new(vec![TypedStatement::new(
            TypedStatementKind::Data {
                values: vec![
                    TypedDataValue::String("Hello".to_string()),
                    TypedDataValue::Integer(42),
                    TypedDataValue::Float(3.14),
                ],
            },
            Span::new(0, 30),
        )]);

        let data_pool = collect_data_values(&program);

        assert_eq!(data_pool.values.len(), 3);
        assert_eq!(data_pool.values[0], ("\"Hello\"".to_string(), "s"));
        assert_eq!(data_pool.values[1], ("42.0".to_string(), "d"));
        assert_eq!(data_pool.values[2], ("3.14".to_string(), "d"));
    }
}
