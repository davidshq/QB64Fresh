//! Program analysis for QB64Fresh C backend.
//!
//! This module provides functions for collecting program-wide information
//! needed for code generation, including:
//!
//! - Global variable declarations
//! - Forward declarations for SUBs and FUNCTIONs
//! - DATA pool values and label indices for RESTORE
//!
//! These analysis passes run before code generation to gather all the
//! information needed for the C output's structure.

use std::collections::HashMap;

use crate::semantic::typed_ir::{
    TypedDataValue, TypedParameter, TypedProgram, TypedStatement, TypedStatementKind,
};

use super::expr::{c_function_name, escape_string};
use super::types::{c_identifier, c_type, default_init};

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
pub(super) fn collect_globals(
    program: &TypedProgram,
    emit_params_fn: impl Fn(&[TypedParameter]) -> String,
) -> (Vec<String>, Vec<String>) {
    use std::collections::HashSet;
    use crate::semantic::types::BasicType;

    let mut globals = Vec::new();
    let mut forward_decls = Vec::new();
    // Track already-declared variable names to avoid duplicates
    let mut declared_vars: HashSet<String> = HashSet::new();

    // Helper to add a global variable if not already declared
    // Note: For strings, we initialize to NULL since qb_string_new() is not a constant
    // expression in C. The generated code should handle NULL strings safely.
    let add_global = |name: &str, basic_type: &BasicType, declared_vars: &mut HashSet<String>, globals: &mut Vec<String>| {
        let c_name = c_identifier(name);
        if !declared_vars.contains(&c_name) {
            let c_ty = c_type(basic_type);
            // For strings, use NULL as initial value since function calls can't be
            // used as global initializers in C
            let init = match basic_type {
                BasicType::String => "NULL".to_string(),
                _ => default_init(basic_type),
            };
            globals.push(format!("{} {} = {};", c_ty, c_name, init));
            declared_vars.insert(c_name);
        }
    };

    // First pass: collect explicit DIM declarations and SUB/FUNCTION forward decls
    for stmt in &program.statements {
        match &stmt.kind {
            TypedStatementKind::Dim {
                variables,
                shared: _,
            } => {
                // Simple global variables (non-array)
                for var in variables {
                    if var.dimensions.is_empty() {
                        add_global(&var.name, &var.basic_type, &mut declared_vars, &mut globals);
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

    // Second pass: collect implicit variables from assignments and FOR loops
    // (only at module level, not inside SUB/FUNCTION definitions)
    for stmt in &program.statements {
        collect_implicit_vars_from_stmt(stmt, &mut declared_vars, &mut globals, false);
    }

    (globals, forward_decls)
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
    // Helper to add a variable
    // For strings, use NULL since function calls aren't valid global initializers in C
    let add_var = |name: &str, basic_type: &crate::semantic::types::BasicType, declared_vars: &mut std::collections::HashSet<String>, globals: &mut Vec<String>| {
        use crate::semantic::types::BasicType;
        let c_name = c_identifier(name);
        if !declared_vars.contains(&c_name) {
            let c_ty = c_type(basic_type);
            let init = match basic_type {
                BasicType::String => "NULL".to_string(),
                _ => default_init(basic_type),
            };
            globals.push(format!("{} {} = {};", c_ty, c_name, init));
            declared_vars.insert(c_name);
        }
    };

    match &stmt.kind {
        // Skip SUB/FUNCTION bodies - local variables don't need global declarations
        TypedStatementKind::SubDefinition { .. } | TypedStatementKind::FunctionDefinition { .. } => {
            // Don't recurse into procedures - their variables are local
        }

        // Assignment introduces an implicit variable at module level
        TypedStatementKind::Assignment { name, target_type, .. } => {
            if !inside_procedure {
                add_var(name, target_type, declared_vars, globals);
            }
        }

        // FOR loop counter variable
        TypedStatementKind::For { variable, var_type, body, .. } => {
            if !inside_procedure {
                add_var(variable, var_type, declared_vars, globals);
            }
            // Recurse into body (still at module level if we're at module level)
            for s in body {
                collect_implicit_vars_from_stmt(s, declared_vars, globals, inside_procedure);
            }
        }

        // Recurse into compound statements
        TypedStatementKind::If { then_branch, elseif_branches, else_branch, .. } => {
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

        TypedStatementKind::SelectCase { cases, case_else, .. }
        | TypedStatementKind::SelectEveryCase { cases, case_else, .. } => {
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
            TypedExprKind::ProcPtr { name, wrapper_name } => {
                if !seen.contains(name) {
                    seen.insert(name.clone());
                    // Generate the C function name for the BASIC procedure
                    let c_func_name = c_function_name(name);
                    wrappers.push(CallbackWrapperInfo {
                        wrapper_name: wrapper_name.clone(),
                        c_func_name,
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
