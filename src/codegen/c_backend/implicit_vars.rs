//! Implicit variable collection for QB64Fresh C backend.
//!
//! This module collects variables that need to be declared for a function or
//! the main program. In BASIC, variables don't need explicit declaration -
//! they're created implicitly on first use. This module identifies those
//! implicit usages so we can generate proper C declarations.
//!
//! # Two-Pass Algorithm
//!
//! 1. **Pass 1 (`collect_dims`)**: Collect all explicit DIM/REDIM/STATIC declarations.
//!    These have function-wide scope in BASIC, so we collect them first.
//!
//! 2. **Pass 2 (`collect_implicits`)**: Walk all statements collecting variables from:
//!    - Assignments to non-declared variables
//!    - FOR loop counters
//!    - ByRef function arguments (variables passed to functions by reference)
//!    - INPUT statement targets
//!
//! # Why ByRef Arguments Need Declaration
//!
//! In QB64, passing an undeclared variable by reference to a function implicitly
//! creates that variable. For example:
//!
//! ```basic
//! CALL MySub(x)  ' x is created if MySub takes a ByRef parameter
//! ```

use std::collections::{HashMap, HashSet};

use crate::semantic::typed_ir::{
    TypedCaseMatch, TypedExpr, TypedExprKind, TypedInputTarget, TypedParameter, TypedStatement,
    TypedStatementKind,
};

use super::types::{add_reserved_identifiers, c_identifier, declare_array_var, declare_scalar_var};

/// Collects implicit local variable declarations for a function body.
///
/// Uses a two-pass approach:
/// 1. First pass: collect all DIM declarations from the entire function body
///    (these should shadow globals - BASIC allows local variables to shadow globals)
/// 2. Second pass: collect implicit variables (assignments to non-DIM'd variables)
///
/// This prevents duplicate declarations when a variable is assigned before its DIM.
///
/// The `is_main_program` flag indicates if this is the main program (not a SUB/FUNCTION).
/// This affects DIM handling: in main, DIM on an existing global allocates to the global
/// (for cross-function sharing). In SUB/FUNCTIONs, DIM always creates locals.
///
/// The `always_exclude` set contains variable names that should never be declared as
/// implicit locals regardless of scope (e.g., FUNCTION return variables which are
/// explicitly declared in the function header).
///
/// The `global_arrays` set contains names of global array variables. Arrays cannot be
/// implicitly declared (they require explicit DIM/REDIM), so global arrays should never
/// be shadowed by implicit scalar declarations.
///
/// The `shared_globals` set contains names of variables declared with `DIM SHARED` at
/// module level. These are automatically accessible from all functions without needing
/// a local SHARED statement, so they shouldn't be shadowed by implicit locals.
///
/// The `global_consts` set contains names of global CONST values. These are emitted as
/// C-level global constants and should NEVER be shadowed by local variable declarations,
/// as this would break code that references the constants (they'd get local var with
/// default value 0 instead of the const value).
///
/// REDIM always uses an existing global if one exists (in both main and SUB/FUNCTION)
/// because BASIC's REDIM on a SHARED array operates on the global, not a new local.
#[allow(clippy::too_many_arguments)]
pub(super) fn collect_implicit_locals(
    body: &[TypedStatement],
    params: &[TypedParameter],
    existing_vars: &HashSet<String>,
    always_exclude: &HashSet<String>,
    global_arrays: &HashSet<String>,
    shared_globals: &HashSet<String>,
    global_consts: &HashSet<String>,
    is_main_program: bool,
    variable_renames: &mut HashMap<String, String>,
) -> Vec<String> {
    let mut locals = Vec::new();

    // Start with a clean set for DIM collection - DIMs should shadow globals
    // We only include reserved identifiers, parameters, always-excluded vars,
    // global arrays, and DIM SHARED globals here
    let mut dim_declared: HashSet<String> = HashSet::new();
    add_reserved_identifiers(&mut dim_declared);

    // Add always-excluded variables (e.g., FUNCTION return variable)
    for var in always_exclude {
        dim_declared.insert(var.clone());
    }

    // Add global array names - arrays can't be implicitly declared as local scalars
    for var in global_arrays {
        dim_declared.insert(var.clone());
    }

    // Add DIM SHARED globals - these are accessible from all functions
    for var in shared_globals {
        dim_declared.insert(var.clone());
    }

    // Add global CONST names - these must NEVER be shadowed by local declarations
    // because they're emitted as C-level global constants
    for var in global_consts {
        dim_declared.insert(var.clone());
    }

    // Add parameter names to declared set
    for p in params {
        dim_declared.insert(c_identifier(&p.name));
    }

    // Track which names are arrays (for dual namespace collision detection)
    let mut array_names: HashSet<String> = HashSet::new();
    // Add global arrays to the set
    for var in global_arrays {
        array_names.insert(var.clone());
    }

    // PASS 1: Collect all DIM/REDIM declarations first (they have function-wide scope in BASIC)
    // Note: We do NOT include globals here - DIM should always create a local that shadows globals
    // EXCEPT in main program: arrays with existing globals use the global (for cross-function sharing)
    for stmt in body {
        collect_dims(
            stmt,
            &mut dim_declared,
            &mut locals,
            existing_vars,
            is_main_program,
            variable_renames,
            &mut array_names,
        );
    }

    // For implicit variable collection:
    // - In SUB/FUNCTION (is_main_program=false): All variables are LOCAL by default.
    //   Globals are only accessed if explicitly imported via SHARED statement.
    //   The SHARED handling in Pass 1 already added those to dim_declared.
    // - In main program (is_main_program=true): There's no separate scope,
    //   so we include existing globals to avoid re-declaring them.
    let mut declared_vars = dim_declared.clone();
    if is_main_program {
        for var in existing_vars {
            declared_vars.insert(var.clone());
        }
    }

    // PASS 2: Collect implicit variables (assignments to non-declared variables)
    for stmt in body {
        collect_implicits(
            stmt,
            &mut declared_vars,
            &mut locals,
            variable_renames,
            &array_names,
        );
    }

    locals
}

/// Pass 1: Collects all DIM/REDIM/STATIC declarations from a statement tree.
///
/// DIM statements in BASIC have function-wide scope (they're "hoisted"), so we
/// collect them first before looking for implicit variables.
///
/// The `is_main_program` flag affects REDIM array handling:
/// - In main: if a global array exists, don't create local (use global for cross-function sharing)
/// - In SUB/FUNCTION: always create local (DIM/REDIM inside procedure = local scope)
fn collect_dims(
    stmt: &TypedStatement,
    declared_vars: &mut HashSet<String>,
    locals: &mut Vec<String>,
    existing_vars: &HashSet<String>,
    is_main_program: bool, // NOTE: No longer used for REDIM (always uses global if exists)
    variable_renames: &mut HashMap<String, String>,
    array_names: &mut HashSet<String>,
) {
    match &stmt.kind {
        TypedStatementKind::Dim { variables, .. } => {
            // Hoist DIM declarations to function scope (BASIC semantics)
            for var in variables {
                let c_name = c_identifier(&var.name);
                if var.dimensions.is_empty() && !var.is_dynamic_array {
                    // Scalar variable - check if global exists AND we're in main
                    // In main program: DIM of existing global = use global (for module-level sharing)
                    // In SUB/FUNCTION: DIM always creates local (can shadow globals)
                    if is_main_program && existing_vars.contains(&c_name) {
                        // Global scalar exists in main: just mark as declared, don't create local.
                        declared_vars.insert(c_name);
                    } else {
                        // Either in SUB/FUNCTION, or no global exists: create local declaration
                        declare_scalar_var(
                            &var.name,
                            &var.basic_type,
                            declared_vars,
                            locals,
                            Some(variable_renames),
                            Some(array_names),
                        );
                    }
                } else if var.dimensions.is_empty() && var.is_dynamic_array {
                    // DIM a() AS LONG = dynamic array: declare pointer + size var
                    if is_main_program && existing_vars.contains(&c_name) {
                        declared_vars.insert(c_name);
                    } else {
                        declare_array_var(
                            &var.name,
                            &var.basic_type,
                            declared_vars,
                            locals,
                            false, // is_global (main body locals)
                            false, // is_static
                            &[],
                        );
                        array_names.insert(c_name);
                    }
                } else {
                    // Array with dimensions - mark as declared only, emit at statement location
                    // (arrays need runtime allocation)
                    declared_vars.insert(c_name.clone());
                    // Track that this is an array (for dual namespace collision detection)
                    array_names.insert(c_name);
                }
            }
        }
        TypedStatementKind::Redim { variables, .. } => {
            // REDIM creates dynamic arrays - emit declarations with NULL initialization
            // If no dimensions, treat as scalar (REDIM can be used for scalars in QB64)
            for var in variables {
                let c_name = c_identifier(&var.name);
                if var.dimensions.is_empty() {
                    // Scalar REDIM - just a type declaration, no array
                    declare_scalar_var(
                        &var.name,
                        &var.element_type,
                        declared_vars,
                        locals,
                        Some(variable_renames),
                        Some(&*array_names),
                    );
                } else if existing_vars.contains(&c_name) {
                    // Global array exists: use it instead of creating a local that would
                    // shadow it. This applies to both main AND SUB/FUNCTION contexts.
                    // In BASIC, REDIM on a SHARED array (even from within a SUB) operates
                    // on the global array, not a new local.
                    declared_vars.insert(c_name);
                } else {
                    // No global exists: create local array declaration
                    // For implicit arrays, we don't know dimensions or if they're static
                    // Default to dynamic (matches QB64pe default behavior)
                    let c_name = c_identifier(&var.name);
                    declare_array_var(
                        &var.name,
                        &var.element_type,
                        declared_vars,
                        locals,
                        false, // is_global
                        false, // is_static (default to dynamic for implicit arrays)
                        &[],   // dimensions unknown for implicit arrays
                    );
                    // Track that this is an array
                    array_names.insert(c_name);
                }
            }
        }
        TypedStatementKind::StaticStmt { variables, .. } => {
            for var in variables {
                declared_vars.insert(c_identifier(&var.name));
            }
        }
        // SHARED variables are already declared at global/module scope
        TypedStatementKind::SharedStmt { variables } => {
            for var_name in variables {
                declared_vars.insert(c_identifier(var_name));
            }
        }
        // Recurse into control flow structures
        TypedStatementKind::For { body, .. } => {
            for s in body {
                collect_dims(
                    s,
                    declared_vars,
                    locals,
                    existing_vars,
                    is_main_program,
                    variable_renames,
                    array_names,
                );
            }
        }
        TypedStatementKind::If {
            then_branch,
            elseif_branches,
            else_branch,
            ..
        } => {
            for s in then_branch {
                collect_dims(
                    s,
                    declared_vars,
                    locals,
                    existing_vars,
                    is_main_program,
                    variable_renames,
                    array_names,
                );
            }
            for (_, branch_body) in elseif_branches {
                for s in branch_body {
                    collect_dims(
                        s,
                        declared_vars,
                        locals,
                        existing_vars,
                        is_main_program,
                        variable_renames,
                        array_names,
                    );
                }
            }
            if let Some(else_stmts) = else_branch {
                for s in else_stmts {
                    collect_dims(
                        s,
                        declared_vars,
                        locals,
                        existing_vars,
                        is_main_program,
                        variable_renames,
                        array_names,
                    );
                }
            }
        }
        TypedStatementKind::While { body, .. } | TypedStatementKind::DoLoop { body, .. } => {
            for s in body {
                collect_dims(
                    s,
                    declared_vars,
                    locals,
                    existing_vars,
                    is_main_program,
                    variable_renames,
                    array_names,
                );
            }
        }
        TypedStatementKind::SelectCase { cases, .. } => {
            for case in cases {
                for s in &case.body {
                    collect_dims(
                        s,
                        declared_vars,
                        locals,
                        existing_vars,
                        is_main_program,
                        variable_renames,
                        array_names,
                    );
                }
            }
        }
        _ => {}
    }
}

/// Pass 2: Collects implicit variables from assignments and expressions.
///
/// This handles:
/// - Direct assignments to undeclared variables
/// - FOR loop counter variables
/// - ByRef function arguments
/// - Variables read but never assigned (common BASIC pattern)
fn collect_implicits(
    stmt: &TypedStatement,
    declared_vars: &mut HashSet<String>,
    locals: &mut Vec<String>,
    variable_renames: &mut HashMap<String, String>,
    array_names: &HashSet<String>,
) {
    match &stmt.kind {
        // Skip DIM - already handled in pass 1
        TypedStatementKind::Dim { .. }
        | TypedStatementKind::Redim { .. }
        | TypedStatementKind::StaticStmt { .. } => {}

        // Assignment to undeclared variable creates implicit local
        // Also scan the value expression for ByRef function arguments
        TypedStatementKind::Assignment {
            name,
            target_type,
            value,
            ..
        } => {
            declare_scalar_var(
                name,
                target_type,
                declared_vars,
                locals,
                Some(variable_renames),
                Some(array_names),
            );
            // Scan for ByRef function arguments in value
            collect_byref_vars(value, declared_vars, locals, variable_renames, array_names);
        }

        // FOR loop counter and expressions
        TypedStatementKind::For {
            variable,
            var_type,
            start,
            end,
            step,
            body,
            ..
        } => {
            declare_scalar_var(
                variable,
                var_type,
                declared_vars,
                locals,
                Some(variable_renames),
                Some(array_names),
            );
            // Scan FOR loop expressions for ByRef function args
            collect_byref_vars(start, declared_vars, locals, variable_renames, array_names);
            collect_byref_vars(end, declared_vars, locals, variable_renames, array_names);
            if let Some(step_expr) = step {
                collect_byref_vars(
                    step_expr,
                    declared_vars,
                    locals,
                    variable_renames,
                    array_names,
                );
            }
            for s in body {
                collect_implicits(s, declared_vars, locals, variable_renames, array_names);
            }
        }

        // Recurse into control flow - also scan conditions
        TypedStatementKind::If {
            condition,
            then_branch,
            elseif_branches,
            else_branch,
        } => {
            collect_byref_vars(
                condition,
                declared_vars,
                locals,
                variable_renames,
                array_names,
            );
            for s in then_branch {
                collect_implicits(s, declared_vars, locals, variable_renames, array_names);
            }
            for (cond, branch_body) in elseif_branches {
                collect_byref_vars(cond, declared_vars, locals, variable_renames, array_names);
                for s in branch_body {
                    collect_implicits(s, declared_vars, locals, variable_renames, array_names);
                }
            }
            if let Some(else_stmts) = else_branch {
                for s in else_stmts {
                    collect_implicits(s, declared_vars, locals, variable_renames, array_names);
                }
            }
        }

        TypedStatementKind::While { condition, body } => {
            collect_byref_vars(
                condition,
                declared_vars,
                locals,
                variable_renames,
                array_names,
            );
            for s in body {
                collect_implicits(s, declared_vars, locals, variable_renames, array_names);
            }
        }

        TypedStatementKind::DoLoop {
            pre_condition,
            post_condition,
            body,
        } => {
            if let Some(cond) = pre_condition {
                collect_byref_vars(
                    &cond.condition,
                    declared_vars,
                    locals,
                    variable_renames,
                    array_names,
                );
            }
            if let Some(cond) = post_condition {
                collect_byref_vars(
                    &cond.condition,
                    declared_vars,
                    locals,
                    variable_renames,
                    array_names,
                );
            }
            for s in body {
                collect_implicits(s, declared_vars, locals, variable_renames, array_names);
            }
        }

        TypedStatementKind::SelectCase {
            test_expr,
            cases,
            case_else,
        }
        | TypedStatementKind::SelectEveryCase {
            test_expr,
            cases,
            case_else,
        } => {
            // Collect variables from test expression
            collect_byref_vars(
                test_expr,
                declared_vars,
                locals,
                variable_renames,
                array_names,
            );
            for case in cases {
                for m in &case.matches {
                    collect_case_match_byref(
                        m,
                        declared_vars,
                        locals,
                        variable_renames,
                        array_names,
                    );
                }
                for s in &case.body {
                    collect_implicits(s, declared_vars, locals, variable_renames, array_names);
                }
            }
            if let Some(else_stmts) = case_else {
                for s in else_stmts {
                    collect_implicits(s, declared_vars, locals, variable_renames, array_names);
                }
            }
        }

        // Collect implicit variables from ByRef function args in Call statements
        TypedStatementKind::Call { args, params, .. } => {
            // For Call statements, check if each argument is ByRef and declare if needed
            for (i, arg) in args.iter().enumerate() {
                let is_byref = params
                    .get(i)
                    .map(|p| !p.by_val && !p.is_array)
                    .unwrap_or(false);

                if is_byref {
                    // If the argument is a simple variable, declare it
                    if let TypedExprKind::Variable(name) = &arg.kind {
                        declare_scalar_var(
                            name,
                            &arg.basic_type,
                            declared_vars,
                            locals,
                            Some(variable_renames),
                            Some(array_names),
                        );
                    }
                }
                // Also recursively collect from nested function calls in the arg
                collect_byref_vars(arg, declared_vars, locals, variable_renames, array_names);
            }
        }

        TypedStatementKind::Print { items, .. } => {
            for item in items {
                collect_byref_vars(
                    &item.expr,
                    declared_vars,
                    locals,
                    variable_renames,
                    array_names,
                );
            }
        }

        _ => {
            // For other statements, collect ByRef args from any expressions they contain
            collect_stmt_byref(stmt, declared_vars, locals, variable_renames, array_names);
        }
    }
}

/// Collects ByRef variables from CASE match conditions.
fn collect_case_match_byref(
    m: &TypedCaseMatch,
    declared_vars: &mut HashSet<String>,
    locals: &mut Vec<String>,
    variable_renames: &mut HashMap<String, String>,
    array_names: &HashSet<String>,
) {
    match m {
        TypedCaseMatch::Single(expr) => {
            collect_byref_vars(expr, declared_vars, locals, variable_renames, array_names);
        }
        TypedCaseMatch::Range { from, to } => {
            collect_byref_vars(from, declared_vars, locals, variable_renames, array_names);
            collect_byref_vars(to, declared_vars, locals, variable_renames, array_names);
        }
        TypedCaseMatch::Comparison { value, .. } => {
            collect_byref_vars(value, declared_vars, locals, variable_renames, array_names);
        }
    }
}

/// Collects implicit variables from ByRef function arguments.
///
/// This is a targeted approach that only declares variables when they're
/// passed by reference to function calls. This avoids incorrectly declaring
/// global/shared arrays as local scalars.
fn collect_byref_vars(
    expr: &TypedExpr,
    declared_vars: &mut HashSet<String>,
    locals: &mut Vec<String>,
    variable_renames: &mut HashMap<String, String>,
    array_names: &HashSet<String>,
) {
    match &expr.kind {
        // For function calls, check if any ByRef argument is a simple variable
        TypedExprKind::FunctionCall { args, params, .. } => {
            for (i, arg) in args.iter().enumerate() {
                // Check if this parameter is ByRef (not ByVal)
                let is_byref = params
                    .get(i)
                    .map(|p| !p.by_val && !p.is_array)
                    .unwrap_or(false);

                if is_byref {
                    // If the argument is a simple variable, declare it
                    if let TypedExprKind::Variable(name) = &arg.kind {
                        declare_scalar_var(
                            name,
                            &arg.basic_type,
                            declared_vars,
                            locals,
                            Some(variable_renames),
                            Some(array_names),
                        );
                    }
                }
                // Recurse into arg expressions to find nested function calls
                collect_byref_vars(arg, declared_vars, locals, variable_renames, array_names);
            }
        }
        // Recurse into sub-expressions to find nested function calls
        TypedExprKind::Binary { left, right, .. } => {
            collect_byref_vars(left, declared_vars, locals, variable_renames, array_names);
            collect_byref_vars(right, declared_vars, locals, variable_renames, array_names);
        }
        TypedExprKind::Unary { operand, .. } => {
            collect_byref_vars(
                operand,
                declared_vars,
                locals,
                variable_renames,
                array_names,
            );
        }
        TypedExprKind::Grouped(inner) => {
            collect_byref_vars(inner, declared_vars, locals, variable_renames, array_names);
        }
        TypedExprKind::ArrayAccess {
            name: _, indices, ..
        } => {
            // This is an array access - the array name is used as an array, not a scalar.
            // We should NOT declare it as a scalar variable. Only process the indices
            // for any ByRef variables they might contain.
            // Note: The array name itself is already declared (via DIM) or is a global,
            // so we don't need to declare it here.
            for idx in indices {
                collect_byref_vars(idx, declared_vars, locals, variable_renames, array_names);
            }
        }
        TypedExprKind::ExternalFunctionCall { args, .. } => {
            for arg in args {
                collect_byref_vars(arg, declared_vars, locals, variable_renames, array_names);
            }
        }
        TypedExprKind::Convert { expr: inner, .. }
        | TypedExprKind::CvFunc { value: inner, .. }
        | TypedExprKind::MkDollarFunc { value: inner, .. }
        | TypedExprKind::CastFunc { value: inner, .. }
        | TypedExprKind::ValWithType { value: inner, .. } => {
            collect_byref_vars(inner, declared_vars, locals, variable_renames, array_names);
        }
        TypedExprKind::FieldAccess { object, .. } => {
            collect_byref_vars(object, declared_vars, locals, variable_renames, array_names);
        }
        // Variables that are READ but never assigned - common BASIC pattern
        // Declare them with default initialization (matches QB64 implicit declaration)
        // Skip variables starting with '_' - these are QB64 built-in constants (#defined)
        //
        // CONSERVATIVE APPROACH: Only declare scalars when we're certain they're needed.
        // If an array with this name exists, we should NOT create a scalar variable
        // unless we're in a clear scalar usage context (like direct assignment).
        // Since this function is called for expressions (not assignments), seeing
        // a Variable here means it's being read, not assigned. If an array exists,
        // this is likely array usage, not scalar usage, so we skip scalar creation.
        //
        // CRITICAL: We must be very conservative here. If an array exists with this name,
        // we should NOT create a scalar at all, even if it means the variable might not
        // be declared. It's better to have a missing declaration error than to create
        // a scalar that conflicts with array usage. The variable will be declared when
        // it's actually used as a scalar (e.g., in an assignment statement).
        TypedExprKind::Variable(name) => {
            if !name.starts_with('_') {
                let c_name = c_identifier(name);
                // CONSERVATIVE: If an array with this name exists, NEVER create a scalar.
                // The variable is being used in an expression context, and if an array
                // exists, this is almost certainly array usage (even if the expression
                // itself is a Variable node, it might be from a context where array access
                // was expected). Only create scalars when we're absolutely certain there's
                // no array with this name.
                //
                // This prevents the error where we create `providedArgs_scalar` when only
                // `providedArgs` array exists, causing invalid C code like `providedArgs_scalar[1]`.
                if !array_names.contains(&c_name) {
                    // Also check if the variable is already declared (might be a global or parameter)
                    if !declared_vars.contains(&c_name) {
                        declare_scalar_var(
                            name,
                            &expr.basic_type,
                            declared_vars,
                            locals,
                            Some(variable_renames),
                            Some(array_names),
                        );
                    }
                }
                // If array exists, skip scalar creation completely - array accesses are handled
                // separately and don't need scalar declarations. The variable will be declared
                // when it's actually used as a scalar (e.g., in an assignment like `x = 5`).
            }
        }
        // Literals and other nodes
        _ => {}
    }
}

/// Collects ByRef vars from expressions in various statement kinds.
///
/// This is a catch-all handler for statement types not explicitly handled
/// in `collect_implicits`.
fn collect_stmt_byref(
    stmt: &TypedStatement,
    declared_vars: &mut HashSet<String>,
    locals: &mut Vec<String>,
    variable_renames: &mut HashMap<String, String>,
    array_names: &HashSet<String>,
) {
    match &stmt.kind {
        TypedStatementKind::Assignment { value, .. } => {
            collect_byref_vars(value, declared_vars, locals, variable_renames, array_names);
        }
        TypedStatementKind::If {
            condition,
            then_branch,
            elseif_branches,
            else_branch,
        } => {
            collect_byref_vars(
                condition,
                declared_vars,
                locals,
                variable_renames,
                array_names,
            );
            for s in then_branch {
                collect_implicits(s, declared_vars, locals, variable_renames, array_names);
            }
            for (cond, branch_body) in elseif_branches {
                collect_byref_vars(cond, declared_vars, locals, variable_renames, array_names);
                for s in branch_body {
                    collect_implicits(s, declared_vars, locals, variable_renames, array_names);
                }
            }
            if let Some(else_stmts) = else_branch {
                for s in else_stmts {
                    collect_implicits(s, declared_vars, locals, variable_renames, array_names);
                }
            }
        }
        TypedStatementKind::While { condition, body } => {
            collect_byref_vars(
                condition,
                declared_vars,
                locals,
                variable_renames,
                array_names,
            );
            for s in body {
                collect_implicits(s, declared_vars, locals, variable_renames, array_names);
            }
        }
        TypedStatementKind::DoLoop {
            pre_condition,
            post_condition,
            body,
        } => {
            if let Some(cond) = pre_condition {
                collect_byref_vars(
                    &cond.condition,
                    declared_vars,
                    locals,
                    variable_renames,
                    array_names,
                );
            }
            if let Some(cond) = post_condition {
                collect_byref_vars(
                    &cond.condition,
                    declared_vars,
                    locals,
                    variable_renames,
                    array_names,
                );
            }
            for s in body {
                collect_implicits(s, declared_vars, locals, variable_renames, array_names);
            }
        }
        TypedStatementKind::For {
            start,
            end,
            step,
            body,
            ..
        } => {
            collect_byref_vars(start, declared_vars, locals, variable_renames, array_names);
            collect_byref_vars(end, declared_vars, locals, variable_renames, array_names);
            if let Some(step_expr) = step {
                collect_byref_vars(
                    step_expr,
                    declared_vars,
                    locals,
                    variable_renames,
                    array_names,
                );
            }
            for s in body {
                collect_implicits(s, declared_vars, locals, variable_renames, array_names);
            }
        }
        // FileGet has a target variable that needs to be declared
        TypedStatementKind::FileGet { target, .. } => {
            declare_input_target(target, declared_vars, locals, variable_renames, array_names);
        }
        // FileLineInput has a target variable that needs to be declared
        TypedStatementKind::FileLineInput { target, .. } => {
            declare_input_target(target, declared_vars, locals, variable_renames, array_names);
        }
        // Input statement has multiple targets that need to be declared
        TypedStatementKind::Input { targets, .. } => {
            for target in targets {
                declare_input_target(target, declared_vars, locals, variable_renames, array_names);
            }
        }
        // LineInput has a target that needs to be declared
        TypedStatementKind::LineInput { target, .. } => {
            declare_input_target(target, declared_vars, locals, variable_renames, array_names);
        }
        // FileInput has targets that need to be declared
        TypedStatementKind::FileInput { targets, .. } => {
            for target in targets {
                declare_input_target(target, declared_vars, locals, variable_renames, array_names);
            }
        }
        // Color statement - scan foreground/background expressions
        TypedStatementKind::Color {
            foreground,
            background,
            border,
        } => {
            if let Some(fg) = foreground {
                collect_byref_vars(fg, declared_vars, locals, variable_renames, array_names);
            }
            if let Some(bg) = background {
                collect_byref_vars(bg, declared_vars, locals, variable_renames, array_names);
            }
            if let Some(b) = border {
                collect_byref_vars(b, declared_vars, locals, variable_renames, array_names);
            }
        }
        // Print statement - scan print items for expressions
        TypedStatementKind::Print { items, .. } => {
            for item in items {
                collect_byref_vars(
                    &item.expr,
                    declared_vars,
                    locals,
                    variable_renames,
                    array_names,
                );
            }
        }
        // PrintStringStmt (_PRINTSTRING) - scan position and text expressions
        TypedStatementKind::PrintStringStmt { x, y, text } => {
            collect_byref_vars(x, declared_vars, locals, variable_renames, array_names);
            collect_byref_vars(y, declared_vars, locals, variable_renames, array_names);
            collect_byref_vars(text, declared_vars, locals, variable_renames, array_names);
        }
        // SelectCase - scan test expression and case conditions
        TypedStatementKind::SelectCase {
            test_expr, cases, ..
        } => {
            collect_byref_vars(
                test_expr,
                declared_vars,
                locals,
                variable_renames,
                array_names,
            );
            for case in cases {
                for m in &case.matches {
                    match m {
                        TypedCaseMatch::Single(expr) => {
                            collect_byref_vars(
                                expr,
                                declared_vars,
                                locals,
                                variable_renames,
                                array_names,
                            );
                        }
                        TypedCaseMatch::Range { from, to } => {
                            collect_byref_vars(
                                from,
                                declared_vars,
                                locals,
                                variable_renames,
                                array_names,
                            );
                            collect_byref_vars(
                                to,
                                declared_vars,
                                locals,
                                variable_renames,
                                array_names,
                            );
                        }
                        TypedCaseMatch::Comparison { value, .. } => {
                            collect_byref_vars(
                                value,
                                declared_vars,
                                locals,
                                variable_renames,
                                array_names,
                            );
                        }
                    }
                }
                for s in &case.body {
                    collect_implicits(s, declared_vars, locals, variable_renames, array_names);
                }
            }
        }
        // Call - scan arguments for variables
        TypedStatementKind::Call { args, .. } => {
            for arg in args {
                collect_byref_vars(arg, declared_vars, locals, variable_renames, array_names);
            }
        }
        // ArrayAssignment - scan value and indices
        TypedStatementKind::ArrayAssignment { value, indices, .. } => {
            collect_byref_vars(value, declared_vars, locals, variable_renames, array_names);
            for idx in indices {
                collect_byref_vars(idx, declared_vars, locals, variable_renames, array_names);
            }
        }
        // FieldAssignment - scan value expression
        TypedStatementKind::FieldAssignment { value, .. } => {
            collect_byref_vars(value, declared_vars, locals, variable_renames, array_names);
        }
        // ArrayFieldAssignment - scan value and indices
        TypedStatementKind::ArrayFieldAssignment { value, indices, .. } => {
            collect_byref_vars(value, declared_vars, locals, variable_renames, array_names);
            for idx in indices {
                collect_byref_vars(idx, declared_vars, locals, variable_renames, array_names);
            }
        }
        _ => {}
    }
}

/// Declares a variable from an input target.
///
/// INPUT targets can be simple variables, array elements, or fields.
/// Only simple variables need declaration here.
fn declare_input_target(
    target: &TypedInputTarget,
    declared_vars: &mut HashSet<String>,
    locals: &mut Vec<String>,
    variable_renames: &mut HashMap<String, String>,
    array_names: &HashSet<String>,
) {
    match target {
        TypedInputTarget::Variable { name, basic_type } => {
            declare_scalar_var(
                name,
                basic_type,
                declared_vars,
                locals,
                Some(variable_renames),
                Some(array_names),
            );
        }
        // Array elements don't need declaration - the array itself is already declared
        TypedInputTarget::ArrayElement { .. } => {}
        TypedInputTarget::ArrayElementField { .. } => {}
        TypedInputTarget::Field { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Span;
    use crate::semantic::typed_ir::TypedExpr;
    use crate::semantic::types::BasicType;

    #[test]
    fn test_collect_implicit_from_assignment() {
        let body = vec![TypedStatement::new(
            TypedStatementKind::Assignment {
                name: "x".to_string(),
                value: TypedExpr::integer(42, Span::new(4, 6, 1)),
                target_type: BasicType::Long,
            },
            Span::new(0, 6, 1),
        )];

        let mut renames = HashMap::new();
        let locals = collect_implicit_locals(
            &body,
            &[],
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            false,
            &mut renames,
        );

        assert_eq!(locals.len(), 1);
        assert!(locals[0].contains("x"));
        assert!(locals[0].contains("int32_t"));
    }

    #[test]
    fn test_reserved_identifiers_not_declared() {
        // Create an assignment that references _TRUE (a reserved identifier)
        let body = vec![TypedStatement::new(
            TypedStatementKind::Assignment {
                name: "_TRUE".to_string(),
                value: TypedExpr::integer(1, Span::new(8, 9, 1)),
                target_type: BasicType::Long,
            },
            Span::new(0, 9, 1),
        )];

        let mut renames = HashMap::new();
        let locals = collect_implicit_locals(
            &body,
            &[],
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            false,
            &mut renames,
        );

        // _TRUE should not be in locals because it's a reserved identifier
        assert!(
            locals.is_empty() || !locals.iter().any(|l| l.contains("_TRUE")),
            "Reserved identifier _TRUE should not be declared"
        );
    }

    #[test]
    fn test_params_not_redeclared() {
        let body = vec![TypedStatement::new(
            TypedStatementKind::Assignment {
                name: "param1".to_string(),
                value: TypedExpr::integer(10, Span::new(10, 12, 1)),
                target_type: BasicType::Long,
            },
            Span::new(0, 12, 1),
        )];

        let params = vec![TypedParameter {
            name: "param1".to_string(),
            basic_type: BasicType::Long,
            by_val: false,
            is_array: false,
        }];

        let mut variable_renames = HashMap::new();
        let locals = collect_implicit_locals(
            &body,
            &params,
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            &HashSet::new(),
            false,
            &mut variable_renames,
        );

        // param1 should not be in locals because it's a parameter
        assert!(
            locals.is_empty() || !locals.iter().any(|l| l.contains("param1")),
            "Parameter should not be redeclared"
        );
    }
}
