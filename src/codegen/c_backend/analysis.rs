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
pub(super) fn collect_globals(
    program: &TypedProgram,
    emit_params_fn: impl Fn(&[TypedParameter]) -> String,
) -> (Vec<String>, Vec<String>) {
    let mut globals = Vec::new();
    let mut forward_decls = Vec::new();

    for stmt in &program.statements {
        match &stmt.kind {
            TypedStatementKind::Dim {
                name,
                basic_type,
                dimensions,
                shared: _,
            } if dimensions.is_empty() => {
                // Simple global variable (non-array)
                let c_ty = c_type(basic_type);
                let c_name = c_identifier(name);
                let init = default_init(basic_type);
                globals.push(format!("{} {} = {};", c_ty, c_name, init));
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

    (globals, forward_decls)
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
