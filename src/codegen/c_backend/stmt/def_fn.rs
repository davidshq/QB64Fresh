//! DEF FN helper methods for statement code generation.
//!
//! This module contains methods for emitting DEF FN statements in C code:
//! - Single-line DEF FN (inline expression)
//! - Multi-line DEF FN (statement block)
//!
//! DEF FN defines inline functions in BASIC. Single-line DEF FN is emitted
//! as a static inline C function with a single expression. Multi-line DEF FN
//! is emitted as a static C function with a local return variable that the
//! body can assign to.
//!
//! These methods are part of [`StmtEmitter`](super::StmtEmitter) and handle
//! the generation of C code for BASIC DEF FN statements.

use std::fmt::Write;

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{TypedExpr, TypedParameter, TypedStatement};
use crate::semantic::types::BasicType;

use crate::codegen::c_backend::expr::emit_expr;
use crate::codegen::c_backend::types::{c_identifier, c_type, default_init};

impl super::StmtEmitter {
    /// Emits DEF FN as an inline function or macro.
    pub(in crate::codegen::c_backend) fn emit_def_fn(
        &self,
        name: &str,
        params: &[TypedParameter],
        return_type: &BasicType,
        body: &TypedExpr,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let fn_name = format!("_fn_{}", c_identifier(name));
        let c_return_type = c_type(return_type);

        // Emit as a static inline function
        let param_list = if params.is_empty() {
            "void".to_string()
        } else {
            params
                .iter()
                .map(|p| format!("{} {}", c_type(&p.basic_type), c_identifier(&p.name)))
                .collect::<Vec<_>>()
                .join(", ")
        };

        let body_code = emit_expr(body, self.no_shell)?;

        writeln!(
            output,
            "static inline {} {}({}) {{ return {}; }}",
            c_return_type, fn_name, param_list, body_code
        )
        .unwrap();

        Ok(())
    }

    /// Emits multi-line DEF FN as a static function.
    ///
    /// In multi-line DEF FN, the return value is set by assigning to the
    /// function name (e.g., `FNSquare = x * x`). We emit a local variable
    /// for the return value and return it at the end.
    pub(in crate::codegen::c_backend) fn emit_def_fn_multiline(
        &mut self,
        name: &str,
        params: &[TypedParameter],
        return_type: &BasicType,
        body: &[TypedStatement],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let fn_name = format!("_fn_{}", c_identifier(name));
        let c_return_type = c_type(return_type);

        // Parameter list
        let param_list = if params.is_empty() {
            "void".to_string()
        } else {
            params
                .iter()
                .map(|p| format!("{} {}", c_type(&p.basic_type), c_identifier(&p.name)))
                .collect::<Vec<_>>()
                .join(", ")
        };

        // Function header
        writeln!(
            output,
            "static {} {}({}) {{",
            c_return_type, fn_name, param_list
        )
        .unwrap();

        // Return value variable (initialized to default)
        let return_var = format!("_fn_{}", c_identifier(name));
        let init = default_init(return_type);
        writeln!(output, "    {} {} = {};", c_return_type, return_var, init).unwrap();

        // Set return variable for EXIT FUNCTION
        let old_ret_var = self.current_func_ret_var.take();
        self.current_func_ret_var = Some(return_var.clone());

        // Emit body statements
        let old_indent = self.indent;
        self.indent = 1;
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.indent = old_indent;
        self.current_func_ret_var = old_ret_var;

        // Return the result
        writeln!(output, "    return {};", return_var).unwrap();
        writeln!(output, "}}").unwrap();

        Ok(())
    }
}
