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

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{TypedExpr, TypedParameter, TypedStatement, TypedStatementKind};
use crate::semantic::types::BasicType;
use crate::writeln_code;

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

        let body_code = self.emit_expr(body)?;

        writeln_code!(
            output,
            "static inline {} {}({}) {{ return {}; }}",
            c_return_type,
            fn_name,
            param_list,
            body_code
        )?;

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
        writeln_code!(
            output,
            "static {} {}({}) {{",
            c_return_type,
            fn_name,
            param_list
        )?;

        // Return value variable (initialized to default)
        let return_var = format!("_fn_{}", c_identifier(name));
        let init = default_init(return_type);
        writeln_code!(output, "    {} {} = {};", c_return_type, return_var, init)?;

        // Set return variable for EXIT FUNCTION
        let old_ret_var = self.procedure.current_func_ret_var.take();
        self.procedure.current_func_ret_var = Some(return_var.clone());

        // Emit body statements
        let old_indent = self.codegen.indent;
        self.codegen.indent = 1;
        for stmt in body {
            self.emit_stmt(stmt, output)?;
        }
        self.codegen.indent = old_indent;
        self.procedure.current_func_ret_var = old_ret_var;

        // Return the result
        writeln_code!(output, "    return {};", return_var)?;
        writeln_code!(output, "}}")?;

        Ok(())
    }
}

/// Dispatcher for DEF FN statement kinds.
pub(super) fn emit_def_fn_stmt(
    emitter: &mut super::StmtEmitter,
    kind: &TypedStatementKind,
    _indent: &str,
    output: &mut String,
) -> Result<(), CodeGenError> {
    match kind {
        TypedStatementKind::DefFn {
            name,
            params,
            return_type,
            body,
        } => emitter.emit_def_fn(name, params, return_type, body, output)?,
        TypedStatementKind::DefFnMultiLine {
            name,
            params,
            return_type,
            body,
        } => emitter.emit_def_fn_multiline(name, params, return_type, body, output)?,
        _ => unreachable!("emit_def_fn_stmt called with non-DEF-FN kind"),
    }
    Ok(())
}
