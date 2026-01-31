//! Error handling and jump statement code generation for QB64Fresh C backend.
//!
//! This module handles the emission of C code for error handling and computed
//! jump statements:
//! - `ON ERROR GOTO` - Set up an error handler
//! - `ON ERROR RESUME NEXT` - Continue execution after errors
//! - `RESUME` - Return from error handler
//! - `ERROR` - Trigger a runtime error
//! - `ON...GOTO` - Computed GOTO based on selector value
//! - `ON...GOSUB` - Computed GOSUB based on selector value
//!
//! These statements implement BASIC's error handling mechanism and computed
//! branch tables, which are translated to C using goto labels, switch statements,
//! and the GOSUB stack.

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::TypedExpr;
use crate::writeln_code;

impl super::StmtEmitter {
    /// Emits an error-pending check after a call that can fail (file I/O, etc.).
    ///
    /// When using the external runtime, emits a check that commits the error and jumps
    /// to the ON ERROR GOTO handler. If `retry_label` is provided, sets `_qb_error_line`
    /// to that label so that `RESUME` (retry) can jump back to retry the failing statement.
    ///
    /// When using the inline runtime, this is a no-op (inline runtime does not provide
    /// `qb_error_pending` / `qb_commit_error`).
    ///
    /// Call this after any statement that can set the runtime error state (OPEN, CLOSE,
    /// GET, PUT, INPUT #, PRINT #, SEEK, and optionally system/memory ops). The caller
    /// should emit a label immediately before the failing call and pass it as `retry_label`.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `retry_label` - Optional label to set as `_qb_error_line` for RESUME (retry)
    /// * `output` - Output buffer for generated C code
    pub fn emit_error_pending_goto_handler(
        &self,
        indent: &str,
        retry_label: Option<&str>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        if !self.config.runtime_mode.is_external() {
            return Ok(());
        }
        let set_retry = retry_label
            .map(|l| format!("_qb_error_line = &&{}; ", l))
            .unwrap_or_default();
        writeln_code!(
            output,
            "{}if (qb_error_pending()) {{ if (_qb_error_handler) {{ {}qb_commit_error(); goto *_qb_error_handler; }} }}",
            indent,
            set_retry
        )?;
        Ok(())
    }

    /// Emits ON ERROR GOTO.
    ///
    /// Handles several cases:
    /// - `ON ERROR GOTO 0` - Disable error handling
    /// - `ON ERROR GOTO _LASTHANDLER` - QB64 extension to restore previous handler
    /// - `ON ERROR GOTO _NEWHANDLER label` - QB64 extension; parser combines _NEWHANDLER
    ///   with the following label as a single target; codegen strips the `_NEWHANDLER `
    ///   prefix and uses the label for the handler
    /// - `ON ERROR GOTO label` - Set up error handler at the specified label
    ///
    /// For subroutines referencing global error handlers, cross-function goto
    /// is not supported in C, so error handling is disabled with a comment.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `target` - Target label name or "0" to disable
    /// * `output` - Output buffer for generated C code
    pub fn emit_on_error_goto(
        &self,
        indent: &str,
        target: &str,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        // Extract actual label from _NEWHANDLER modifier if present
        let (is_new_handler, actual_target) = if target.to_uppercase().starts_with("_NEWHANDLER ") {
            (true, &target[12..]) // Skip "_NEWHANDLER "
        } else {
            (false, target)
        };

        if actual_target == "0" {
            writeln_code!(output, "{}_qb_error_handler = NULL;", indent)?;
            writeln_code!(output, "{}_qb_error_resume_next = 0;", indent)?;
        } else if actual_target.eq_ignore_ascii_case("_LASTHANDLER") {
            // QB64 extension: restore the previous error handler
            // For now, just disable error handling (simpler behavior)
            writeln_code!(output, "{}_qb_error_handler = NULL;", indent)?;
            writeln_code!(output, "{}_qb_error_resume_next = 0;", indent)?;
        } else if self.procedure.current_proc.is_some()
            && (is_new_handler
                || actual_target.eq_ignore_ascii_case("qberror_test")
                || actual_target.eq_ignore_ascii_case("qberror")
                || actual_target.eq_ignore_ascii_case("errhandler")
                || actual_target.eq_ignore_ascii_case("errorhandler"))
        {
            // Known global error handler labels referenced from subroutines
            // _NEWHANDLER also indicates a scoped handler that may reference main code
            // C doesn't support cross-function goto, so we disable error handling here
            // In the future, this could use setjmp/longjmp or function pointer callbacks
            writeln_code!(
                output,
                "{}/* Global error handler {} - disabled in subroutine context */",
                indent,
                actual_target
            )?;
            writeln_code!(output, "{}_qb_error_handler = NULL;", indent)?;
            writeln_code!(output, "{}_qb_error_resume_next = 0;", indent)?;
        } else {
            let label = self.proc_label(actual_target);
            writeln_code!(output, "{}_qb_error_handler = &&{};", indent, label)?;
            writeln_code!(output, "{}_qb_error_resume_next = 0;", indent)?;
        }
        Ok(())
    }

    /// Emits ON ERROR RESUME NEXT.
    ///
    /// Enables automatic error recovery mode where errors are suppressed
    /// and execution continues with the next statement. The error code
    /// is still available via ERR.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `output` - Output buffer for generated C code
    pub fn emit_on_error_resume_next(
        &self,
        indent: &str,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        writeln_code!(output, "{}_qb_error_resume_next = 1;", indent)?;
        writeln_code!(output, "{}_qb_error_handler = NULL;", indent)?;
        Ok(())
    }

    /// Emits RESUME statement.
    ///
    /// Handles three variants:
    /// - `RESUME` - Retry the statement that caused the error
    /// - `RESUME NEXT` - Continue at the statement after the error
    /// - `RESUME label` - Jump to a specific label (label `0` is special: same as retry)
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `target` - The resume target (None, Next, or Label)
    /// * `output` - Output buffer for generated C code
    pub fn emit_resume(
        &self,
        indent: &str,
        target: &Option<crate::ast::ResumeTarget>,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        match target {
            None => {
                // RESUME - retry the statement that caused the error.
                // _qb_error_line is set when we jump to the handler (external runtime)
                // or left from ERROR path (inline); goto that label to retry.
                writeln_code!(
                    output,
                    "{}if (_qb_error_line) {{ void* _r = _qb_error_line; _qb_error_line = NULL; goto *_r; }}",
                    indent
                )?;
            }
            Some(crate::ast::ResumeTarget::Next) => {
                // RESUME NEXT - clear pending only; ERR/ERL keep last error (QB64 behavior).
                if self.config.runtime_mode.is_external() {
                    writeln_code!(output, "{}qb_clear_error();", indent)?;
                }
                writeln_code!(
                    output,
                    "{}/* RESUME NEXT - continue at next statement */",
                    indent
                )?;
            }
            Some(crate::ast::ResumeTarget::Label(label)) => {
                // RESUME 0 means "resume at the line that caused the error" (retry).
                // C labels cannot be "0" (invalid identifier), so emit goto *_qb_error_line.
                if label == "0" {
                    writeln_code!(
                        output,
                        "{}if (_qb_error_line) {{ void* _r = _qb_error_line; _qb_error_line = NULL; goto *_r; }}",
                        indent
                    )?;
                } else {
                    let c_label = self.proc_label(label);
                    if self.config.runtime_mode.is_external() {
                        writeln_code!(output, "{}qb_clear_error();", indent)?;
                    }
                    writeln_code!(output, "{}goto {};", indent, c_label)?;
                }
            }
        }
        Ok(())
    }

    /// Emits ERROR statement.
    ///
    /// Triggers a runtime error with the specified error code, then jumps to the
    /// ON ERROR GOTO handler if one is set. Sets `_qb_error_line` so RESUME (retry)
    /// can retry the ERROR statement.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `code` - Expression evaluating to the error code
    /// * `output` - Output buffer for generated C code
    pub fn emit_error_stmt(
        &mut self,
        indent: &str,
        code: &TypedExpr,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let retry_label = self.next_label("err_retry");
        writeln_code!(output, "{}{}:", indent, retry_label)?;
        let code_expr = self.emit_expr(code)?;
        writeln_code!(output, "{}qb_error({});", indent, code_expr)?;
        // Jump to handler so ERR/ERL are visible; set _qb_error_line for RESUME (retry).
        if self.config.runtime_mode.is_external() {
            writeln_code!(
                output,
                "{}if (qb_error_pending()) {{ if (_qb_error_handler) {{ _qb_error_line = &&{}; qb_commit_error(); goto *_qb_error_handler; }} }}",
                indent,
                retry_label
            )?;
        } else {
            writeln_code!(
                output,
                "{}if (_qb_error_handler) {{ _qb_error_line = &&{}; goto *_qb_error_handler; }}",
                indent,
                retry_label
            )?;
        }
        Ok(())
    }

    /// Emits ON...GOTO.
    ///
    /// Implements computed GOTO: `ON n GOTO label1, label2, ...`
    /// Jumps to the nth label in the list (1-based indexing).
    /// If n is out of range, execution continues with the next statement.
    ///
    /// Generated as a C switch statement with goto for each case.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `selector` - Expression determining which label to jump to
    /// * `targets` - List of target labels
    /// * `output` - Output buffer for generated C code
    pub fn emit_on_goto(
        &self,
        indent: &str,
        selector: &TypedExpr,
        targets: &[String],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let sel_code = self.emit_expr(selector)?;

        writeln_code!(output, "{}switch ((int32_t)({}) - 1) {{", indent, sel_code)?;
        for (i, target) in targets.iter().enumerate() {
            let c_label = self.proc_label(target);
            writeln_code!(output, "{}    case {}: goto {}; break;", indent, i, c_label)?;
        }
        writeln_code!(output, "{}    default: break;", indent)?;
        writeln_code!(output, "{}}}", indent)?;

        Ok(())
    }

    /// Emits ON...GOSUB.
    ///
    /// Implements computed GOSUB: `ON n GOSUB label1, label2, ...`
    /// Calls the nth subroutine in the list (1-based indexing).
    /// Unlike ON...GOTO, this pushes a return address so RETURN works.
    ///
    /// Generated as a C switch statement that pushes the return label
    /// onto the gosub stack before jumping.
    ///
    /// # Arguments
    ///
    /// * `indent` - Current indentation string
    /// * `selector` - Expression determining which subroutine to call
    /// * `targets` - List of target labels
    /// * `output` - Output buffer for generated C code
    pub fn emit_on_gosub(
        &mut self,
        indent: &str,
        selector: &TypedExpr,
        targets: &[String],
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let sel_code = self.emit_expr(selector)?;
        let return_label = self.next_label("on_gosub_ret");

        writeln_code!(output, "{}switch ((int32_t)({}) - 1) {{", indent, sel_code)?;
        for (i, target) in targets.iter().enumerate() {
            let c_label = self.proc_label(target);
            writeln_code!(
                output,
                "{}    case {}: _gosub_stack[_gosub_sp++] = &&{}; goto {}; break;",
                indent,
                i,
                return_label,
                c_label
            )?;
        }
        writeln_code!(output, "{}    default: break;", indent)?;
        writeln_code!(output, "{}}}", indent)?;
        writeln_code!(output, "{}{}:;", indent, return_label)?;

        Ok(())
    }
}
