//! System integration statement code generation.
//!
//! This module handles the emission of C code for system-related statements,
//! including file operations (KILL, RENAME, MKDIR), shell commands, and memory operations.

use crate::codegen::error::{CodeGenError, CodeGenErrorKind};
use crate::semantic::typed_ir::TypedStatementKind;
use crate::writeln_code;

use super::StmtEmitter;
use crate::codegen::c_backend::expr::emit_string_data_access;

/// Emits code for system integration statements.
pub(super) fn emit_system_stmt(
    emitter: &mut StmtEmitter,
    kind: &TypedStatementKind,
    indent: &str,
    output: &mut String,
) -> Result<(), CodeGenError> {
    match kind {
        TypedStatementKind::Kill { filename } => {
            let filename_code = emitter.emit_expr(filename)?;
            let filename_access =
                emit_string_data_access(filename, &filename_code, &emitter.config.runtime_mode);
            writeln_code!(output, "{}qb_file_kill({});", indent, filename_access)?;
            emitter.emit_error_pending_goto_handler(indent, output)?;
        }

        TypedStatementKind::Rename { old_name, new_name } => {
            let old_code = emitter.emit_expr(old_name)?;
            let new_code = emitter.emit_expr(new_name)?;
            let old_access =
                emit_string_data_access(old_name, &old_code, &emitter.config.runtime_mode);
            let new_access =
                emit_string_data_access(new_name, &new_code, &emitter.config.runtime_mode);
            writeln_code!(
                output,
                "{}qb_file_rename({}, {});",
                indent,
                old_access,
                new_access
            )?;
            emitter.emit_error_pending_goto_handler(indent, output)?;
        }

        TypedStatementKind::Mkdir { path } => {
            let path_code = emitter.emit_expr(path)?;
            let path_access =
                emit_string_data_access(path, &path_code, &emitter.config.runtime_mode);
            writeln_code!(output, "{}qb_mkdir({});", indent, path_access)?;
            emitter.emit_error_pending_goto_handler(indent, output)?;
        }

        TypedStatementKind::Rmdir { path } => {
            let path_code = emitter.emit_expr(path)?;
            let path_access =
                emit_string_data_access(path, &path_code, &emitter.config.runtime_mode);
            writeln_code!(output, "{}qb_rmdir({});", indent, path_access)?;
            emitter.emit_error_pending_goto_handler(indent, output)?;
        }

        TypedStatementKind::Chdir { path } => {
            let path_code = emitter.emit_expr(path)?;
            let path_access =
                emit_string_data_access(path, &path_code, &emitter.config.runtime_mode);
            writeln_code!(output, "{}qb_chdir({});", indent, path_access)?;
            emitter.emit_error_pending_goto_handler(indent, output)?;
        }

        TypedStatementKind::Environ { env_string } => {
            let env_string_code = emitter.emit_expr(env_string)?;
            writeln_code!(output, "{}qb_sub_environ({});", indent, env_string_code)?;
        }

        TypedStatementKind::ShellCmd { command } => {
            if emitter.config.no_shell {
                return Err(CodeGenError::new(CodeGenErrorKind::ShellDisabled));
            }
            if let Some(cmd) = command {
                let cmd_code = emitter.emit_expr(cmd)?;
                // qb_shell expects const char*, not qb_string*
                let cmd_data =
                    emit_string_data_access(cmd, &cmd_code, &emitter.config.runtime_mode);
                writeln_code!(output, "{}qb_shell({});", indent, cmd_data)?;
            } else {
                writeln_code!(output, "{}qb_shell(NULL);", indent)?;
            }
            emitter.emit_error_pending_goto_handler(indent, output)?;
        }

        TypedStatementKind::ShellHide { command } => {
            if emitter.config.no_shell {
                return Err(CodeGenError::new(CodeGenErrorKind::ShellDisabled));
            }
            let cmd_code = emitter.emit_expr(command)?;
            writeln_code!(output, "{}qb_shellhide({});", indent, cmd_code)?;
        }

        TypedStatementKind::Bload { filename, address } => {
            let filename_code = emitter.emit_expr(filename)?;
            let filename_access =
                emit_string_data_access(filename, &filename_code, &emitter.config.runtime_mode);
            if let Some(addr) = address {
                let addr_code = emitter.emit_expr(addr)?;
                writeln_code!(
                    output,
                    "{}qb_bload({}, (void*)(intptr_t){});",
                    indent,
                    filename_access,
                    addr_code
                )?;
            } else {
                writeln_code!(output, "{}qb_bload({}, NULL);", indent, filename_access)?;
            }
        }

        TypedStatementKind::Bsave {
            filename,
            address,
            length,
        } => {
            let filename_code = emitter.emit_expr(filename)?;
            let filename_access =
                emit_string_data_access(filename, &filename_code, &emitter.config.runtime_mode);
            let addr_code = emitter.emit_expr(address)?;
            let len_code = emitter.emit_expr(length)?;
            writeln_code!(
                output,
                "{}qb_bsave({}, (void*)(intptr_t){}, (size_t){});",
                indent,
                filename_access,
                addr_code,
                len_code
            )?;
        }

        TypedStatementKind::Setmem { bytes } => {
            // SETMEM is a no-op in modern systems - just evaluate the expression
            let bytes_code = emitter.emit_expr(bytes)?;
            writeln_code!(
                output,
                "{}(void){}; /* SETMEM: no-op in flat memory model */",
                indent,
                bytes_code
            )?;
        }

        TypedStatementKind::CallAbsolute { args: _, address } => {
            // CALL ABSOLUTE is a legacy statement that cannot be safely implemented
            let addr_code = emitter.emit_expr(address)?;
            writeln_code!(
                output,
                "{}fprintf(stderr, \"Warning: CALL ABSOLUTE at address %ld not supported in flat memory model\\n\", (long){});",
                indent,
                addr_code
            )?;
            writeln_code!(output, "{}fflush(stderr);", indent)?;
        }
        TypedStatementKind::MouseHide => {
            writeln_code!(output, "{}qb_mouse_hide();", indent)?;
        }

        TypedStatementKind::MouseShow => {
            writeln_code!(output, "{}qb_mouse_show();", indent)?;
        }

        TypedStatementKind::MouseMoveStmt { x, y } => {
            let x_code = emitter.emit_expr(x)?;
            let y_code = emitter.emit_expr(y)?;
            writeln_code!(
                output,
                "{}qb_mouse_move((int32_t){}, (int32_t){});",
                indent,
                x_code,
                y_code
            )?;
        }
        TypedStatementKind::ClipboardSet { text } => {
            let text_code = emitter.emit_expr(text)?;
            let text_access =
                emit_string_data_access(text, &text_code, &emitter.config.runtime_mode);
            writeln_code!(output, "{}qb_clipboard_set({});", indent, text_access)?;
        }
        TypedStatementKind::DeclareLibrary {
            library_name,
            is_dynamic,
            declarations,
        } => {
            // Static libraries: emit extern declarations here.
            // Dynamic libraries: handle/pointers and init are emitted in the
            // dynamic library section (qb_init_dynamic_libs); calls use qb_dyn_<c_name>.
            if *is_dynamic {
                writeln_code!(
                    output,
                    "{}// DECLARE DYNAMIC LIBRARY (loaded in qb_init_dynamic_libs)",
                    indent
                )?;
                if let Some(lib) = library_name {
                    writeln_code!(output, "{}// Library: {}", indent, lib)?;
                }
                // No extern declarations - we use function pointers (qb_dyn_<c_name>) emitted above.
            } else {
                writeln_code!(output, "{}// DECLARE LIBRARY - extern declarations", indent)?;
                if let Some(lib) = library_name {
                    writeln_code!(output, "{}// Library: {}", indent, lib)?;
                }
                for decl in declarations {
                    emitter.emit_extern_declaration(indent, decl, output)?;
                }
            }
        }

        // Forward declarations - no code generated, just comments for documentation
        TypedStatementKind::DeclareSub { name } => {
            writeln_code!(output, "{}/* DECLARE SUB {} */", indent, name)?;
        }

        TypedStatementKind::DeclareFunction { name } => {
            writeln_code!(output, "{}/* DECLARE FUNCTION {} */", indent, name)?;
        }
        _ => return Ok(()), // Not a system statement
    }
    Ok(())
}
