//! System integration statement type checking.
//!
//! This module handles type checking for system-related statements:
//! - Runtime: SYSTEM, SLEEP, WAIT, DELAY, LIMIT, ERASE, KEY CLEAR, RUN, CHAIN, TRON, TROFF
//! - File/shell: KILL, RENAME, MKDIR, RMDIR, CHDIR, ENVIRON, SHELL, SHELL HIDE
//! - Memory: BLOAD, BSAVE, SETMEM, CALL ABSOLUTE
//! - Mouse/clipboard: _MOUSEHIDE, _MOUSESHOW, _MOUSEMOVE, _CLIPBOARD
//! - C library: DECLARE LIBRARY, DECLARE SUB, DECLARE FUNCTION

use crate::ast::{ExternalDeclaration, Span, StatementKind};
use crate::semantic::{error::SemanticError, typed_ir::*};

use super::super::TypeChecker;

/// Type checks system integration statements.
pub(super) fn check_system_stmt(
    checker: &mut TypeChecker,
    kind: &StatementKind,
    span: Span,
) -> TypedStatement {
    match kind {
        StatementKind::System { exit_code } => {
            let typed_exit_code = exit_code.as_ref().map(|e| checker.check_expr(e));
            TypedStatement::new(
                TypedStatementKind::System {
                    exit_code: typed_exit_code,
                },
                span,
            )
        }
        StatementKind::Sleep { seconds } => {
            let typed_seconds = seconds.as_ref().map(|s| checker.check_expr(s));
            TypedStatement::new(
                TypedStatementKind::Sleep {
                    seconds: typed_seconds,
                },
                span,
            )
        }
        StatementKind::Wait {
            port,
            and_mask,
            xor_mask,
        } => {
            let typed_port = checker.check_expr(port);
            let typed_and = checker.check_expr(and_mask);
            let typed_xor = xor_mask.as_ref().map(|x| checker.check_expr(x));
            TypedStatement::new(
                TypedStatementKind::Wait {
                    port: typed_port,
                    and_mask: typed_and,
                    xor_mask: typed_xor,
                },
                span,
            )
        }
        StatementKind::Delay { seconds } => {
            let typed_seconds = checker.check_expr(seconds);
            TypedStatement::new(
                TypedStatementKind::Delay {
                    seconds: typed_seconds,
                },
                span,
            )
        }
        StatementKind::Limit { fps } => {
            let typed_fps = checker.check_expr(fps);
            TypedStatement::new(TypedStatementKind::Limit { fps: typed_fps }, span)
        }
        StatementKind::Erase { arrays } => TypedStatement::new(
            TypedStatementKind::Erase {
                arrays: arrays.clone(),
            },
            span,
        ),
        StatementKind::KeyClear => TypedStatement::new(TypedStatementKind::KeyClear, span),
        StatementKind::Run { target } => {
            let typed_target = target.as_ref().map(|t| checker.check_expr(t));
            TypedStatement::new(
                TypedStatementKind::Run {
                    target: typed_target,
                },
                span,
            )
        }
        StatementKind::Chain { filename } => {
            let typed_filename = checker.check_expr(filename);
            if !typed_filename.basic_type.is_string() {
                checker.errors.push(SemanticError::type_mismatch(
                    "STRING",
                    format!("{:?}", typed_filename.basic_type),
                    span,
                ));
            }
            TypedStatement::new(
                TypedStatementKind::Chain {
                    filename: typed_filename,
                },
                span,
            )
        }
        StatementKind::Tron => TypedStatement::new(TypedStatementKind::Tron, span),
        StatementKind::Troff => TypedStatement::new(TypedStatementKind::Troff, span),
        StatementKind::Kill { filename } => {
            let typed_filename = checker.check_expr(filename);
            TypedStatement::new(
                TypedStatementKind::Kill {
                    filename: typed_filename,
                },
                span,
            )
        }

        StatementKind::Rename { old_name, new_name } => {
            let typed_old = checker.check_expr(old_name);
            let typed_new = checker.check_expr(new_name);
            TypedStatement::new(
                TypedStatementKind::Rename {
                    old_name: typed_old,
                    new_name: typed_new,
                },
                span,
            )
        }

        StatementKind::Mkdir { path } => {
            let typed_path = checker.check_expr(path);
            TypedStatement::new(TypedStatementKind::Mkdir { path: typed_path }, span)
        }

        StatementKind::Rmdir { path } => {
            let typed_path = checker.check_expr(path);
            TypedStatement::new(TypedStatementKind::Rmdir { path: typed_path }, span)
        }

        StatementKind::Chdir { path } => {
            let typed_path = checker.check_expr(path);
            TypedStatement::new(TypedStatementKind::Chdir { path: typed_path }, span)
        }

        StatementKind::Environ { env_string } => {
            let typed_env_string = checker.check_expr(env_string);
            if !typed_env_string.basic_type.is_string() {
                checker.errors.push(SemanticError::type_mismatch(
                    "STRING",
                    format!("{:?}", typed_env_string.basic_type),
                    span,
                ));
            }
            TypedStatement::new(
                TypedStatementKind::Environ {
                    env_string: typed_env_string,
                },
                span,
            )
        }

        StatementKind::ShellCmd { command } => {
            let typed_command = command.as_ref().map(|c| checker.check_expr(c));
            TypedStatement::new(
                TypedStatementKind::ShellCmd {
                    command: typed_command,
                },
                span,
            )
        }

        StatementKind::ShellHide { command } => {
            let typed_command = checker.check_expr(command);
            TypedStatement::new(
                TypedStatementKind::ShellHide {
                    command: typed_command,
                },
                span,
            )
        }

        StatementKind::Bload { filename, address } => {
            let typed_filename = checker.check_expr(filename);
            let typed_address = address.as_ref().map(|a| checker.check_expr(a));
            TypedStatement::new(
                TypedStatementKind::Bload {
                    filename: typed_filename,
                    address: typed_address,
                },
                span,
            )
        }

        StatementKind::Bsave {
            filename,
            address,
            length,
        } => {
            let typed_filename = checker.check_expr(filename);
            let typed_address = checker.check_expr(address);
            let typed_length = checker.check_expr(length);
            TypedStatement::new(
                TypedStatementKind::Bsave {
                    filename: typed_filename,
                    address: typed_address,
                    length: typed_length,
                },
                span,
            )
        }

        StatementKind::Setmem { bytes } => {
            checker.errors.push(SemanticError::CommandNotImplemented {
                name: "SETMEM".to_string(),
                span,
            });
            let typed_bytes = checker.check_expr(bytes);
            TypedStatement::new(TypedStatementKind::Setmem { bytes: typed_bytes }, span)
        }

        StatementKind::CallAbsolute { args, address } => {
            let typed_args: Vec<_> = args.iter().map(|e| checker.check_expr(e)).collect();
            let typed_address = checker.check_expr(address);
            TypedStatement::new(
                TypedStatementKind::CallAbsolute {
                    args: typed_args,
                    address: typed_address,
                },
                span,
            )
        }

        StatementKind::MouseHide => TypedStatement::new(TypedStatementKind::MouseHide, span),

        StatementKind::MouseShow => TypedStatement::new(TypedStatementKind::MouseShow, span),

        StatementKind::MouseMoveStmt { x, y } => {
            let typed_x = checker.check_expr(x);
            let typed_y = checker.check_expr(y);
            TypedStatement::new(
                TypedStatementKind::MouseMoveStmt {
                    x: typed_x,
                    y: typed_y,
                },
                span,
            )
        }

        StatementKind::ClipboardSet { text } => {
            let typed_text = checker.check_expr(text);
            TypedStatement::new(
                TypedStatementKind::ClipboardSet { text: typed_text },
                span,
            )
        }

        StatementKind::DeclareLibrary {
            library_name,
            is_dynamic,
            declarations,
        } => {
            #[cfg(feature = "header-parsing")]
            let header_declarations: Vec<ExternalDeclaration> =
                if let Some(lib_name) = library_name.as_ref() {
                    if lib_name.ends_with(".h") {
                        checker.parse_header_file(lib_name, span)
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };

            #[cfg(not(feature = "header-parsing"))]
            let header_declarations: Vec<ExternalDeclaration> = Vec::new();

            let mut all_declarations: Vec<TypedExternalDeclaration> = header_declarations
                .iter()
                .map(|decl| checker.register_external_function(decl))
                .collect();

            for decl in declarations {
                all_declarations.push(checker.register_external_function(decl));
            }

            TypedStatement::new(
                TypedStatementKind::DeclareLibrary {
                    library_name: library_name.clone(),
                    is_dynamic: *is_dynamic,
                    declarations: all_declarations,
                },
                span,
            )
        }

        StatementKind::DeclareSub { name, params: _ } => TypedStatement::new(
            TypedStatementKind::DeclareSub { name: name.clone() },
            span,
        ),

        StatementKind::DeclareFunction {
            name,
            params: _,
            return_type: _,
        } => TypedStatement::new(
            TypedStatementKind::DeclareFunction { name: name.clone() },
            span,
        ),

        _ => unreachable!("check_system_stmt called with non-system kind"),
    }
}
