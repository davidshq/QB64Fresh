//! Statement type checking dispatcher.
//!
//! This module contains the main `check_statement` method that dispatches
//! to appropriate handlers based on statement type. Simple pass-through
//! statements are handled directly here, while complex statements delegate
//! to specialized modules.
//!
//! # Module Structure
//!
//! Statement type checking is split into focused submodules:
//! - [`assignments`] - Assignment statements (LET, array assignments, MID$, etc.)
//! - [`audio`] - Audio statements (BEEP, SOUND, PLAY, _SND*)
//! - [`control_flow`] - Control flow (IF, FOR, WHILE, DO, SELECT CASE, GOTO, etc.)
//! - [`data`] - DATA/READ/RESTORE statements
//! - [`definitions`] - Definition statements (DIM, CONST, SUB, FUNCTION, etc.)
//! - [`error_flow`] - Error handling and computed control flow (ON ERROR, ON...GOTO)
//! - [`graphics`] - Graphics statements (SCREEN, PSET, LINE, CIRCLE, etc.)
//! - [`io`] - File I/O statements (OPEN, CLOSE, PRINT #, GET, PUT)
//! - [`misc`] - Miscellaneous statements (SWAP, POKE, meta directives, etc.)

mod assignments;
mod audio;
mod control_flow;
mod data;
mod definitions;
mod error_flow;
mod graphics;
mod io;
mod misc;

use crate::ast::{
    ArrayDimension, ExternalDeclaration, PrintItem, Span, Statement, StatementKind, ViewCoords,
};
#[cfg(feature = "header-parsing")]
use crate::ast::{ExternalParam, TypeSpec};
use crate::semantic::{
    error::SemanticError,
    symbols::{ConstValue, Symbol, SymbolKind},
    typed_ir::*,
    types::{BasicType, from_type_spec},
};

use super::TypeChecker;

impl<'a> TypeChecker<'a> {
    // ========================================================================
    // Statement Type Checking
    // ========================================================================

    /// Type checks a statement.
    pub fn check_statement(&mut self, stmt: &Statement) -> TypedStatement {
        match &stmt.kind {
            // Assignment statements
            k @ (StatementKind::Let { .. }
            | StatementKind::ArrayAssignment { .. }
            | StatementKind::FieldAssignment { .. }
            | StatementKind::ArrayFieldAssignment { .. }
            | StatementKind::MidAssignment { .. }
            | StatementKind::AscAssignment { .. }) => {
                assignments::check_assignments_stmt(self, k, stmt.span)
            }

            StatementKind::Print { values, newline } => {
                self.check_print(values, *newline, stmt.span)
            }

            StatementKind::PrintUsing {
                format,
                values,
                newline,
            } => {
                let typed_format = self.check_expr(format);
                // Format string should be a string type (STRING or STRING * N)
                if !typed_format.basic_type.is_string() {
                    self.errors.push(SemanticError::type_mismatch(
                        "STRING",
                        format!("{:?}", typed_format.basic_type),
                        stmt.span,
                    ));
                }
                let typed_values: Vec<_> = values.iter().map(|v| self.check_expr(v)).collect();
                TypedStatement::new(
                    TypedStatementKind::PrintUsing {
                        format: typed_format,
                        values: typed_values,
                        newline: *newline,
                    },
                    stmt.span,
                )
            }

            StatementKind::Input {
                prompt,
                show_question_mark,
                same_line,
                targets,
            } => self.check_input(prompt, *show_question_mark, *same_line, targets, stmt.span),

            StatementKind::LineInput {
                suppress_newline: _,
                prompt,
                target,
            } => self.check_line_input(prompt, target, stmt.span),

            // Control flow statements
            k @ (StatementKind::If { .. }
            | StatementKind::SelectCase { .. }
            | StatementKind::SelectEveryCase { .. }
            | StatementKind::For { .. }
            | StatementKind::While { .. }
            | StatementKind::DoLoop { .. }
            | StatementKind::Goto { .. }
            | StatementKind::Gosub { .. }
            | StatementKind::Return
            | StatementKind::Exit { .. }
            | StatementKind::End { .. }
            | StatementKind::Stop
            | StatementKind::Continue { .. }) => {
                control_flow::check_control_flow_stmt(self, k, stmt.span)
            }

            StatementKind::System { exit_code } => {
                let typed_exit_code = exit_code.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::System {
                        exit_code: typed_exit_code,
                    },
                    stmt.span,
                )
            }

            StatementKind::Sleep { seconds } => {
                let typed_seconds = seconds.as_ref().map(|s| self.check_expr(s));
                TypedStatement::new(
                    TypedStatementKind::Sleep {
                        seconds: typed_seconds,
                    },
                    stmt.span,
                )
            }

            StatementKind::Wait {
                port,
                and_mask,
                xor_mask,
            } => {
                let typed_port = self.check_expr(port);
                let typed_and = self.check_expr(and_mask);
                let typed_xor = xor_mask.as_ref().map(|x| self.check_expr(x));
                TypedStatement::new(
                    TypedStatementKind::Wait {
                        port: typed_port,
                        and_mask: typed_and,
                        xor_mask: typed_xor,
                    },
                    stmt.span,
                )
            }

            StatementKind::Delay { seconds } => {
                let typed_seconds = self.check_expr(seconds);
                TypedStatement::new(
                    TypedStatementKind::Delay {
                        seconds: typed_seconds,
                    },
                    stmt.span,
                )
            }

            StatementKind::Limit { fps } => {
                let typed_fps = self.check_expr(fps);
                TypedStatement::new(TypedStatementKind::Limit { fps: typed_fps }, stmt.span)
            }

            StatementKind::Erase { arrays } => TypedStatement::new(
                TypedStatementKind::Erase {
                    arrays: arrays.clone(),
                },
                stmt.span,
            ),

            StatementKind::KeyClear => TypedStatement::new(TypedStatementKind::KeyClear, stmt.span),

            // Definition statements
            k @ (StatementKind::Call { .. }
            | StatementKind::Dim { .. }
            | StatementKind::Const { .. }
            | StatementKind::DefType { .. }
            | StatementKind::Define { .. }
            | StatementKind::OptionBase { .. }
            | StatementKind::OptionExplicit
            | StatementKind::OptionExplicitArray
            | StatementKind::Label { .. }
            | StatementKind::SubDefinition { .. }
            | StatementKind::FunctionDefinition { .. }) => {
                definitions::check_definitions_stmt(self, k, stmt.span)
            }

            // File I/O statements
            k @ (StatementKind::OpenFile { .. }
            | StatementKind::OpenFileLegacy { .. }
            | StatementKind::CloseFile { .. }
            | StatementKind::LockFile { .. }
            | StatementKind::UnlockFile { .. }
            | StatementKind::FilePrint { .. }
            | StatementKind::FileWrite { .. }
            | StatementKind::FileInput { .. }
            | StatementKind::FileLineInput { .. }
            | StatementKind::FileGet { .. }
            | StatementKind::FilePut { .. }
            | StatementKind::FileSeek { .. }) => io::check_io_stmt(self, k, stmt.span),

            // Data statements
            k @ (StatementKind::Data { .. }
            | StatementKind::Read { .. }
            | StatementKind::Restore { .. }
            | StatementKind::Randomize { .. }) => data::check_data_stmt(self, k, stmt.span),

            // Error handling and computed control flow
            k @ (StatementKind::OnErrorGoto { .. }
            | StatementKind::OnErrorResumeNext
            | StatementKind::ResumeStmt { .. }
            | StatementKind::ErrorStmt { .. }
            | StatementKind::OnGoto { .. }
            | StatementKind::OnGosub { .. }
            | StatementKind::DefFn { .. }
            | StatementKind::DefFnMultiLine { .. }
            | StatementKind::DefSeg { .. }) => {
                error_flow::check_error_flow_stmt(self, k, stmt.span)
            }

            // Expression, comment, preprocessor, meta, and misc statements
            k @ (StatementKind::Expression { .. }
            | StatementKind::Comment { .. }
            | StatementKind::IncludeDirective { .. }
            | StatementKind::ConditionalBlock { .. }
            | StatementKind::MetaCommand { .. }
            | StatementKind::MetaLet { .. }
            | StatementKind::MetaChecking { .. }
            | StatementKind::MetaConsole { .. }
            | StatementKind::MetaScreenHide
            | StatementKind::MetaScreenShow
            | StatementKind::Swap { .. }
            | StatementKind::TypeDefinition { .. }
            | StatementKind::Poke { .. }
            | StatementKind::MemPutTyped { .. }
            | StatementKind::CommonStmt { .. }
            | StatementKind::SharedStmt { .. }
            | StatementKind::StaticStmt { .. }
            | StatementKind::Redim { .. }) => misc::check_misc_stmt(self, k, stmt.span),

            // Graphics statements
            k @ (StatementKind::Screen { .. }
            | StatementKind::Cls { .. }
            | StatementKind::Color { .. }
            | StatementKind::Locate { .. }
            | StatementKind::Pset { .. }
            | StatementKind::Preset { .. }
            | StatementKind::Line { .. }
            | StatementKind::Circle { .. }
            | StatementKind::Paint { .. }
            | StatementKind::GfxDisplay
            | StatementKind::ControlChr { .. }
            | StatementKind::MapUnicode { .. }
            | StatementKind::GfxResize { .. }
            | StatementKind::Palette { .. }
            | StatementKind::Pcopy { .. }
            | StatementKind::Width { .. }
            | StatementKind::View { .. }
            | StatementKind::ViewPrint { .. }
            | StatementKind::WindowCoords { .. }
            | StatementKind::DrawCmd { .. }
            | StatementKind::GraphicsGet { .. }
            | StatementKind::GraphicsPut { .. }
            | StatementKind::FreeImage { .. }
            | StatementKind::PutImage { .. }
            | StatementKind::SourceImg { .. }
            | StatementKind::DestImg { .. }
            | StatementKind::PrintStringStmt { .. }
            | StatementKind::AutoDisplay { .. }) => {
                graphics::check_graphics_stmt(self, k, stmt.span)
            }

            // Audio statements
            k @ (StatementKind::Beep
            | StatementKind::SoundStmt { .. }
            | StatementKind::PlayStmt { .. }
            | StatementKind::SndClose { .. }
            | StatementKind::SndPlay { .. }
            | StatementKind::SndStop { .. }
            | StatementKind::SndPause { .. }
            | StatementKind::SndLoop { .. }
            | StatementKind::SndVol { .. }
            | StatementKind::SndBal { .. }
            | StatementKind::SndRaw { .. }
            | StatementKind::SndPlayFile { .. }
            | StatementKind::SndPlayCopy { .. }
            | StatementKind::SndSetPos { .. }) => audio::check_audio_stmt(self, k, stmt.span),

            // ==================== System Integration Statements ====================
            StatementKind::Kill { filename } => {
                let typed_filename = self.check_expr(filename);
                TypedStatement::new(
                    TypedStatementKind::Kill {
                        filename: typed_filename,
                    },
                    stmt.span,
                )
            }

            StatementKind::Rename { old_name, new_name } => {
                let typed_old = self.check_expr(old_name);
                let typed_new = self.check_expr(new_name);
                TypedStatement::new(
                    TypedStatementKind::Rename {
                        old_name: typed_old,
                        new_name: typed_new,
                    },
                    stmt.span,
                )
            }

            StatementKind::Mkdir { path } => {
                let typed_path = self.check_expr(path);
                TypedStatement::new(TypedStatementKind::Mkdir { path: typed_path }, stmt.span)
            }

            StatementKind::Rmdir { path } => {
                let typed_path = self.check_expr(path);
                TypedStatement::new(TypedStatementKind::Rmdir { path: typed_path }, stmt.span)
            }

            StatementKind::Chdir { path } => {
                let typed_path = self.check_expr(path);
                TypedStatement::new(TypedStatementKind::Chdir { path: typed_path }, stmt.span)
            }

            StatementKind::Environ { env_string } => {
                let typed_env_string = self.check_expr(env_string);
                // ENVIRON expects a string argument
                if !typed_env_string.basic_type.is_string() {
                    self.errors.push(SemanticError::type_mismatch(
                        "STRING",
                        format!("{:?}", typed_env_string.basic_type),
                        stmt.span,
                    ));
                }
                TypedStatement::new(
                    TypedStatementKind::Environ {
                        env_string: typed_env_string,
                    },
                    stmt.span,
                )
            }

            StatementKind::ShellCmd { command } => {
                let typed_command = command.as_ref().map(|c| self.check_expr(c));
                TypedStatement::new(
                    TypedStatementKind::ShellCmd {
                        command: typed_command,
                    },
                    stmt.span,
                )
            }

            StatementKind::ShellHide { command } => {
                let typed_command = self.check_expr(command);
                TypedStatement::new(
                    TypedStatementKind::ShellHide {
                        command: typed_command,
                    },
                    stmt.span,
                )
            }

            StatementKind::Bload { filename, address } => {
                let typed_filename = self.check_expr(filename);
                let typed_address = address.as_ref().map(|a| self.check_expr(a));
                TypedStatement::new(
                    TypedStatementKind::Bload {
                        filename: typed_filename,
                        address: typed_address,
                    },
                    stmt.span,
                )
            }

            StatementKind::Bsave {
                filename,
                address,
                length,
            } => {
                let typed_filename = self.check_expr(filename);
                let typed_address = self.check_expr(address);
                let typed_length = self.check_expr(length);
                TypedStatement::new(
                    TypedStatementKind::Bsave {
                        filename: typed_filename,
                        address: typed_address,
                        length: typed_length,
                    },
                    stmt.span,
                )
            }

            StatementKind::Setmem { bytes } => {
                // SETMEM is obsolete - throw compile error matching QB64pe behavior
                self.errors.push(SemanticError::CommandNotImplemented {
                    name: "SETMEM".to_string(),
                    span: stmt.span,
                });
                let typed_bytes = self.check_expr(bytes);
                TypedStatement::new(TypedStatementKind::Setmem { bytes: typed_bytes }, stmt.span)
            }

            StatementKind::CallAbsolute { args, address } => {
                let typed_args: Vec<_> = args.iter().map(|e| self.check_expr(e)).collect();
                let typed_address = self.check_expr(address);
                TypedStatement::new(
                    TypedStatementKind::CallAbsolute {
                        args: typed_args,
                        address: typed_address,
                    },
                    stmt.span,
                )
            }

            // ==================== Mouse Input Statements ====================
            StatementKind::MouseHide => {
                TypedStatement::new(TypedStatementKind::MouseHide, stmt.span)
            }

            StatementKind::MouseShow => {
                TypedStatement::new(TypedStatementKind::MouseShow, stmt.span)
            }

            StatementKind::MouseMoveStmt { x, y } => {
                let typed_x = self.check_expr(x);
                let typed_y = self.check_expr(y);
                TypedStatement::new(
                    TypedStatementKind::MouseMoveStmt {
                        x: typed_x,
                        y: typed_y,
                    },
                    stmt.span,
                )
            }

            // ==================== Clipboard Statement ====================
            StatementKind::ClipboardSet { text } => {
                let typed_text = self.check_expr(text);
                TypedStatement::new(
                    TypedStatementKind::ClipboardSet { text: typed_text },
                    stmt.span,
                )
            }

            // ==================== C Library Integration ====================
            StatementKind::DeclareLibrary {
                library_name,
                is_dynamic,
                declarations,
            } => {
                // Check if library_name is a header file (.h extension)
                // If so, parse the header to auto-generate function declarations
                #[cfg(feature = "header-parsing")]
                let header_declarations = if let Some(lib_name) = library_name.as_ref() {
                    if lib_name.ends_with(".h") {
                        self.parse_header_file(lib_name, stmt.span)
                    } else {
                        Vec::new()
                    }
                } else {
                    Vec::new()
                };

                #[cfg(not(feature = "header-parsing"))]
                let header_declarations: Vec<ExternalDeclaration> = Vec::new();

                // Merge header declarations with manual declarations
                // Manual declarations take precedence (appear later in the list)
                let mut all_declarations: Vec<TypedExternalDeclaration> = header_declarations
                    .iter()
                    .map(|decl| self.register_external_function(decl))
                    .collect();

                // Add manual declarations (these can override header-parsed ones)
                for decl in declarations {
                    all_declarations.push(self.register_external_function(decl));
                }

                TypedStatement::new(
                    TypedStatementKind::DeclareLibrary {
                        library_name: library_name.clone(),
                        is_dynamic: *is_dynamic,
                        declarations: all_declarations,
                    },
                    stmt.span,
                )
            }

            // Forward declarations - parsed for compatibility but don't generate code
            StatementKind::DeclareSub { name, params: _ } => TypedStatement::new(
                TypedStatementKind::DeclareSub { name: name.clone() },
                stmt.span,
            ),

            StatementKind::DeclareFunction {
                name,
                params: _,
                return_type: _,
            } => TypedStatement::new(
                TypedStatementKind::DeclareFunction { name: name.clone() },
                stmt.span,
            ),

            // ==================== Phase 7: Additional Statements ====================
            StatementKind::Run { target } => {
                let typed_target = target.as_ref().map(|t| self.check_expr(t));
                TypedStatement::new(
                    TypedStatementKind::Run {
                        target: typed_target,
                    },
                    stmt.span,
                )
            }

            StatementKind::Chain { filename } => {
                let typed_filename = self.check_expr(filename);
                // Filename should be a string (STRING or STRING * N)
                if !typed_filename.basic_type.is_string() {
                    self.errors.push(SemanticError::type_mismatch(
                        "STRING",
                        format!("{:?}", typed_filename.basic_type),
                        stmt.span,
                    ));
                }
                TypedStatement::new(
                    TypedStatementKind::Chain {
                        filename: typed_filename,
                    },
                    stmt.span,
                )
            }

            StatementKind::Tron => TypedStatement::new(TypedStatementKind::Tron, stmt.span),

            StatementKind::Troff => TypedStatement::new(TypedStatementKind::Troff, stmt.span),

            StatementKind::Lprint { values, newline } => {
                let typed_values = self.check_print_items(values);
                TypedStatement::new(
                    TypedStatementKind::Lprint {
                        values: typed_values,
                        newline: *newline,
                    },
                    stmt.span,
                )
            }

            StatementKind::FilesStmt { filespec } => {
                let typed_filespec = filespec.as_ref().map(|f| self.check_expr(f));
                TypedStatement::new(
                    TypedStatementKind::FilesStmt {
                        filespec: typed_filespec,
                    },
                    stmt.span,
                )
            }

            StatementKind::FieldStmt { file_num, fields } => {
                let typed_file_num = self.check_expr(file_num);
                let typed_fields: Vec<_> = fields
                    .iter()
                    .map(|f| TypedFieldSpec {
                        width: self.check_expr(&f.width),
                        variable: f.variable.clone(),
                    })
                    .collect();
                TypedStatement::new(
                    TypedStatementKind::FieldStmt {
                        file_num: typed_file_num,
                        fields: typed_fields,
                    },
                    stmt.span,
                )
            }

            StatementKind::Lset { variable, value } => {
                let typed_value = self.check_expr(value);
                // Resolve variable name through symbol lookup (handles suffix mismatch)
                let resolved_name = if let Some(symbol) = self.symbols.lookup_symbol(variable) {
                    symbol.name.clone()
                } else {
                    variable.clone()
                };
                TypedStatement::new(
                    TypedStatementKind::Lset {
                        variable: resolved_name,
                        value: typed_value,
                    },
                    stmt.span,
                )
            }

            StatementKind::Rset { variable, value } => {
                let typed_value = self.check_expr(value);
                // Resolve variable name through symbol lookup (handles suffix mismatch)
                let resolved_name = if let Some(symbol) = self.symbols.lookup_symbol(variable) {
                    symbol.name.clone()
                } else {
                    variable.clone()
                };
                TypedStatement::new(
                    TypedStatementKind::Rset {
                        variable: resolved_name,
                        value: typed_value,
                    },
                    stmt.span,
                )
            }

            StatementKind::OnKey { key_num, target } => {
                let typed_key_num = self.check_expr(key_num);
                TypedStatement::new(
                    TypedStatementKind::OnKey {
                        key_num: typed_key_num,
                        target: target.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::KeyControl { key_num, mode } => {
                let typed_key_num = self.check_expr(key_num);
                TypedStatement::new(
                    TypedStatementKind::KeyControl {
                        key_num: typed_key_num,
                        mode: *mode,
                    },
                    stmt.span,
                )
            }

            StatementKind::OnTimer { interval, target } => {
                let typed_interval = self.check_expr(interval);
                TypedStatement::new(
                    TypedStatementKind::OnTimer {
                        interval: typed_interval,
                        target: target.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::TimerControl { mode } => {
                TypedStatement::new(TypedStatementKind::TimerControl { mode: *mode }, stmt.span)
            }

            StatementKind::StrigControl { button_num, mode } => {
                let typed_button_num = self.check_expr(button_num);
                TypedStatement::new(
                    TypedStatementKind::StrigControl {
                        button_num: typed_button_num,
                        mode: *mode,
                    },
                    stmt.span,
                )
            }

            StatementKind::OnStrig { button_num, target } => {
                let typed_button_num = self.check_expr(button_num);
                TypedStatement::new(
                    TypedStatementKind::OnStrig {
                        button_num: typed_button_num,
                        target: target.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::OnCom { port_num, target } => {
                let typed_port_num = self.check_expr(port_num);
                TypedStatement::new(
                    TypedStatementKind::OnCom {
                        port_num: typed_port_num,
                        target: target.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::ComControl { port_num, mode } => {
                let typed_port_num = self.check_expr(port_num);
                TypedStatement::new(
                    TypedStatementKind::ComControl {
                        port_num: typed_port_num,
                        mode: *mode,
                    },
                    stmt.span,
                )
            }

            StatementKind::OnPen { target } => TypedStatement::new(
                TypedStatementKind::OnPen {
                    target: target.clone(),
                },
                stmt.span,
            ),

            StatementKind::PenControl { mode } => {
                TypedStatement::new(TypedStatementKind::PenControl { mode: *mode }, stmt.span)
            }

            StatementKind::OnUevent { target } => TypedStatement::new(
                TypedStatementKind::OnUevent {
                    target: target.clone(),
                },
                stmt.span,
            ),

            StatementKind::UeventControl { mode } => {
                TypedStatement::new(TypedStatementKind::UeventControl { mode: *mode }, stmt.span)
            }

            StatementKind::UeventTrigger => {
                TypedStatement::new(TypedStatementKind::UeventTrigger, stmt.span)
            }

            StatementKind::OnSignal { signal_num, target } => {
                let typed_signal_num = self.check_expr(signal_num);
                TypedStatement::new(
                    TypedStatementKind::OnSignal {
                        signal_num: typed_signal_num,
                        target: target.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::SignalControl { signal_num, mode } => {
                let typed_signal_num = self.check_expr(signal_num);
                TypedStatement::new(
                    TypedStatementKind::SignalControl {
                        signal_num: typed_signal_num,
                        mode: *mode,
                    },
                    stmt.span,
                )
            }

            StatementKind::OutPort { port, value } => {
                let typed_port = self.check_expr(port);
                let typed_value = self.check_expr(value);
                TypedStatement::new(
                    TypedStatementKind::OutPort {
                        port: typed_port,
                        value: typed_value,
                    },
                    stmt.span,
                )
            }

            StatementKind::InterruptStmt {
                int_num,
                in_regs,
                out_regs,
            } => {
                let typed_int_num = self.check_expr(int_num);
                TypedStatement::new(
                    TypedStatementKind::InterruptStmt {
                        int_num: typed_int_num,
                        in_regs: in_regs.clone(),
                        out_regs: out_regs.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::InterruptXStmt {
                int_num,
                in_regs,
                out_regs,
            } => {
                let typed_int_num = self.check_expr(int_num);
                TypedStatement::new(
                    TypedStatementKind::InterruptXStmt {
                        int_num: typed_int_num,
                        in_regs: in_regs.clone(),
                        out_regs: out_regs.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::IoctlStmt {
                file_num,
                control_string,
            } => {
                // IOCTL is a stub (legacy DOS device control)
                // QB64pe also stubs this, so we just compile it to a no-op
                let typed_file_num = self.check_expr(file_num);
                let typed_control_string = self.check_expr(control_string);
                TypedStatement::new(
                    TypedStatementKind::IoctlStmt {
                        file_num: typed_file_num,
                        control_string: typed_control_string,
                    },
                    stmt.span,
                )
            }

            StatementKind::FreeStmt => TypedStatement::new(TypedStatementKind::FreeStmt, stmt.span),

            StatementKind::ClearStmt { stack_size } => {
                let typed_stack_size = stack_size.as_ref().map(|s| self.check_expr(s));
                TypedStatement::new(
                    TypedStatementKind::ClearStmt {
                        stack_size: typed_stack_size,
                    },
                    stmt.span,
                )
            }

            StatementKind::ResetStmt => {
                TypedStatement::new(TypedStatementKind::ResetStmt, stmt.span)
            }

            // Window/Desktop statements (QB64)
            StatementKind::TitleStmt { title } => {
                let typed_title = self.check_expr(title);
                TypedStatement::new(
                    TypedStatementKind::TitleStmt { title: typed_title },
                    stmt.span,
                )
            }

            StatementKind::ScreenMoveStmt { x, y, center } => {
                let typed_x = x.as_ref().map(|e| self.check_expr(e));
                let typed_y = y.as_ref().map(|e| self.check_expr(e));
                TypedStatement::new(
                    TypedStatementKind::ScreenMoveStmt {
                        x: typed_x,
                        y: typed_y,
                        center: *center,
                    },
                    stmt.span,
                )
            }

            StatementKind::FullScreenStmt { mode } => TypedStatement::new(
                TypedStatementKind::FullScreenStmt { mode: *mode },
                stmt.span,
            ),

            StatementKind::AllowFullScreenStmt { mode } => TypedStatement::new(
                TypedStatementKind::AllowFullScreenStmt { mode: *mode },
                stmt.span,
            ),

            StatementKind::ScreenIconStmt => {
                TypedStatement::new(TypedStatementKind::ScreenIconStmt, stmt.span)
            }

            StatementKind::IconStmt { handle } => {
                let typed_handle = handle.as_ref().map(|h| self.check_expr(h));
                TypedStatement::new(
                    TypedStatementKind::IconStmt {
                        handle: typed_handle,
                    },
                    stmt.span,
                )
            }

            StatementKind::ScreenHideStmt => {
                TypedStatement::new(TypedStatementKind::ScreenHideStmt, stmt.span)
            }

            StatementKind::ScreenShowStmt => {
                TypedStatement::new(TypedStatementKind::ScreenShowStmt, stmt.span)
            }

            StatementKind::ConsoleTitleStmt { title } => {
                let typed_title = self.check_expr(title);
                TypedStatement::new(
                    TypedStatementKind::ConsoleTitleStmt { title: typed_title },
                    stmt.span,
                )
            }

            StatementKind::ConsoleStmt { visible } => TypedStatement::new(
                TypedStatementKind::ConsoleStmt { visible: *visible },
                stmt.span,
            ),

            StatementKind::AssertStmt { condition, message } => {
                let typed_condition = self.check_expr(condition);
                let typed_message = message.as_ref().map(|m| self.check_expr(m));
                TypedStatement::new(
                    TypedStatementKind::AssertStmt {
                        condition: typed_condition,
                        message: typed_message,
                    },
                    stmt.span,
                )
            }

            StatementKind::MetaAsserts { console } => {
                // Set preprocessor variables: _ASSERTS_ = 1, and _CONSOLE_ = 1 if console mode
                self.symbols.define_meta_let("_ASSERTS_", 1, stmt.span);
                if *console {
                    self.symbols.define_meta_let("_CONSOLE_", 1, stmt.span);
                }
                TypedStatement::new(
                    TypedStatementKind::MetaAsserts { console: *console },
                    stmt.span,
                )
            }

            StatementKind::MetaNoPrefix => {
                TypedStatement::new(TypedStatementKind::MetaNoPrefix, stmt.span)
            }

            StatementKind::MetaColor { depth } => {
                TypedStatement::new(TypedStatementKind::MetaColor { depth: *depth }, stmt.span)
            }

            StatementKind::MetaResize { enabled } => TypedStatement::new(
                TypedStatementKind::MetaResize { enabled: *enabled },
                stmt.span,
            ),

            StatementKind::MetaResizeStretch => {
                TypedStatement::new(TypedStatementKind::MetaResizeStretch, stmt.span)
            }

            StatementKind::MetaResizeSmooth => {
                TypedStatement::new(TypedStatementKind::MetaResizeSmooth, stmt.span)
            }

            StatementKind::MetaStatic => {
                // Set array mode to static - affects all arrays declared after this directive
                self.array_mode_static = true;
                TypedStatement::new(TypedStatementKind::MetaStatic, stmt.span)
            }

            StatementKind::MetaDynamic => {
                // Set array mode to dynamic - affects all arrays declared after this directive
                self.array_mode_static = false;
                TypedStatement::new(TypedStatementKind::MetaDynamic, stmt.span)
            }

            StatementKind::MetaDebug => {
                TypedStatement::new(TypedStatementKind::MetaDebug, stmt.span)
            }

            StatementKind::MetaIncludeOnce => {
                TypedStatement::new(TypedStatementKind::MetaIncludeOnce, stmt.span)
            }

            StatementKind::MetaExeIcon { filename } => TypedStatement::new(
                TypedStatementKind::MetaExeIcon {
                    filename: filename.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaVersionInfo { key, value } => TypedStatement::new(
                TypedStatementKind::MetaVersionInfo {
                    key: key.clone(),
                    value: value.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaErrorDirective { message } => {
                // $ERROR halts compilation: push error so analyzer returns Err and driver exits.
                self.errors.push(SemanticError::CompileTimeError {
                    message: message.clone(),
                    span: stmt.span,
                });
                TypedStatement::new(
                    TypedStatementKind::MetaErrorDirective {
                        message: message.clone(),
                    },
                    stmt.span,
                )
            }

            StatementKind::MetaEmbed { filename } => TypedStatement::new(
                TypedStatementKind::MetaEmbed {
                    filename: filename.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaMidiSoundFont { filename } => TypedStatement::new(
                TypedStatementKind::MetaMidiSoundFont {
                    filename: filename.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaUnstable { feature } => TypedStatement::new(
                TypedStatementKind::MetaUnstable {
                    feature: feature.clone(),
                },
                stmt.span,
            ),

            StatementKind::MetaFormat => {
                TypedStatement::new(TypedStatementKind::MetaFormat, stmt.span)
            }

            StatementKind::MetaUseLibrary { library } => TypedStatement::new(
                TypedStatementKind::MetaUseLibrary {
                    library: library.clone(),
                },
                stmt.span,
            ),
        }
    }

    /// Registers an external function/sub in the symbol table.
    fn register_external_function(
        &mut self,
        decl: &ExternalDeclaration,
    ) -> TypedExternalDeclaration {
        // Determine the return type for functions
        let return_type = if decl.is_function {
            decl.return_type
                .as_ref()
                .map(from_type_spec)
                .unwrap_or(BasicType::Single) // Default return type
        } else {
            BasicType::Void
        };

        // Convert parameters to typed form
        let typed_params: Vec<TypedExternalParam> = decl
            .params
            .iter()
            .map(|p| TypedExternalParam {
                name: p.name.clone(),
                typ: from_type_spec(&p.type_spec),
                is_byval: p.is_byval,
            })
            .collect();

        // Build parameter type list for the symbol
        let param_types: Vec<BasicType> = typed_params.iter().map(|p| p.typ.clone()).collect();

        // Strip type suffix from function name for symbol lookup
        // e.g., "myabs&" -> "myabs" so calls can omit the suffix
        use crate::semantic::types::strip_suffix;
        let base_name = strip_suffix(&decl.name).to_string();

        // For C name, use ALIAS if provided, otherwise strip BASIC suffix
        // (C doesn't have type suffixes like & % $ in identifiers)
        let c_name = decl.alias.clone().unwrap_or_else(|| base_name.clone());

        // Register the function in the symbol table
        // External functions can be redeclared, but only if they have the same signature
        let new_symbol = Symbol {
            name: base_name.clone(),
            kind: SymbolKind::ExternalFunction {
                c_name: c_name.clone(),
                params: param_types.clone(),
                return_type: return_type.clone(),
            },
            basic_type: return_type.clone(),
            span: Span::new(0, 0, 1), // External functions don't have source location
            is_mutable: false,
        };

        match self.symbols.define_symbol(new_symbol) {
            Ok(_) => {
                // Successfully defined
            }
            Err(duplicate) => {
                // Check if the duplicate is also an external function with the same signature
                let (existing, new) = *duplicate;
                // We know `new.kind` is ExternalFunction because we just created it
                match &existing.kind {
                    SymbolKind::ExternalFunction {
                        c_name: existing_c_name,
                        params: existing_params,
                        return_type: existing_return,
                    } => {
                        // Both are external functions - check if signatures match
                        // Extract new function details (we know it's ExternalFunction)
                        if let SymbolKind::ExternalFunction {
                            c_name: new_c_name,
                            params: new_params,
                            return_type: new_return,
                        } = &new.kind
                        {
                            if existing_c_name == new_c_name
                                && existing_params == new_params
                                && existing_return == new_return
                            {
                                // Same signature - allow redeclaration (ignore the error)
                            } else {
                                // Different signature - report duplicate variable error
                                self.errors.push(SemanticError::DuplicateVariable {
                                    name: base_name.clone(),
                                    original_span: existing.span,
                                    duplicate_span: new.span,
                                });
                            }
                        }
                    }
                    _ => {
                        // Existing symbol is not an external function - report error
                        self.errors.push(SemanticError::DuplicateVariable {
                            name: base_name.clone(),
                            original_span: existing.span,
                            duplicate_span: new.span,
                        });
                    }
                }
            }
        }

        TypedExternalDeclaration {
            name: base_name,
            c_name,
            params: typed_params,
            return_type,
            is_function: decl.is_function,
        }
    }

    /// Type checks ViewCoords for VIEW/WINDOW statements.
    fn check_view_coords(&mut self, coords: &ViewCoords) -> TypedViewCoords {
        TypedViewCoords {
            x1: self.check_expr(&coords.x1),
            y1: self.check_expr(&coords.y1),
            x2: self.check_expr(&coords.x2),
            y2: self.check_expr(&coords.y2),
        }
    }

    /// Type checks print items (shared between PRINT and PRINT #).
    fn check_print_items(&mut self, values: &[PrintItem]) -> Vec<TypedPrintItem> {
        values
            .iter()
            .map(|item| TypedPrintItem {
                expr: self.check_expr(&item.expr),
                separator: item.separator,
            })
            .collect()
    }

    /// Evaluates array dimensions for STATIC arrays.
    /// While STATIC arrays ideally have constant bounds, we allow variable bounds
    /// for compatibility with QB code that uses variables in DIM.
    fn evaluate_array_dimension(
        &mut self,
        dim: &ArrayDimension,
        _span: crate::ast::Span,
    ) -> TypedArrayDimension {
        // Evaluate lower bound (if provided)
        let lower = if let Some(lower_expr) = &dim.lower {
            let typed_lower = self.check_expr(lower_expr);
            // Ensure the expression is numeric
            if !typed_lower.basic_type.is_numeric() && typed_lower.basic_type != BasicType::Unknown
            {
                self.errors.push(SemanticError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: typed_lower.basic_type.to_string(),
                    span: lower_expr.span,
                });
            }
            // Try to evaluate as constant, but don't error if not possible
            match self.try_evaluate_const_expr(&typed_lower) {
                Some(ConstValue::Integer(v)) => v,
                Some(ConstValue::Float(v)) => v as i64,
                _ => 0, // Runtime bound - use 0 as placeholder
            }
        } else {
            self.symbols.option_base()
        };

        // Evaluate upper bound (required)
        let typed_upper = self.check_expr(&dim.upper);
        // Ensure the expression is numeric
        if !typed_upper.basic_type.is_numeric() && typed_upper.basic_type != BasicType::Unknown {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_upper.basic_type.to_string(),
                span: dim.upper.span,
            });
        }
        // Try to evaluate as constant, but don't error if not possible
        let upper = match self.try_evaluate_const_expr(&typed_upper) {
            Some(ConstValue::Integer(v)) => v,
            Some(ConstValue::Float(v)) => v as i64,
            _ => 10, // Runtime bound - use 10 as placeholder
        };

        TypedArrayDimension { lower, upper }
    }

    /// Evaluates array dimensions for REDIM where runtime expressions are allowed.
    /// Since we can't know the values at compile time, we use placeholder values.
    fn evaluate_array_dimension_runtime(
        &mut self,
        dim: &ArrayDimension,
    ) -> crate::semantic::typed_ir::TypedRedimDimension {
        use crate::semantic::typed_ir::TypedRedimDimension;

        // For REDIM, we type-check the expressions and store them for codegen
        let typed_lower = if let Some(lower_expr) = &dim.lower {
            let typed = self.check_expr(lower_expr);
            if !typed.basic_type.is_numeric() {
                self.errors.push(SemanticError::TypeMismatch {
                    expected: "numeric".to_string(),
                    found: typed.basic_type.to_string(),
                    span: typed.span,
                });
            }
            Some(typed)
        } else {
            None
        };

        let typed_upper = self.check_expr(&dim.upper);
        if !typed_upper.basic_type.is_numeric() {
            self.errors.push(SemanticError::TypeMismatch {
                expected: "numeric".to_string(),
                found: typed_upper.basic_type.to_string(),
                span: typed_upper.span,
            });
        }

        TypedRedimDimension {
            lower: typed_lower,
            upper: typed_upper,
        }
    }

    // ========================================================================
    // Header File Parsing for DECLARE LIBRARY "file.h"
    // ========================================================================

    /// Parses a C header file and converts function declarations to ExternalDeclaration.
    ///
    /// This enables `DECLARE LIBRARY "header.h"` to automatically import function
    /// signatures from the header, reducing the need for manual declarations.
    ///
    /// Also processes constants (#define) and structs (struct/typedef struct) from the
    /// header file, adding them to the symbol table as CONST and TYPE definitions.
    ///
    /// # Arguments
    /// - `header_path`: Path to the header file (relative to CWD or absolute)
    /// - `span`: Source span for error reporting and symbol definitions
    ///
    /// # Returns
    /// Vector of ExternalDeclaration parsed from the header file.
    /// Returns empty vector if the file cannot be read or parsed.
    #[cfg(feature = "header-parsing")]
    fn parse_header_file(&mut self, header_path: &str, span: Span) -> Vec<ExternalDeclaration> {
        use crate::header_parser::{Platform, parse_header_full};
        use std::fs;

        // Try to read the header file
        let header_content = match fs::read_to_string(header_path) {
            Ok(content) => content,
            Err(_) => {
                // Header parsing is optional - silently return empty if file not found.
                // User can still manually declare functions inside the DECLARE LIBRARY block.
                return Vec::new();
            }
        };

        // Parse the header file
        let result = parse_header_full(&header_content, Some(Platform::current()));

        // Convert CFunction entries to ExternalDeclaration
        let mut declarations = Vec::with_capacity(result.functions.len());

        for func in result.functions {
            declarations.push(self.c_function_to_external_decl(&func));
        }

        // Process constants from C header (#define directives)
        for constant in &result.constants {
            self.process_c_constant(constant, span);
        }

        // Process structs from C header (struct/typedef struct definitions)
        for struct_def in &result.structs {
            self.process_c_struct(struct_def, span);
        }

        declarations
    }

    /// Converts a CFunction from the header parser to an ExternalDeclaration.
    #[cfg(feature = "header-parsing")]
    fn c_function_to_external_decl(
        &self,
        func: &crate::header_parser::CFunction,
    ) -> ExternalDeclaration {
        // Convert parameters
        let params: Vec<ExternalParam> = func
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let name = if p.name.is_empty() {
                    format!("arg{}", i + 1)
                } else {
                    p.name.clone()
                };
                ExternalParam {
                    name,
                    type_spec: basic_type_to_type_spec(&p.typ),
                    is_byval: true, // C passes by value by default
                }
            })
            .collect();

        // Determine if it's a function (has return value) or sub (void)
        let is_function = func.return_type != BasicType::Void;
        let return_type = if is_function {
            Some(basic_type_to_type_spec(&func.return_type))
        } else {
            None
        };

        ExternalDeclaration {
            name: func.name.clone(),
            alias: None, // C name matches BASIC name
            params,
            return_type,
            is_function,
        }
    }

    /// Processes a C constant from a header file and adds it to the symbol table.
    ///
    /// Converts C `#define` constants (e.g., `#define MAX_PATH 260`) into BASIC
    /// CONST definitions that can be used in the program.
    ///
    /// # Arguments
    /// - `constant`: The C constant from the header parser
    /// - `span`: Source span for the symbol definition
    #[cfg(feature = "header-parsing")]
    fn process_c_constant(&mut self, constant: &crate::header_parser::CConstant, span: Span) {
        use crate::header_parser::ConstantValue as CConstantValue;
        use crate::semantic::symbols::{ConstValue, Symbol, SymbolKind};

        // Skip constants with empty names (shouldn't happen, but be defensive)
        if constant.name.is_empty() {
            return;
        }

        // Convert C constant value to BASIC ConstValue and determine type
        let (const_value, basic_type) = match &constant.value {
            CConstantValue::Integer(n) => {
                let cv = ConstValue::Integer(*n);
                // Choose smallest type that fits
                let bt = if *n >= i16::MIN as i64 && *n <= i16::MAX as i64 {
                    BasicType::Integer
                } else if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 {
                    BasicType::Long
                } else {
                    BasicType::Integer64
                };
                (cv, bt)
            }
            CConstantValue::Float(f) => (ConstValue::Float(*f), BasicType::Double),
            CConstantValue::String(s) => (ConstValue::String(s.clone()), BasicType::String),
            CConstantValue::Expression(_) => {
                // For expressions we can't evaluate, skip them
                // (they would require a full C expression evaluator)
                return;
            }
        };

        // Create the symbol
        let symbol = Symbol {
            name: constant.name.clone(),
            kind: SymbolKind::Constant { value: const_value },
            basic_type,
            span,
            is_mutable: false,
        };

        // Add to symbol table
        // Allow duplicates for constants: user can override with manual CONST,
        // or constants may be defined in multiple headers
        //
        // Note: `define_symbol` only returns errors for duplicate symbols (it returns
        // `Box<(Symbol, Symbol)>` containing the existing and new symbols).
        // There are no other error types, so it's safe to handle all errors as duplicates.
        match self.symbols.define_symbol(symbol.clone()) {
            Ok(_) => {
                // Successfully defined
            }
            Err(duplicate) => {
                // `define_symbol` only fails for duplicate symbols, so this is always a duplicate
                let (existing, new) = *duplicate;
                // Check if the existing symbol is also a constant
                match &existing.kind {
                    SymbolKind::Constant {
                        value: existing_val,
                    } => {
                        // Both are constants - check if values match
                        // We know `new.kind` is Constant because we just created it
                        if let SymbolKind::Constant { value: new_val } = &new.kind {
                            if existing_val == new_val {
                                // Same constant value - allow redeclaration (ignore)
                            } else {
                                // Different values - user override or header conflict
                                // Allow it (user/manual definition takes precedence)
                            }
                        }
                    }
                    _ => {
                        // Existing symbol is not a constant - this is a conflict
                        // Report duplicate variable error
                        self.errors.push(SemanticError::DuplicateVariable {
                            name: constant.name.clone(),
                            original_span: existing.span,
                            duplicate_span: new.span,
                        });
                    }
                }
            }
        }
    }

    /// Processes a C struct from a header file and adds it to the symbol table.
    ///
    /// Converts C struct definitions (e.g., `struct Point { int x; int y; }`) into
    /// BASIC TYPE definitions that can be used in the program.
    ///
    /// # Arguments
    /// - `struct_def`: The C struct from the header parser
    /// - `span`: Source span for the type definition
    #[cfg(feature = "header-parsing")]
    fn process_c_struct(&mut self, struct_def: &crate::header_parser::CStruct, span: Span) {
        use crate::semantic::symbols::{UserTypeDefinition, UserTypeMember};

        // Skip structs with empty names (shouldn't happen, but be defensive)
        if struct_def.name.is_empty() {
            return;
        }

        // Convert C struct members to BASIC type members (C structs have no array field syntax here)
        let members: Vec<UserTypeMember> = struct_def
            .members
            .iter()
            .map(|m| UserTypeMember {
                name: m.name.clone(),
                dimensions: vec![],
                basic_type: m.typ.clone(),
            })
            .collect();

        // Create the user type definition
        let user_type = UserTypeDefinition {
            name: struct_def.name.clone(),
            members,
            span,
            custom_type: true, // C structs are C-compatible by definition
        };

        // Add to symbol table
        // Allow duplicates for types: user can override with manual TYPE,
        // or types may be defined in multiple headers
        //
        // Note: `define_user_type` only returns errors for duplicate types (it returns
        // the existing `UserTypeDefinition`). There are no other error types, so it's
        // safe to handle all errors as duplicates.
        match self.symbols.define_user_type(user_type.clone()) {
            Ok(_) => {
                // Successfully defined
            }
            Err(existing) => {
                // `define_user_type` only fails for duplicate types, so this is always a duplicate
                // Check if it's the same definition
                if existing.members == user_type.members
                    && existing.custom_type == user_type.custom_type
                {
                    // Same type definition - allow redeclaration (ignore)
                } else {
                    // Different type definition - this is a conflict
                    // Report duplicate type error
                    self.errors.push(SemanticError::DuplicateType {
                        name: struct_def.name.clone(),
                        original_span: existing.span,
                        duplicate_span: user_type.span,
                    });
                }
            }
        }
    }
}

/// Converts a BasicType back to a TypeSpec for AST construction.
///
/// This is the inverse of `from_type_spec` and is used when creating
/// ExternalDeclaration nodes from parsed C headers.
#[cfg(feature = "header-parsing")]
fn basic_type_to_type_spec(typ: &BasicType) -> TypeSpec {
    match typ {
        BasicType::Integer => TypeSpec::Integer,
        BasicType::Long => TypeSpec::Long,
        BasicType::Integer64 => TypeSpec::Integer64,
        BasicType::Single => TypeSpec::Single,
        BasicType::Double => TypeSpec::Double,
        BasicType::String => TypeSpec::String,
        BasicType::FixedString(n) => TypeSpec::FixedString(*n),
        BasicType::Byte => TypeSpec::Byte,
        BasicType::Bit => TypeSpec::Bit,
        BasicType::UnsignedByte => TypeSpec::Unsigned(Box::new(TypeSpec::Byte)),
        BasicType::UnsignedBit => TypeSpec::Unsigned(Box::new(TypeSpec::Bit)),
        BasicType::UnsignedInteger => TypeSpec::Unsigned(Box::new(TypeSpec::Integer)),
        BasicType::UnsignedLong => TypeSpec::Unsigned(Box::new(TypeSpec::Long)),
        BasicType::UnsignedInteger64 => TypeSpec::Unsigned(Box::new(TypeSpec::Integer64)),
        BasicType::Offset => TypeSpec::Offset,
        BasicType::Mem => TypeSpec::Mem,
        BasicType::Float => TypeSpec::Float,
        BasicType::Void => TypeSpec::Long, // Void shouldn't appear, default to Long
        BasicType::UserDefined(name) => TypeSpec::UserDefined(name.clone()),
        BasicType::Array { .. } => TypeSpec::Long, // Arrays not directly representable
        BasicType::Unknown => TypeSpec::Long,      // Default fallback
    }
}
