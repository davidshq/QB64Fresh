//! Statement code generation for QB64Fresh C backend.
//!
//! This module handles the emission of C code for all statement types,
//! including control flow, I/O operations, procedure definitions, and more.
//!
//! # Module Organization
//!
//! The statement emitter is split across multiple files for maintainability:
//!
//! - `mod.rs` (this file) - Thin `emit_stmt()` dispatcher and evnt wrapper
//! - `state.rs` - [`StmtEmitter`] struct and context types (labels, procedure, data, etc.)
//! - `assignments.rs` - Assignment statement helpers
//! - `audio.rs` - Audio statement code generation (BEEP, SOUND, PLAY, _SND*)
//! - `call.rs` - SUB (CALL) procedure invocation and built-in name mapping
//! - `control_flow.rs` - IF, FOR, WHILE, DO, SELECT CASE
//! - `data.rs` - DATA/READ/RESTORE/RANDOMIZE handling
//! - `def_fn.rs` - DEF FN single-line and multi-line functions
//! - `definitions.rs` - DIM, REDIM, SUB/FUNCTION definitions, CONST, STATIC, DECLARE LIBRARY
//! - `error_jump.rs` - Error handling (ON ERROR) and computed jumps (ON...GOTO/GOSUB)
//! - `graphics.rs` - Graphics statement code generation (SCREEN, PSET, LINE, CIRCLE, etc.)
//! - `io.rs` - PRINT and INPUT helpers
//! - `meta.rs` - Meta directive code generation ($IF, $LET, $CHECKING, etc.)
//! - `misc.rs` - Miscellaneous (CALL, Label, Comment, DefSeg, Poke, RUN, events, window, etc.)
//! - `system.rs` - System integration (KILL, RENAME, SHELL, BLOAD/BSAVE, etc.)
//!
//! # Loop Handling
//!
//! Loops are tracked on a stack to support EXIT statements. Each loop
//! type (FOR, WHILE, DO) generates a break label that EXIT can target.

mod assignments;
mod audio;
mod call;
mod control_flow;
mod data;
mod def_fn;
mod definitions;
mod error_jump;
mod graphics;
mod io;
mod meta;
mod misc;
mod state;
mod system;

// Re-export standalone functions for use by parent module
pub(in crate::codegen::c_backend) use definitions::{emit_dynamic_library_section, emit_params};

// Re-export emitter and context types so submodules and parent can use them (used for type resolution in c_backend).
#[allow(unused_imports)]
pub(super) use state::{
    CodeGenState, Config, DataContext, DebugContext, EventContext, GlobalSymbols,
    LoopContext, ProcedureContext, StmtEmitter,
};

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::{TypedStatement, TypedStatementKind};
use crate::writeln_code;

/// Drop guard: decrements `CodeGenState::indent` when dropped.
/// Used so evnt wrapper indent is restored even when statement emission returns `Err`.
struct EvntIndentGuard(*mut usize);

impl Drop for EvntIndentGuard {
    fn drop(&mut self) {
        if !self.0.is_null() {
            unsafe {
                *self.0 = (*self.0).saturating_sub(1);
            }
        }
    }
}

impl StmtEmitter {
    /// Emits a statement.
    ///
    /// When debug is enabled, executable statements are wrapped in
    /// `do { ...; if (!qbevent) break; qb_evnt(line, incline, file); } while(0);`
    /// so the IDE/debugger can intercept at statement boundaries (QB64pe-style evnt).
    pub fn emit_stmt(
        &mut self,
        stmt: &TypedStatement,
        output: &mut String,
    ) -> Result<(), CodeGenError> {
        let indent = self.indent_str();
        let is_exec = Self::is_executable_statement(&stmt.kind);
        let evnt_opened = self.debug.enabled && is_exec;

        // Optional evnt wrapper: do { ... } while(0). Guard restores indent on error path.
        let evnt_indent_ptr = if evnt_opened {
            writeln_code!(output, "{}do {{", indent)?;
            self.codegen.indent += 1;
            &mut self.codegen.indent as *mut usize
        } else {
            std::ptr::null_mut()
        };
        let mut _evnt_guard = if evnt_opened {
            Some(EvntIndentGuard(evnt_indent_ptr))
        } else {
            None
        };

        // Emit debug line hook for executable statements
        if self.debug.enabled && is_exec {
            self.emit_debug_line(stmt, output)?;
        }

        match &stmt.kind {
            k @ (TypedStatementKind::Assignment { .. }
            | TypedStatementKind::ArrayAssignment { .. }
            | TypedStatementKind::ArrayFieldAssignment { .. }
            | TypedStatementKind::FieldAssignment { .. }
            | TypedStatementKind::MidAssignment { .. }
            | TypedStatementKind::AscAssignment { .. }) => {
                assignments::emit_assignments_stmt(self, k, &indent, output)?;
            }

            k @ (TypedStatementKind::Print { .. }
            | TypedStatementKind::PrintUsing { .. }
            | TypedStatementKind::Input { .. }
            | TypedStatementKind::LineInput { .. }) => {
                io::emit_console_io_stmt(self, k, &indent, output)?;
            }

            k @ (TypedStatementKind::If { .. }
            | TypedStatementKind::SelectCase { .. }
            | TypedStatementKind::SelectEveryCase { .. }
            | TypedStatementKind::For { .. }
            | TypedStatementKind::While { .. }
            | TypedStatementKind::DoLoop { .. }
            | TypedStatementKind::Goto { .. }
            | TypedStatementKind::Gosub { .. }
            | TypedStatementKind::Return
            | TypedStatementKind::Exit { .. }
            | TypedStatementKind::End { .. }
            | TypedStatementKind::Stop
            | TypedStatementKind::System { .. }
            | TypedStatementKind::Sleep { .. }
            | TypedStatementKind::Wait { .. }
            | TypedStatementKind::Delay { .. }
            | TypedStatementKind::Limit { .. }
            | TypedStatementKind::Erase { .. }
            | TypedStatementKind::KeyClear) => {
                control_flow::emit_control_flow_stmt(self, k, &indent, output)?;
            }

            k @ (TypedStatementKind::SubDefinition { .. }
            | TypedStatementKind::FunctionDefinition { .. }
            | TypedStatementKind::Dim { .. }
            | TypedStatementKind::Const { .. }
            | TypedStatementKind::DefType
            | TypedStatementKind::Define
            | TypedStatementKind::OptionBase
            | TypedStatementKind::OptionExplicit
            | TypedStatementKind::OptionExplicitArray
            | TypedStatementKind::CommonStmt { .. }
            | TypedStatementKind::SharedStmt { .. }
            | TypedStatementKind::StaticStmt { .. }
            | TypedStatementKind::Redim { .. }) => {
                definitions::emit_definitions_stmt(self, k, &indent, output)?;
            }

            k @ (TypedStatementKind::Read { .. }
            | TypedStatementKind::Restore { .. }
            | TypedStatementKind::Randomize { .. }) => {
                data::emit_data_related_stmt(self, k, &indent, output)?;
            }

            k @ (TypedStatementKind::DefSeg { .. } | TypedStatementKind::Poke { .. }
            | TypedStatementKind::MemPutTyped { .. }
            | TypedStatementKind::Label { .. }
            | TypedStatementKind::Comment(_)
            | TypedStatementKind::Expression(_)
            | TypedStatementKind::IncludeDirective { .. }
            | TypedStatementKind::ConditionalBlock { .. }
            | TypedStatementKind::ConditionalBlockResolved { .. }
            | TypedStatementKind::Call { .. }
            | TypedStatementKind::TypeDefinition { .. }
            | TypedStatementKind::Data { .. }
            | TypedStatementKind::Swap { .. }
            | TypedStatementKind::Continue { .. }
            | TypedStatementKind::Run { .. }
            | TypedStatementKind::Chain { .. }
            | TypedStatementKind::Tron
            | TypedStatementKind::Troff
            | TypedStatementKind::Lprint { .. }
            | TypedStatementKind::FilesStmt { .. }
            | TypedStatementKind::FieldStmt { .. }
            | TypedStatementKind::Lset { .. }
            | TypedStatementKind::Rset { .. }
            | TypedStatementKind::OnKey { .. }
            | TypedStatementKind::KeyControl { .. }
            | TypedStatementKind::OnTimer { .. }
            | TypedStatementKind::TimerControl { .. }
            | TypedStatementKind::StrigControl { .. }
            | TypedStatementKind::OnStrig { .. }
            | TypedStatementKind::OnCom { .. }
            | TypedStatementKind::ComControl { .. }
            | TypedStatementKind::OnPen { .. }
            | TypedStatementKind::PenControl { .. }
            | TypedStatementKind::OnUevent { .. }
            | TypedStatementKind::UeventControl { .. }
            | TypedStatementKind::UeventTrigger
            | TypedStatementKind::OnSignal { .. }
            | TypedStatementKind::SignalControl { .. }
            | TypedStatementKind::OutPort { .. }
            | TypedStatementKind::InterruptStmt { .. }
            | TypedStatementKind::InterruptXStmt { .. }
            | TypedStatementKind::IoctlStmt { .. }
            | TypedStatementKind::FreeStmt
            | TypedStatementKind::ClearStmt { .. }
            | TypedStatementKind::ResetStmt
            | TypedStatementKind::TitleStmt { .. }
            | TypedStatementKind::ScreenMoveStmt { .. }
            | TypedStatementKind::FullScreenStmt { .. }
            | TypedStatementKind::AllowFullScreenStmt { .. }
            | TypedStatementKind::ScreenIconStmt
            | TypedStatementKind::IconStmt { .. }
            | TypedStatementKind::ScreenHideStmt
            | TypedStatementKind::ScreenShowStmt
            | TypedStatementKind::ConsoleTitleStmt { .. }
            | TypedStatementKind::ConsoleStmt { .. }
            | TypedStatementKind::AssertStmt { .. }) => {
                misc::emit_misc_stmt(self, k, &indent, output)?;
            }

            k @ (TypedStatementKind::MetaCommand { .. }
            | TypedStatementKind::MetaLet { .. }
            | TypedStatementKind::MetaChecking { .. }
            | TypedStatementKind::MetaConsole { .. }
            | TypedStatementKind::MetaScreenHide
            | TypedStatementKind::MetaScreenShow) => {
                meta::emit_meta_stmt(self, k, &indent, output)?;
            }

            // ==================== File I/O Statements ====================
            k @ (TypedStatementKind::OpenFile { .. }
            | TypedStatementKind::OpenFileLegacy { .. }
            | TypedStatementKind::CloseFile { .. }
            | TypedStatementKind::LockFile { .. }
            | TypedStatementKind::UnlockFile { .. }
            | TypedStatementKind::FilePrint { .. }
            | TypedStatementKind::FileWrite { .. }
            | TypedStatementKind::FileInput { .. }
            | TypedStatementKind::FileLineInput { .. }
            | TypedStatementKind::FileGet { .. }
            | TypedStatementKind::FilePut { .. }
            | TypedStatementKind::FileSeek { .. }) => {
                super::file_io::emit_file_io_stmt(self, k, &indent, output)?;
            }

            // ==================== Error Handling & Computed Jumps ====================
            k @ (TypedStatementKind::OnErrorGoto { .. }
            | TypedStatementKind::OnErrorResumeNext
            | TypedStatementKind::ResumeStmt { .. }
            | TypedStatementKind::ErrorStmt { .. }
            | TypedStatementKind::OnGoto { .. }
            | TypedStatementKind::OnGosub { .. }) => {
                error_jump::emit_error_jump_stmt(self, k, &indent, output)?;
            }

            // ==================== DEF FN ====================
            k @ (TypedStatementKind::DefFn { .. } | TypedStatementKind::DefFnMultiLine { .. }) => {
                def_fn::emit_def_fn_stmt(self, k, &indent, output)?;
            }

            // Graphics statements
            k @ (TypedStatementKind::Screen { .. }
            | TypedStatementKind::Cls { .. }
            | TypedStatementKind::Color { .. }
            | TypedStatementKind::Locate { .. }
            | TypedStatementKind::Pset { .. }
            | TypedStatementKind::Preset { .. }
            | TypedStatementKind::Line { .. }
            | TypedStatementKind::Circle { .. }
            | TypedStatementKind::Paint { .. }
            | TypedStatementKind::GfxDisplay
            | TypedStatementKind::ControlChr { .. }
            | TypedStatementKind::MapUnicode { .. }
            | TypedStatementKind::GfxResize { .. }
            | TypedStatementKind::Palette { .. }
            | TypedStatementKind::Pcopy { .. }
            | TypedStatementKind::Width { .. }
            | TypedStatementKind::View { .. }
            | TypedStatementKind::ViewPrint { .. }
            | TypedStatementKind::WindowCoords { .. }
            | TypedStatementKind::DrawCmd { .. }
            | TypedStatementKind::GraphicsGet { .. }
            | TypedStatementKind::GraphicsPut { .. }
            | TypedStatementKind::FreeImage { .. }
            | TypedStatementKind::PutImage { .. }
            | TypedStatementKind::SourceImg { .. }
            | TypedStatementKind::DestImg { .. }
            | TypedStatementKind::PrintStringStmt { .. }
            | TypedStatementKind::AutoDisplay { .. }) => {
                graphics::emit_graphics_stmt(self, k, &indent, output)?;
            }

            // Audio statements
            k @ (TypedStatementKind::Beep
            | TypedStatementKind::SoundStmt { .. }
            | TypedStatementKind::PlayStmt { .. }
            | TypedStatementKind::SndClose { .. }
            | TypedStatementKind::SndPlay { .. }
            | TypedStatementKind::SndStop { .. }
            | TypedStatementKind::SndPause { .. }
            | TypedStatementKind::SndLoop { .. }
            | TypedStatementKind::SndVol { .. }
            | TypedStatementKind::SndBal { .. }
            | TypedStatementKind::SndRaw { .. }
            | TypedStatementKind::SndPlayFile { .. }
            | TypedStatementKind::SndPlayCopy { .. }
            | TypedStatementKind::SndSetPos { .. }) => {
                audio::emit_audio_stmt(self, k, &indent, output)?;
            }

            // System integration statements
            k @ (TypedStatementKind::Kill { .. }
            | TypedStatementKind::Rename { .. }
            | TypedStatementKind::Mkdir { .. }
            | TypedStatementKind::Rmdir { .. }
            | TypedStatementKind::Chdir { .. }
            | TypedStatementKind::Environ { .. }
            | TypedStatementKind::ShellCmd { .. }
            | TypedStatementKind::ShellHide { .. }
            | TypedStatementKind::Bload { .. }
            | TypedStatementKind::Bsave { .. }
            | TypedStatementKind::Setmem { .. }
            | TypedStatementKind::CallAbsolute { .. }
            | TypedStatementKind::MouseHide
            | TypedStatementKind::MouseShow
            | TypedStatementKind::MouseMoveStmt { .. }
            | TypedStatementKind::ClipboardSet { .. }
            | TypedStatementKind::DeclareLibrary { .. }
            | TypedStatementKind::DeclareSub { .. }
            | TypedStatementKind::DeclareFunction { .. }) => {
                system::emit_system_stmt(self, k, &indent, output)?;
            }

            // Meta directives (part 2)
            k @ (TypedStatementKind::MetaAsserts { .. }
            | TypedStatementKind::MetaNoPrefix
            | TypedStatementKind::MetaColor { .. }
            | TypedStatementKind::MetaResize { .. }
            | TypedStatementKind::MetaResizeStretch
            | TypedStatementKind::MetaResizeSmooth
            | TypedStatementKind::MetaStatic
            | TypedStatementKind::MetaDynamic
            | TypedStatementKind::MetaDebug
            | TypedStatementKind::MetaIncludeOnce
            | TypedStatementKind::MetaExeIcon { .. }
            | TypedStatementKind::MetaVersionInfo { .. }
            | TypedStatementKind::MetaErrorDirective { .. }
            | TypedStatementKind::MetaEmbed { .. }
            | TypedStatementKind::MetaMidiSoundFont { .. }
            | TypedStatementKind::MetaUnstable { .. }
            | TypedStatementKind::MetaFormat
            | TypedStatementKind::MetaUseLibrary { .. }) => {
                meta::emit_meta_stmt(self, k, &indent, output)?;
            }
        }

        // Emit evnt closing line (guard already decremented indent on drop when we reach here).
        if evnt_opened {
            drop(_evnt_guard.take());
            let close_indent = self.indent_str();
            let evnt_line = stmt.span.line;
            let evnt_file = self.debug_file_expr();
            writeln_code!(
                output,
                "{}if (!qbevent) break; qb_evnt({}, {}, {}); }} while(0);",
                close_indent,
                evnt_line,
                evnt_line,
                evnt_file
            )?;
        }

        Ok(())
    }
}
