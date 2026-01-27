//! Statement parsing for the parser.
//!
//! This module contains the main statement dispatcher and parsing for
//! all statement types. The parsing logic is organized into submodules:
//!
//! - [`assignments`] - Variable assignments, MID$, ASC statements
//! - [`print_input`] - PRINT, INPUT, LINE INPUT statements
//! - [`declare`] - DECLARE SUB/FUNCTION/LIBRARY statements
//! - [`data_dims`] - DIM, REDIM, DATA, CONST statements
//! - [`control_etc`] - Control flow helpers and miscellaneous statements
//!
//! The main entry point is [`Parser::parse_statement`] which dispatches to
//! the appropriate helper based on the current token.

mod assignments;
mod control_etc;
mod data_dims;
mod declare;
mod print_input;

use crate::ast::{Span, Statement, StatementKind};
use crate::lexer::TokenKind;

use super::{ParseError, Parser};

impl<'a> Parser<'a> {
    // ==================== Statement Dispatcher ====================

    /// Parses a single statement.
    pub(super) fn parse_statement(&mut self) -> Result<Statement, ()> {
        let token = match self.peek() {
            Some(t) => t,
            None => {
                self.errors.push(ParseError::eof("statement"));
                return Err(());
            }
        };

        let _start = token.span.start;

        match &token.kind {
            // I/O statements
            TokenKind::Print => self.parse_print_or_file_print(),
            TokenKind::Input => self.parse_input_or_file_input(),
            TokenKind::Line => self.parse_line_statement(),

            // File I/O statements
            TokenKind::Open => self.parse_open(),
            TokenKind::Close => self.parse_close(),
            TokenKind::Write => self.parse_write(),
            TokenKind::Get => self.parse_get(),
            TokenKind::Put => self.parse_put(),
            TokenKind::Seek => self.parse_seek(),

            // Variable statements
            TokenKind::Let => self.parse_let_explicit(),
            TokenKind::Dim => self.parse_dim(),
            TokenKind::Redim => self.parse_redim(),
            TokenKind::Const => self.parse_const(),
            TokenKind::Swap => self.parse_swap(),
            TokenKind::Common => self.parse_common(),
            TokenKind::Shared => self.parse_shared_stmt(),
            TokenKind::Static => self.parse_static_stmt(),

            // Default type declarations
            TokenKind::DefInt => self.parse_deftype(),
            TokenKind::DefLng => self.parse_deftype(),
            TokenKind::DefSng => self.parse_deftype(),
            TokenKind::DefDbl => self.parse_deftype(),
            TokenKind::DefStr => self.parse_deftype(),
            TokenKind::Define => self.parse_define(),

            // OPTION BASE
            TokenKind::Option => self.parse_option(),

            // Control flow (delegated to control_flow.rs)
            TokenKind::If => self.parse_if(),
            TokenKind::Select => self.parse_select_case(),
            TokenKind::For => self.parse_for(),
            TokenKind::While => self.parse_while(),
            TokenKind::Do => self.parse_do_loop(),
            TokenKind::Goto => self.parse_goto(),
            TokenKind::Gosub => self.parse_gosub(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Exit => self.parse_exit(),
            TokenKind::Continue => self.parse_continue(),
            TokenKind::End => self.parse_end(),
            TokenKind::Stop => self.parse_stop(),
            TokenKind::System => self.parse_system(),
            TokenKind::Sleep => self.parse_sleep(),
            TokenKind::Wait => self.parse_wait(),
            TokenKind::Poke => self.parse_poke(),
            TokenKind::Delay => self.parse_delay(),
            TokenKind::Limit => self.parse_limit(),
            TokenKind::Erase => self.parse_erase(),
            TokenKind::KeyClear => self.parse_keyclear(),
            TokenKind::On => self.parse_on_statement(),

            // Error handling
            TokenKind::Resume => self.parse_resume(),
            TokenKind::ErrorKw => self.parse_error_stmt(),

            // DATA statements
            TokenKind::Data => self.parse_data(),
            TokenKind::Read => self.parse_read(),
            TokenKind::Restore => self.parse_restore(),

            // Random number seeding
            TokenKind::Randomize => self.parse_randomize(),

            // Procedure definitions (delegated to procedures.rs)
            TokenKind::Sub => self.parse_sub(),
            TokenKind::Function => self.parse_function(),
            TokenKind::Type => self.parse_type_definition(),
            TokenKind::Call => self.parse_call(),
            TokenKind::Def => self.parse_def_fn(),

            // C Library Integration
            TokenKind::Declare => self.parse_declare(),

            // Preprocessor directives (delegated to directives.rs)
            TokenKind::IncludeDirective => self.parse_include_directive(),
            TokenKind::MetaIf => self.parse_meta_if(),
            TokenKind::MetaLet => self.parse_meta_let(),
            TokenKind::MetaChecking => self.parse_meta_checking(),
            TokenKind::MetaConsole => self.parse_meta_console(false),
            TokenKind::MetaConsoleOnly => self.parse_meta_console(true),
            TokenKind::MetaScreenHide => self.parse_meta_screenhide(),
            TokenKind::MetaScreenShow => self.parse_meta_screenshow(),
            TokenKind::MetaResizeOn => self.parse_meta_resize(true),
            TokenKind::MetaResizeOff => self.parse_meta_resize(false),
            TokenKind::MetaResizeStretch => self.parse_meta_resize_stretch(),
            TokenKind::MetaResizeSmooth => self.parse_meta_resize_smooth(),
            TokenKind::MetaStatic => self.parse_meta_static(),
            TokenKind::MetaDynamic => self.parse_meta_dynamic(),
            TokenKind::MetaDebug => self.parse_meta_debug(),
            TokenKind::MetaIncludeOnce => self.parse_meta_includeonce(),
            TokenKind::MetaExeIcon => self.parse_meta_exeicon(),
            TokenKind::MetaVersionInfo => self.parse_meta_versioninfo(),
            TokenKind::MetaError => self.parse_meta_error(),
            TokenKind::MetaEmbed => self.parse_meta_embed(),
            TokenKind::MetaMidiSoundFont => self.parse_meta_midisoundfont(),
            TokenKind::MetaUnstable => self.parse_meta_unstable(),
            TokenKind::MetaFormat => self.parse_meta_format(),
            TokenKind::MetaUseLibrary => self.parse_meta_uselibrary(),
            TokenKind::MetaCommand => self.parse_meta_command(),

            // Graphics statements
            TokenKind::Screen => self.parse_screen(),
            TokenKind::Cls => self.parse_cls(),
            TokenKind::Color => self.parse_color(),
            TokenKind::Locate => self.parse_locate(),
            TokenKind::Pset => self.parse_pset(),
            TokenKind::Preset => self.parse_preset(),
            TokenKind::Circle => self.parse_circle(),
            TokenKind::Paint => self.parse_paint(),
            TokenKind::Palette => self.parse_palette(),
            TokenKind::Pcopy => self.parse_pcopy(),
            TokenKind::Display => self.parse_display(),
            TokenKind::ControlChr => self.parse_controlchr(),
            // _MAPUNICODE is both a statement (value TO position) and a function (returns codepoint)
            // Only parse as statement if NOT followed by ( (function call syntax)
            TokenKind::MapUnicode
                if !self
                    .peek_ahead(1)
                    .is_some_and(|t| t.kind == TokenKind::LeftParen) =>
            {
                self.parse_mapunicode()
            }
            // _RESIZE is both a statement (ON/OFF) and a function (returns bool)
            // Only parse as statement if followed by ON or OFF
            TokenKind::Resize if self.peek_is_on_or_off() => self.parse_resize(),
            TokenKind::Width => self.parse_width(),
            TokenKind::View => self.parse_view(),
            TokenKind::Window => self.parse_window(),
            TokenKind::Draw => self.parse_draw(),

            // QB64 Graphics Extensions
            TokenKind::FreeImage => self.parse_freeimage(),
            TokenKind::PutImage => self.parse_putimage(),
            TokenKind::Source => self.parse_source(),
            TokenKind::Dest => self.parse_dest(),
            TokenKind::PrintString => self.parse_printstring(),
            TokenKind::AutoDisplay => self.parse_autodisplay(),

            // Audio statements
            TokenKind::Beep => self.parse_beep(),
            TokenKind::Sound => self.parse_sound(),
            TokenKind::Play => self.parse_play(),
            TokenKind::SndClose => self.parse_sndclose(),
            TokenKind::SndPlay => self.parse_sndplay(),
            TokenKind::SndStop => self.parse_sndstop(),
            TokenKind::SndPause => self.parse_sndpause(),
            TokenKind::SndLoop => self.parse_sndloop(),
            TokenKind::SndVol => self.parse_sndvol(),
            TokenKind::SndBal => self.parse_sndbal(),
            TokenKind::SndRaw => self.parse_sndraw(),
            TokenKind::SndPlayFile => self.parse_sndplayfile(),
            TokenKind::SndPlayCopy => self.parse_sndplaycopy(),
            TokenKind::SndSetPos => self.parse_sndsetpos(),

            // System integration statements
            TokenKind::Kill => self.parse_kill(),
            TokenKind::Name => self.parse_name(),
            TokenKind::Mkdir => self.parse_mkdir(),
            TokenKind::Rmdir => self.parse_rmdir(),
            TokenKind::Chdir => self.parse_chdir(),
            TokenKind::Environ => self.parse_environ(),
            TokenKind::Shell => self.parse_shell(),
            TokenKind::ShellHide => self.parse_shellhide(),
            TokenKind::Bload => self.parse_bload(),
            TokenKind::Bsave => self.parse_bsave(),
            TokenKind::Setmem => self.parse_setmem(),
            TokenKind::Calls => self.parse_calls(),

            // Mouse statements
            TokenKind::MouseHide => self.parse_mousehide(),
            TokenKind::MouseShow => self.parse_mouseshow(),
            TokenKind::MouseMove => self.parse_mousemove(),

            // Clipboard statement (assignment form)
            TokenKind::Clipboard => self.parse_clipboard_set(),

            // Phase 7: Additional statements
            TokenKind::Run => self.parse_run(),
            TokenKind::Chain => self.parse_chain(),
            TokenKind::Tron => self.parse_tron(),
            TokenKind::Troff => self.parse_troff(),
            TokenKind::Lprint => self.parse_lprint(),
            TokenKind::Files => self.parse_files(),
            TokenKind::Field => self.parse_field(),
            TokenKind::Lset => self.parse_lset(),
            TokenKind::Rset => self.parse_rset(),
            TokenKind::Key => self.parse_key_statement(),
            TokenKind::Strig => self.parse_strig_statement(),
            TokenKind::Com => self.parse_com_statement(),
            TokenKind::Pen => self.parse_pen_statement(),
            TokenKind::Uevent => self.parse_uevent_statement(),
            TokenKind::Signal => self.parse_signal_statement(),
            TokenKind::Timer => self.parse_timer_statement(),
            TokenKind::Out => self.parse_out_statement(),
            TokenKind::Interrupt => self.parse_interrupt_statement(),
            TokenKind::InterruptX => self.parse_interruptx_statement(),
            TokenKind::Ioctl => self.parse_ioctl_statement(),
            TokenKind::Free => self.parse_free_statement(),
            TokenKind::Clear => self.parse_clear(),
            TokenKind::Reset => self.parse_reset(),

            // Window/Desktop statements (QB64)
            // Note: _TITLE, _SCREENMOVE, _FULLSCREEN, _ICON, _SCREENHIDE, _SCREENSHOW
            // are handled in parse_identifier_statement since they can also be functions
            TokenKind::AllowFullScreen => self.parse_allowfullscreen(),
            TokenKind::ScreenIcon => self.parse_screenicon(),
            TokenKind::ConsoleTitle => self.parse_consoletitle(),
            TokenKind::Console => self.parse_console(),
            TokenKind::Assert => self.parse_assert(),

            // Other
            TokenKind::Comment => self.parse_comment(),
            TokenKind::RemComment => self.parse_rem_comment(),
            TokenKind::Identifier => self.parse_identifier_statement(),

            // Line numbers (e.g., "100 PRINT" or "1.1" becomes label + statement)
            // Classic BASIC allows both integer and decimal line numbers
            TokenKind::IntegerLiteral | TokenKind::FloatLiteral => {
                self.parse_line_number_statement()
            }

            // Handle Error tokens that might be $CONSOLE (logos lexer bug workaround)
            TokenKind::Error => {
                let text = token.text.trim().to_uppercase();
                if text == "$CONSOLE" {
                    let span: Span = token.span;
                    self.advance();
                    return Ok(Statement::new(
                        StatementKind::MetaConsole { only: false },
                        span,
                    ));
                }
                if let Some(rest) = text.strip_prefix("$CONSOLE:") {
                    let span: Span = token.span;
                    let arg = rest.trim();
                    let only = arg.eq_ignore_ascii_case("ONLY");
                    self.advance();
                    return Ok(Statement::new(StatementKind::MetaConsole { only }, span));
                }
                // Not a $CONSOLE, fall through to error
                let span: Span = token.span;
                self.errors.push(ParseError::InvalidStatement {
                    span,
                    message: format!("unexpected token {:?}", token.kind),
                });
                self.advance();
                Err(())
            }

            _ => {
                let span: Span = token.span;
                self.errors.push(ParseError::InvalidStatement {
                    span,
                    message: format!("unexpected token {:?}", token.kind),
                });
                self.advance();
                Err(())
            }
        }
    }
}
