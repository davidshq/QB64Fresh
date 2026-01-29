//! Meta directive statement code generation.
//!
//! This module handles the emission of C code for meta directives ($IF, $LET, $CHECKING, etc.).
//! Most meta directives generate comments in the output C code.

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::TypedStatementKind;
use crate::writeln_code;

use super::StmtEmitter;

/// Emits code for meta directive statements.
pub(super) fn emit_meta_stmt(
    _emitter: &mut StmtEmitter,
    kind: &TypedStatementKind,
    indent: &str,
    output: &mut String,
) -> Result<(), CodeGenError> {
    match kind {
        TypedStatementKind::MetaCommand { command, args } => {
            let args_str = args.as_deref().unwrap_or("");
            writeln_code!(output, "{}/* ${} {} */", indent, command, args_str)?;
        }

        TypedStatementKind::MetaLet { name, value } => {
            // Compile-time variable assignment - generates a comment
            writeln_code!(output, "{}/* $LET {} = {} */", indent, name, value)?;
        }

        TypedStatementKind::MetaChecking { enabled } => {
            // Compile-time bounds checking directive - generates a comment
            let state = if *enabled { "ON" } else { "OFF" };
            writeln_code!(output, "{}/* $CHECKING:{} */", indent, state)?;
        }

        TypedStatementKind::MetaConsole { only } => {
            // Console mode directive - affects program initialization
            // For now, generate a comment; actual console setup is runtime-dependent
            if *only {
                writeln_code!(output, "{}/* $CONSOLE:ONLY - console-only mode */", indent)?;
            } else {
                writeln_code!(output, "{}/* $CONSOLE - enable console window */", indent)?;
            }
        }

        TypedStatementKind::MetaScreenHide => {
            // Hide graphics window on startup
            writeln_code!(output, "{}/* $SCREENHIDE */", indent)?;
        }

        TypedStatementKind::MetaScreenShow => {
            // Show graphics window on startup (default)
            writeln_code!(output, "{}/* $SCREENSHOW */", indent)?;
        }

        TypedStatementKind::MetaAsserts { console } => {
            // Enable assertions and optionally console output
            writeln_code!(output, "{}_qb_asserts_enabled = 1;", indent)?;
            if *console {
                writeln_code!(output, "{}_qb_asserts_console = 1;", indent)?;
                writeln_code!(output, "{}/* $ASSERTS:CONSOLE */", indent)?;
            } else {
                writeln_code!(output, "{}/* $ASSERTS */", indent)?;
            }
        }

        TypedStatementKind::MetaNoPrefix => {
            writeln_code!(output, "{}/* $NOPREFIX */", indent)?;
        }

        TypedStatementKind::MetaColor { depth } => {
            if let Some(d) = depth {
                writeln_code!(output, "{}/* $COLOR:{} */", indent, d)?;
            } else {
                writeln_code!(output, "{}/* $COLOR:0 */", indent)?;
            }
        }

        TypedStatementKind::MetaResize { enabled } => {
            if *enabled {
                writeln_code!(output, "{}/* $RESIZE:ON */", indent)?;
            } else {
                writeln_code!(output, "{}/* $RESIZE:OFF */", indent)?;
            }
        }

        TypedStatementKind::MetaResizeStretch => {
            writeln_code!(output, "{}/* $RESIZE:STRETCH */", indent)?;
        }

        TypedStatementKind::MetaResizeSmooth => {
            writeln_code!(output, "{}/* $RESIZE:SMOOTH */", indent)?;
        }

        TypedStatementKind::MetaStatic => {
            writeln_code!(output, "{}/* $STATIC */", indent)?;
        }

        TypedStatementKind::MetaDynamic => {
            writeln_code!(output, "{}/* $DYNAMIC */", indent)?;
        }

        TypedStatementKind::MetaDebug => {
            writeln_code!(output, "{}/* $DEBUG */", indent)?;
        }

        TypedStatementKind::MetaIncludeOnce => {
            writeln_code!(output, "{}/* $INCLUDEONCE */", indent)?;
        }

        TypedStatementKind::MetaExeIcon { filename } => {
            writeln_code!(output, "{}/* $EXEICON:'{}' */", indent, filename)?;
        }

        TypedStatementKind::MetaVersionInfo { key, value } => {
            writeln_code!(output, "{}/* $VERSIONINFO:{}={} */", indent, key, value)?;
        }

        TypedStatementKind::MetaErrorDirective { message } => {
            // $ERROR should ideally stop compilation, but we'll emit a warning comment
            writeln_code!(output, "{}#error \"{}\"", indent, message)?;
        }

        TypedStatementKind::MetaEmbed { filename } => {
            // $EMBED embeds a file into the executable - emit as comment
            // Runtime function _EMBEDDED$ can retrieve embedded content
            writeln_code!(output, "{}/* $EMBED:'{}' */", indent, filename)?;
        }

        TypedStatementKind::MetaMidiSoundFont { filename } => {
            // $MIDISOUNDFONT sets the MIDI soundfont file for playback
            writeln_code!(output, "{}/* $MIDISOUNDFONT:'{}' */", indent, filename)?;
        }

        TypedStatementKind::MetaUnstable { feature } => {
            // $UNSTABLE enables an experimental feature
            writeln_code!(output, "{}/* $UNSTABLE:{} */", indent, feature)?;
        }

        TypedStatementKind::MetaFormat => {
            // $FORMAT is a no-op for code formatting (IDE support only)
            writeln_code!(output, "{}/* $FORMAT */", indent)?;
        }

        TypedStatementKind::MetaUseLibrary { library } => {
            // $USELIBRARY includes an external library
            writeln_code!(output, "{}/* $USELIBRARY:'{}' */", indent, library)?;
        }

        _ => return Ok(()), // Not a meta statement
    }
    Ok(())
}
