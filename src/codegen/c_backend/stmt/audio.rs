//! Audio statement code generation.
//!
//! This module handles the emission of C code for all audio-related statements,
//! including BEEP, SOUND, PLAY, and QB64 audio extensions (_SND*).

use crate::codegen::error::CodeGenError;
use crate::semantic::typed_ir::TypedStatementKind;
use crate::writeln_code;

use super::StmtEmitter;
use crate::codegen::c_backend::expr::emit_string_data_access;

/// Emits code for audio-related statements.
pub(super) fn emit_audio_stmt(
    emitter: &mut StmtEmitter,
    kind: &TypedStatementKind,
    indent: &str,
    output: &mut String,
) -> Result<(), CodeGenError> {
    match kind {
        TypedStatementKind::Beep => {
            writeln_code!(output, "{}qb_beep();", indent)?;
        }

        TypedStatementKind::SoundStmt {
            frequency,
            duration,
        } => {
            let freq_code = emitter.emit_expr(frequency)?;
            let dur_code = emitter.emit_expr(duration)?;
            writeln_code!(
                output,
                "{}qb_sound((double){}, (double){});",
                indent,
                freq_code,
                dur_code
            )?;
        }

        TypedStatementKind::PlayStmt { commands } => {
            let cmd_code = emitter.emit_expr(commands)?;
            writeln_code!(output, "{}qb_play({});", indent, cmd_code)?;
        }

        TypedStatementKind::SndClose { handle } => {
            let h_code = emitter.emit_expr(handle)?;
            writeln_code!(output, "{}qb_sndclose((int32_t){});", indent, h_code)?;
        }

        TypedStatementKind::SndPlay { handle } => {
            let h_code = emitter.emit_expr(handle)?;
            writeln_code!(output, "{}qb_sndplay((int32_t){});", indent, h_code)?;
        }

        TypedStatementKind::SndStop { handle } => {
            let h_code = emitter.emit_expr(handle)?;
            writeln_code!(output, "{}qb_sndstop((int32_t){});", indent, h_code)?;
        }

        TypedStatementKind::SndPause { handle } => {
            let h_code = emitter.emit_expr(handle)?;
            writeln_code!(output, "{}qb_sndpause((int32_t){});", indent, h_code)?;
        }

        TypedStatementKind::SndLoop { handle } => {
            let h_code = emitter.emit_expr(handle)?;
            writeln_code!(output, "{}qb_sndloop((int32_t){});", indent, h_code)?;
        }

        TypedStatementKind::SndVol { handle, volume } => {
            let h_code = emitter.emit_expr(handle)?;
            let vol_code = emitter.emit_expr(volume)?;
            writeln_code!(
                output,
                "{}qb_sndvol((int32_t){}, (double){});",
                indent,
                h_code,
                vol_code
            )?;
        }

        TypedStatementKind::SndBal {
            handle,
            x,
            y,
            z,
            channel,
        } => {
            let h_code = emitter.emit_expr(handle)?;
            let x_code = match x {
                Some(e) => emitter.emit_expr(e)?,
                None => "0.0".to_string(),
            };
            let y_code = match y {
                Some(e) => emitter.emit_expr(e)?,
                None => "0.0".to_string(),
            };
            let z_code = match z {
                Some(e) => emitter.emit_expr(e)?,
                None => "0.0".to_string(),
            };
            let ch_code = match channel {
                Some(e) => emitter.emit_expr(e)?,
                None => "0".to_string(),
            };
            writeln_code!(
                output,
                "{}qb_sndbal((int32_t){}, (double){}, (double){}, (double){}, (int32_t){});",
                indent,
                h_code,
                x_code,
                y_code,
                z_code,
                ch_code
            )?;
        }

        TypedStatementKind::SndRaw { left, right } => {
            let left_code = emitter.emit_expr(left)?;
            if let Some(r) = right {
                let right_code = emitter.emit_expr(r)?;
                writeln_code!(
                    output,
                    "{}qb_sndraw_stereo((double){}, (double){});",
                    indent,
                    left_code,
                    right_code
                )?;
            } else {
                writeln_code!(output, "{}qb_sndraw((double){});", indent, left_code)?;
            }
        }

        TypedStatementKind::SndPlayFile {
            filename,
            volume,
            x,
            y,
            z,
        } => {
            let filename_code = emitter.emit_expr(filename)?;
            let volume_code = volume
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "1.0".to_string());
            let x_code = x
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "0.0".to_string());
            let y_code = y
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "0.0".to_string());
            let z_code = z
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "0.0".to_string());
            let filename_access =
                emit_string_data_access(filename, &filename_code, &emitter.config.runtime_mode);
            writeln_code!(
                output,
                "{}qb_sndplayfile({}, (double){}, (double){}, (double){}, (double){});",
                indent,
                filename_access,
                volume_code,
                x_code,
                y_code,
                z_code
            )?;
        }

        TypedStatementKind::SndPlayCopy { handle, volume } => {
            let handle_code = emitter.emit_expr(handle)?;
            let volume_code = volume
                .as_ref()
                .map(|e| emitter.emit_expr(e))
                .transpose()?
                .unwrap_or_else(|| "1.0".to_string());
            writeln_code!(
                output,
                "{}qb_sndplaycopy((int32_t){}, (double){});",
                indent,
                handle_code,
                volume_code
            )?;
        }

        TypedStatementKind::SndSetPos { handle, position } => {
            let handle_code = emitter.emit_expr(handle)?;
            let position_code = emitter.emit_expr(position)?;
            writeln_code!(
                output,
                "{}qb_sndsetpos((int32_t){}, (double){});",
                indent,
                handle_code,
                position_code
            )?;
        }
        _ => return Ok(()), // Not an audio statement
    }
    Ok(())
}
