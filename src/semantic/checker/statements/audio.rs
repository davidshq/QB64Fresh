//! Audio statement type checking.
//!
//! This module handles type checking for audio statements:
//! - BEEP, SOUND, PLAY
//! - _SNDPLAY, _SNDPLAYCOPY, _SNDPLAYFILE
//! - _SNDLOOP, _SNDPAUSE, _SNDSTOP, _SNDCLOSE
//! - _SNDVOL, _SNDBAL, _SNDSETPOS
//! - _SNDRAW

use crate::ast::{Expr, Span};
use crate::semantic::typed_ir::*;

use super::super::TypeChecker;

impl<'a> TypeChecker<'a> {
    /// Type checks BEEP statement.
    ///
    /// BEEP produces a simple beep sound. It takes no arguments.
    pub(in crate::semantic::checker) fn check_beep(&mut self, span: Span) -> TypedStatement {
        TypedStatement::new(TypedStatementKind::Beep, span)
    }

    /// Type checks SOUND statement.
    ///
    /// SOUND frequency, duration
    ///
    /// Produces a sound at the specified frequency (Hz) for the specified duration.
    pub(in crate::semantic::checker) fn check_sound(
        &mut self,
        frequency: &Expr,
        duration: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_freq = self.check_expr(frequency);
        let typed_dur = self.check_expr(duration);

        TypedStatement::new(
            TypedStatementKind::SoundStmt {
                frequency: typed_freq,
                duration: typed_dur,
            },
            span,
        )
    }

    /// Type checks PLAY statement.
    ///
    /// PLAY commands$
    ///
    /// Plays music using a command string with notes, tempo, and other directives.
    pub(in crate::semantic::checker) fn check_play(
        &mut self,
        commands: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_commands = self.check_expr(commands);

        TypedStatement::new(
            TypedStatementKind::PlayStmt {
                commands: typed_commands,
            },
            span,
        )
    }

    /// Type checks _SNDCLOSE statement.
    ///
    /// _SNDCLOSE handle&
    ///
    /// Closes a sound handle and frees its resources.
    pub(in crate::semantic::checker) fn check_snd_close(
        &mut self,
        handle: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);

        TypedStatement::new(
            TypedStatementKind::SndClose {
                handle: typed_handle,
            },
            span,
        )
    }

    /// Type checks _SNDPLAY statement.
    ///
    /// _SNDPLAY handle&
    ///
    /// Plays a sound from the beginning.
    pub(in crate::semantic::checker) fn check_snd_play(
        &mut self,
        handle: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);

        TypedStatement::new(
            TypedStatementKind::SndPlay {
                handle: typed_handle,
            },
            span,
        )
    }

    /// Type checks _SNDSTOP statement.
    ///
    /// _SNDSTOP handle&
    ///
    /// Stops a playing sound.
    pub(in crate::semantic::checker) fn check_snd_stop(
        &mut self,
        handle: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);

        TypedStatement::new(
            TypedStatementKind::SndStop {
                handle: typed_handle,
            },
            span,
        )
    }

    /// Type checks _SNDPAUSE statement.
    ///
    /// _SNDPAUSE handle&
    ///
    /// Pauses a playing sound. Can be resumed with _SNDPLAY.
    pub(in crate::semantic::checker) fn check_snd_pause(
        &mut self,
        handle: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);

        TypedStatement::new(
            TypedStatementKind::SndPause {
                handle: typed_handle,
            },
            span,
        )
    }

    /// Type checks _SNDLOOP statement.
    ///
    /// _SNDLOOP handle&
    ///
    /// Plays a sound in a continuous loop.
    pub(in crate::semantic::checker) fn check_snd_loop(
        &mut self,
        handle: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);

        TypedStatement::new(
            TypedStatementKind::SndLoop {
                handle: typed_handle,
            },
            span,
        )
    }

    /// Type checks _SNDVOL statement.
    ///
    /// _SNDVOL handle&, volume!
    ///
    /// Sets the volume for a sound (0.0 = silent, 1.0 = full volume).
    pub(in crate::semantic::checker) fn check_snd_vol(
        &mut self,
        handle: &Expr,
        volume: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);
        let typed_volume = self.check_expr(volume);

        TypedStatement::new(
            TypedStatementKind::SndVol {
                handle: typed_handle,
                volume: typed_volume,
            },
            span,
        )
    }

    /// Type checks _SNDBAL statement.
    ///
    /// _SNDBAL handle& [, x!] [, y!] [, z!] [, channel&]
    ///
    /// Sets the balance/3D position for a sound.
    /// - With just x: Sets stereo balance (-1.0 = left, 0 = center, 1.0 = right)
    /// - With x, y, z: Sets 3D position
    /// - channel: Optional channel specification
    pub(in crate::semantic::checker) fn check_snd_bal(
        &mut self,
        handle: &Expr,
        x: Option<&Expr>,
        y: Option<&Expr>,
        z: Option<&Expr>,
        channel: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);
        let typed_x = x.map(|e| self.check_expr(e));
        let typed_y = y.map(|e| self.check_expr(e));
        let typed_z = z.map(|e| self.check_expr(e));
        let typed_channel = channel.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::SndBal {
                handle: typed_handle,
                x: typed_x,
                y: typed_y,
                z: typed_z,
                channel: typed_channel,
            },
            span,
        )
    }

    /// Type checks _SNDRAW statement.
    ///
    /// _SNDRAW left! [, right!]
    ///
    /// Outputs raw audio samples to the default sound device.
    /// - left: Left channel sample (or mono if right is not provided)
    /// - right: Optional right channel sample for stereo
    pub(in crate::semantic::checker) fn check_snd_raw(
        &mut self,
        left: &Expr,
        right: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_left = self.check_expr(left);
        let typed_right = right.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::SndRaw {
                left: typed_left,
                right: typed_right,
            },
            span,
        )
    }

    /// Type checks _SNDPLAYFILE statement.
    ///
    /// _SNDPLAYFILE filename$ [, volume!] [, x!] [, y!] [, z!]
    ///
    /// Plays an audio file directly without loading it first.
    /// - filename: Path to the audio file
    /// - volume: Optional volume (0.0-1.0)
    /// - x, y, z: Optional 3D position
    pub(in crate::semantic::checker) fn check_snd_playfile(
        &mut self,
        filename: &Expr,
        volume: Option<&Expr>,
        x: Option<&Expr>,
        y: Option<&Expr>,
        z: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_filename = self.check_expr(filename);
        let typed_volume = volume.map(|e| self.check_expr(e));
        let typed_x = x.map(|e| self.check_expr(e));
        let typed_y = y.map(|e| self.check_expr(e));
        let typed_z = z.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::SndPlayFile {
                filename: typed_filename,
                volume: typed_volume,
                x: typed_x,
                y: typed_y,
                z: typed_z,
            },
            span,
        )
    }

    /// Type checks _SNDPLAYCOPY statement.
    ///
    /// _SNDPLAYCOPY handle& [, volume!]
    ///
    /// Plays a copy of a sound, allowing the same sound to be played
    /// multiple times simultaneously.
    pub(in crate::semantic::checker) fn check_snd_playcopy(
        &mut self,
        handle: &Expr,
        volume: Option<&Expr>,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);
        let typed_volume = volume.map(|e| self.check_expr(e));

        TypedStatement::new(
            TypedStatementKind::SndPlayCopy {
                handle: typed_handle,
                volume: typed_volume,
            },
            span,
        )
    }

    /// Type checks _SNDSETPOS statement.
    ///
    /// _SNDSETPOS handle&, position#
    ///
    /// Sets the playback position of a sound in seconds.
    pub(in crate::semantic::checker) fn check_snd_setpos(
        &mut self,
        handle: &Expr,
        position: &Expr,
        span: Span,
    ) -> TypedStatement {
        let typed_handle = self.check_expr(handle);
        let typed_position = self.check_expr(position);

        TypedStatement::new(
            TypedStatementKind::SndSetPos {
                handle: typed_handle,
                position: typed_position,
            },
            span,
        )
    }
}

/// Dispatch function for audio statements.
pub(super) fn check_audio_stmt(
    checker: &mut super::super::TypeChecker,
    kind: &crate::ast::StatementKind,
    span: crate::ast::Span,
) -> crate::semantic::typed_ir::TypedStatement {
    match kind {
        crate::ast::StatementKind::Beep => checker.check_beep(span),
        crate::ast::StatementKind::SoundStmt {
            frequency,
            duration,
        } => checker.check_sound(frequency, duration, span),
        crate::ast::StatementKind::PlayStmt { commands } => checker.check_play(commands, span),
        crate::ast::StatementKind::SndClose { handle } => checker.check_snd_close(handle, span),
        crate::ast::StatementKind::SndPlay { handle } => checker.check_snd_play(handle, span),
        crate::ast::StatementKind::SndStop { handle } => checker.check_snd_stop(handle, span),
        crate::ast::StatementKind::SndPause { handle } => checker.check_snd_pause(handle, span),
        crate::ast::StatementKind::SndLoop { handle } => checker.check_snd_loop(handle, span),
        crate::ast::StatementKind::SndVol { handle, volume } => {
            checker.check_snd_vol(handle, volume, span)
        }
        crate::ast::StatementKind::SndBal {
            handle,
            x,
            y,
            z,
            channel,
        } => checker.check_snd_bal(
            handle,
            x.as_ref(),
            y.as_ref(),
            z.as_ref(),
            channel.as_ref(),
            span,
        ),
        crate::ast::StatementKind::SndRaw { left, right } => {
            checker.check_snd_raw(left, right.as_ref(), span)
        }
        crate::ast::StatementKind::SndPlayFile {
            filename,
            volume,
            x,
            y,
            z,
        } => checker.check_snd_playfile(
            filename,
            volume.as_ref(),
            x.as_ref(),
            y.as_ref(),
            z.as_ref(),
            span,
        ),
        crate::ast::StatementKind::SndPlayCopy { handle, volume } => {
            checker.check_snd_playcopy(handle, volume.as_ref(), span)
        }
        crate::ast::StatementKind::SndSetPos { handle, position } => {
            checker.check_snd_setpos(handle, position, span)
        }
        _ => unreachable!("Not an audio statement"),
    }
}
