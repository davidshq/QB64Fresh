//! Audio function emission for the C backend runtime.
//!
//! This module contains the `emit_audio_functions` function which generates
//! inline C code for audio-related QB64 functions. These include:
//!
//! - `BEEP` - Console beep
//! - `SOUND` - Tone generation
//! - `PLAY` - MML music string playback
//! - `_SND*` family - Modern sound file operations
//! - `_WAVE` - Waveform type constant / wave output device
//! - `_SNDNEW` - Create new sound buffer
//! - `_MIDISOUNDBANK` - Set MIDI sound bank (soundfont) file path
//!
//! For full audio support, use `--runtime external` and link with `libqb64fresh_rt`.

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits audio function stubs for the inline runtime.
///
/// These stubs allow programs that use audio commands to compile even when
/// using the inline runtime. Most functions print a warning message on first
/// use and return safe default values.
///
/// # Arguments
///
/// * `output` - The string buffer to write the generated C code to
///
/// # Platform Notes
///
/// - Windows: `BEEP` and `SOUND` use the native `Beep()` function
/// - Other platforms: `BEEP` uses ASCII bell, `SOUND` prints a warning
pub(super) fn emit_audio_functions(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Audio Functions (Inline Runtime) */")?;
    writeln_code!(
        output,
        "/* For full audio support, use --runtime external and link with libqb64fresh_rt */"
    )?;
    writeln_code!(output)?;

    // Warning flag for audio
    writeln_code!(output, "static int _qb_audio_warned = 0;")?;
    writeln_code!(output, "static void _qb_audio_warn(void) {{")?;
    writeln_code!(output, "    if (!_qb_audio_warned) {{")?;
    writeln_code!(
        output,
        "        fprintf(stderr, \"Warning: Audio functions require external runtime. Use --runtime external\\n\");"
    )?;
    writeln_code!(output, "        fflush(stderr);")?;
    writeln_code!(output, "        _qb_audio_warned = 1;")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // BEEP - simple console beep
    writeln_code!(output, "void qb_beep(void) {{")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    Beep(800, 250); /* 800Hz for 250ms */")?;
    writeln_code!(output, "#else")?;
    writeln_code!(
        output,
        "    printf(\"\\a\"); fflush(stdout); /* ASCII bell */"
    )?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // SOUND - play tone (stub)
    writeln_code!(
        output,
        "void qb_sound(double frequency, double duration) {{"
    )?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "    if (frequency > 37 && frequency < 32767) {{")?;
    writeln_code!(
        output,
        "        /* Duration is in clock ticks (18.2/sec) */"
    )?;
    writeln_code!(output, "        int ms = (int)(duration * 1000.0 / 18.2);")?;
    writeln_code!(output, "        Beep((DWORD)frequency, (DWORD)ms);")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "    _qb_audio_warn();")?;
    writeln_code!(output, "    (void)frequency; (void)duration;")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // PLAY - play MML string (stub)
    writeln_code!(output, "void qb_play(qb_string* commands) {{")?;
    writeln_code!(output, "    _qb_audio_warn();")?;
    writeln_code!(output, "    (void)commands;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDOPEN - open sound file (stub)
    writeln_code!(output, "int32_t qb_sndopen(qb_string* filename) {{")?;
    writeln_code!(output, "    _qb_audio_warn();")?;
    writeln_code!(output, "    (void)filename;")?;
    writeln_code!(output, "    return -1; /* Return invalid handle */")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDCLOSE - close sound
    writeln_code!(output, "void qb_sndclose(int32_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDPLAY - play sound
    writeln_code!(output, "void qb_sndplay(int32_t handle) {{")?;
    writeln_code!(output, "    _qb_audio_warn();")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDSTOP - stop sound
    writeln_code!(output, "void qb_sndstop(int32_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDPAUSE - pause sound
    writeln_code!(output, "void qb_sndpause(int32_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDLOOP - play sound in loop
    writeln_code!(output, "void qb_sndloop(int32_t handle) {{")?;
    writeln_code!(output, "    _qb_audio_warn();")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDVOL - set sound volume
    writeln_code!(output, "void qb_sndvol(int32_t handle, double volume) {{")?;
    writeln_code!(output, "    (void)handle; (void)volume;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDBAL - set sound balance/3D position
    writeln_code!(
        output,
        "void qb_sndbal(int32_t handle, double x, double y, double z, int32_t channel) {{"
    )?;
    writeln_code!(
        output,
        "    (void)handle; (void)x; (void)y; (void)z; (void)channel;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDLEN - get sound length (returns 0)
    writeln_code!(output, "double qb_sndlen(int32_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "    return 0.0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDGETPOS - get playback position (returns 0)
    writeln_code!(output, "double qb_sndgetpos(int32_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "    return 0.0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDSETPOS - set playback position
    writeln_code!(
        output,
        "void qb_sndsetpos(int32_t handle, double position) {{"
    )?;
    writeln_code!(output, "    (void)handle; (void)position;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDPLAYING - check if sound is playing (returns 0)
    writeln_code!(output, "int32_t qb_sndplaying(int32_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDPAUSED - check if sound is paused (returns 0)
    writeln_code!(output, "int32_t qb_sndpaused(int32_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDRATE - get sample rate (returns 44100)
    writeln_code!(output, "int32_t qb_sndrate(void) {{")?;
    writeln_code!(output, "    return 44100;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDPLAYFILE - play a sound file directly
    writeln_code!(
        output,
        "void qb_sndplayfile(const char* filename, double volume, double x, double y, double z) {{"
    )?;
    writeln_code!(
        output,
        "    (void)filename; (void)volume; (void)x; (void)y; (void)z;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDPLAYCOPY - play a copy of a sound
    writeln_code!(
        output,
        "void qb_sndplaycopy(int32_t handle, double volume) {{"
    )?;
    writeln_code!(output, "    (void)handle; (void)volume;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDCOPY - copy a sound handle (returns 0)
    writeln_code!(output, "int32_t qb_sndcopy(int32_t handle) {{")?;
    writeln_code!(output, "    (void)handle;")?;
    writeln_code!(output, "    return 0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDOPENRAW - open raw audio stream for synthesis (returns handle)
    writeln_code!(output, "int32_t qb_sndopenraw(void) {{")?;
    writeln_code!(output, "    _qb_audio_warn();")?;
    writeln_code!(output, "    return -1; /* Return invalid handle */")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDRAWLEN - get length of queued raw audio in seconds (returns 0)
    writeln_code!(output, "double qb_sndrawlen(void) {{")?;
    writeln_code!(output, "    return 0.0;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _WAVE - waveform type constant / get wave output device (QB64 compatibility)
    writeln_code!(output, "int32_t qb_wave(void) {{")?;
    writeln_code!(
        output,
        "    return 0; /* Waveform constant / device; 0 = default */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _SNDNEW - create new sound buffer (frames, channels, bits [, sampleRate])
    writeln_code!(
        output,
        "int32_t qb_sndnew(int32_t frames, int32_t channels, int32_t bits) {{"
    )?;
    writeln_code!(output, "    _qb_audio_warn();")?;
    writeln_code!(output, "    (void)frames; (void)channels; (void)bits;")?;
    writeln_code!(
        output,
        "    return -1; /* Invalid handle when inline runtime */"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;

    // _MIDISOUNDBANK - set MIDI sound bank (soundfont) file path
    writeln_code!(output, "void qb_midisoundbank(qb_string* filename) {{")?;
    writeln_code!(output, "    _qb_audio_warn();")?;
    writeln_code!(output, "    (void)filename;")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
