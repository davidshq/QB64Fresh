//! Audio function emission for the C backend runtime.
//!
//! This module contains the `emit_audio_functions` function which generates
//! inline C code for audio-related QB64 functions. These include:
//!
//! - `BEEP` - Console beep
//! - `SOUND` - Tone generation
//! - `PLAY` - MML music string playback
//! - `_SND*` family - Modern sound file operations
//!
//! For full audio support, use `--runtime external` and link with `libqb64fresh_rt`.

use std::fmt::Write;

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
pub(super) fn emit_audio_functions(output: &mut String) {
    writeln!(output, "/* Audio Functions (Inline Runtime) */").unwrap();
    writeln!(
        output,
        "/* For full audio support, use --runtime external and link with libqb64fresh_rt */"
    )
    .unwrap();
    writeln!(output).unwrap();

    // Warning flag for audio
    writeln!(output, "static int _qb_audio_warned = 0;").unwrap();
    writeln!(output, "static void _qb_audio_warn(void) {{").unwrap();
    writeln!(output, "    if (!_qb_audio_warned) {{").unwrap();
    writeln!(output, "        fprintf(stderr, \"Warning: Audio functions require external runtime. Use --runtime external\\n\");").unwrap();
    writeln!(output, "        fflush(stderr);").unwrap();
    writeln!(output, "        _qb_audio_warned = 1;").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // BEEP - simple console beep
    writeln!(output, "void qb_beep(void) {{").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    Beep(800, 250); /* 800Hz for 250ms */").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(
        output,
        "    printf(\"\\a\"); fflush(stdout); /* ASCII bell */"
    )
    .unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // SOUND - play tone (stub)
    writeln!(
        output,
        "void qb_sound(double frequency, double duration) {{"
    )
    .unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "    if (frequency > 37 && frequency < 32767) {{").unwrap();
    writeln!(
        output,
        "        /* Duration is in clock ticks (18.2/sec) */"
    )
    .unwrap();
    writeln!(output, "        int ms = (int)(duration * 1000.0 / 18.2);").unwrap();
    writeln!(output, "        Beep((DWORD)frequency, (DWORD)ms);").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "    _qb_audio_warn();").unwrap();
    writeln!(output, "    (void)frequency; (void)duration;").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // PLAY - play MML string (stub)
    writeln!(output, "void qb_play(qb_string* commands) {{").unwrap();
    writeln!(output, "    _qb_audio_warn();").unwrap();
    writeln!(output, "    (void)commands;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDOPEN - open sound file (stub)
    writeln!(output, "int32_t qb_sndopen(qb_string* filename) {{").unwrap();
    writeln!(output, "    _qb_audio_warn();").unwrap();
    writeln!(output, "    (void)filename;").unwrap();
    writeln!(output, "    return -1; /* Return invalid handle */").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDCLOSE - close sound
    writeln!(output, "void qb_sndclose(int32_t handle) {{").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDPLAY - play sound
    writeln!(output, "void qb_sndplay(int32_t handle) {{").unwrap();
    writeln!(output, "    _qb_audio_warn();").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDSTOP - stop sound
    writeln!(output, "void qb_sndstop(int32_t handle) {{").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDPAUSE - pause sound
    writeln!(output, "void qb_sndpause(int32_t handle) {{").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDLOOP - play sound in loop
    writeln!(output, "void qb_sndloop(int32_t handle) {{").unwrap();
    writeln!(output, "    _qb_audio_warn();").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDVOL - set sound volume
    writeln!(output, "void qb_sndvol(int32_t handle, double volume) {{").unwrap();
    writeln!(output, "    (void)handle; (void)volume;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDBAL - set sound balance/3D position
    writeln!(
        output,
        "void qb_sndbal(int32_t handle, double x, double y, double z, int32_t channel) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    (void)handle; (void)x; (void)y; (void)z; (void)channel;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDLEN - get sound length (returns 0)
    writeln!(output, "double qb_sndlen(int32_t handle) {{").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "    return 0.0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDGETPOS - get playback position (returns 0)
    writeln!(output, "double qb_sndgetpos(int32_t handle) {{").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "    return 0.0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDSETPOS - set playback position
    writeln!(
        output,
        "void qb_sndsetpos(int32_t handle, double position) {{"
    )
    .unwrap();
    writeln!(output, "    (void)handle; (void)position;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDPLAYING - check if sound is playing (returns 0)
    writeln!(output, "int32_t qb_sndplaying(int32_t handle) {{").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDPAUSED - check if sound is paused (returns 0)
    writeln!(output, "int32_t qb_sndpaused(int32_t handle) {{").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDRATE - get sample rate (returns 44100)
    writeln!(output, "int32_t qb_sndrate(void) {{").unwrap();
    writeln!(output, "    return 44100;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDPLAYFILE - play a sound file directly
    writeln!(
        output,
        "void qb_sndplayfile(const char* filename, double volume, double x, double y, double z) {{"
    )
    .unwrap();
    writeln!(
        output,
        "    (void)filename; (void)volume; (void)x; (void)y; (void)z;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDPLAYCOPY - play a copy of a sound
    writeln!(
        output,
        "void qb_sndplaycopy(int32_t handle, double volume) {{"
    )
    .unwrap();
    writeln!(output, "    (void)handle; (void)volume;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDCOPY - copy a sound handle (returns 0)
    writeln!(output, "int32_t qb_sndcopy(int32_t handle) {{").unwrap();
    writeln!(output, "    (void)handle;").unwrap();
    writeln!(output, "    return 0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDOPENRAW - open raw audio stream for synthesis (returns handle)
    writeln!(output, "int32_t qb_sndopenraw(void) {{").unwrap();
    writeln!(output, "    _qb_audio_warn();").unwrap();
    writeln!(output, "    return -1; /* Return invalid handle */").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();

    // _SNDRAWLEN - get length of queued raw audio in seconds (returns 0)
    writeln!(output, "double qb_sndrawlen(void) {{").unwrap();
    writeln!(output, "    return 0.0;").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}
