//! Timer and Timing Functions
//!
//! This module provides C code generation for QB64 timing functions including:
//! - `SLEEP` - Pause execution for a number of seconds or until keypress
//! - `_DELAY` - Pause execution for a fractional number of seconds
//! - `_LIMIT` - Frame rate limiter for game loops
//!
//! The generated code handles platform differences between Windows and Unix systems,
//! using appropriate system calls for each platform (Sleep/QueryPerformanceCounter
//! on Windows, usleep/gettimeofday on Unix).

use crate::codegen::error::CodeGenError;
use crate::writeln_code;

/// Emits timing-related runtime functions including SLEEP, _DELAY, and _LIMIT.
///
/// Generates platform-specific implementations using:
/// - Windows: `Sleep()`, `QueryPerformanceCounter()`, `QueryPerformanceFrequency()`
/// - Unix: `sleep()`, `usleep()`, `gettimeofday()`
///
/// # Functions Generated
///
/// - `qb_get_time_seconds()` - High-precision time helper (static)
/// - `qb_sleep(int seconds)` - SLEEP with integer seconds
/// - `qb_sleep_keypress()` - SLEEP with no argument (wait for keypress)
/// - `qb_delay(double seconds)` - _DELAY with fractional seconds
/// - `qb_limit(int fps)` - _LIMIT frame rate limiter
pub(super) fn emit_timing_functions(output: &mut String) -> Result<(), CodeGenError> {
    writeln_code!(output, "/* Timing Functions */")?;
    writeln_code!(output)?;

    // Track last frame time for _LIMIT
    writeln_code!(output, "static double qb_last_frame_time = 0.0;")?;
    writeln_code!(output)?;

    // Helper to get current time in seconds (high precision)
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "static double qb_get_time_seconds(void) {{")?;
    writeln_code!(output, "    LARGE_INTEGER freq, count;")?;
    writeln_code!(output, "    QueryPerformanceFrequency(&freq);")?;
    writeln_code!(output, "    QueryPerformanceCounter(&count);")?;
    writeln_code!(
        output,
        "    return (double)count.QuadPart / (double)freq.QuadPart;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "static double qb_get_time_seconds(void) {{")?;
    writeln_code!(output, "    struct timeval tv;")?;
    writeln_code!(output, "    gettimeofday(&tv, NULL);")?;
    writeln_code!(
        output,
        "    return (double)tv.tv_sec + (double)tv.tv_usec / 1000000.0;"
    )?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // SLEEP with integer seconds
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "void qb_sleep(int seconds) {{")?;
    writeln_code!(output, "    Sleep(seconds * 1000);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "void qb_sleep(int seconds) {{")?;
    writeln_code!(output, "    sleep(seconds);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // SLEEP with no argument - wait for keypress
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "void qb_sleep_keypress(void) {{")?;
    writeln_code!(output, "    _getch();")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "void qb_sleep_keypress(void) {{")?;
    writeln_code!(output, "    struct termios oldt, newt;")?;
    writeln_code!(output, "    tcgetattr(STDIN_FILENO, &oldt);")?;
    writeln_code!(output, "    newt = oldt;")?;
    writeln_code!(output, "    newt.c_lflag &= ~(ICANON | ECHO);")?;
    writeln_code!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &newt);")?;
    writeln_code!(output, "    getchar();")?;
    writeln_code!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // _DELAY with float seconds (QB64)
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "void qb_delay(double seconds) {{")?;
    writeln_code!(output, "    Sleep((DWORD)(seconds * 1000.0));")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#else")?;
    writeln_code!(output, "void qb_delay(double seconds) {{")?;
    writeln_code!(output, "    usleep((useconds_t)(seconds * 1000000.0));")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output)?;

    // _LIMIT - frame rate limiter (QB64)
    writeln_code!(output, "void qb_limit(int fps) {{")?;
    writeln_code!(output, "    if (fps <= 0) return;")?;
    writeln_code!(output, "    double target_frame_time = 1.0 / (double)fps;")?;
    writeln_code!(output, "    double current_time = qb_get_time_seconds();")?;
    writeln_code!(output, "    if (qb_last_frame_time > 0.0) {{")?;
    writeln_code!(
        output,
        "        double elapsed = current_time - qb_last_frame_time;"
    )?;
    writeln_code!(
        output,
        "        double wait_time = target_frame_time - elapsed;"
    )?;
    writeln_code!(output, "        if (wait_time > 0.0) {{")?;
    writeln_code!(output, "#ifdef _WIN32")?;
    writeln_code!(output, "            Sleep((DWORD)(wait_time * 1000.0));")?;
    writeln_code!(output, "#else")?;
    writeln_code!(
        output,
        "            usleep((useconds_t)(wait_time * 1000000.0));"
    )?;
    writeln_code!(output, "#endif")?;
    writeln_code!(output, "        }}")?;
    writeln_code!(output, "    }}")?;
    writeln_code!(output, "    qb_last_frame_time = qb_get_time_seconds();")?;
    writeln_code!(output, "}}")?;
    writeln_code!(output)?;
    Ok(())
}
