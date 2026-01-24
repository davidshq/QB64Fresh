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

use std::fmt::Write;

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
pub(super) fn emit_timing_functions(output: &mut String) {
    writeln!(output, "/* Timing Functions */").unwrap();
    writeln!(output).unwrap();

    // Track last frame time for _LIMIT
    writeln!(output, "static double qb_last_frame_time = 0.0;").unwrap();
    writeln!(output).unwrap();

    // Helper to get current time in seconds (high precision)
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "static double qb_get_time_seconds(void) {{").unwrap();
    writeln!(output, "    LARGE_INTEGER freq, count;").unwrap();
    writeln!(output, "    QueryPerformanceFrequency(&freq);").unwrap();
    writeln!(output, "    QueryPerformanceCounter(&count);").unwrap();
    writeln!(
        output,
        "    return (double)count.QuadPart / (double)freq.QuadPart;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "static double qb_get_time_seconds(void) {{").unwrap();
    writeln!(output, "    struct timeval tv;").unwrap();
    writeln!(output, "    gettimeofday(&tv, NULL);").unwrap();
    writeln!(
        output,
        "    return (double)tv.tv_sec + (double)tv.tv_usec / 1000000.0;"
    )
    .unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // SLEEP with integer seconds
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "void qb_sleep(int seconds) {{").unwrap();
    writeln!(output, "    Sleep(seconds * 1000);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "void qb_sleep(int seconds) {{").unwrap();
    writeln!(output, "    sleep(seconds);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // SLEEP with no argument - wait for keypress
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "void qb_sleep_keypress(void) {{").unwrap();
    writeln!(output, "    _getch();").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "void qb_sleep_keypress(void) {{").unwrap();
    writeln!(output, "    struct termios oldt, newt;").unwrap();
    writeln!(output, "    tcgetattr(STDIN_FILENO, &oldt);").unwrap();
    writeln!(output, "    newt = oldt;").unwrap();
    writeln!(output, "    newt.c_lflag &= ~(ICANON | ECHO);").unwrap();
    writeln!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &newt);").unwrap();
    writeln!(output, "    getchar();").unwrap();
    writeln!(output, "    tcsetattr(STDIN_FILENO, TCSANOW, &oldt);").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // _DELAY with float seconds (QB64)
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "void qb_delay(double seconds) {{").unwrap();
    writeln!(output, "    Sleep((DWORD)(seconds * 1000.0));").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(output, "void qb_delay(double seconds) {{").unwrap();
    writeln!(output, "    usleep((useconds_t)(seconds * 1000000.0));").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output).unwrap();

    // _LIMIT - frame rate limiter (QB64)
    writeln!(output, "void qb_limit(int fps) {{").unwrap();
    writeln!(output, "    if (fps <= 0) return;").unwrap();
    writeln!(output, "    double target_frame_time = 1.0 / (double)fps;").unwrap();
    writeln!(output, "    double current_time = qb_get_time_seconds();").unwrap();
    writeln!(output, "    if (qb_last_frame_time > 0.0) {{").unwrap();
    writeln!(
        output,
        "        double elapsed = current_time - qb_last_frame_time;"
    )
    .unwrap();
    writeln!(
        output,
        "        double wait_time = target_frame_time - elapsed;"
    )
    .unwrap();
    writeln!(output, "        if (wait_time > 0.0) {{").unwrap();
    writeln!(output, "#ifdef _WIN32").unwrap();
    writeln!(output, "            Sleep((DWORD)(wait_time * 1000.0));").unwrap();
    writeln!(output, "#else").unwrap();
    writeln!(
        output,
        "            usleep((useconds_t)(wait_time * 1000000.0));"
    )
    .unwrap();
    writeln!(output, "#endif").unwrap();
    writeln!(output, "        }}").unwrap();
    writeln!(output, "    }}").unwrap();
    writeln!(output, "    qb_last_frame_time = qb_get_time_seconds();").unwrap();
    writeln!(output, "}}").unwrap();
    writeln!(output).unwrap();
}
