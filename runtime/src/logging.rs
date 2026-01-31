//! Scoped logging for QB64Fresh runtime (libqb logging.h compatibility).
//!
//! Provides level- and scope-based logging with output to stderr.
//! Levels: Trace, Information, Warning, Error.
//! Scopes: Runtime, QB64, Libqb, Audio, Image.

use std::sync::atomic::{AtomicI32, Ordering};
use std::time::Instant;

/// Minimum log level: 0=Trace, 1=Information, 2=Warning, 3=Error.
/// Log entries below this level are dropped.
static MIN_LEVEL: AtomicI32 = AtomicI32::new(0);

/// Program start time for timestamps.
static START: std::sync::OnceLock<Instant> = std::sync::OnceLock::new();

/// Log levels matching libqb loglevel enum.
#[repr(i32)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LogLevel {
    Trace = 0,
    Information = 1,
    Warning = 2,
    Error = 3,
}

/// Log scopes matching libqb logscope enum.
#[repr(i32)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LogScope {
    Runtime = 0,
    QB64 = 1,
    Libqb = 2,
    Audio = 3,
    Image = 4,
}

fn level_name(lvl: LogLevel) -> &'static str {
    match lvl {
        LogLevel::Trace => "TRACE",
        LogLevel::Information => "INFO",
        LogLevel::Warning => "WARN",
        LogLevel::Error => "ERROR",
    }
}

fn scope_name(scope: LogScope) -> &'static str {
    match scope {
        LogScope::Runtime => "Runtime",
        LogScope::QB64 => "QB64",
        LogScope::Libqb => "Libqb",
        LogScope::Audio => "Audio",
        LogScope::Image => "Image",
    }
}

/// Initialize logging. Call once at runtime start.
pub fn init() {
    START.get_or_init(Instant::now);
}

/// Log a preformatted message (called from FFI or after vsnprintf on C side).
pub fn log_message(
    level: i32,
    scope: i32,
    file: Option<&str>,
    func: Option<&str>,
    line: i32,
    message: &str,
) {
    let min = MIN_LEVEL.load(Ordering::Relaxed);
    if level < min {
        return;
    }
    let lvl = match level {
        0 => LogLevel::Trace,
        1 => LogLevel::Information,
        2 => LogLevel::Warning,
        3 => LogLevel::Error,
        _ => LogLevel::Information,
    };
    let sc = match scope {
        0 => LogScope::Runtime,
        1 => LogScope::QB64,
        2 => LogScope::Libqb,
        3 => LogScope::Audio,
        4 => LogScope::Image,
        _ => LogScope::Libqb,
    };
    let elapsed = START
        .get()
        .map(|t| t.elapsed().as_secs_f64())
        .unwrap_or(0.0);
    let file = file.unwrap_or("?");
    let func = func.unwrap_or("?");
    eprintln!(
        "[{:.3}] {} [{}] {}:{} in {} {}",
        elapsed,
        level_name(lvl),
        scope_name(sc),
        file,
        line,
        func,
        message
    );
}

/// Set minimum log level (0=Trace .. 3=Error). Returns previous value.
pub fn set_min_level(level: i32) -> i32 {
    let prev = MIN_LEVEL.swap(level.clamp(0, 3), Ordering::Relaxed);
    prev
}

/// Get current minimum log level.
pub fn get_min_level() -> i32 {
    MIN_LEVEL.load(Ordering::Relaxed)
}
