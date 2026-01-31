//! QB64Fresh Runtime Library
//!
//! This library provides the runtime support for programs compiled by QB64Fresh.
//! It is compiled to a static library (`libqb64fresh_rt.a`) that gets linked with
//! the generated C code.
//!
//! # Architecture
//!
//! The runtime is organized into modules:
//! - `array_registry` - LBOUND/UBOUND array bounds tracking (external runtime)
//! - `string` - Dynamic string type with reference counting
//! - `io` - PRINT, INPUT, and file operations
//! - `math` - Mathematical functions
//! - `graphics` - Graphics backend abstraction and implementations
//! - `graphics_ffi` - C FFI layer for graphics operations
//! - `audio` - Audio backend abstraction and implementations
//! - `audio_ffi` - C FFI layer for audio operations
//!
//! All public functions use `extern "C"` for C ABI compatibility.
//!
//! # Memory Management
//!
//! The runtime uses reference counting for strings. Each `QbString` has a reference
//! count, and memory is freed when the count reaches zero. The generated C code
//! must call `qb_string_release` when done with a string.

pub mod array_registry;
pub mod audio;
pub mod audio_ffi;
pub mod bitops;
pub mod buffer;
pub mod cmem;
pub mod completion;
pub mod condvar;
pub mod condvar_ffi;
pub mod console_display_ffi;
pub mod cp437;
pub mod dialogs;
pub mod events;
pub mod filepath;
pub mod font_ffi;
#[cfg(feature = "freetype")]
pub mod font_manager;
pub mod game_controller_ffi;
#[cfg(feature = "opengl")]
pub mod gl_ffi;
pub mod graphics;
pub mod graphics_ffi;
pub mod http;
pub mod http_ffi;
pub mod io;
pub mod joystick;
pub mod logging;
pub mod logging_ffi;
pub mod math;
pub mod mem_lock;
pub mod memory;
pub mod mutex;
pub mod mutex_ffi;
pub mod qbs_compat;
pub mod string;
pub mod thread;

// Re-export everything at the crate root for C access
pub use array_registry::*;
pub use audio::*;
pub use audio_ffi::*;
pub use buffer::*;
pub use completion::*;
pub use condvar_ffi::*;
pub use console_display_ffi::*;
pub use dialogs::*;
pub use events::*;
pub use filepath::*;
pub use font_ffi::*;
#[cfg(feature = "freetype")]
pub use font_manager::*;
pub use graphics::*;
pub use graphics_ffi::*;
pub use http::*;
pub use http_ffi::*;
pub use io::*;
pub use joystick::*;
pub use math::*;
pub use mem_lock::*;
pub use memory::*;
pub use mutex_ffi::*;
pub use qbs_compat::*;
pub use string::*;
pub use thread::*;

/// Initialize the runtime. Call this at program start.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_runtime_init() {
    cmem::init_dblock();
    logging::init();
    http::http_init();
    game_controller_ffi::game_controller_init();
    // Future: Initialize graphics, audio, etc.
}

/// Shutdown the runtime. Call this at program end.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_runtime_shutdown() {
    http::http_stop();
    // Future: Cleanup graphics, audio, etc.
}

/// Exit the program with the given exit code.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_end(exit_code: i32) {
    std::process::exit(exit_code);
}

/// Global flag set by the runtime when the user closes the window (clicks X).
///
/// QB64pe-generated code checks `if (stop_program) end();` in the main loop.
/// We set this to 1 when `qb_gfx_poll_events()` sees an SDL Quit event so the
/// program exits cleanly instead of ignoring the close button.
///
/// # C compatibility
/// Exported as `uint8_t stop_program` for C code (e.g. qb64pe_fresh).
#[no_mangle]
pub static mut stop_program: u8 = 0;

/// Stop the program (for debugging).
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_stop() {
    eprintln!("STOP statement executed");
    std::process::exit(1);
}

/// RUN statement: run another program or no-op (restart).
///
/// - `path == NULL`: RUN with no arguments; minimal implementation does nothing
///   (QB64pe would restart the current program; we do not support that yet).
/// - `path` non-NULL: run the given path as a program via the system shell,
///   then exit the current process (matches QB64pe behavior for RUN "file").
///
/// # Safety
/// Caller must ensure `path` is either NULL or a valid `QbString*` from the runtime.
#[no_mangle]
pub unsafe extern "C" fn qb_run(path: *const crate::string::QbString) {
    if path.is_null() {
        return;
    }
    let data = crate::string::qb_string_data(path);
    if data.is_null() || unsafe { *data == 0 } {
        return;
    }
    let _ = libc::system(data);
    std::process::exit(0);
}

// ============================================================================
// Program Initialization Functions
// ============================================================================

use std::sync::OnceLock;

/// Storage for starting directory
static START_DIR: OnceLock<String> = OnceLock::new();

/// Command-line args are set by generated C's `qb_init_args` (sets _qb_argc/_qb_argv)
/// so COMMAND$(n) and _COMMANDCOUNT work. The library does not export qb_init_args.

/// Initialize the starting directory.
///
/// Stores the current working directory at program start.
#[no_mangle]
pub extern "C" fn qb_init_startdir() {
    if let Ok(cwd) = std::env::current_dir() {
        let _ = START_DIR.set(cwd.to_string_lossy().to_string());
    } else {
        let _ = START_DIR.set(String::new());
    }
}

/// _STARTDIR$ - Returns program start directory.
///
/// Uses the value stored by `qb_init_startdir()`.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_startdir() -> *mut string::QbString {
    if let Some(start_dir) = START_DIR.get() {
        unsafe {
            let c_str = std::ffi::CString::new(start_dir.as_str()).unwrap_or_default();
            string::qb_string_new(c_str.as_ptr())
        }
    } else {
        string::qb_string_empty()
    }
}

/// Initialize the default palette.
///
/// Sets up the 256-color palette with default VGA colors.
/// This is a no-op if graphics haven't been initialized.
#[no_mangle]
pub extern "C" fn _qb_init_palette() {
    // The graphics backend initializes its own palette when created.
    // This function exists for compatibility with code that expects
    // explicit palette initialization.
}

// ============================================================================
// Error Handling Functions
// ============================================================================
//
// QB64pe-style flow:
// - When a runtime operation fails, it calls qb_set_error(code, line) which sets
//   NEW_ERROR (pending). No jump happens yet.
// - Generated code (or ON ERROR path) checks qb_error_pending(); if non-zero,
//   it calls qb_commit_error() then goto handler. qb_commit_error() copies
//   pending to ERR_CODE/ERR_LINE so ERR/ERL work in the handler, then clears pending.
// - RESUME clears state via qb_clear_error() so execution can continue.

use std::sync::atomic::{AtomicI32, AtomicU32, Ordering};

/// Committed error code (ERR function). Set when error is delivered to handler.
static ERR_CODE: AtomicI32 = AtomicI32::new(0);
/// Committed error line (ERL). Set when error is delivered to handler.
static ERR_LINE: AtomicI32 = AtomicI32::new(0);
/// Include-file error line (_INCLERRORLINE).
static INCL_ERROR_LINE: AtomicI32 = AtomicI32::new(0);

/// Pending error code (not yet delivered). Non-zero means an error occurred and
/// should be handled (e.g. goto ON ERROR handler). Set by runtime on failure.
static NEW_ERROR: AtomicU32 = AtomicU32::new(0);
/// Line number for the pending error (for ERL after commit).
static NEW_ERROR_LINE: AtomicI32 = AtomicI32::new(0);

/// Returns non-zero if an error is pending (runtime failed and handler should run).
///
/// Equivalent to QB64pe's `new_error != 0`. Generated code may check this after
/// operations that can fail (e.g. OPEN, file I/O) and jump to ON ERROR handler.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_error_pending() -> u32 {
    NEW_ERROR.load(Ordering::Relaxed)
}

/// Sets the pending error (and its line). Call from runtime when an operation fails.
///
/// Does not transfer control; the program must check qb_error_pending() and
/// jump to the error handler (and call qb_commit_error() before jumping).
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_set_error(code: u32, line: i32) {
    if std::env::var("QB64FRESH_ERROR_TRACE").is_ok() {
        eprintln!("QB64Fresh: ERROR code={} line={}", code, line);
        let _ = std::io::Write::flush(&mut std::io::stderr());
    }
    NEW_ERROR.store(code, Ordering::Relaxed);
    NEW_ERROR_LINE.store(line, Ordering::Relaxed);
}

/// Clears the pending error. Call on RESUME NEXT (or when resuming after handling)
/// so that qb_error_pending() returns 0 and execution can continue.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_clear_error() {
    NEW_ERROR.store(0, Ordering::Relaxed);
    NEW_ERROR_LINE.store(0, Ordering::Relaxed);
}

/// Commits the pending error into ERR_CODE and ERR_LINE, then clears pending.
///
/// Call immediately before jumping to the ON ERROR handler so that ERR and ERL
/// return the correct values in the handler. After this, qb_error_pending() is 0.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_commit_error() {
    let code = NEW_ERROR.load(Ordering::Relaxed);
    let line = NEW_ERROR_LINE.load(Ordering::Relaxed);
    ERR_CODE.store(code as i32, Ordering::Relaxed);
    ERR_LINE.store(line, Ordering::Relaxed);
    NEW_ERROR.store(0, Ordering::Relaxed);
    NEW_ERROR_LINE.store(0, Ordering::Relaxed);
}

/// ERR function - returns current error code.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_err_code() -> i32 {
    ERR_CODE.load(Ordering::Relaxed)
}

/// ERL function - returns error line number.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_err_line() -> i32 {
    ERR_LINE.load(Ordering::Relaxed)
}

/// _ERRORLINE - returns error line as 64-bit integer.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_errorline() -> i64 {
    ERR_LINE.load(Ordering::Relaxed) as i64
}

/// _ERRORMESSAGE$ - returns error message string.
///
/// Returns a string describing the current error code.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_errormessage() -> *mut string::QbString {
    let code = ERR_CODE.load(Ordering::Relaxed);
    let message = match code {
        0 => "No error",
        1 => "NEXT without FOR",
        2 => "Syntax error",
        3 => "RETURN without GOSUB",
        4 => "Out of DATA",
        5 => "Illegal function call",
        6 => "Overflow",
        7 => "Out of memory",
        8 => "Label not defined",
        9 => "Subscript out of range",
        10 => "Duplicate definition",
        11 => "Division by zero",
        13 => "Type mismatch",
        14 => "Out of string space",
        15 => "String too long",
        16 => "String formula too complex",
        52 => "Bad file number", // File not open or invalid handle (Option B Step 1.2)
        53 => "File not found",  // OPEN failure (Option B: set by qb_file_open)
        54 => "Bad file mode",   // Wrong mode for operation (Option B Step 1.2)
        57 => "Device I/O error", // Read/write failure (Option B Step 1.2)
        62 => "Input past end",  // Read past EOF (Option B Step 1.2)
        64 => "Bad file name",   // Empty or invalid filename (Option B Step 1.2)
        _ => {
            // Unknown error code - format as "Error N" (must be null-terminated for qb_string_new)
            match std::ffi::CString::new(format!("Error {}", code)) {
                Ok(c_str) => return unsafe { string::qb_string_new(c_str.as_ptr()) },
                Err(_) => {
                    return unsafe {
                        string::qb_string_new(b"Error\0".as_ptr() as *const std::os::raw::c_char)
                    }
                }
            }
        }
    };
    unsafe { string::qb_string_new(message.as_ptr() as *const std::os::raw::c_char) }
}

/// _INCLERRORLINE - returns error line in include file.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_inclerrorline() -> i32 {
    INCL_ERROR_LINE.load(Ordering::Relaxed)
}

/// _INCLERRORFILE$ - returns include file with error.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_inclerrorfile() -> *mut string::QbString {
    // For now, return empty string (include error tracking not fully implemented)
    // In the future, this would return the file path from INCL_ERROR_FILE
    string::qb_string_empty()
}

/// Process error state when no ON ERROR handler is active (libqb fix_error equivalent).
///
/// If an error is pending, commits it to ERR/ERL, prints the error message to stderr,
/// and exits with status 1. Call this when `qb_error_pending()` is non-zero and the
/// program has no error handler (e.g. no ON ERROR GOTO was set).
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_fix_error() {
    if qb_error_pending() == 0 {
        return;
    }
    qb_commit_error();
    let code = qb_err_code();
    let line = qb_err_line();
    let msg = qb_errormessage();
    let msg_ptr = unsafe { string::qb_string_data(msg) };
    let msg_owned = if msg_ptr.is_null() {
        std::borrow::Cow::from("Unknown error")
    } else {
        unsafe { std::ffi::CStr::from_ptr(msg_ptr).to_string_lossy() }
    };
    eprintln!("Unhandled Error #{} - {}", code, msg_owned.as_ref());
    eprintln!("Line: {}", line);
    let _ = std::io::Write::flush(&mut std::io::stderr());
    unsafe {
        string::qb_string_release(msg);
    }
    std::process::exit(1);
}

// Error handling state (libqb error_handle.h compatibility)

static ERROR_HANDLING: AtomicU32 = AtomicU32::new(0);
static ERROR_RETRY: AtomicU32 = AtomicU32::new(0);

/// Non-zero while inside an error handler (prevents re-entry from timers/callbacks).
#[no_mangle]
pub extern "C" fn qb_error_handling_get() -> u32 {
    ERROR_HANDLING.load(Ordering::Relaxed)
}

/// Set error-handling flag (e.g. 1 when entering handler, 0 when leaving).
#[no_mangle]
pub extern "C" fn qb_error_handling_set(v: u32) {
    ERROR_HANDLING.store(v, Ordering::Relaxed);
}

/// Non-zero when RESUME (retry) was used; cleared after retry.
#[no_mangle]
pub extern "C" fn qb_error_retry_get() -> u32 {
    ERROR_RETRY.load(Ordering::Relaxed)
}

/// Set error-retry flag (e.g. 1 for RESUME (retry), 0 after retry).
#[no_mangle]
pub extern "C" fn qb_error_retry_set(v: u32) {
    ERROR_RETRY.store(v, Ordering::Relaxed);
}

/// Error handler history string (libqb error_handler_history).
/// Caller must not release the returned string.
static ERROR_HANDLER_HISTORY: std::sync::atomic::AtomicPtr<std::ffi::c_void> =
    std::sync::atomic::AtomicPtr::new(std::ptr::null_mut());

#[no_mangle]
pub extern "C" fn qb_error_handler_history_get() -> *mut string::QbString {
    ERROR_HANDLER_HISTORY.load(Ordering::Relaxed) as *mut string::QbString
}

/// Set error handler history string. Takes ownership of `s` (caller must not release it).
#[no_mangle]
pub extern "C" fn qb_error_handler_history_set(s: *mut string::QbString) {
    let old = ERROR_HANDLER_HISTORY.swap(s as *mut std::ffi::c_void, Ordering::Relaxed);
    if !old.is_null() {
        unsafe { string::qb_string_release(old as *mut string::QbString) };
    }
}

// ============================================================================
// Debug Event Hooks
// ============================================================================
//
// QB64pe wraps statements in do { ... ; if (!qbevent) break; evnt(line, file); } while (r);
// so the IDE/debugger can intercept. We provide qbevent (global) and qb_evnt() so
// codegen can emit this pattern later.

/// Debug event flag: 0 = no debug, non-zero = call qb_evnt at statement boundaries.
///
/// C code may read/write this (e.g. `if (!qbevent) break;`). Exported for QB64pe-style
/// generated code. Single-threaded BASIC execution; no synchronization.
#[no_mangle]
pub static mut qbevent: u32 = 0;

/// Statement-level debug hook (evnt). Called when qbevent is non-zero.
///
/// No-op for now; can be used by IDE/debugger to set breakpoints or single-step.
/// `line` / `incline` are source line numbers; `incfile` is include filename (may be NULL).
///
/// # Safety
/// Caller must ensure `incfile` is either NULL or a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn qb_evnt(line: u32, incline: u32, incfile: *const std::ffi::c_char) {
    let _ = (line, incline, incfile);
    // No-op; hook for future debugger integration
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_init_shutdown() {
        qb_runtime_init();
        qb_runtime_shutdown();
        // Should not panic
    }

    #[test]
    fn test_error_pending_clear_commit() {
        // Option B: error-pending API
        assert_eq!(qb_error_pending(), 0);
        qb_set_error(53, 10);
        assert_ne!(qb_error_pending(), 0);
        assert_eq!(qb_err_code(), 0); // not committed yet
        qb_commit_error();
        assert_eq!(qb_error_pending(), 0);
        assert_eq!(qb_err_code(), 53);
        assert_eq!(qb_err_line(), 10);
        qb_clear_error(); // RESUME NEXT: clear pending only; ERR/ERL may still return last error
        assert_eq!(qb_error_pending(), 0);
    }
}
