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
pub mod dialogs;
pub mod events;
pub mod font_ffi;
#[cfg(feature = "freetype")]
pub mod font_manager;
pub mod graphics;
pub mod graphics_ffi;
pub mod io;
pub mod joystick;
pub mod math;
pub mod memory;
pub mod string;

// Re-export everything at the crate root for C access
pub use array_registry::*;
pub use audio::*;
pub use audio_ffi::*;
pub use dialogs::*;
pub use events::*;
pub use font_ffi::*;
#[cfg(feature = "freetype")]
pub use font_manager::*;
pub use graphics::*;
pub use graphics_ffi::*;
pub use io::*;
pub use joystick::*;
pub use math::*;
pub use memory::*;
pub use string::*;

/// Initialize the runtime. Call this at program start.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_runtime_init() {
    // Future: Initialize graphics, audio, etc.
}

/// Shutdown the runtime. Call this at program end.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_runtime_shutdown() {
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

use std::sync::atomic::{AtomicI32, Ordering};

/// Error state variables
static ERR_CODE: AtomicI32 = AtomicI32::new(0);
static ERR_LINE: AtomicI32 = AtomicI32::new(0);
static INCL_ERROR_LINE: AtomicI32 = AtomicI32::new(0);

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
        _ => {
            // Unknown error code - format as "Error N"
            unsafe {
                let buf = format!("Error {}", code);
                return string::qb_string_new(buf.as_ptr() as *const std::os::raw::c_char);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_init_shutdown() {
        qb_runtime_init();
        qb_runtime_shutdown();
        // Should not panic
    }
}
