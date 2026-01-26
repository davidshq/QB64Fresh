//! QB64Fresh Runtime Library
//!
//! This library provides the runtime support for programs compiled by QB64Fresh.
//! It is compiled to a static library (`libqb64fresh_rt.a`) that gets linked with
//! the generated C code.
//!
//! # Architecture
//!
//! The runtime is organized into modules:
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

pub mod audio;
pub mod audio_ffi;
pub mod dialogs;
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
pub use audio::*;
pub use audio_ffi::*;
pub use dialogs::*;
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

// ============================================================================
// Program Initialization Functions
// ============================================================================

use std::os::raw::c_char;
use std::sync::OnceLock;

/// Storage for command line arguments
static ARGS: OnceLock<(i32, Vec<String>)> = OnceLock::new();

/// Storage for starting directory
static START_DIR: OnceLock<String> = OnceLock::new();

/// Initialize command line arguments.
///
/// Stores argc and argv for later access by COMMAND$ function.
///
/// # Safety
/// - `argv` must be a valid array of null-terminated C strings with `argc` elements
#[no_mangle]
pub unsafe extern "C" fn qb_init_args(argc: i32, argv: *const *const c_char) {
    let mut args = Vec::new();
    if !argv.is_null() && argc > 0 {
        for i in 0..argc as usize {
            let arg_ptr = *argv.add(i);
            if !arg_ptr.is_null() {
                if let Ok(s) = std::ffi::CStr::from_ptr(arg_ptr).to_str() {
                    args.push(s.to_string());
                }
            }
        }
    }
    let _ = ARGS.set((argc, args));
}

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
