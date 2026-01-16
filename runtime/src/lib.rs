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
//!
//! All public functions use `extern "C"` for C ABI compatibility.
//!
//! # Memory Management
//!
//! The runtime uses reference counting for strings. Each `QbString` has a reference
//! count, and memory is freed when the count reaches zero. The generated C code
//! must call `qb_string_release` when done with a string.

pub mod io;
pub mod math;
pub mod string;

// Re-export everything at the crate root for C access
pub use io::*;
pub use math::*;
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
