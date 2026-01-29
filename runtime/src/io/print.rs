//! I/O Functions for QB64Fresh Runtime
//!
//! This module provides PRINT, INPUT, and file I/O operations.

use crate::string::{
    qb_string_data, qb_string_from_bytes, qb_string_len, qb_string_retain, QbString,
};
use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader, BufWriter, Read, Seek, SeekFrom, Write};
use std::os::raw::c_char;
use std::sync::Mutex;

#[cfg(unix)]
use std::os::unix::io::AsRawFd;

// ============================================================================
// PRINT Functions
// ============================================================================

// ============================================================================
// PRINT Functions
// ============================================================================

/// Print an integer value.
#[no_mangle]
pub extern "C" fn qb_print_int(n: i64) {
    print!("{}", n);
}

/// Print a floating-point value.
#[no_mangle]
pub extern "C" fn qb_print_float(n: f64) {
    // BASIC typically displays floats without trailing zeros
    if n == n.trunc() && n.abs() < 1e15 {
        print!("{}", n as i64);
    } else {
        print!("{}", n);
    }
}

/// Print a string.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_print_string(s: *const QbString) {
    if s.is_null() {
        return;
    }
    let data = qb_string_data(s);
    let len = qb_string_len(s);
    let slice = std::slice::from_raw_parts(data as *const u8, len);
    let _ = io::stdout().write_all(slice);
}

/// Print a newline.
#[no_mangle]
pub extern "C" fn qb_print_newline() {
    println!();
}

/// Print a tab (move to next print zone).
///
/// In BASIC, print zones are typically 14 columns wide.
#[no_mangle]
pub extern "C" fn qb_print_tab() {
    print!("\t");
}

/// Print a space (semicolon separator in some contexts).
#[no_mangle]
pub extern "C" fn qb_print_space() {
    print!(" ");
}

/// Flush stdout to ensure output is visible.
#[no_mangle]
pub extern "C" fn qb_print_flush() {
    let _ = io::stdout().flush();
}

/// Echo text to console (QB64 _ECHO statement).
///
/// Outputs the given string to the console followed by a newline.
/// This is equivalent to `PRINT text$` but is explicitly for console output.
///
/// # Safety
/// - `text` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_echo(text: *const QbString) {
    if text.is_null() {
        return;
    }
    qb_print_string(text);
    qb_print_newline();
}

// ============================================================================
// INPUT Functions
// ============================================================================

/// Read a line of input into a string variable.
///
/// # Arguments
/// * `prompt` - Optional prompt string to display before input (can be null)
/// * `var` - Pointer to QbString pointer that will receive the input
/// * `same_line` - If true, cursor stays on same line after input (no newline printed)
///
/// # Safety
/// - `prompt` can be null (no prompt) or a valid C string
/// - `var` must be a valid pointer to a QbString pointer
#[no_mangle]
pub unsafe extern "C" fn qb_input_string(
    prompt: *const c_char,
    var: *mut *mut QbString,
    same_line: i32,
) {
    // Print prompt if provided
    if !prompt.is_null() {
        let prompt_str = std::ffi::CStr::from_ptr(prompt);
        print!("{}", prompt_str.to_string_lossy());
        let _ = io::stdout().flush();
    }

    // Read line
    let mut line = String::new();
    if io::stdin().lock().read_line(&mut line).is_ok() {
        // Remove trailing newline
        if line.ends_with('\n') {
            line.pop();
            if line.ends_with('\r') {
                line.pop();
            }
        }

        // Release old string if any
        if !(*var).is_null() {
            crate::string::qb_string_release(*var);
        }

        // Create new string
        *var = qb_string_from_bytes(line.as_ptr(), line.len());
    }

    // Print newline unless same_line is true
    if same_line == 0 {
        println!();
    }
}

/// Read an integer from input.
///
/// # Arguments
/// * `prompt` - Optional prompt string to display before input (can be null)
/// * `var` - Pointer to i32 that will receive the input
/// * `same_line` - If true, cursor stays on same line after input (no newline printed)
///
/// # Safety
/// - `prompt` can be null or a valid C string
/// - `var` must be a valid pointer to an i32
#[no_mangle]
pub unsafe extern "C" fn qb_input_int(prompt: *const c_char, var: *mut i32, same_line: i32) {
    if !prompt.is_null() {
        let prompt_str = std::ffi::CStr::from_ptr(prompt);
        print!("{}", prompt_str.to_string_lossy());
        let _ = io::stdout().flush();
    }

    let mut line = String::new();
    if io::stdin().lock().read_line(&mut line).is_ok() {
        *var = line.trim().parse().unwrap_or(0);
    }

    // Print newline unless same_line is true
    if same_line == 0 {
        println!();
    }
}

/// Read a long integer from input.
///
/// # Safety
/// - `prompt` can be null or a valid C string
/// - `var` must be a valid pointer to an i64
#[no_mangle]
pub unsafe extern "C" fn qb_input_long(prompt: *const c_char, var: *mut i64) {
    if !prompt.is_null() {
        let prompt_str = std::ffi::CStr::from_ptr(prompt);
        print!("{}", prompt_str.to_string_lossy());
        let _ = io::stdout().flush();
    }

    let mut line = String::new();
    if io::stdin().lock().read_line(&mut line).is_ok() {
        *var = line.trim().parse().unwrap_or(0);
    }
}

/// Read a floating-point number from input.
///
/// # Arguments
/// * `prompt` - Optional prompt string to display before input (can be null)
/// * `var` - Pointer to f64 that will receive the input
/// * `same_line` - If true, cursor stays on same line after input (no newline printed)
///
/// # Safety
/// - `prompt` can be null or a valid C string
/// - `var` must be a valid pointer to a f64
#[no_mangle]
pub unsafe extern "C" fn qb_input_float(prompt: *const c_char, var: *mut f64, same_line: i32) {
    if !prompt.is_null() {
        let prompt_str = std::ffi::CStr::from_ptr(prompt);
        print!("{}", prompt_str.to_string_lossy());
        let _ = io::stdout().flush();
    }

    let mut line = String::new();
    if io::stdin().lock().read_line(&mut line).is_ok() {
        *var = line.trim().parse().unwrap_or(0.0);
    }

    // Print newline unless same_line is true
    if same_line == 0 {
        println!();
    }
}

/// Read a single-precision float from input.
///
/// # Safety
/// - `prompt` can be null or a valid C string
/// - `var` must be a valid pointer to a f32
#[no_mangle]
pub unsafe extern "C" fn qb_input_single(prompt: *const c_char, var: *mut f32) {
    if !prompt.is_null() {
        let prompt_str = std::ffi::CStr::from_ptr(prompt);
        print!("{}", prompt_str.to_string_lossy());
        let _ = io::stdout().flush();
    }

    let mut line = String::new();
    if io::stdin().lock().read_line(&mut line).is_ok() {
        *var = line.trim().parse().unwrap_or(0.0);
    }
}
