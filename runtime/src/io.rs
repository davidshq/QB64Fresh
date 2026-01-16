//! I/O Functions for QB64Fresh Runtime
//!
//! This module provides PRINT, INPUT, and file I/O operations.

use crate::string::{qb_string_data, qb_string_from_bytes, qb_string_len, QbString};
use std::io::{self, BufRead, Write};
use std::os::raw::c_char;

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

// ============================================================================
// INPUT Functions
// ============================================================================

/// Read a line of input into a string variable.
///
/// # Safety
/// - `prompt` can be null (no prompt) or a valid C string
/// - `var` must be a valid pointer to a QbString pointer
#[no_mangle]
pub unsafe extern "C" fn qb_input_string(prompt: *const c_char, var: *mut *mut QbString) {
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
}

/// Read an integer from input.
///
/// # Safety
/// - `prompt` can be null or a valid C string
/// - `var` must be a valid pointer to an i32
#[no_mangle]
pub unsafe extern "C" fn qb_input_int(prompt: *const c_char, var: *mut i32) {
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
/// # Safety
/// - `prompt` can be null or a valid C string
/// - `var` must be a valid pointer to a f64
#[no_mangle]
pub unsafe extern "C" fn qb_input_float(prompt: *const c_char, var: *mut f64) {
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

// ============================================================================
// LINE INPUT
// ============================================================================

/// Read a complete line of input (LINE INPUT).
///
/// Unlike regular INPUT, LINE INPUT reads the entire line including commas.
///
/// # Safety
/// - `prompt` can be null or a valid C string
/// - `var` must be a valid pointer to a QbString pointer
#[no_mangle]
pub unsafe extern "C" fn qb_line_input(prompt: *const c_char, var: *mut *mut QbString) {
    // Same as qb_input_string for now
    qb_input_string(prompt, var);
}

// ============================================================================
// Console Functions
// ============================================================================

/// Clear the screen (CLS).
#[no_mangle]
pub extern "C" fn qb_cls() {
    // ANSI escape sequence to clear screen and move cursor to home
    print!("\x1B[2J\x1B[H");
    let _ = io::stdout().flush();
}

/// Position the cursor (LOCATE).
///
/// # Arguments
/// * `row` - 1-based row number
/// * `col` - 1-based column number
#[no_mangle]
pub extern "C" fn qb_locate(row: i32, col: i32) {
    // ANSI escape sequence for cursor positioning
    print!("\x1B[{};{}H", row, col);
    let _ = io::stdout().flush();
}

/// Set text color (COLOR).
///
/// # Arguments
/// * `foreground` - Foreground color (0-15)
/// * `background` - Background color (0-15, -1 to keep current)
#[no_mangle]
pub extern "C" fn qb_color(foreground: i32, background: i32) {
    // Map BASIC colors to ANSI colors
    let fg_ansi = basic_to_ansi_color(foreground);

    if background >= 0 {
        let bg_ansi = basic_to_ansi_color(background) + 10; // Background codes are +10
        print!("\x1B[{};{}m", fg_ansi, bg_ansi);
    } else {
        print!("\x1B[{}m", fg_ansi);
    }
    let _ = io::stdout().flush();
}

/// Convert BASIC color number to ANSI color code.
fn basic_to_ansi_color(color: i32) -> i32 {
    // BASIC colors 0-7 map to ANSI 30-37 (dark)
    // BASIC colors 8-15 map to ANSI 90-97 (bright)
    match color {
        0 => 30,  // Black
        1 => 34,  // Blue
        2 => 32,  // Green
        3 => 36,  // Cyan
        4 => 31,  // Red
        5 => 35,  // Magenta
        6 => 33,  // Brown/Yellow
        7 => 37,  // White (light gray)
        8 => 90,  // Gray (dark gray)
        9 => 94,  // Light Blue
        10 => 92, // Light Green
        11 => 96, // Light Cyan
        12 => 91, // Light Red
        13 => 95, // Light Magenta
        14 => 93, // Yellow
        15 => 97, // Bright White
        _ => 37,  // Default to white
    }
}

/// Reset text attributes to default.
#[no_mangle]
pub extern "C" fn qb_color_reset() {
    print!("\x1B[0m");
    let _ = io::stdout().flush();
}

// ============================================================================
// Keyboard Functions
// ============================================================================

/// Check if a key has been pressed (INKEY$).
///
/// Returns an empty string if no key is pressed, otherwise returns the key.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_inkey() -> *mut QbString {
    // For now, return empty string (non-blocking keyboard input is complex)
    // A full implementation would use platform-specific APIs
    crate::string::qb_string_empty()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_to_ansi_color() {
        assert_eq!(basic_to_ansi_color(0), 30); // Black
        assert_eq!(basic_to_ansi_color(7), 37); // White
        assert_eq!(basic_to_ansi_color(15), 97); // Bright white
    }
}
