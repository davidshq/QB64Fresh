//! I/O Functions for QB64Fresh Runtime
//!
//! This module provides PRINT, INPUT, and file I/O operations.

use super::print::qb_input_string;
use crate::string::{
    qb_string_data, qb_string_from_bytes, qb_string_len, qb_string_retain, QbString,
};
use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::os::raw::c_char;
use std::sync::Mutex;

// ============================================================================
// INPUT Functions
// ============================================================================

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
    // LINE INPUT always prints a newline after input (same_line = 0)
    qb_input_string(prompt, var, 0);
}

// ============================================================================
// _IIF / _IIF$ - inline conditionals (for --runtime external)
// ============================================================================

/// _IIF(cond, true_val, false_val) - inline conditional for numeric values.
///
/// Returns `true_val` if `cond` is non-zero, otherwise `false_val`.
/// Used when generated C calls `qb_iif` instead of an inline definition.
#[no_mangle]
pub extern "C" fn qb_iif(cond: i64, true_val: f64, false_val: f64) -> f64 {
    if cond != 0 {
        true_val
    } else {
        false_val
    }
}

/// _IIF$(cond, true_val, false_val) - inline conditional for string values.
///
/// Returns the `true_val` or `false_val` pointer; does not retain.
/// Caller is responsible for the chosen string's lifetime.
#[no_mangle]
pub extern "C" fn qb_iif_str(
    cond: i64,
    true_val: *mut QbString,
    false_val: *mut QbString,
) -> *mut QbString {
    if cond != 0 {
        true_val
    } else {
        false_val
    }
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

#[cfg(unix)]
mod keyboard {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Mutex;

    /// Tracks whether we've set up raw mode for the terminal
    static RAW_MODE_ACTIVE: AtomicBool = AtomicBool::new(false);

    /// Stores the original termios settings so we can restore them
    static ORIGINAL_TERMIOS: Mutex<Option<libc::termios>> = Mutex::new(None);

    /// Buffer for multi-byte key sequences (like arrow keys)
    static KEY_BUFFER: Mutex<Vec<u8>> = Mutex::new(Vec::new());

    /// Enable raw mode for non-blocking keyboard input.
    ///
    /// This disables canonical mode (line buffering), echo, and enables
    /// non-blocking reads.
    pub fn enable_raw_mode() {
        if RAW_MODE_ACTIVE.load(Ordering::SeqCst) {
            return;
        }

        unsafe {
            let mut termios: libc::termios = std::mem::zeroed();

            // Get current terminal attributes
            if libc::tcgetattr(libc::STDIN_FILENO, &mut termios) != 0 {
                return;
            }

            // Store original settings for restoration
            {
                let mut orig = ORIGINAL_TERMIOS.lock().unwrap();
                *orig = Some(termios);
            }

            // Disable canonical mode (line buffering) and echo
            termios.c_lflag &= !(libc::ICANON | libc::ECHO);

            // Set minimum characters and timeout for non-blocking read
            // VMIN=0, VTIME=0 means read returns immediately with whatever is available
            termios.c_cc[libc::VMIN] = 0;
            termios.c_cc[libc::VTIME] = 0;

            // Apply new settings
            if libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &termios) == 0 {
                RAW_MODE_ACTIVE.store(true, Ordering::SeqCst);

                // Register cleanup on program exit
                extern "C" fn cleanup() {
                    super::keyboard::disable_raw_mode();
                }
                libc::atexit(cleanup);
            }
        }
    }

    /// Disable raw mode and restore original terminal settings.
    pub fn disable_raw_mode() {
        if !RAW_MODE_ACTIVE.load(Ordering::SeqCst) {
            return;
        }

        let orig = ORIGINAL_TERMIOS.lock().unwrap();
        if let Some(termios) = *orig {
            unsafe {
                libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &termios);
            }
            RAW_MODE_ACTIVE.store(false, Ordering::SeqCst);
        }
    }

    /// Check if input is available on stdin without blocking.
    ///
    /// Uses poll() with a timeout of 0 to check immediately.
    pub fn input_available() -> bool {
        // First check our buffer
        {
            let buffer = KEY_BUFFER.lock().unwrap();
            if !buffer.is_empty() {
                return true;
            }
        }

        unsafe {
            let mut pfd = libc::pollfd {
                fd: libc::STDIN_FILENO,
                events: libc::POLLIN,
                revents: 0,
            };

            // Poll with 0 timeout = immediate return
            let result = libc::poll(&mut pfd, 1, 0);
            result > 0 && (pfd.revents & libc::POLLIN) != 0
        }
    }

    /// Read a key from stdin (non-blocking).
    ///
    /// Returns None if no key is available. Returns Some(bytes) where bytes
    /// contains the key code(s). Multi-byte sequences (like escape sequences
    /// for arrow keys) are returned as a single result.
    pub fn read_key() -> Option<Vec<u8>> {
        // Ensure raw mode is active
        enable_raw_mode();

        // Check if we have buffered data
        {
            let mut buffer = KEY_BUFFER.lock().unwrap();
            if !buffer.is_empty() {
                let result = buffer.clone();
                buffer.clear();
                return Some(result);
            }
        }

        // Check if input is available
        if !input_available() {
            return None;
        }

        // Read available bytes
        let mut buf = [0u8; 16];
        let n = unsafe { libc::read(libc::STDIN_FILENO, buf.as_mut_ptr() as *mut _, buf.len()) };

        if n <= 0 {
            return None;
        }

        let bytes = buf[..n as usize].to_vec();

        // Check for escape sequences (arrow keys, function keys, etc.)
        if bytes.len() == 1 && bytes[0] == 27 {
            // Got ESC - might be start of escape sequence, wait briefly for more
            std::thread::sleep(std::time::Duration::from_millis(1));

            if input_available() {
                // Read the rest of the sequence
                let n2 = unsafe {
                    libc::read(libc::STDIN_FILENO, buf.as_mut_ptr() as *mut _, buf.len())
                };
                if n2 > 0 {
                    let mut full_seq = bytes;
                    full_seq.extend_from_slice(&buf[..n2 as usize]);
                    return Some(full_seq);
                }
            }
        }

        Some(bytes)
    }

    /// Convert a key sequence to QB64-compatible INKEY$ result.
    ///
    /// For regular ASCII keys, returns the character.
    /// For special keys (arrows, function keys), returns the two-byte
    /// sequence that QB64 uses: CHR$(0) + scan code, or CHR$(255) + code.
    pub fn key_to_inkey_string(key: &[u8]) -> Vec<u8> {
        match key {
            // Arrow keys - ESC [ A/B/C/D
            [27, 91, 65] => vec![0, 72], // Up arrow: CHR$(0) + CHR$(72)
            [27, 91, 66] => vec![0, 80], // Down arrow: CHR$(0) + CHR$(80)
            [27, 91, 67] => vec![0, 77], // Right arrow: CHR$(0) + CHR$(77)
            [27, 91, 68] => vec![0, 75], // Left arrow: CHR$(0) + CHR$(75)

            // Home/End/Insert/Delete/PageUp/PageDown
            [27, 91, 72] => vec![0, 71],      // Home: CHR$(0) + CHR$(71)
            [27, 91, 70] => vec![0, 79],      // End: CHR$(0) + CHR$(79)
            [27, 91, 50, 126] => vec![0, 82], // Insert: CHR$(0) + CHR$(82)
            [27, 91, 51, 126] => vec![0, 83], // Delete: CHR$(0) + CHR$(83)
            [27, 91, 53, 126] => vec![0, 73], // Page Up: CHR$(0) + CHR$(73)
            [27, 91, 54, 126] => vec![0, 81], // Page Down: CHR$(0) + CHR$(81)

            // Function keys F1-F4 (common escape sequences)
            [27, 79, 80] => vec![0, 59], // F1: CHR$(0) + CHR$(59)
            [27, 79, 81] => vec![0, 60], // F2: CHR$(0) + CHR$(60)
            [27, 79, 82] => vec![0, 61], // F3: CHR$(0) + CHR$(61)
            [27, 79, 83] => vec![0, 62], // F4: CHR$(0) + CHR$(62)

            // Function keys F5-F12 (CSI sequences)
            [27, 91, 49, 53, 126] => vec![0, 63],  // F5
            [27, 91, 49, 55, 126] => vec![0, 64],  // F6
            [27, 91, 49, 56, 126] => vec![0, 65],  // F7
            [27, 91, 49, 57, 126] => vec![0, 66],  // F8
            [27, 91, 50, 48, 126] => vec![0, 67],  // F9
            [27, 91, 50, 49, 126] => vec![0, 68],  // F10
            [27, 91, 50, 51, 126] => vec![0, 133], // F11
            [27, 91, 50, 52, 126] => vec![0, 134], // F12

            // Single ESC key
            [27] => vec![27],

            // Backspace (often sent as DEL=127 or BS=8)
            [127] => vec![8], // Convert DEL to backspace
            [8] => vec![8],   // Keep backspace as-is

            // Enter key
            [10] | [13] => vec![13], // Convert LF to CR for consistency

            // Regular single-byte characters
            [c] if *c < 128 => vec![*c],

            // Multi-byte UTF-8 or other sequences - return as-is
            _ => key.to_vec(),
        }
    }
}

#[cfg(windows)]
mod keyboard {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Mutex;

    static KEY_BUFFER: Mutex<Vec<u8>> = Mutex::new(Vec::new());
    static RAW_MODE_ACTIVE: AtomicBool = AtomicBool::new(false);

    // Windows console API functions from <conio.h>
    // These are part of the C runtime library and are automatically linked
    // on both MSVC and MinGW toolchains
    extern "C" {
        fn _kbhit() -> i32;
        fn _getch() -> i32;
    }

    pub fn enable_raw_mode() {
        // On Windows, console is already in a mode suitable for _kbhit/_getch
        // No special setup needed - these functions work with the console as-is
        RAW_MODE_ACTIVE.store(true, Ordering::SeqCst);
    }

    pub fn disable_raw_mode() {
        // No cleanup needed for Windows console
        RAW_MODE_ACTIVE.store(false, Ordering::SeqCst);
    }

    pub fn input_available() -> bool {
        unsafe { _kbhit() != 0 }
    }

    pub fn read_key() -> Option<Vec<u8>> {
        // Check if we have buffered data
        {
            let mut buffer = KEY_BUFFER.lock().unwrap();
            if !buffer.is_empty() {
                let result = buffer.clone();
                buffer.clear();
                return Some(result);
            }
        }

        // Check if input is available
        if !input_available() {
            return None;
        }

        unsafe {
            let ch = _getch();

            // Handle extended keys (function keys, arrows, etc.)
            // Windows returns 0 or 224 for extended keys, followed by the scan code
            if ch == 0 || ch == 224 {
                let ext = _getch();
                // Return as two-byte sequence: [0, scan_code]
                return Some(vec![0, ext as u8]);
            }

            // Regular character
            Some(vec![ch as u8])
        }
    }

    pub fn key_to_inkey_string(key: &[u8]) -> Vec<u8> {
        // Windows scan codes match QB64 conventions
        match key {
            // Extended keys: [0, scan_code]
            [0, code] => {
                // Map Windows scan codes to QB64 key codes
                let qb_code = match *code {
                    72 => 72,   // Up arrow
                    80 => 80,   // Down arrow
                    75 => 75,   // Left arrow
                    77 => 77,   // Right arrow
                    71 => 71,   // Home
                    79 => 79,   // End
                    82 => 82,   // Insert
                    83 => 83,   // Delete
                    73 => 73,   // Page Up
                    81 => 81,   // Page Down
                    59 => 59,   // F1
                    60 => 60,   // F2
                    61 => 61,   // F3
                    62 => 62,   // F4
                    63 => 63,   // F5
                    64 => 64,   // F6
                    65 => 65,   // F7
                    66 => 66,   // F8
                    67 => 67,   // F9
                    68 => 68,   // F10
                    _ => *code, // Use scan code as-is
                };
                vec![0, qb_code]
            }
            // Regular single-byte characters
            [c] => vec![*c],
            // Multi-byte sequences (shouldn't happen, but return as-is)
            _ => key.to_vec(),
        }
    }
}

/// Check if a key has been pressed (INKEY$).
///
/// Returns an empty string if no key is pressed, otherwise returns the key.
/// For special keys (arrows, function keys), returns a two-byte sequence
/// compatible with QBasic/QB64 conventions.
///
/// # Key Codes
///
/// - Regular ASCII characters: returned as-is
/// - Arrow keys: CHR$(0) + scan code
///   - Up: CHR$(0) + CHR$(72)
///   - Down: CHR$(0) + CHR$(80)
///   - Left: CHR$(0) + CHR$(75)
///   - Right: CHR$(0) + CHR$(77)
/// - Function keys: CHR$(0) + scan code (F1=59, F2=60, etc.)
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_inkey() -> *mut QbString {
    if let Some(key) = keyboard::read_key() {
        let inkey_bytes = keyboard::key_to_inkey_string(&key);
        if !inkey_bytes.is_empty() {
            return unsafe { qb_string_from_bytes(inkey_bytes.as_ptr(), inkey_bytes.len()) };
        }
    }

    crate::string::qb_string_empty()
}

/// Get a key code for _KEYHIT function.
///
/// Returns 0 if no key is pressed, otherwise returns the key code.
/// For special keys, returns negative codes.
///
/// # Safety
/// This function is safe to call from C.
#[no_mangle]
pub extern "C" fn qb_keyhit() -> i64 {
    if let Some(key) = keyboard::read_key() {
        let inkey_bytes = keyboard::key_to_inkey_string(&key);
        match inkey_bytes.len() {
            1 => inkey_bytes[0] as i64,
            2 if inkey_bytes[0] == 0 => {
                // Extended key: return as negative
                -(inkey_bytes[1] as i64 * 256)
            }
            _ => inkey_bytes.first().copied().unwrap_or(0) as i64,
        }
    } else {
        0
    }
}

/// Check if a specific key is currently pressed (_KEYDOWN).
///
/// Uses SDL2 keyboard state when graphics backend is initialized.
/// Falls back to returning 0 (not pressed) if graphics is not available.
///
/// # Arguments
/// - `keycode`: The QB64 keycode to check
///
/// # Returns
/// - -1 (true) if the key is pressed
/// - 0 (false) if not pressed
#[no_mangle]
pub extern "C" fn qb_keydown(keycode: i64) -> i32 {
    // Try graphics backend first (SDL2 keyboard state)
    unsafe {
        if let Some(ref backend) = crate::graphics::GRAPHICS_BACKEND {
            if backend.is_key_pressed(keycode) {
                return -1;
            }
        }
    }
    // Fallback: return 0 (not pressed) if graphics backend unavailable
    0
}

/// Clear the keyboard buffer (_KEYCLEAR).
#[no_mangle]
pub extern "C" fn qb_keyclear() {
    // Clear any buffered input
    keyboard::enable_raw_mode();
    while keyboard::input_available() {
        let _ = keyboard::read_key();
    }
}

/// Disable raw terminal mode (call at program end).
///
/// This restores normal terminal behavior (line buffering, echo).
#[no_mangle]
pub extern "C" fn qb_keyboard_shutdown() {
    keyboard::disable_raw_mode();
}

// ============================================================================
// System Integration Functions (Phase 5)
// ============================================================================

/// Normalize Windows path separators on non-Windows: `\` → `/`.
/// On Windows, returns the path unchanged.
#[cfg(not(target_os = "windows"))]
fn normalize_path_for_fs(s: &str) -> std::borrow::Cow<'_, str> {
    if s.contains('\\') {
        std::borrow::Cow::Owned(s.replace('\\', "/"))
    } else {
        std::borrow::Cow::Borrowed(s)
    }
}

#[cfg(target_os = "windows")]
fn normalize_path_for_fs(s: &str) -> std::borrow::Cow<'_, str> {
    std::borrow::Cow::Borrowed(s)
}

/// KILL - Delete a file.
///
/// On non-Windows, normalizes `\` to `/` in the path so Windows-style paths in
/// BASIC source work when the program runs on Unix-like systems.
///
/// # Safety
/// - `filename` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_file_kill(filename: *const c_char) -> i32 {
    if filename.is_null() {
        return 1; // Error
    }

    let path_str = match std::ffi::CStr::from_ptr(filename).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };
    let normalized = normalize_path_for_fs(path_str);
    let path = std::path::Path::new(normalized.as_ref());

    match std::fs::remove_file(path) {
        Ok(()) => 0, // Success
        Err(_) => 1, // Error
    }
}

/// NAME AS - Rename a file.
///
/// On non-Windows, normalizes `\` to `/` in both paths so Windows-style paths
/// in BASIC source work when the program runs on Unix-like systems.
///
/// # Safety
/// - Both `old_name` and `new_name` must be valid null-terminated C strings
#[no_mangle]
pub unsafe extern "C" fn qb_file_rename(old_name: *const c_char, new_name: *const c_char) -> i32 {
    if old_name.is_null() || new_name.is_null() {
        return 1;
    }

    let old_str = match std::ffi::CStr::from_ptr(old_name).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };

    let new_str = match std::ffi::CStr::from_ptr(new_name).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };

    let old_n = normalize_path_for_fs(old_str);
    let new_n = normalize_path_for_fs(new_str);
    let old_p = std::path::Path::new(old_n.as_ref());
    let new_p = std::path::Path::new(new_n.as_ref());

    match std::fs::rename(old_p, new_p) {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// MKDIR - Create a directory.
///
/// On non-Windows, normalizes `\` to `/` in the path so Windows-style paths in
/// BASIC source work when the program runs on Unix-like systems.
///
/// # Safety
/// - `path` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_mkdir(path: *const c_char) -> i32 {
    if path.is_null() {
        return 1;
    }

    let path_str = match std::ffi::CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };
    let normalized = normalize_path_for_fs(path_str);
    let p = std::path::Path::new(normalized.as_ref());

    match std::fs::create_dir(p) {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// RMDIR - Remove a directory.
///
/// On non-Windows, normalizes `\` to `/` in the path so Windows-style paths in
/// BASIC source work when the program runs on Unix-like systems.
///
/// # Safety
/// - `path` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_rmdir(path: *const c_char) -> i32 {
    if path.is_null() {
        return 1;
    }

    let path_str = match std::ffi::CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };
    let normalized = normalize_path_for_fs(path_str);
    let p = std::path::Path::new(normalized.as_ref());

    match std::fs::remove_dir(p) {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// CHDIR - Change current directory.
///
/// On non-Windows, normalizes `\` to `/` in the path so Windows-style paths in
/// BASIC source work when the program runs on Unix-like systems.
///
/// # Safety
/// - `path` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_chdir(path: *const c_char) -> i32 {
    if path.is_null() {
        return 1;
    }

    let path_str = match std::ffi::CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };
    let normalized = normalize_path_for_fs(path_str);
    let p = std::path::Path::new(normalized.as_ref());

    match std::env::set_current_dir(p) {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// SHELL - Execute an external command.
///
/// If `command` is NULL, opens an interactive shell.
///
/// # Safety
/// - `command` must be a valid null-terminated C string or NULL
#[no_mangle]
pub unsafe extern "C" fn qb_shell(command: *const c_char) -> i32 {
    if command.is_null() {
        // Open interactive shell
        #[cfg(target_os = "windows")]
        {
            match std::process::Command::new("cmd").status() {
                Ok(status) => status.code().unwrap_or(1),
                Err(_) => 1,
            }
        }
        #[cfg(not(target_os = "windows"))]
        {
            match std::process::Command::new("sh").status() {
                Ok(status) => status.code().unwrap_or(1),
                Err(_) => 1,
            }
        }
    } else {
        let cmd_str = match std::ffi::CStr::from_ptr(command).to_str() {
            Ok(s) => s,
            Err(_) => return 1,
        };

        #[cfg(target_os = "windows")]
        {
            match std::process::Command::new("cmd")
                .args(["/C", cmd_str])
                .status()
            {
                Ok(status) => status.code().unwrap_or(1),
                Err(_) => 1,
            }
        }
        #[cfg(not(target_os = "windows"))]
        {
            match std::process::Command::new("sh")
                .args(["-c", cmd_str])
                .status()
            {
                Ok(status) => status.code().unwrap_or(1),
                Err(_) => 1,
            }
        }
    }
}

/// _SHELLHIDE - Execute a command without showing console window.
///
/// # Safety
/// - `command` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_shell_hide(command: *const c_char) -> i32 {
    if command.is_null() {
        return 1;
    }

    let cmd_str = match std::ffi::CStr::from_ptr(command).to_str() {
        Ok(s) => s,
        Err(_) => return 1,
    };

    #[cfg(target_os = "windows")]
    {
        use std::os::windows::process::CommandExt;
        const CREATE_NO_WINDOW: u32 = 0x08000000;

        match std::process::Command::new("cmd")
            .args(["/C", cmd_str])
            .creation_flags(CREATE_NO_WINDOW)
            .status()
        {
            Ok(status) => status.code().unwrap_or(1),
            Err(_) => 1,
        }
    }
    #[cfg(not(target_os = "windows"))]
    {
        // On Unix, there's no concept of hidden console
        // Just run the command normally
        match std::process::Command::new("sh")
            .args(["-c", cmd_str])
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
        {
            Ok(status) => status.code().unwrap_or(1),
            Err(_) => 1,
        }
    }
}

/// ENVIRON statement - Set an environment variable.
///
/// Sets an environment variable for the current process.
/// The argument should be a string in the format "name=value".
///
/// # Safety
/// - `env` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_sub_environ(env: *mut QbString) {
    if env.is_null() {
        return;
    }

    let env_data = qb_string_data(env);
    if env_data.is_null() {
        return;
    }

    let env_str = match std::ffi::CStr::from_ptr(env_data).to_str() {
        Ok(s) => s,
        Err(_) => return,
    };

    // Parse "name=value" format
    if let Some(eq_pos) = env_str.find('=') {
        // Validate: name must not be empty
        if eq_pos == 0 {
            return; // Invalid: name is empty
        }
        let name = &env_str[..eq_pos];
        let value = &env_str[eq_pos + 1..];
        std::env::set_var(name, value);
    }
    // If no '=' found, ignore (invalid format)
}

/// _FILEEXISTS - Check if a file exists.
///
/// Returns -1 (true) if file exists, 0 (false) otherwise.
///
/// On non-Windows, normalizes `\` to `/` in the path so Windows-style paths in
/// BASIC source work when the program runs on Unix-like systems.
///
/// # Safety
/// - `path` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_file_exists(path: *const QbString) -> i32 {
    if path.is_null() {
        return 0;
    }

    let path_ptr = qb_string_data(path);
    if path_ptr.is_null() {
        return 0;
    }

    let path_str = match std::ffi::CStr::from_ptr(path_ptr).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };
    let normalized = normalize_path_for_fs(path_str);
    let p = std::path::Path::new(normalized.as_ref());

    if p.exists() && p.is_file() {
        -1 // True in BASIC
    } else {
        0 // False
    }
}

/// _DIREXISTS - Check if a directory exists.
///
/// Returns -1 (true) if directory exists, 0 (false) otherwise.
///
/// On non-Windows, normalizes `\` to `/` in the path so Windows-style paths in
/// BASIC source work when the program runs on Unix-like systems.
///
/// # Safety
/// - `path` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_dir_exists(path: *const QbString) -> i32 {
    if path.is_null() {
        return 0;
    }

    let path_ptr = qb_string_data(path);
    if path_ptr.is_null() {
        return 0;
    }

    let path_str = match std::ffi::CStr::from_ptr(path_ptr).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };
    let normalized = normalize_path_for_fs(path_str);
    let p = std::path::Path::new(normalized.as_ref());

    if p.exists() && p.is_dir() {
        -1 // True in BASIC
    } else {
        0 // False
    }
}

/// _DIR$ - Get next file in directory listing.
///
/// First call with a filespec (e.g., "*.txt"), subsequent calls with empty string.
///
/// # Safety
/// - `spec` must be a valid QbString pointer or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_dir(spec: *const QbString) -> *mut QbString {
    // This is a simplified implementation using a static iterator
    // A full implementation would need to handle the iterator state properly
    use std::sync::Mutex;

    static DIR_STATE: Mutex<Option<std::vec::IntoIter<String>>> = Mutex::new(None);

    let mut state = DIR_STATE.lock().unwrap();

    if spec.is_null() {
        return qb_string_from_bytes(std::ptr::null(), 0);
    }

    let spec_ptr = qb_string_data(spec);
    if spec_ptr.is_null() {
        return qb_string_from_bytes(std::ptr::null(), 0);
    }

    let spec_str = match std::ffi::CStr::from_ptr(spec_ptr).to_str() {
        Ok(s) => s,
        Err(_) => return qb_string_from_bytes(std::ptr::null(), 0),
    };

    // If spec is not empty, start a new listing
    if !spec_str.is_empty() {
        let pattern = std::path::Path::new(spec_str);
        let dir = pattern.parent().unwrap_or(std::path::Path::new("."));

        if let Ok(entries) = std::fs::read_dir(dir) {
            let names: Vec<String> = entries
                .filter_map(|e| e.ok())
                .filter_map(|e| {
                    let name = e.file_name().to_string_lossy().to_string();
                    // Simple glob matching (just * for now)
                    if let Some(file_name) = pattern.file_name() {
                        let pat = file_name.to_string_lossy();
                        if pat == "*" || pat == "*.*" || name.contains(&pat.replace('*', "")) {
                            Some(name)
                        } else {
                            None
                        }
                    } else {
                        Some(name)
                    }
                })
                .collect();

            *state = Some(names.into_iter());
        } else {
            *state = None;
            return qb_string_from_bytes(std::ptr::null(), 0);
        }
    }

    // Return next entry from iterator
    if let Some(ref mut iter) = *state {
        if let Some(name) = iter.next() {
            return qb_string_from_bytes(name.as_ptr(), name.len());
        }
    }

    qb_string_from_bytes(std::ptr::null(), 0)
}

// ============================================================================
// Networking Functions (Phase 5)
// ============================================================================

/// Buffered stream wrapper for network connections.
///
/// TCP streams may return partial data on read, so we buffer incoming data
/// to allow QB64's `GET #` to read exact byte counts. The buffer grows as
/// data arrives and shrinks as data is consumed.
struct BufferedStream {
    /// The underlying TCP stream.
    stream: TcpStream,
    /// Input buffer for received data.
    in_buffer: Vec<u8>,
    /// True if the connection has been closed by the remote end.
    eof: bool,
}

impl BufferedStream {
    /// Creates a new buffered stream wrapping a TCP stream.
    fn new(stream: TcpStream) -> Self {
        Self {
            stream,
            in_buffer: Vec::with_capacity(4096),
            eof: false,
        }
    }

    /// Reads any available data from the stream into the buffer (non-blocking).
    ///
    /// Returns the number of bytes read, or 0 if no data available.
    fn update(&mut self) -> usize {
        if self.eof {
            return 0;
        }

        let mut temp = [0u8; 4096];
        match self.stream.read(&mut temp) {
            Ok(0) => {
                // EOF - connection closed by remote
                self.eof = true;
                0
            }
            Ok(n) => {
                self.in_buffer.extend_from_slice(&temp[..n]);
                n
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                // No data available right now
                0
            }
            Err(_) => {
                // Error - mark as EOF
                self.eof = true;
                0
            }
        }
    }

    /// Returns the number of bytes available in the buffer.
    fn available(&self) -> usize {
        self.in_buffer.len()
    }

    /// Returns true if EOF has been reached.
    fn is_eof(&self) -> bool {
        self.eof && self.in_buffer.is_empty()
    }

    /// Reads up to `size` bytes from the buffer into `data`.
    ///
    /// Returns the number of bytes actually read.
    fn read(&mut self, data: &mut [u8], size: usize) -> usize {
        // First, try to get more data from the stream
        self.update();

        let to_read = size.min(self.in_buffer.len()).min(data.len());
        if to_read > 0 {
            data[..to_read].copy_from_slice(&self.in_buffer[..to_read]);
            self.in_buffer.drain(..to_read);
        }
        to_read
    }

    /// Writes data to the stream.
    ///
    /// Returns the number of bytes written, or 0 on error.
    fn write(&mut self, data: &[u8]) -> usize {
        match self.stream.write_all(data) {
            Ok(()) => {
                let _ = self.stream.flush();
                data.len()
            }
            Err(_) => 0,
        }
    }
}

/// Network handle types
enum NetHandle {
    /// TCP server listener
    Host(TcpListener),
    /// TCP connection (client or accepted) with buffered I/O
    Connection(BufferedStream),
}

/// Global network handle storage
/// Handles are negative numbers to distinguish from file handles
static NET_HANDLES: Mutex<Option<HashMap<i64, NetHandle>>> = Mutex::new(None);
static NET_NEXT_HANDLE: Mutex<i64> = Mutex::new(-1);

/// Initialize the network handle storage if needed
fn init_net_handles() {
    let mut handles = NET_HANDLES.lock().unwrap();
    if handles.is_none() {
        *handles = Some(HashMap::new());
    }
}

/// Get the next available network handle (negative numbers)
fn next_net_handle() -> i64 {
    let mut handle = NET_NEXT_HANDLE.lock().unwrap();
    let h = *handle;
    *handle -= 1;
    h
}

/// _OPENHOST - Open a TCP server on a port.
///
/// Returns a negative handle on success, 0 on failure.
///
/// # Arguments
/// - `connection_string`: Connection string like "TCP/IP:portNumber" or just port number as string
#[no_mangle]
pub extern "C" fn qb_net_openhost(connection_string: *const libc::c_char) -> i64 {
    init_net_handles();

    // Parse connection string (e.g., "TCP/IP:8080" -> extract port 8080)
    let c_str = unsafe {
        if connection_string.is_null() {
            return 0;
        }
        std::ffi::CStr::from_ptr(connection_string)
    };
    let conn_str = match c_str.to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };

    // Extract port number from connection string
    // Format: "TCP/IP:portNumber" or just "portNumber"
    let port_str = if let Some(colon_pos) = conn_str.rfind(':') {
        &conn_str[colon_pos + 1..]
    } else {
        conn_str
    };

    let port: i64 = match port_str.parse() {
        Ok(p) => p,
        Err(_) => return 0,
    };

    let addr = format!("0.0.0.0:{}", port);
    match TcpListener::bind(&addr) {
        Ok(listener) => {
            // Set non-blocking so _OPENCONNECTION doesn't block
            if listener.set_nonblocking(true).is_err() {
                return 0;
            }

            let handle = next_net_handle();
            let mut handles = NET_HANDLES.lock().unwrap();
            if let Some(ref mut map) = *handles {
                map.insert(handle, NetHandle::Host(listener));
            }
            handle
        }
        Err(_) => 0,
    }
}

/// _OPENCONNECTION - Accept an incoming connection on a host.
///
/// Returns a negative handle on success, 0 if no connection waiting.
/// This is non-blocking - returns immediately if no client is connecting.
///
/// # Arguments
/// - `host_handle`: The handle returned by _OPENHOST
#[no_mangle]
pub extern "C" fn qb_net_openconnection(host_handle: i64) -> i64 {
    init_net_handles();

    let mut handles = NET_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(NetHandle::Host(listener)) = map.get(&host_handle) {
            // Non-blocking accept
            match listener.accept() {
                Ok((stream, _addr)) => {
                    // Set the stream to non-blocking for I/O
                    if stream.set_nonblocking(true).is_err() {
                        return 0;
                    }

                    let handle = next_net_handle();
                    map.insert(handle, NetHandle::Connection(BufferedStream::new(stream)));
                    return handle;
                }
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    // No connection waiting - not an error
                    return 0;
                }
                Err(_) => return 0,
            }
        }
    }
    0
}

/// _OPENCLIENT - Connect to a TCP server.
///
/// Connection string format: "TCP/IP:port:address"
/// Example: "TCP/IP:8080:localhost" or "TCP/IP:80:192.168.1.1"
///
/// Returns a negative handle on success, 0 on failure.
///
/// # Safety
/// - `connection_string` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_net_openclient(connection_string: *const c_char) -> i64 {
    init_net_handles();

    if connection_string.is_null() {
        return 0;
    }

    let conn_str = match std::ffi::CStr::from_ptr(connection_string).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };

    // Parse connection string: "TCP/IP:port:address"
    let parts: Vec<&str> = conn_str.split(':').collect();
    if parts.len() != 3 {
        return 0;
    }

    let protocol = parts[0].to_uppercase();
    if protocol != "TCP/IP" {
        return 0; // Only TCP/IP supported
    }

    let port: u16 = match parts[1].parse() {
        Ok(p) => p,
        Err(_) => return 0,
    };

    let address = parts[2];
    let addr = format!("{}:{}", address, port);

    match TcpStream::connect(&addr) {
        Ok(stream) => {
            // Set non-blocking for I/O operations
            if stream.set_nonblocking(true).is_err() {
                return 0;
            }

            let handle = next_net_handle();
            let mut handles = NET_HANDLES.lock().unwrap();
            if let Some(ref mut map) = *handles {
                map.insert(handle, NetHandle::Connection(BufferedStream::new(stream)));
            }
            handle
        }
        Err(_) => 0,
    }
}

/// _CONNECTED - Check if a network connection is still active.
///
/// Returns -1 (true) if connected, 0 (false) if disconnected or invalid handle.
///
/// # Arguments
/// - `handle`: The network handle to check
#[no_mangle]
pub extern "C" fn qb_net_connected(handle: i64) -> i32 {
    init_net_handles();

    let mut handles = NET_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(NetHandle::Connection(ref mut buf_stream)) = map.get_mut(&handle) {
            // If we already know it's EOF, return disconnected
            if buf_stream.is_eof() {
                return 0;
            }

            // Try to update the buffer - this will detect EOF
            buf_stream.update();

            // If EOF now, return disconnected (but only if buffer is empty)
            if buf_stream.is_eof() {
                return 0;
            }

            // Still connected
            return -1;
        } else if map.contains_key(&handle) {
            // It's a host handle, which is always "connected" while open
            return -1;
        }
    }
    0 // Invalid handle
}

// ============================================================================
// Network I/O Functions (PUT/GET for network handles)
// ============================================================================

/// Read binary data from a network connection (GET #).
///
/// Reads up to `size` bytes into the provided buffer. May read fewer bytes
/// if not enough data is available in the buffer.
///
/// # Safety
/// - `data` must be a valid pointer to a buffer of at least `size` bytes
#[no_mangle]
pub unsafe extern "C" fn qb_net_get(handle: i64, data: *mut u8, size: usize) -> usize {
    if data.is_null() || size == 0 {
        return 0;
    }

    init_net_handles();

    let mut handles = NET_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(NetHandle::Connection(ref mut buf_stream)) = map.get_mut(&handle) {
            let slice = std::slice::from_raw_parts_mut(data, size);
            return buf_stream.read(slice, size);
        }
    }
    0
}

/// Write binary data to a network connection (PUT #).
///
/// Writes `size` bytes from the provided buffer to the connection.
///
/// # Safety
/// - `data` must be a valid pointer to a buffer of at least `size` bytes
#[no_mangle]
pub unsafe extern "C" fn qb_net_put(handle: i64, data: *const u8, size: usize) -> usize {
    if data.is_null() || size == 0 {
        return 0;
    }

    init_net_handles();

    let mut handles = NET_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(NetHandle::Connection(ref mut buf_stream)) = map.get_mut(&handle) {
            let slice = std::slice::from_raw_parts(data, size);
            return buf_stream.write(slice);
        }
    }
    0
}

/// Read a string from a network connection (GET # for strings).
///
/// Reads into the string's existing buffer (fixed-length string or pre-sized).
///
/// # Safety
/// - `s` must be a valid QbString pointer
#[no_mangle]
pub unsafe extern "C" fn qb_net_get_string(handle: i64, s: *mut QbString) -> usize {
    if s.is_null() {
        return 0;
    }

    let data_ptr = qb_string_data(s);
    let len = qb_string_len(s);

    if data_ptr.is_null() || len == 0 {
        return 0;
    }

    qb_net_get(handle, data_ptr as *mut u8, len)
}

/// Write a string to a network connection (PUT # for strings).
///
/// Writes the entire string content to the connection.
///
/// # Safety
/// - `s` must be a valid QbString pointer
#[no_mangle]
pub unsafe extern "C" fn qb_net_put_string(handle: i64, s: *const QbString) -> usize {
    if s.is_null() {
        return 0;
    }

    let data_ptr = qb_string_data(s);
    let len = qb_string_len(s);

    if data_ptr.is_null() || len == 0 {
        return 0;
    }

    qb_net_put(handle, data_ptr as *const u8, len)
}

/// Check if EOF has been reached on a network connection.
///
/// Returns -1 (true) if EOF, 0 (false) if more data may be available.
#[no_mangle]
pub extern "C" fn qb_net_eof(handle: i64) -> i32 {
    init_net_handles();

    let mut handles = NET_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(NetHandle::Connection(ref mut buf_stream)) = map.get_mut(&handle) {
            // Update buffer to check for new data
            buf_stream.update();
            return if buf_stream.is_eof() { -1 } else { 0 };
        }
    }
    -1 // Invalid handle = EOF
}

/// Return the number of bytes available in the network buffer (LOF for networks).
///
/// For network handles, this returns the amount of buffered input data.
#[no_mangle]
pub extern "C" fn qb_net_lof(handle: i64) -> i64 {
    init_net_handles();

    let mut handles = NET_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(NetHandle::Connection(ref mut buf_stream)) = map.get_mut(&handle) {
            // Update buffer to get latest data
            buf_stream.update();
            return buf_stream.available() as i64;
        }
    }
    0
}

/// Close a network handle (internal helper).
///
/// Called when CLOSE is used on a network handle.
#[no_mangle]
pub extern "C" fn qb_net_close(handle: i64) {
    init_net_handles();

    let mut handles = NET_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        map.remove(&handle);
        // TcpListener and TcpStream are automatically closed when dropped
    }
}

// ============================================================================
// Date/Time and Directory Functions
// ============================================================================

/// DATE$ - Returns date in MM-DD-YYYY format (classic QBasic format).
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_date() -> *mut QbString {
    use std::time::SystemTime;
    let now = SystemTime::now();
    let duration = now
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = duration.as_secs();

    unsafe {
        let tm = libc::localtime(&(secs as i64));
        if tm.is_null() {
            return crate::string::qb_string_empty();
        }
        let mut buf = [0u8; 16];
        libc::strftime(
            buf.as_mut_ptr() as *mut libc::c_char,
            buf.len(),
            b"%m-%d-%Y\0".as_ptr() as *const libc::c_char,
            tm,
        );
        crate::string::qb_string_new(buf.as_ptr() as *const c_char)
    }
}

/// TIME$ - Returns time in HH:MM:SS format.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_time() -> *mut QbString {
    use std::time::SystemTime;
    let now = SystemTime::now();
    let duration = now
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = duration.as_secs();

    unsafe {
        let tm = libc::localtime(&(secs as i64));
        if tm.is_null() {
            return crate::string::qb_string_empty();
        }
        let mut buf = [0u8; 16];
        libc::strftime(
            buf.as_mut_ptr() as *mut libc::c_char,
            buf.len(),
            b"%H:%M:%S\0".as_ptr() as *const libc::c_char,
            tm,
        );
        crate::string::qb_string_new(buf.as_ptr() as *const c_char)
    }
}

/// _DATE$ - Returns date in YYYY-MM-DD format (QB64 format).
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_date64() -> *mut QbString {
    use std::time::SystemTime;
    let now = SystemTime::now();
    let duration = now
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = duration.as_secs();

    unsafe {
        let tm = libc::localtime(&(secs as i64));
        if tm.is_null() {
            return crate::string::qb_string_empty();
        }
        let mut buf = [0u8; 16];
        libc::strftime(
            buf.as_mut_ptr() as *mut libc::c_char,
            buf.len(),
            b"%Y-%m-%d\0".as_ptr() as *const libc::c_char,
            tm,
        );
        crate::string::qb_string_new(buf.as_ptr() as *const c_char)
    }
}

/// _TIME$ - Returns time in HH:MM:SS format (same as TIME$ but for consistency).
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_time64() -> *mut QbString {
    qb_time() // Same as TIME$
}

/// _CWD$ - Returns current working directory.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_cwd() -> *mut QbString {
    match std::env::current_dir() {
        Ok(path) => {
            let path_str = path.to_string_lossy();
            unsafe { crate::string::qb_string_new(path_str.as_ptr() as *const c_char) }
        }
        Err(_) => crate::string::qb_string_empty(),
    }
}

/// _OS$ - Returns operating system string in QB64 format: [PLATFORM][BITS].
///
/// Examples: "[LINUX][64BIT]", "[WINDOWS][64BIT]", "[MACOSX][64BIT]"
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_os() -> *mut QbString {
    let os_str = if cfg!(target_os = "windows") {
        if cfg!(target_arch = "x86_64") || cfg!(target_arch = "aarch64") {
            "[WINDOWS][64BIT]"
        } else {
            "[WINDOWS][32BIT]"
        }
    } else if cfg!(target_os = "macos") {
        if cfg!(target_arch = "x86_64") || cfg!(target_arch = "aarch64") {
            "[MACOSX][64BIT]"
        } else {
            "[MACOSX][32BIT]"
        }
    } else {
        // Assume Linux
        if cfg!(target_arch = "x86_64") || cfg!(target_arch = "aarch64") {
            "[LINUX][64BIT]"
        } else {
            "[LINUX][32BIT]"
        }
    };
    unsafe { crate::string::qb_string_new(os_str.as_ptr() as *const c_char) }
}
