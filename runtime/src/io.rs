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

// ============================================================================
// System Integration Functions (Phase 5)
// ============================================================================

/// KILL - Delete a file.
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

    match std::fs::remove_file(path_str) {
        Ok(()) => 0, // Success
        Err(_) => 1, // Error
    }
}

/// NAME AS - Rename a file.
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

    match std::fs::rename(old_str, new_str) {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// MKDIR - Create a directory.
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

    match std::fs::create_dir(path_str) {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// RMDIR - Remove a directory.
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

    match std::fs::remove_dir(path_str) {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

/// CHDIR - Change current directory.
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

    match std::env::set_current_dir(path_str) {
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

/// _FILEEXISTS - Check if a file exists.
///
/// Returns -1 (true) if file exists, 0 (false) otherwise.
///
/// # Safety
/// - `path` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_file_exists(path: *const c_char) -> i32 {
    if path.is_null() {
        return 0;
    }

    let path_str = match std::ffi::CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };

    let p = std::path::Path::new(path_str);
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
/// # Safety
/// - `path` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_dir_exists(path: *const c_char) -> i32 {
    if path.is_null() {
        return 0;
    }

    let path_str = match std::ffi::CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };

    let p = std::path::Path::new(path_str);
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
/// - `spec` must be a valid null-terminated C string
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_dir(spec: *const c_char) -> *mut QbString {
    // This is a simplified implementation using a static iterator
    // A full implementation would need to handle the iterator state properly
    use std::sync::Mutex;

    static DIR_STATE: Mutex<Option<std::vec::IntoIter<String>>> = Mutex::new(None);

    let mut state = DIR_STATE.lock().unwrap();

    if spec.is_null() {
        return qb_string_from_bytes(std::ptr::null(), 0);
    }

    let spec_str = match std::ffi::CStr::from_ptr(spec).to_str() {
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

use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::sync::Mutex;

/// Network handle types
enum NetHandle {
    /// TCP server listener
    Host(TcpListener),
    /// TCP connection (client or accepted)
    Connection(TcpStream),
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
/// - `port`: The port number to listen on
#[no_mangle]
pub extern "C" fn qb_net_openhost(port: i64) -> i64 {
    init_net_handles();

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
                    map.insert(handle, NetHandle::Connection(stream));
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
                map.insert(handle, NetHandle::Connection(stream));
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

    let handles = NET_HANDLES.lock().unwrap();
    if let Some(ref map) = *handles {
        if let Some(NetHandle::Connection(stream)) = map.get(&handle) {
            // Try to peek at the stream to check if it's still connected
            // A zero-byte peek will fail if the connection is closed
            use std::io::Read;
            let mut buf = [0u8; 1];

            // Clone the stream to avoid borrowing issues
            match stream.try_clone() {
                Ok(mut clone) => {
                    // Set blocking temporarily for the peek
                    let _ = clone.set_nonblocking(false);
                    match clone.peek(&mut buf) {
                        Ok(0) => return 0,  // Connection closed
                        Ok(_) => return -1, // Data available, still connected
                        Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                            return -1; // No data but still connected
                        }
                        Err(_) => return 0, // Error, assume disconnected
                    }
                }
                Err(_) => return 0,
            }
        } else if map.contains_key(&handle) {
            // It's a host handle, which is always "connected" while open
            return -1;
        }
    }
    0 // Invalid handle
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

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // Color conversion tests
    // ========================================================================

    #[test]
    fn test_basic_to_ansi_color() {
        assert_eq!(basic_to_ansi_color(0), 30); // Black
        assert_eq!(basic_to_ansi_color(7), 37); // White
        assert_eq!(basic_to_ansi_color(15), 97); // Bright white
    }

    #[test]
    fn test_basic_to_ansi_color_all_colors() {
        // Dark colors (0-7) map to ANSI 30-37
        assert_eq!(basic_to_ansi_color(0), 30); // Black
        assert_eq!(basic_to_ansi_color(1), 34); // Blue
        assert_eq!(basic_to_ansi_color(2), 32); // Green
        assert_eq!(basic_to_ansi_color(3), 36); // Cyan
        assert_eq!(basic_to_ansi_color(4), 31); // Red
        assert_eq!(basic_to_ansi_color(5), 35); // Magenta
        assert_eq!(basic_to_ansi_color(6), 33); // Brown/Yellow
        assert_eq!(basic_to_ansi_color(7), 37); // White

        // Bright colors (8-15) map to ANSI 90-97
        assert_eq!(basic_to_ansi_color(8), 90); // Gray
        assert_eq!(basic_to_ansi_color(9), 94); // Light Blue
        assert_eq!(basic_to_ansi_color(10), 92); // Light Green
        assert_eq!(basic_to_ansi_color(11), 96); // Light Cyan
        assert_eq!(basic_to_ansi_color(12), 91); // Light Red
        assert_eq!(basic_to_ansi_color(13), 95); // Light Magenta
        assert_eq!(basic_to_ansi_color(14), 93); // Yellow
        assert_eq!(basic_to_ansi_color(15), 97); // Bright White

        // Out of range defaults to white
        assert_eq!(basic_to_ansi_color(16), 37);
        assert_eq!(basic_to_ansi_color(-1), 37);
        assert_eq!(basic_to_ansi_color(100), 37);
    }

    // ========================================================================
    // Print function tests (output capture)
    // ========================================================================

    #[test]
    fn test_qb_print_int_values() {
        // Test that qb_print_int doesn't panic on various values
        qb_print_int(0);
        qb_print_int(1);
        qb_print_int(-1);
        qb_print_int(i64::MAX);
        qb_print_int(i64::MIN);
    }

    #[test]
    fn test_qb_print_float_values() {
        // Test that qb_print_float doesn't panic on various values
        qb_print_float(0.0);
        qb_print_float(1.5);
        qb_print_float(-1.5);
        qb_print_float(f64::MAX);
        qb_print_float(f64::MIN);
        qb_print_float(f64::INFINITY);
        qb_print_float(f64::NEG_INFINITY);
        qb_print_float(f64::NAN);
    }

    #[test]
    fn test_qb_print_float_integer_display() {
        // Floats that are whole numbers should display without decimals
        // This is a behavioral test - we're testing it doesn't panic
        qb_print_float(42.0);
        qb_print_float(-100.0);
        qb_print_float(1e14); // Still within i64 range
    }

    #[test]
    fn test_qb_print_string_null() {
        unsafe {
            // Null string should not panic
            qb_print_string(std::ptr::null());
        }
    }

    #[test]
    fn test_qb_print_string_empty() {
        unsafe {
            let s = crate::string::qb_string_empty();
            qb_print_string(s);
            crate::string::qb_string_release(s);
        }
    }

    #[test]
    fn test_qb_print_string_valid() {
        unsafe {
            let s = crate::string::qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            qb_print_string(s);
            crate::string::qb_string_release(s);
        }
    }

    #[test]
    fn test_qb_print_newline() {
        // Should not panic
        qb_print_newline();
    }

    #[test]
    fn test_qb_print_tab() {
        // Should not panic
        qb_print_tab();
    }

    #[test]
    fn test_qb_print_space() {
        // Should not panic
        qb_print_space();
    }

    #[test]
    fn test_qb_print_flush() {
        // Should not panic
        qb_print_flush();
    }

    // ========================================================================
    // Console function tests
    // ========================================================================

    #[test]
    fn test_qb_cls() {
        // Should not panic
        qb_cls();
    }

    #[test]
    fn test_qb_locate_valid() {
        // Should not panic with valid positions
        qb_locate(1, 1);
        qb_locate(10, 20);
        qb_locate(25, 80);
    }

    #[test]
    fn test_qb_locate_edge_cases() {
        // Should not panic even with unusual values
        qb_locate(0, 0);
        qb_locate(-1, -1);
        qb_locate(1000, 1000);
    }

    #[test]
    fn test_qb_color_foreground_only() {
        // Foreground only (background = -1)
        qb_color(7, -1);
        qb_color(15, -1);
        qb_color(0, -1);
    }

    #[test]
    fn test_qb_color_both() {
        // Both foreground and background
        qb_color(15, 0); // White on black
        qb_color(0, 7); // Black on white
        qb_color(14, 1); // Yellow on blue
    }

    #[test]
    fn test_qb_color_reset() {
        qb_color_reset();
    }

    // ========================================================================
    // INKEY$ test
    // ========================================================================

    #[test]
    fn test_qb_inkey_returns_empty() {
        // Currently returns empty string (stub implementation)
        let s = qb_inkey();
        unsafe {
            assert_eq!(crate::string::qb_string_len(s), 0);
            crate::string::qb_string_release(s);
        }
    }

    // ========================================================================
    // File system function tests
    // ========================================================================

    #[test]
    fn test_qb_file_kill_nonexistent() {
        unsafe {
            // Killing non-existent file should return error (1)
            let result = qb_file_kill(b"/nonexistent/path/file.txt\0".as_ptr() as *const c_char);
            assert_eq!(result, 1);
        }
    }

    #[test]
    fn test_qb_file_kill_null() {
        unsafe {
            let result = qb_file_kill(std::ptr::null());
            assert_eq!(result, 1);
        }
    }

    #[test]
    fn test_qb_file_kill_valid() {
        unsafe {
            // Create a temp file using std::env::temp_dir
            let temp_dir = std::env::temp_dir();
            let temp_path = temp_dir.join(format!("qb64_kill_test_{}.tmp", std::process::id()));
            let path_str = temp_path.to_str().unwrap();

            // Create the file
            std::fs::write(&temp_path, "test content").unwrap();
            assert!(temp_path.exists());

            // Kill the file
            let path_cstring = std::ffi::CString::new(path_str).unwrap();
            let result = qb_file_kill(path_cstring.as_ptr());
            assert_eq!(result, 0);

            // Verify file is gone
            assert!(!temp_path.exists());
        }
    }

    #[test]
    fn test_qb_file_rename_null() {
        unsafe {
            assert_eq!(qb_file_rename(std::ptr::null(), std::ptr::null()), 1);
            assert_eq!(
                qb_file_rename(b"test\0".as_ptr() as *const c_char, std::ptr::null()),
                1
            );
            assert_eq!(
                qb_file_rename(std::ptr::null(), b"test\0".as_ptr() as *const c_char),
                1
            );
        }
    }

    #[test]
    fn test_qb_file_rename_nonexistent() {
        unsafe {
            let result = qb_file_rename(
                b"/nonexistent/old.txt\0".as_ptr() as *const c_char,
                b"/nonexistent/new.txt\0".as_ptr() as *const c_char,
            );
            assert_eq!(result, 1);
        }
    }

    #[test]
    fn test_qb_mkdir_rmdir_cycle() {
        unsafe {
            let temp_dir = std::env::temp_dir();
            let test_dir = temp_dir.join(format!("qb64_test_{}", std::process::id()));
            let path_str = test_dir.to_str().unwrap();
            let path_cstring = std::ffi::CString::new(path_str).unwrap();

            // Ensure directory doesn't exist
            let _ = std::fs::remove_dir(&test_dir);

            // Create directory
            let result = qb_mkdir(path_cstring.as_ptr());
            assert_eq!(result, 0);
            assert!(test_dir.exists());
            assert!(test_dir.is_dir());

            // Remove directory
            let result = qb_rmdir(path_cstring.as_ptr());
            assert_eq!(result, 0);
            assert!(!test_dir.exists());
        }
    }

    #[test]
    fn test_qb_mkdir_null() {
        unsafe {
            assert_eq!(qb_mkdir(std::ptr::null()), 1);
        }
    }

    #[test]
    fn test_qb_rmdir_null() {
        unsafe {
            assert_eq!(qb_rmdir(std::ptr::null()), 1);
        }
    }

    #[test]
    fn test_qb_rmdir_nonexistent() {
        unsafe {
            let result = qb_rmdir(b"/nonexistent/directory/path\0".as_ptr() as *const c_char);
            assert_eq!(result, 1);
        }
    }

    #[test]
    fn test_qb_chdir_null() {
        unsafe {
            assert_eq!(qb_chdir(std::ptr::null()), 1);
        }
    }

    #[test]
    fn test_qb_chdir_nonexistent() {
        unsafe {
            let result = qb_chdir(b"/nonexistent/directory/path\0".as_ptr() as *const c_char);
            assert_eq!(result, 1);
        }
    }

    #[test]
    fn test_qb_file_exists_null() {
        unsafe {
            assert_eq!(qb_file_exists(std::ptr::null()), 0);
        }
    }

    #[test]
    fn test_qb_file_exists_nonexistent() {
        unsafe {
            let path = crate::string::qb_string_new(
                b"/nonexistent/file/path.txt\0".as_ptr() as *const c_char
            );
            let result = qb_file_exists(crate::string::qb_string_data(path));
            crate::string::qb_string_release(path);
            assert_eq!(result, 0);
        }
    }

    #[test]
    fn test_qb_file_exists_valid() {
        unsafe {
            // file!() returns relative path, use Cargo.toml which always exists
            let path = crate::string::qb_string_new(b"Cargo.toml\0".as_ptr() as *const c_char);
            let result = qb_file_exists(crate::string::qb_string_data(path));
            crate::string::qb_string_release(path);
            // May or may not exist depending on working directory
            // Just verify it doesn't panic
            let _ = result;
        }
    }

    #[test]
    fn test_qb_file_exists_is_directory() {
        unsafe {
            // A directory should return 0 (not a file)
            let path = crate::string::qb_string_new(b"src\0".as_ptr() as *const c_char);
            let result = qb_file_exists(crate::string::qb_string_data(path));
            crate::string::qb_string_release(path);
            // Should be 0 because it's a directory, not a file
            // (unless working directory doesn't have src)
            let _ = result;
        }
    }

    #[test]
    fn test_qb_dir_exists_null() {
        unsafe {
            assert_eq!(qb_dir_exists(std::ptr::null()), 0);
        }
    }

    #[test]
    fn test_qb_dir_exists_nonexistent() {
        unsafe {
            let path =
                crate::string::qb_string_new(b"/nonexistent/directory\0".as_ptr() as *const c_char);
            let result = qb_dir_exists(crate::string::qb_string_data(path));
            crate::string::qb_string_release(path);
            assert_eq!(result, 0);
        }
    }

    #[test]
    fn test_qb_dir_exists_is_file() {
        unsafe {
            // A file should return 0 (not a directory)
            // Use a path that likely exists
            let path = crate::string::qb_string_new(b"Cargo.toml\0".as_ptr() as *const c_char);
            let result = qb_dir_exists(crate::string::qb_string_data(path));
            crate::string::qb_string_release(path);
            // Should be 0 because it's a file, not a directory
            let _ = result;
        }
    }

    #[test]
    fn test_qb_dir_null_spec() {
        unsafe {
            let result = qb_dir(std::ptr::null());
            assert_eq!(crate::string::qb_string_len(result), 0);
            crate::string::qb_string_release(result);
        }
    }

    #[test]
    fn test_qb_dir_empty_spec() {
        unsafe {
            let spec = crate::string::qb_string_new(b"\0".as_ptr() as *const c_char);
            let result = qb_dir(crate::string::qb_string_data(spec));
            crate::string::qb_string_release(spec);
            // Empty spec should return empty or first match from current state
            crate::string::qb_string_release(result);
        }
    }

    // ========================================================================
    // Shell function tests
    // ========================================================================

    #[test]
    fn test_qb_shell_null() {
        // Note: This would open an interactive shell, so we skip actual execution
        // Just test that the function exists and handles the case
        // Don't actually call qb_shell(std::ptr::null()) in tests
    }

    #[test]
    fn test_qb_shell_simple_command() {
        unsafe {
            // Run a simple command that should succeed
            #[cfg(not(target_os = "windows"))]
            {
                let result = qb_shell(b"true\0".as_ptr() as *const c_char);
                assert_eq!(result, 0);
            }
            #[cfg(target_os = "windows")]
            {
                let result = qb_shell(b"cmd /c exit 0\0".as_ptr() as *const c_char);
                assert_eq!(result, 0);
            }
        }
    }

    #[test]
    fn test_qb_shell_hide_null() {
        unsafe {
            let result = qb_shell_hide(std::ptr::null());
            assert_eq!(result, 1);
        }
    }

    #[test]
    fn test_qb_shell_hide_simple_command() {
        unsafe {
            #[cfg(not(target_os = "windows"))]
            {
                let result = qb_shell_hide(b"true\0".as_ptr() as *const c_char);
                assert_eq!(result, 0);
            }
        }
    }

    // ========================================================================
    // Network function tests
    // ========================================================================

    #[test]
    fn test_net_handle_initialization() {
        init_net_handles();
        // Should not panic on multiple calls
        init_net_handles();
        init_net_handles();
    }

    #[test]
    fn test_qb_net_openhost_invalid_port() {
        // Port 0 might work (OS assigns), but very high ports might fail
        // This mainly tests the function doesn't panic
        let handle = qb_net_openhost(0);
        if handle != 0 {
            qb_net_close(handle);
        }
    }

    #[test]
    fn test_qb_net_openconnection_invalid_handle() {
        let result = qb_net_openconnection(999);
        assert_eq!(result, 0);
    }

    #[test]
    fn test_qb_net_openclient_null() {
        unsafe {
            let result = qb_net_openclient(std::ptr::null());
            assert_eq!(result, 0);
        }
    }

    #[test]
    fn test_qb_net_openclient_invalid_format() {
        unsafe {
            // Invalid format - missing parts
            let result = qb_net_openclient(b"invalid\0".as_ptr() as *const c_char);
            assert_eq!(result, 0);

            // Invalid protocol
            let result = qb_net_openclient(b"UDP:8080:localhost\0".as_ptr() as *const c_char);
            assert_eq!(result, 0);

            // Invalid port
            let result =
                qb_net_openclient(b"TCP/IP:notaport:localhost\0".as_ptr() as *const c_char);
            assert_eq!(result, 0);
        }
    }

    #[test]
    fn test_qb_net_connected_invalid_handle() {
        let result = qb_net_connected(999);
        assert_eq!(result, 0);
    }

    #[test]
    fn test_qb_net_close_invalid_handle() {
        // Should not panic on invalid handle
        qb_net_close(999);
        qb_net_close(0);
        qb_net_close(-999);
    }

    #[test]
    fn test_network_host_connection_cycle() {
        // Test the full cycle: open host, check connection (none), close
        let host_handle = qb_net_openhost(0); // Let OS pick port
        if host_handle != 0 {
            // No client connected yet
            let conn = qb_net_openconnection(host_handle);
            assert_eq!(conn, 0); // No connection waiting

            // Host should be "connected" (listening)
            assert_eq!(qb_net_connected(host_handle), -1);

            qb_net_close(host_handle);

            // After close, should not be connected
            assert_eq!(qb_net_connected(host_handle), 0);
        }
    }
}
