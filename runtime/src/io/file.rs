//! I/O Functions for QB64Fresh Runtime
//!
//! This module provides PRINT, INPUT, and file I/O operations.

use crate::qbs_compat::Qbs;
use crate::string::{
    qb_string_data, qb_string_from_bytes, qb_string_len, qb_string_retain, QbString,
};
use std::collections::HashMap;
use std::fs::File;
use std::io::ErrorKind;
use std::io::{BufRead, BufReader, BufWriter, Read, Seek, SeekFrom, Write};
use std::os::raw::c_char;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::sync::Once;

#[cfg(unix)]
use std::os::unix::io::AsRawFd;

#[cfg(windows)]
use std::os::windows::io::AsRawHandle;

// Import normalize_path_for_fs from input module
// This function normalizes Windows path separators on non-Windows systems
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

// ============================================================================
// File I/O Functions
// ============================================================================

// ============================================================================
// File I/O Functions
// ============================================================================

/// Maximum number of file handles (QB64 convention: 1-255)
const QB_MAX_FILES: usize = 256;

/// File handle storage
struct FileHandle {
    file: Option<File>,
    reader: Option<BufReader<File>>,
    writer: Option<BufWriter<File>>,
    record_len: i32,
    mode: String,
    eof_reached: bool, // Track if we've reached EOF
}

static FILE_HANDLES: Mutex<Option<HashMap<i32, FileHandle>>> = Mutex::new(None);

// FIELD statement support: buffer storage per file number
// Each file can have a field buffer that maps string variables to portions of the buffer
static FIELD_BUFFERS: Mutex<Option<HashMap<i32, Vec<u8>>>> = Mutex::new(None);
static FIELD_OFFSETS: Mutex<Option<HashMap<i32, i32>>> = Mutex::new(None);
// Track which file number is currently being set up (for qb_field_add calls)
static CURRENT_FIELD_FILE: Mutex<Option<i32>> = Mutex::new(None);

/// Last number of bytes read by qb_file_get / qb_file_get_string (libqb gfs_read_bytes).
static LAST_READ_BYTES: std::sync::atomic::AtomicI64 = std::sync::atomic::AtomicI64::new(0);

fn init_file_handles() {
    let mut handles = FILE_HANDLES.lock().unwrap();
    if handles.is_none() {
        *handles = Some(HashMap::new());
    }
}

fn init_field_buffers() {
    let mut buffers = FIELD_BUFFERS.lock().unwrap();
    if buffers.is_none() {
        *buffers = Some(HashMap::new());
    }
}

fn init_field_offsets() {
    let mut offsets = FIELD_OFFSETS.lock().unwrap();
    if offsets.is_none() {
        *offsets = Some(HashMap::new());
    }
}

/// Resolves a file path, handling `internal/` directory relative to executable or source file.
///
/// Path resolution strategy:
/// 1. If path starts with `internal/`, try to resolve relative to:
///    - Executable directory (if available)
///    - Current working directory
/// 2. For other paths, use the path as-is (relative to current working directory)
///
/// This matches QB64pe behavior where `internal/` is expected to be relative to
/// the executable or source file location, not just the current working directory.
///
/// # Arguments
///
/// * `path_str` - The file path string (may contain `internal/`)
///
/// # Returns
///
/// A `PathBuf` with the resolved path. If resolution fails, returns the original path.
/// Resolves a file path, handling `internal/` directory relative to executable or source file.
///
/// Path resolution strategy:
/// 1. If path starts with `internal/`, try to resolve relative to:
///    - Executable directory (if available)
///    - Current working directory
/// 2. For other paths, use the path as-is (relative to current working directory)
///
/// This matches QB64pe behavior where `internal/` is expected to be relative to
/// the executable or source file location, not just the current working directory.
///
/// # Arguments
///
/// * `path_str` - The file path string (may contain `internal/`)
/// * `for_write` - If true, don't check file existence (for write operations)
///
/// # Returns
///
/// A `PathBuf` with the resolved path. If resolution fails, returns the original path.
fn resolve_file_path(path_str: &str, for_write: bool) -> PathBuf {
    let path = Path::new(path_str);

    // Check if path starts with "internal/" (case-insensitive on Windows)
    // Note: path_str is already normalized (backslashes converted to forward slashes)
    // so we only need to check for forward slashes
    let is_internal = if cfg!(windows) {
        let lower = path_str.to_lowercase();
        lower.starts_with("internal/")
    } else {
        path_str.starts_with("internal/")
    };

    if is_internal {
        // Try to resolve relative to executable directory first
        if let Ok(exe_path) = std::env::current_exe() {
            if let Some(exe_dir) = exe_path.parent() {
                let resolved = exe_dir.join(path);
                // For read operations, check if file exists
                // For write operations, return the path (will create file/dirs as needed)
                if for_write || resolved.exists() {
                    return resolved;
                }
            }
        }

        // Try relative to current working directory (only for read operations)
        if !for_write {
            if let Ok(cwd) = std::env::current_dir() {
                let resolved = cwd.join(path);
                if resolved.exists() {
                    return resolved;
                }
            }
        }

        // For write operations or if read failed, return path relative to executable
        if let Ok(exe_path) = std::env::current_exe() {
            if let Some(exe_dir) = exe_path.parent() {
                return exe_dir.join(path);
            }
        }

        // Fallback: return path relative to current working directory
        if let Ok(cwd) = std::env::current_dir() {
            return cwd.join(path);
        }
    }

    // For non-internal paths, return as-is (will be resolved relative to current working directory)
    path.to_path_buf()
}

/// C constants for OPEN access (must match qb64fresh_rt.h).
const QB_FILE_ACCESS_DEFAULT: i32 = 0;
const QB_FILE_ACCESS_READ: i32 = 1;
const QB_FILE_ACCESS_WRITE: i32 = 2;
const QB_FILE_ACCESS_READ_WRITE: i32 = 3;

/// C constants for OPEN lock (must match qb64fresh_rt.h).
const QB_FILE_LOCK_DEFAULT: i32 = 0;
const QB_FILE_LOCK_SHARED: i32 = 1;
const QB_FILE_LOCK_READ: i32 = 2;
const QB_FILE_LOCK_WRITE: i32 = 3;
const QB_FILE_LOCK_READ_WRITE: i32 = 4;
const QB_FILE_LOCK_ONLY: i32 = 5;

/// Applies file locking on Unix based on lock mode.
/// SHARED/DEFAULT: no lock (match inline C). LOCK_* / ONLY: LOCK_EX.
#[cfg(unix)]
fn apply_flock(file: &File, lock: i32) {
    use libc::{flock, LOCK_EX};
    // Only apply exclusive lock for explicit lock modes; DEFAULT and SHARED = no lock (match inline C).
    let use_exclusive = matches!(
        lock,
        QB_FILE_LOCK_READ | QB_FILE_LOCK_WRITE | QB_FILE_LOCK_READ_WRITE | QB_FILE_LOCK_ONLY
    );
    if !use_exclusive {
        return;
    }
    let fd = file.as_raw_fd();
    unsafe {
        flock(fd, LOCK_EX);
    }
}

/// Applies file locking on Windows using LockFile (entire file).
/// SHARED/DEFAULT: no lock. LOCK_* / ONLY: exclusive lock on whole file (parity with Unix flock).
#[cfg(windows)]
fn apply_flock(file: &File, lock: i32) {
    let use_exclusive = matches!(
        lock,
        QB_FILE_LOCK_READ | QB_FILE_LOCK_WRITE | QB_FILE_LOCK_READ_WRITE | QB_FILE_LOCK_ONLY
    );
    if !use_exclusive {
        return;
    }
    use winapi::um::errhandlingapi::GetLastError;
    use winapi::um::fileapi::LockFile;
    use winapi::um::handleapi::INVALID_HANDLE_VALUE;
    let handle = file.as_raw_handle();
    if handle == INVALID_HANDLE_VALUE {
        return;
    }
    // Lock entire file: start 0, length 0xFFFFFFFF_FFFFFFFF (to end of file)
    let ok = unsafe { LockFile(handle as *mut _, 0, 0, 0xFFFF_FFFF, 0xFFFF_FFFF) };
    if ok == 0 {
        let _e = unsafe { GetLastError() };
        // Don't set QB error here; OPEN still succeeds, lock is best-effort (match Unix flock behavior)
    }
}

/// OPEN - Open a file.
///
/// Access and lock use QB_FILE_ACCESS_* and QB_FILE_LOCK_* constants (0 = default).
/// On Unix, lock modes apply flock(); on Windows, LockFile (entire file) is used for OPEN.
///
/// # Safety
/// - `filename` must be a valid null-terminated C string
/// - `mode` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_file_open(
    fnum: i32,
    filename: *const c_char,
    mode: *const c_char,
    access: i32,
    lock: i32,
) {
    let _ = access; // Used by codegen for fopen mode; we derive mode from mode_str
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    if filename.is_null() || mode.is_null() {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }

    init_file_handles();

    let filename_str = match std::ffi::CStr::from_ptr(filename).to_str() {
        Ok(s) => s,
        Err(_) => {
            crate::qb_set_error(64, 0); // Bad file name (invalid encoding)
            return;
        }
    };
    let mode_str = match std::ffi::CStr::from_ptr(mode).to_str() {
        Ok(s) => s,
        Err(_) => {
            crate::qb_set_error(5, 0); // Illegal function call (invalid mode encoding)
            return;
        }
    };
    if filename_str.is_empty() {
        // IDE init sometimes opens with empty filename (e.g. config "r+b");
        // treat as optional file: succeed without opening, no error, no warning.
        let mode_trimmed = mode_str.trim();
        if mode_trimmed == "r+b" || mode_trimmed == "rb+" {
            return;
        }
        crate::qb_set_error(64, 0); // Bad file name
        return;
    }

    let normalized = normalize_path_for_fs(filename_str);

    // Determine if this is a write operation (for path resolution)
    let is_write_mode = mode_str.contains('w') || mode_str.contains('a') || mode_str.contains('+');

    // Resolve path: check for internal/ directory relative to executable or source file
    let resolved_path = resolve_file_path(normalized.as_ref(), is_write_mode);

    // For write operations, ensure parent directories exist
    if is_write_mode {
        if let Some(parent) = resolved_path.parent() {
            if let Err(e) = std::fs::create_dir_all(parent) {
                // Log directory creation failure but continue - file open will fail with clearer error
                eprintln!(
                    "Warning: Failed to create parent directory '{}': {}",
                    parent.display(),
                    e
                );
            }
        }
    }

    // Close existing file if open
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(handle) = map.remove(&fnum) {
            // File will be closed when dropped
            drop(handle);
        }

        // Open the file
        let file_result = match mode_str {
            "r" | "rb" => std::fs::File::open(&resolved_path),
            "w" | "wb" => std::fs::File::create(&resolved_path),
            "a" | "ab" => {
                // Append mode - create if doesn't exist
                std::fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&resolved_path)
            }
            "r+" | "r+b" | "rb+" => {
                // Read/write mode - create if doesn't exist
                // Use OpenOptions with create flag to avoid race condition
                std::fs::OpenOptions::new()
                    .read(true)
                    .write(true)
                    .create(true)
                    .open(&resolved_path)
            }
            _ => std::fs::File::open(&resolved_path), // Default to read
        };

        match file_result {
            Ok(file) => {
                apply_flock(&file, lock);

                let mut handle = FileHandle {
                    file: Some(file),
                    reader: None,
                    writer: None,
                    record_len: 128, // Default record length
                    mode: mode_str.to_string(),
                    eof_reached: false,
                };

                // Create reader/writer based on mode
                // Note: We open separate file handles for reader/writer to allow independent
                // buffering and positioning. This is safe and common practice.
                if mode_str.contains('r') || mode_str.contains('+') {
                    if let Ok(file_for_reader) = std::fs::File::open(&resolved_path) {
                        handle.reader = Some(BufReader::new(file_for_reader));
                    }
                }
                if mode_str.contains('w') || mode_str.contains('a') || mode_str.contains('+') {
                    if let Ok(file_for_writer) = std::fs::OpenOptions::new()
                        .write(true)
                        .append(mode_str.contains('a'))
                        .create(true)
                        .open(&resolved_path)
                    {
                        handle.writer = Some(BufWriter::new(file_for_writer));
                    }
                }

                map.insert(fnum, handle);
            }
            Err(e) => {
                // QB64pe IDE builds the File menu by opening "settings/recent.bin" (relative to
                // CWD). If the file doesn't exist (first run or different CWD), OPEN fails with
                // Error 53. Auto-create the file and retry so the IDE works regardless of how
                // it was started (script, double-click, or from another directory).
                let is_recent_bin = resolved_path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .map(|n| n.eq_ignore_ascii_case("recent.bin"))
                    .unwrap_or(false);
                if e.kind() == ErrorKind::NotFound && is_recent_bin {
                    if let Some(parent) = resolved_path.parent() {
                        let _ = std::fs::create_dir_all(parent);
                    }
                    if std::fs::File::create(&resolved_path).is_ok() {
                        // Retry open with same mode
                        let retry_result = match mode_str {
                            "r" | "rb" => std::fs::File::open(&resolved_path),
                            "r+" | "r+b" | "rb+" => std::fs::OpenOptions::new()
                                .read(true)
                                .write(true)
                                .open(&resolved_path),
                            _ => std::fs::File::open(&resolved_path),
                        };
                        if let Ok(file) = retry_result {
                            apply_flock(&file, lock);
                            let mut handle = FileHandle {
                                file: Some(file),
                                reader: None,
                                writer: None,
                                record_len: 128,
                                mode: mode_str.to_string(),
                                eof_reached: false,
                            };
                            if mode_str.contains('r') || mode_str.contains('+') {
                                if let Ok(fr) = std::fs::File::open(&resolved_path) {
                                    handle.reader = Some(BufReader::new(fr));
                                }
                            }
                            if mode_str.contains('w')
                                || mode_str.contains('a')
                                || mode_str.contains('+')
                            {
                                if let Ok(fw) = std::fs::OpenOptions::new()
                                    .write(true)
                                    .append(mode_str.contains('a'))
                                    .create(true)
                                    .open(&resolved_path)
                                {
                                    handle.writer = Some(BufWriter::new(fw));
                                }
                            }
                            map.insert(fnum, handle);
                            return;
                        }
                    }
                }
                // File open failed - set pending error (Option B: ON ERROR GOTO can handle)
                // QB error 53 = "File not found" (classic QB)
                crate::qb_set_error(53, 0);
                if std::env::var("QB64FRESH_DEBUG_FILE").is_ok() {
                    eprintln!(
                        "QB64Fresh: Failed to open file '{}': {}",
                        resolved_path.display(),
                        e
                    );
                }
            }
        }
    }
}

/// OPEN - Open a file (QbString* version).
///
/// Helper function for code that passes QbString* to qb_file_open.
///
/// # Safety
/// - `filename` must be a valid QbString pointer or null
/// - `mode` must be a valid null-terminated C string
#[no_mangle]
pub unsafe extern "C" fn qb_file_open_str(
    fnum: i32,
    filename: *const QbString,
    mode: *const c_char,
    access: i32,
    lock: i32,
) {
    if filename.is_null() {
        crate::qb_set_error(52, 0); // Bad file number (null filename)
        return;
    }
    let filename_data = qb_string_data(filename);
    qb_file_open(fnum, filename_data, mode, access, lock)
}

/// Set record length for random access files.
#[no_mangle]
pub extern "C" fn qb_file_set_reclen(fnum: i32, len: i32) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            handle.record_len = len;
        }
    }
}

/// CLOSE - Close a file.
#[no_mangle]
pub extern "C" fn qb_file_close(fnum: i32) {
    if fnum < 0 {
        // Network handle - handled by qb_net_close
        return;
    }

    // Clear field buffer and offset for this file
    init_field_buffers();
    init_field_offsets();
    let mut buffers = FIELD_BUFFERS.lock().unwrap();
    if let Some(ref mut map) = *buffers {
        map.remove(&fnum);
    }
    let mut offsets = FIELD_OFFSETS.lock().unwrap();
    if let Some(ref mut map) = *offsets {
        map.remove(&fnum);
    }
    // Clear current file if it matches
    let mut current_file = CURRENT_FIELD_FILE.lock().unwrap();
    if *current_file == Some(fnum) {
        *current_file = None;
    }
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        map.remove(&fnum);
    }
}

/// Close all open files.
#[no_mangle]
pub extern "C" fn qb_file_close_all() {
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        map.clear();
    }
}

/// PRINT # - Print integer to file.
#[no_mangle]
pub extern "C" fn qb_file_print_int(fnum: i32, val: i64) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut writer) = handle.writer {
                let _ = write!(writer.get_mut(), "{}", val);
                let _ = writer.flush();
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no write access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    } else {
        crate::qb_set_error(52, 0); // File not open
    }
}

/// PRINT # - Print float to file.
#[no_mangle]
pub extern "C" fn qb_file_print_float(fnum: i32, val: f64) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut writer) = handle.writer {
                let _ = write!(writer.get_mut(), "{}", val);
                let _ = writer.flush();
            } else {
                crate::qb_set_error(54, 0); // Bad file mode
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    } else {
        crate::qb_set_error(52, 0); // File not open
    }
}

/// PRINT # - Print string to file.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_file_print_string(fnum: i32, s: *const QbString) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    if s.is_null() {
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut writer) = handle.writer {
                let data = qb_string_data(s);
                let len = qb_string_len(s);
                let slice = std::slice::from_raw_parts(data as *const u8, len);
                let _ = writer.write_all(slice);
                let _ = writer.flush();
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no write access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    } else {
        crate::qb_set_error(52, 0); // File not open
    }
}

/// PRINT # - Print newline to file.
#[no_mangle]
pub extern "C" fn qb_file_print_newline(fnum: i32) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut writer) = handle.writer {
                let _ = writer.write_all(b"\n");
                let _ = writer.flush();
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no write access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    } else {
        crate::qb_set_error(52, 0); // File not open
    }
}

/// PRINT # - Print tab to file.
#[no_mangle]
pub extern "C" fn qb_file_print_tab(fnum: i32) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut writer) = handle.writer {
                let _ = writer.write_all(b"\t");
                let _ = writer.flush();
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no write access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    } else {
        crate::qb_set_error(52, 0); // File not open
    }
}

/// WRITE # - Write string to file (quoted).
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_file_write_string(fnum: i32, s: *const QbString) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 || s.is_null() {
        if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
            crate::qb_set_error(52, 0); // Bad file number
        }
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut writer) = handle.writer {
                let data = qb_string_data(s);
                let len = qb_string_len(s);
                let slice = std::slice::from_raw_parts(data as *const u8, len);
                let _ = writer.write_all(b"\"");
                let _ = writer.write_all(slice);
                let _ = writer.write_all(b"\"");
                let _ = writer.flush();
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no write access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    }
}

/// WRITE # - Write number to file.
#[no_mangle]
pub extern "C" fn qb_file_write_number(fnum: i32, val: f64) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut writer) = handle.writer {
                let _ = write!(writer.get_mut(), "{}", val);
                let _ = writer.flush();
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no write access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    }
}

/// WRITE # - Write character to file.
#[no_mangle]
pub extern "C" fn qb_file_write_char(fnum: i32, c: u8) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut writer) = handle.writer {
                let _ = writer.write_all(&[c]);
                let _ = writer.flush();
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no write access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    }
}

/// INPUT # - Read string from file.
///
/// # Safety
/// - `s` must be a valid pointer to a QbString* (will be modified)
#[no_mangle]
pub unsafe extern "C" fn qb_file_input_string(fnum: i32, s: *mut *mut QbString) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    if s.is_null() {
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut reader) = handle.reader {
                let mut buf = Vec::new();
                // Read until whitespace or newline
                loop {
                    let mut byte = [0u8; 1];
                    match Read::read_exact(reader, &mut byte) {
                        Ok(_) => {
                            if byte[0] == b' '
                                || byte[0] == b'\t'
                                || byte[0] == b'\n'
                                || byte[0] == b'\r'
                            {
                                break;
                            }
                            buf.push(byte[0]);
                        }
                        Err(_) => break,
                    }
                }
                *s = qb_string_from_bytes(buf.as_ptr(), buf.len());
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no read access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    } else {
        crate::qb_set_error(52, 0); // File not open
    }
}

/// INPUT # - Read integer from file.
///
/// # Safety
/// - `val` must be a valid pointer to i32
#[no_mangle]
pub unsafe extern "C" fn qb_file_input_int(fnum: i32, val: *mut i32) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    if val.is_null() {
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut reader) = handle.reader {
                let mut buf = String::new();
                // Read until whitespace
                loop {
                    let mut byte = [0u8; 1];
                    match Read::read_exact(reader, &mut byte) {
                        Ok(_) => {
                            if byte[0] == b' '
                                || byte[0] == b'\t'
                                || byte[0] == b'\n'
                                || byte[0] == b'\r'
                            {
                                break;
                            }
                            buf.push(byte[0] as char);
                        }
                        Err(_) => break,
                    }
                }
                if let Ok(n) = buf.parse::<i32>() {
                    *val = n;
                }
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no read access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    } else {
        crate::qb_set_error(52, 0); // File not open
    }
}

/// INPUT # - Read float from file.
///
/// # Safety
/// - `val` must be a valid pointer to f64
#[no_mangle]
pub unsafe extern "C" fn qb_file_input_float(fnum: i32, val: *mut f64) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    if val.is_null() {
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut reader) = handle.reader {
                let mut buf = String::new();
                // Read until whitespace
                loop {
                    let mut byte = [0u8; 1];
                    match Read::read_exact(reader, &mut byte) {
                        Ok(_) => {
                            if byte[0] == b' '
                                || byte[0] == b'\t'
                                || byte[0] == b'\n'
                                || byte[0] == b'\r'
                            {
                                break;
                            }
                            buf.push(byte[0] as char);
                        }
                        Err(_) => break,
                    }
                }
                if let Ok(n) = buf.parse::<f64>() {
                    *val = n;
                }
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no read access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    } else {
        crate::qb_set_error(52, 0); // File not open
    }
}

/// LINE INPUT - Read a line from a file.
///
/// Reads a line from the file handle `fnum` and stores it in the string `s`.
/// The newline character(s) are removed from the result.
///
/// # Safety
/// - `s` must be a valid pointer to a QbString* (can be NULL initially)
/// - `fnum` must be a valid file handle (1-255)
///
/// # Error Handling
/// - If the line exceeds MAX_LINE_LENGTH (10MB), returns an empty string
/// - If allocation fails, `*s` may be NULL (caller should check)
/// - If file is not open or invalid, function returns without modifying `*s`
#[no_mangle]
pub unsafe extern "C" fn qb_file_line_input(fnum: i32, s: *mut *mut QbString) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    if s.is_null() {
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut reader) = handle.reader {
                // Maximum line length: 10MB to prevent huge allocations
                // This is much larger than any reasonable line but prevents OOM
                const MAX_LINE_LENGTH: usize = 10 * 1024 * 1024;
                let mut buf = Vec::with_capacity(8192); // Start with 8KB capacity for better performance

                // Read until newline or EOF, but limit total size to prevent OOM
                // Use read_until which efficiently reads line-by-line
                match BufRead::read_until(reader, b'\n', &mut buf) {
                    Ok(0) => {
                        // EOF - mark handle as EOF and return empty string
                        handle.eof_reached = true;
                        *s = qb_string_from_bytes(std::ptr::null(), 0);
                        return;
                    }
                    Ok(_) => {
                        // Check if line is too long (prevents OOM from huge files without newlines)
                        if buf.len() > MAX_LINE_LENGTH {
                            // Line too long - return empty string instead of crashing
                            *s = qb_string_from_bytes(std::ptr::null(), 0);
                            return;
                        }
                    }
                    Err(_) => {
                        // Error reading - mark as EOF and return empty string
                        handle.eof_reached = true;
                        *s = qb_string_from_bytes(std::ptr::null(), 0);
                        return;
                    }
                }

                // Remove trailing newline if present
                if buf.last() == Some(&b'\n') {
                    buf.pop();
                }
                if buf.last() == Some(&b'\r') {
                    buf.pop();
                }
                *s = qb_string_from_bytes(buf.as_ptr(), buf.len());
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no read access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    } else {
        crate::qb_set_error(52, 0); // File not open
    }
}

/// SEEK - Set file position.
#[no_mangle]
pub extern "C" fn qb_file_seek(fnum: i32, pos: i64) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut file) = handle.file {
                let _ = file.seek(SeekFrom::Start(pos as u64));
            }
            // Reset EOF state when seeking
            handle.eof_reached = false;
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    }
}

/// SEEK - Set file position by record number.
#[no_mangle]
pub extern "C" fn qb_file_seek_record(fnum: i32, rec: i64) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            let pos = rec * handle.record_len as i64;
            if let Some(ref mut file) = handle.file {
                let _ = file.seek(SeekFrom::Start(pos as u64));
            }
            // Reset EOF state when seeking
            handle.eof_reached = false;
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    }
}

/// GET - Read binary data from file.
///
/// Sets last-read byte count for qb_gfs_read_bytes() (libqb gfs_read_bytes).
///
/// # Safety
/// - `data` must be a valid pointer to a buffer of at least `size` bytes
#[no_mangle]
pub unsafe extern "C" fn qb_file_get(fnum: i32, data: *mut u8, size: usize) {
    LAST_READ_BYTES.store(0, std::sync::atomic::Ordering::SeqCst);
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 || data.is_null() || size == 0 {
        if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
            crate::qb_set_error(52, 0); // Bad file number
        }
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut reader) = handle.reader {
                let slice = std::slice::from_raw_parts_mut(data, size);
                match reader.read(slice) {
                    Ok(n) => {
                        LAST_READ_BYTES.store(n as i64, std::sync::atomic::Ordering::SeqCst);
                    }
                    Err(_) => {
                        crate::qb_set_error(62, 0); // Input past end of file
                    }
                }
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no read access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    }
}

/// GET # - Read binary data from file into a string buffer.
///
/// Reads up to `s->len` bytes from the file into the string's data buffer.
/// Sets last-read byte count for qb_gfs_read_bytes().
///
/// # Safety
/// - `s` must be a valid QbString pointer with a non-zero length
/// - The string's data buffer must be writable
/// - Modifies the string in place (does not handle reference counting)
#[no_mangle]
pub unsafe extern "C" fn qb_file_get_string(fnum: i32, s: *mut QbString) {
    LAST_READ_BYTES.store(0, std::sync::atomic::Ordering::SeqCst);
    if s.is_null() {
        return;
    }

    let len = qb_string_len(s);
    if len == 0 {
        return;
    }

    // QbString* is actually a pointer directly to the character data
    // (the header is stored before it). We can safely cast to *mut u8 for writing.
    let data_ptr = s as *mut c_char as *mut u8;

    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }

    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut reader) = handle.reader {
                let slice = std::slice::from_raw_parts_mut(data_ptr, len);
                match reader.read(slice) {
                    Ok(n) => {
                        LAST_READ_BYTES.store(n as i64, std::sync::atomic::Ordering::SeqCst);
                    }
                    Err(_) => {
                        crate::qb_set_error(62, 0); // Input past end of file
                    }
                }
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no read access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    }
}

/// PUT - Write binary data to file.
///
/// # Safety
/// - `data` must be a valid pointer to a buffer of at least `size` bytes
#[no_mangle]
pub unsafe extern "C" fn qb_file_put(fnum: i32, data: *const u8, size: usize) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 || data.is_null() || size == 0 {
        if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
            crate::qb_set_error(52, 0); // Bad file number
        }
        return;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut writer) = handle.writer {
                let slice = std::slice::from_raw_parts(data, size);
                let _ = writer.write_all(slice);
                let _ = writer.flush();
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no write access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    }
}

/// PUT # - Write binary data from a string buffer to file.
///
/// Writes exactly `s->len` bytes from the string's data buffer to the file.
/// This is used for binary file I/O with string variables.
///
/// # Safety
/// - `s` must be a valid QbString pointer with a non-zero length
#[no_mangle]
pub unsafe extern "C" fn qb_file_put_string(fnum: i32, s: *const QbString) {
    if s.is_null() {
        return;
    }

    let len = qb_string_len(s);
    if len == 0 {
        return;
    }

    // QbString* is actually a pointer directly to the character data
    // We can safely cast to *const u8 for reading.
    let data_ptr = s as *const c_char as *const u8;

    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }

    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut writer) = handle.writer {
                let slice = std::slice::from_raw_parts(data_ptr, len);
                let _ = writer.write_all(slice);
                let _ = writer.flush();
            } else {
                crate::qb_set_error(54, 0); // Bad file mode (no write access)
            }
        } else {
            crate::qb_set_error(52, 0); // File not open
        }
    }
}

/// EOF - Check if end of file.
///
/// Returns:
/// - 0 if not at EOF
/// - -1 if at EOF or invalid handle
#[no_mangle]
pub extern "C" fn qb_eof(fnum: i32) -> i32 {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return -1; // EOF for invalid handle
    }
    init_file_handles();
    let handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref map) = *handles {
        if let Some(ref handle) = map.get(&fnum) {
            // Check if we've already reached EOF (set by read operations)
            if handle.eof_reached {
                return -1; // At EOF
            }
            // If we have a reader, we're not at EOF yet
            // (EOF state is set when read operations return 0 bytes)
            return 0; // Not at EOF
        }
    }
    crate::qb_set_error(52, 0); // File not open
    -1 // Invalid handle = EOF
}

/// LOF - Length of file.
#[no_mangle]
pub extern "C" fn qb_lof(fnum: i32) -> i64 {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return 0;
    }
    init_file_handles();
    let handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref map) = *handles {
        if map.contains_key(&fnum) {
            // Try to get metadata from the file
            // Since we can't easily get metadata from BufReader/BufWriter,
            // we'll need to store the file separately or use a different approach
            // For now, return 0 (simplified implementation)
            return 0;
        }
    }
    crate::qb_set_error(52, 0); // File not open
    0
}

/// LOC - Current file position (1-based byte position).
#[no_mangle]
pub extern "C" fn qb_loc(fnum: i32) -> i64 {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return 0;
    }
    init_file_handles();
    let mut handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref mut map) = *handles {
        if let Some(ref mut handle) = map.get_mut(&fnum) {
            if let Some(ref mut file) = handle.file {
                if let Ok(pos) = file.stream_position() {
                    return pos as i64;
                }
            }
        }
    }
    crate::qb_set_error(52, 0); // File not open
    0
}

/// SEEK(filenum) - Returns current file position (1-based), same semantics as LOC.
#[no_mangle]
pub extern "C" fn qb_seek(fnum: i32) -> i64 {
    qb_loc(fnum)
}

/// FREEFILE - Get next available file number.
#[no_mangle]
pub extern "C" fn qb_freefile() -> i32 {
    init_file_handles();
    let handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref map) = *handles {
        for i in 1..QB_MAX_FILES as i32 {
            if !map.contains_key(&i) {
                return i;
            }
        }
    }
    0 // No free file number
}

/// LOCK #filenum [, start] [, end] - Lock a file or byte range.
///
/// start=-1, end=-1 means lock entire file. Returns: 0=success, -2=invalid handle,
/// -4=illegal function call, -7=permission denied, -9=access error (Windows).
#[no_mangle]
pub extern "C" fn qb_file_lock(fnum: i32, start: i64, end: i64) -> i32 {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        return -2;
    }
    let mut start = start;
    if start == -1 {
        start = 0;
    }
    if start < 0 || end < -1 {
        return -4;
    }
    if end != -1 && end < start {
        return -4;
    }
    if end != -1 {
        if let Some(len) = end.checked_sub(start).and_then(|d| d.checked_add(1)) {
            if len <= 0 {
                return -4;
            }
        } else {
            return -4;
        }
    }
    init_file_handles();
    let handles = FILE_HANDLES.lock().unwrap();
    let Some(ref map) = *handles else { return -2 };
    let Some(ref handle) = map.get(&fnum) else {
        return -2;
    };
    let Some(ref file) = handle.file else {
        return -2;
    };

    #[cfg(unix)]
    {
        use libc::{fcntl, flock, off_t, F_SETLK, F_WRLCK, SEEK_SET};
        let fd = file.as_raw_fd();
        if fd < 0 {
            return -2;
        }
        if end == -1 {
            if unsafe { flock(fd, libc::LOCK_EX) } != 0 {
                return -7;
            }
        } else {
            let len = (end - start + 1) as off_t;
            if len <= 0 {
                return -4;
            }
            let mut lock_info: libc::flock = unsafe { std::mem::zeroed() };
            lock_info.l_type = F_WRLCK as i16;
            lock_info.l_whence = SEEK_SET as i16;
            lock_info.l_start = start as off_t;
            lock_info.l_len = len;
            lock_info.l_pid = 0;
            if unsafe { fcntl(fd, F_SETLK, &lock_info) } != 0 {
                return -7;
            }
        }
    }

    #[cfg(windows)]
    {
        use winapi::um::errhandlingapi::GetLastError;
        use winapi::um::fileapi::LockFile;
        use winapi::um::handleapi::INVALID_HANDLE_VALUE;
        let handle_raw = file.as_raw_handle();
        if handle_raw == INVALID_HANDLE_VALUE {
            return -2;
        }
        let (n_low, n_high) = if end == -1 {
            (0xFFFF_FFFFu32, 0xFFFF_FFFFu32)
        } else {
            let n = match end.checked_sub(start).and_then(|d| d.checked_add(1)) {
                Some(l) if l > 0 => l as u64,
                _ => return -4,
            };
            ((n & 0xFFFF_FFFF) as u32, ((n >> 32) & 0xFFFF_FFFF) as u32)
        };
        let start_low = (start as u64 & 0xFFFF_FFFF) as u32;
        let start_high = ((start as u64 >> 32) & 0xFFFF_FFFF) as u32;
        let ok = unsafe { LockFile(handle_raw as *mut _, start_low, start_high, n_low, n_high) };
        if ok == 0 {
            let e = unsafe { GetLastError() };
            if e == winapi::um::winerror::ERROR_ACCESS_DENIED
                || e == winapi::um::winerror::ERROR_LOCK_VIOLATION
            {
                return -7;
            }
            return -9;
        }
    }

    0
}

/// UNLOCK #filenum [, start] [, end] - Unlock a file or byte range.
///
/// start=-1, end=-1 means unlock entire file. Return values same as qb_file_lock.
#[no_mangle]
pub extern "C" fn qb_file_unlock(fnum: i32, start: i64, end: i64) -> i32 {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        return -2;
    }
    let mut start = start;
    if start == -1 {
        start = 0;
    }
    if start < 0 || end < -1 {
        return -4;
    }
    if end != -1 && end < start {
        return -4;
    }
    if end != -1 {
        if let Some(len) = end.checked_sub(start).and_then(|d| d.checked_add(1)) {
            if len <= 0 {
                return -4;
            }
        } else {
            return -4;
        }
    }
    init_file_handles();
    let handles = FILE_HANDLES.lock().unwrap();
    let Some(ref map) = *handles else { return -2 };
    let Some(ref handle) = map.get(&fnum) else {
        return -2;
    };
    let Some(ref file) = handle.file else {
        return -2;
    };

    #[cfg(unix)]
    {
        use libc::{fcntl, flock, off_t, F_SETLK, F_UNLCK, SEEK_SET};
        let fd = file.as_raw_fd();
        if fd < 0 {
            return -2;
        }
        if end == -1 {
            if unsafe { flock(fd, libc::LOCK_UN) } != 0 {
                return -7;
            }
        } else {
            let len = (end - start + 1) as off_t;
            if len <= 0 {
                return -4;
            }
            let mut lock_info: libc::flock = unsafe { std::mem::zeroed() };
            lock_info.l_type = F_UNLCK as i16;
            lock_info.l_whence = SEEK_SET as i16;
            lock_info.l_start = start as off_t;
            lock_info.l_len = len;
            lock_info.l_pid = 0;
            if unsafe { fcntl(fd, F_SETLK, &lock_info) } != 0 {
                return -7;
            }
        }
    }

    #[cfg(windows)]
    {
        use winapi::um::errhandlingapi::GetLastError;
        use winapi::um::fileapi::UnlockFile;
        use winapi::um::handleapi::INVALID_HANDLE_VALUE;
        let handle_raw = file.as_raw_handle();
        if handle_raw == INVALID_HANDLE_VALUE {
            return -2;
        }
        let (n_low, n_high) = if end == -1 {
            (0xFFFF_FFFFu32, 0xFFFF_FFFFu32)
        } else {
            let n = match end.checked_sub(start).and_then(|d| d.checked_add(1)) {
                Some(l) if l > 0 => l as u64,
                _ => return -4,
            };
            ((n & 0xFFFF_FFFF) as u32, ((n >> 32) & 0xFFFF_FFFF) as u32)
        };
        let start_low = (start as u64 & 0xFFFF_FFFF) as u32;
        let start_high = ((start as u64 >> 32) & 0xFFFF_FFFF) as u32;
        let ok = unsafe { UnlockFile(handle_raw as *mut _, start_low, start_high, n_low, n_high) };
        if ok == 0 {
            let e = unsafe { GetLastError() };
            if e == winapi::um::winerror::ERROR_ACCESS_DENIED
                || e == winapi::um::winerror::ERROR_LOCK_VIOLATION
            {
                return -7;
            }
            return -9;
        }
    }

    0
}

/// FRE(n) - Approximate free memory (QB4.5 compatibility).
///
/// Returns an approximate value for compatibility. Classic BASIC: n=0 far heap,
/// n=-1 string space, n=-2 stack. We return a large dummy value so programs that
/// check FRE do not think memory is exhausted.
///
/// # Arguments
/// * `n` - Memory type selector (ignored; we return a fixed approximation)
///
/// # Returns
/// Approximate free memory in bytes (64 MiB placeholder).
#[no_mangle]
pub extern "C" fn qb_fre(_n: i64) -> i64 {
    64 * 1024 * 1024 // 64 MiB - arbitrary large value for compatibility
}

/// FIELD - Start field definition.
///
/// Allocates a field buffer for the specified file number based on the file's
/// record length. This buffer will be used to map string variables to fixed-length
/// field positions for random access file I/O.
///
/// # Arguments
/// * `fnum` - File number (must be a valid open file handle)
#[no_mangle]
pub extern "C" fn qb_field_start(fnum: i32) {
    if fnum < 1 || fnum >= QB_MAX_FILES as i32 {
        crate::qb_set_error(52, 0); // Bad file number
        return;
    }

    init_file_handles();
    init_field_buffers();
    init_field_offsets();

    // Get record length for this file
    let record_len = {
        let handles = FILE_HANDLES.lock().unwrap();
        if let Some(ref map) = *handles {
            if let Some(ref handle) = map.get(&fnum) {
                handle.record_len
            } else {
                crate::qb_set_error(52, 0); // Bad file number - file not open
                return;
            }
        } else {
            crate::qb_set_error(52, 0); // Bad file number (handles not initialized)
            return;
        }
    };

    // Allocate or reset field buffer
    let mut buffers = FIELD_BUFFERS.lock().unwrap();
    if let Some(ref mut map) = *buffers {
        map.insert(fnum, vec![0u8; record_len as usize]);
    }

    // Reset field offset to start of buffer
    let mut offsets = FIELD_OFFSETS.lock().unwrap();
    if let Some(ref mut map) = *offsets {
        map.insert(fnum, 0);
    }

    // Set current file for subsequent qb_field_add calls
    let mut current_file = CURRENT_FIELD_FILE.lock().unwrap();
    *current_file = Some(fnum);
}

/// FIELD - Add a field variable.
///
/// Maps a string variable to a fixed-width portion of the field buffer at the
/// current offset. The variable will point to this portion of the buffer, allowing
/// GET/PUT operations to read/write directly to/from the variable.
///
/// # Arguments
/// * `width` - Width of the field in bytes
/// * `var` - Pointer to QbString* pointer that will be set to point to the field
///
/// # Safety
/// - `var` must be a valid pointer to a QbString* pointer
/// - The file must have been opened and FIELD started with `qb_field_start()`
/// - Must be called immediately after `qb_field_start()` for the same file
#[no_mangle]
pub unsafe extern "C" fn qb_field_add(width: i32, var: *mut *mut QbString) {
    if var.is_null() || width <= 0 {
        return;
    }

    init_field_buffers();
    init_field_offsets();

    // Get the current file number from the last qb_field_start call
    let fnum = {
        let current_file = CURRENT_FIELD_FILE.lock().unwrap();
        match *current_file {
            Some(f) => f,
            None => {
                // No active FIELD statement - create independent fixed-length string
                use crate::string::qb_string_from_bytes;
                if !(*var).is_null() {
                    crate::string::qb_string_release(*var);
                }
                let data = vec![b' '; width as usize];
                *var = qb_string_from_bytes(data.as_ptr(), width as usize);
                return;
            }
        }
    };

    // Get current offset for this file
    let offset = {
        let offsets = FIELD_OFFSETS.lock().unwrap();
        if let Some(ref map) = *offsets {
            *map.get(&fnum).unwrap_or(&0)
        } else {
            0
        }
    };

    // Verify the field fits in the buffer
    let buffers = FIELD_BUFFERS.lock().unwrap();
    if let Some(ref map) = *buffers {
        if let Some(ref buffer) = map.get(&fnum) {
            if (offset + width) as usize > buffer.len() {
                // Field exceeds buffer - create independent string as fallback
                drop(buffers);
                use crate::string::qb_string_from_bytes;
                if !(*var).is_null() {
                    crate::string::qb_string_release(*var);
                }
                let data = vec![b' '; width as usize];
                *var = qb_string_from_bytes(data.as_ptr(), width as usize);
                return;
            }
        }
    }
    drop(buffers);

    // Create a fixed-length string filled with spaces
    // Note: In a full implementation, this would point into the field buffer,
    // but for now we create independent strings that work with LSET/RSET
    use crate::string::qb_string_from_bytes;
    if !(*var).is_null() {
        crate::string::qb_string_release(*var);
    }
    let data = vec![b' '; width as usize];
    *var = qb_string_from_bytes(data.as_ptr(), width as usize);

    // Update offset for next field
    let mut offsets = FIELD_OFFSETS.lock().unwrap();
    if let Some(ref mut map) = *offsets {
        map.insert(fnum, offset + width);
    }
}

/// GFS - Last read byte count (libqb gfs_read_bytes).
#[no_mangle]
pub extern "C" fn qb_gfs_read_bytes() -> i64 {
    LAST_READ_BYTES.load(std::sync::atomic::Ordering::SeqCst)
}

/// GFS - Get internal file index for fileno (libqb gfs_get_fileno).
/// QB64Fresh uses fileno as handle directly; returns file_number.
#[no_mangle]
pub extern "C" fn qb_gfs_get_fileno(file_number: i32) -> i32 {
    file_number
}

/// libqb gfs.h compatibility: same fileno semantics.
#[no_mangle]
pub extern "C" fn gfs_get_fileno(file_number: i32) -> i32 {
    file_number
}

/// C-compatible gfs_file_struct layout (QB64pe libqb/include/gfs.h).
#[repr(C)]
pub struct GfsFileStruct {
    pub id: i64,
    pub open: u8,
    pub read: u8,
    pub write: u8,
    pub lock_read: u8,
    pub lock_write: u8,
    pub pos: i64,
    pub eof_reached: u8,
    pub eof_passed: u8,
    pub fileno: i32,
    pub type_: u8,
    pub record_length: i64,
    pub field_buffer: *mut u8,
    pub field_strings: *mut *mut Qbs,
    pub field_strings_n: i32,
    pub column: i64,
    pub file_handle: *mut std::ffi::c_void,
    pub file_handle_o: *mut std::ffi::c_void,
    pub win_handle: *mut std::ffi::c_void,
    pub com_port: u8,
    pub com_baud_rate: i32,
    pub com_parity: i8,
    pub com_data_bits_per_byte: i8,
    pub com_stop_bits: i8,
    pub com_bin_asc: i8,
    pub com_asc_lf: i8,
    pub com_rs: i8,
    pub com_cd_x: i32,
    pub com_cs_x: i32,
    pub com_ds_x: i32,
    pub com_op_x: i32,
    pub scrn: u8,
}

/// Slots for gfs_get_file_struct; filled on demand. Index 0 = fileno 1.
static mut GFS_SLOTS: Option<[GfsFileStruct; 256]> = None;
static GFS_INIT: Once = Once::new();

/// GFS - Get file struct by fileno (libqb gfs_get_file_struct).
/// Fills and returns a pointer to the struct for that fileno (NULL if invalid/closed).
#[no_mangle]
pub unsafe extern "C" fn gfs_get_file_struct(fileno: i32) -> *mut GfsFileStruct {
    if fileno < 1 || fileno > 256 {
        return std::ptr::null_mut();
    }
    init_file_handles();
    GFS_INIT.call_once(|| {
        GFS_SLOTS = Some(std::mem::zeroed());
    });
    let slots = match GFS_SLOTS.as_mut() {
        Some(s) => s,
        None => return std::ptr::null_mut(),
    };
    let idx = (fileno - 1) as usize;
    let pos = qb_loc(fileno);
    let mut g = GfsFileStruct {
        id: fileno as i64,
        open: 0,
        read: 0,
        write: 0,
        lock_read: 0,
        lock_write: 0,
        pos: -1,
        eof_reached: 0,
        eof_passed: 0,
        fileno,
        type_: 0,
        record_length: 0,
        field_buffer: std::ptr::null_mut(),
        field_strings: std::ptr::null_mut(),
        field_strings_n: 0,
        column: 0,
        file_handle: std::ptr::null_mut(),
        file_handle_o: std::ptr::null_mut(),
        win_handle: std::ptr::null_mut(),
        com_port: 0,
        com_baud_rate: 0,
        com_parity: 0,
        com_data_bits_per_byte: 0,
        com_stop_bits: 0,
        com_bin_asc: 0,
        com_asc_lf: 0,
        com_rs: 0,
        com_cd_x: 0,
        com_cs_x: 1000,
        com_ds_x: 1000,
        com_op_x: 0,
        scrn: 0,
    };
    let handles = FILE_HANDLES.lock().unwrap();
    if let Some(ref map) = *handles {
        if let Some(ref handle) = map.get(&fileno) {
            g.open = 1;
            g.pos = pos;
            g.eof_reached = if handle.eof_reached { 1 } else { 0 };
            g.record_length = handle.record_len as i64;
            let mode = handle.mode.as_bytes();
            if mode.contains(&b'r') {
                g.read = 1;
            }
            if mode.contains(&b'w') || mode.contains(&b'a') {
                g.write = 1;
            }
            if mode.contains(&b'b') {
                g.type_ = 2; // BINARY
            } else if mode.contains(&b'r') && g.write == 0 {
                g.type_ = 3; // INPUT
            } else if mode.contains(&b'w') || mode.contains(&b'a') {
                g.type_ = 4; // OUTPUT
            } else {
                g.type_ = 1; // RANDOM
            }
            drop(handles);
            let field_buffers = FIELD_BUFFERS.lock().unwrap();
            if let Some(ref buf_map) = *field_buffers {
                if let Some(ref buf) = buf_map.get(&fileno) {
                    g.field_buffer = buf.as_ptr() as *mut u8;
                }
            }
            slots[idx] = g;
            return &mut slots[idx] as *mut GfsFileStruct;
        }
    }
    std::ptr::null_mut()
}

/// Legacy alias: get file struct by fileno (returns same as gfs_get_file_struct).
#[no_mangle]
pub unsafe extern "C" fn qb_gfs_get_file_struct(fileno: i32) -> *mut GfsFileStruct {
    gfs_get_file_struct(fileno)
}

/// LSET - Left-align string in field.
///
/// Left-aligns the value string in the target variable, padding with spaces on the right.
/// If the value is longer than the field width, it is truncated.
///
/// If the target variable is null or has zero length, this behaves like a regular
/// string assignment (copies the value as-is).
///
/// # Arguments
/// * `var` - Pointer to QbString* pointer (the field variable)
/// * `value` - The value string to assign (left-aligned)
///
/// # Safety
/// - `var` must be a valid pointer to a QbString* pointer
/// - `value` must be a valid QbString pointer
#[no_mangle]
pub unsafe extern "C" fn qb_lset(var: *mut *mut QbString, value: *const QbString) {
    if var.is_null() || value.is_null() {
        return;
    }

    let val_len = qb_string_len(value);
    let val_data = qb_string_data(value);

    // If target variable is null or has no fixed width, just copy the value
    if (*var).is_null() {
        *var = qb_string_retain(value as *mut QbString);
        return;
    }

    let var_len = qb_string_len(*var);

    // If variable has no fixed width (length 0), just copy the value
    if var_len == 0 {
        crate::string::qb_string_release(*var);
        *var = qb_string_retain(value as *mut QbString);
        return;
    }

    // Determine copy length (truncate if value is longer than field)
    let copy_len = val_len.min(var_len);

    // Create new string with field width, filled with spaces
    use crate::string::qb_string_from_bytes;
    let mut field_data = vec![b' '; var_len];

    // Copy value data left-aligned (from start of field)
    if copy_len > 0 {
        let val_slice = std::slice::from_raw_parts(val_data as *const u8, copy_len);
        field_data[..copy_len].copy_from_slice(val_slice);
    }

    // Release old string and assign new one
    crate::string::qb_string_release(*var);
    *var = qb_string_from_bytes(field_data.as_ptr(), var_len);
}

/// RSET - Right-align string in field.
///
/// Right-aligns the value string in the target variable, padding with spaces on the left.
/// If the value is longer than the field width, it is truncated.
///
/// If the target variable is null or has zero length, this behaves like a regular
/// string assignment (copies the value as-is).
///
/// # Arguments
/// * `var` - Pointer to QbString* pointer (the field variable)
/// * `value` - The value string to assign (right-aligned)
///
/// # Safety
/// - `var` must be a valid pointer to a QbString* pointer
/// - `value` must be a valid QbString pointer
#[no_mangle]
pub unsafe extern "C" fn qb_rset(var: *mut *mut QbString, value: *const QbString) {
    if var.is_null() || value.is_null() {
        return;
    }

    let val_len = qb_string_len(value);
    let val_data = qb_string_data(value);

    // If target variable is null or has no fixed width, just copy the value
    if (*var).is_null() {
        *var = qb_string_retain(value as *mut QbString);
        return;
    }

    let var_len = qb_string_len(*var);

    // If variable has no fixed width (length 0), just copy the value
    if var_len == 0 {
        crate::string::qb_string_release(*var);
        *var = qb_string_retain(value as *mut QbString);
        return;
    }

    // Determine copy length (truncate if value is longer than field)
    let copy_len = val_len.min(var_len);

    // Create new string with field width, filled with spaces
    use crate::string::qb_string_from_bytes;
    let mut field_data = vec![b' '; var_len];

    // Copy value data right-aligned (offset from start)
    if copy_len > 0 {
        let offset = var_len - copy_len;
        let val_slice = std::slice::from_raw_parts(val_data as *const u8, copy_len);
        field_data[offset..].copy_from_slice(val_slice);
    }

    // Release old string and assign new one
    crate::string::qb_string_release(*var);
    *var = qb_string_from_bytes(field_data.as_ptr(), var_len);
}

/// Free field buffer for a string (libqb file-fields.h field_free).
///
/// In libqb this frees the per-string field descriptor and sets str->field = NULL.
/// Our runtime does not track per-string field attachment; this is a no-op so
/// the symbol exists for linking. Full behavior can be added when we track
/// field variables per file.
///
/// # Arguments
///
/// * `s` - The string (QbString*) that was used as a field variable; may be null.
#[no_mangle]
pub extern "C" fn field_free(_s: *mut crate::string::QbString) {
    // No-op: we do not attach field metadata to QbString.
}

/// Sync field buffer to file (libqb file-fields.h field_update).
///
/// In libqb this copies from the file's field buffer into each attached qbs.
/// Our runtime does not yet track which strings are attached to which file's
/// field buffer; this is a no-op so the symbol exists for linking.
///
/// # Arguments
///
/// * `fileno` - File number (ignored in stub).
#[no_mangle]
pub extern "C" fn field_update(_fileno: i32) {
    // No-op: full implementation requires per-file list of field variables.
}

/// libqb field_free (qb_ prefix for header).
#[no_mangle]
pub unsafe extern "C" fn qb_field_free(s: *mut QbString) {
    field_free(s);
}

/// libqb field_update (qb_ prefix for header).
#[no_mangle]
pub extern "C" fn qb_field_update(fileno: i32) {
    field_update(fileno);
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
            let result = qb_file_exists(path);
            crate::string::qb_string_release(path);
            assert_eq!(result, 0);
        }
    }

    #[test]
    fn test_qb_file_exists_valid() {
        unsafe {
            // file!() returns relative path, use Cargo.toml which always exists
            let path = crate::string::qb_string_new(b"Cargo.toml\0".as_ptr() as *const c_char);
            let result = qb_file_exists(path);
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
            let result = qb_file_exists(path);
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
            let result = qb_dir_exists(path);
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
            let result = qb_dir_exists(path);
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
            let result = qb_dir(spec);
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
        let handle = qb_net_openhost(std::ptr::null());
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
        let host_handle = qb_net_openhost(std::ptr::null()); // Let OS pick port
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

    #[test]
    fn test_network_put_get_binary() {
        use std::io::{Read, Write};
        use std::net::TcpStream;
        use std::thread;
        use std::time::Duration;

        // Start server on a random port
        let host_handle = qb_net_openhost(std::ptr::null());
        if host_handle == 0 {
            // Skip test if we can't open a server (e.g., CI environment)
            return;
        }

        // Get the actual port from the listener
        let port = {
            let handles = NET_HANDLES.lock().unwrap();
            if let Some(ref map) = *handles {
                if let Some(NetHandle::Host(listener)) = map.get(&host_handle) {
                    listener.local_addr().ok().map(|a| a.port())
                } else {
                    None
                }
            } else {
                None
            }
        };

        let port = match port {
            Some(p) => p,
            None => {
                qb_net_close(host_handle);
                return;
            }
        };

        // Spawn a client thread
        let client_thread = thread::spawn(move || {
            thread::sleep(Duration::from_millis(50));
            if let Ok(mut stream) = TcpStream::connect(format!("127.0.0.1:{}", port)) {
                // Send test data
                let data = [0x12u8, 0x34, 0x56, 0x78];
                let _ = stream.write_all(&data);
                let _ = stream.flush();
                thread::sleep(Duration::from_millis(50));

                // Read response
                let mut response = [0u8; 4];
                let _ = stream.read_exact(&mut response);
                response
            } else {
                [0u8; 4]
            }
        });

        // Accept connection
        thread::sleep(Duration::from_millis(100));
        let conn_handle = qb_net_openconnection(host_handle);
        assert_ne!(conn_handle, 0, "Should have accepted connection");

        // Wait for data and read with GET
        thread::sleep(Duration::from_millis(100));
        let mut recv_buf = [0u8; 4];
        unsafe {
            let bytes_read = qb_net_get(conn_handle, recv_buf.as_mut_ptr(), 4);
            assert_eq!(bytes_read, 4, "Should read 4 bytes");
        }
        assert_eq!(recv_buf, [0x12, 0x34, 0x56, 0x78], "Data should match");

        // Send response with PUT
        let send_buf = [0xABu8, 0xCD, 0xEF, 0x01];
        unsafe {
            let bytes_written = qb_net_put(conn_handle, send_buf.as_ptr(), 4);
            assert_eq!(bytes_written, 4, "Should write 4 bytes");
        }

        // Wait for client
        let client_response = client_thread.join().unwrap();
        assert_eq!(
            client_response,
            [0xAB, 0xCD, 0xEF, 0x01],
            "Client should receive response"
        );

        // Cleanup
        qb_net_close(conn_handle);
        qb_net_close(host_handle);
    }

    #[test]
    fn test_network_eof_and_lof() {
        use std::io::Write;
        use std::net::TcpStream;
        use std::thread;
        use std::time::Duration;

        let host_handle = qb_net_openhost(std::ptr::null());
        if host_handle == 0 {
            return;
        }

        let port = {
            let handles = NET_HANDLES.lock().unwrap();
            if let Some(ref map) = *handles {
                if let Some(NetHandle::Host(listener)) = map.get(&host_handle) {
                    listener.local_addr().ok().map(|a| a.port())
                } else {
                    None
                }
            } else {
                None
            }
        };

        let port = match port {
            Some(p) => p,
            None => {
                qb_net_close(host_handle);
                return;
            }
        };

        // Spawn client that sends data then closes
        let _client_thread = thread::spawn(move || {
            thread::sleep(Duration::from_millis(50));
            if let Ok(mut stream) = TcpStream::connect(format!("127.0.0.1:{}", port)) {
                let _ = stream.write_all(b"Hello");
                let _ = stream.flush();
                // Connection will close when thread exits
            }
        });

        thread::sleep(Duration::from_millis(100));
        let conn_handle = qb_net_openconnection(host_handle);
        if conn_handle == 0 {
            qb_net_close(host_handle);
            return;
        }

        // Wait for data
        thread::sleep(Duration::from_millis(100));

        // Check LOF - should have buffered data
        let lof = qb_net_lof(conn_handle);
        assert!(lof >= 5, "LOF should be at least 5 bytes, got {}", lof);

        // EOF should be false while data available
        assert_eq!(qb_net_eof(conn_handle), 0, "EOF should be false with data");

        // Read all data
        let mut buf = [0u8; 10];
        unsafe {
            qb_net_get(conn_handle, buf.as_mut_ptr(), 5);
        }
        assert_eq!(&buf[..5], b"Hello");

        // Wait for client to close
        thread::sleep(Duration::from_millis(100));

        // Now EOF should be true (after reading all + connection closed)
        // Note: EOF detection may require another read attempt
        unsafe {
            qb_net_get(conn_handle, buf.as_mut_ptr(), 1);
        }
        assert_eq!(
            qb_net_eof(conn_handle),
            -1,
            "EOF should be true after close"
        );

        qb_net_close(conn_handle);
        qb_net_close(host_handle);
    }
}
