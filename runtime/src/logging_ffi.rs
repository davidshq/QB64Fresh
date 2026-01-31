//! C FFI for scoped logging (libqb logging.h compatibility).
//!
//! Exposes libqb_log_init, qb_log_message (for C variadic wrapper),
//! and libqb_log_qbs for QbString message logging.

use std::ffi::CStr;
use std::os::raw::c_char;

use crate::logging;
use crate::string;
use crate::string::QbString;

/// Initialize logging. Call once at program start.
///
/// # Safety
/// Safe to call from C.
#[no_mangle]
pub unsafe extern "C" fn libqb_log_init() {
    logging::init();
}

/// Log a preformatted message (called from C after vsnprintf or from libqb_log_qbs).
///
/// # Arguments
/// * `level` - 0=Trace, 1=Information, 2=Warning, 3=Error
/// * `scope` - 0=Runtime, 1=QB64, 2=Libqb, 3=Audio, 4=Image
/// * `file` - Source file (nullable)
/// * `func` - Function name (nullable)
/// * `line` - Line number
/// * `message` - NUL-terminated message string (not null)
///
/// # Safety
/// Caller must ensure file, func, message are valid C strings or null.
#[no_mangle]
pub unsafe extern "C" fn qb_log_message(
    level: i32,
    scope: i32,
    file: *const c_char,
    func: *const c_char,
    line: i32,
    message: *const c_char,
) {
    if message.is_null() {
        return;
    }
    let msg = match CStr::from_ptr(message).to_str() {
        Ok(s) => s,
        Err(_) => return,
    };
    let file_str = file
        .as_ref()
        .map(|p| CStr::from_ptr(p).to_str().unwrap_or("?"));
    let func_str = func
        .as_ref()
        .map(|p| CStr::from_ptr(p).to_str().unwrap_or("?"));
    logging::log_message(level, scope, file_str, func_str, line, msg);
}

/// Log a QbString message (libqb_log_qbs).
///
/// # Arguments
/// * `level` - 0=Trace, 1=Information, 2=Warning, 3=Error
/// * `scope` - 0=Runtime, 1=QB64, 2=Libqb, 3=Audio, 4=Image
/// * `file` - Source file (nullable)
/// * `func` - Function name (nullable)
/// * `line` - Line number
/// * `str` - Message as QbString (nullable; null or empty logs nothing)
///
/// # Safety
/// Caller must ensure file, func are valid C strings or null. str may be null.
#[no_mangle]
pub unsafe extern "C" fn libqb_log_qbs(
    level: i32,
    scope: i32,
    file: *const c_char,
    func: *const c_char,
    line: i32,
    str: *const QbString,
) {
    if str.is_null() {
        return;
    }
    let s = &*str;
    let data = string::qb_string_data(s);
    if data.is_null() {
        return;
    }
    let msg = CStr::from_ptr(data).to_str().unwrap_or("");
    let file_str = file
        .as_ref()
        .map(|p| CStr::from_ptr(p).to_str().unwrap_or("?"));
    let func_str = func
        .as_ref()
        .map(|p| CStr::from_ptr(p).to_str().unwrap_or("?"));
    logging::log_message(level, scope, file_str, func_str, line, msg);
}

/// Set minimum log level (0=Trace .. 3=Error). Returns previous value.
///
/// # Safety
/// Safe to call from C.
#[no_mangle]
pub unsafe extern "C" fn qb_log_set_min_level(level: i32) -> i32 {
    logging::set_min_level(level)
}

/// Get current minimum log level (0=Trace .. 3=Error).
///
/// # Safety
/// Safe to call from C.
#[no_mangle]
pub unsafe extern "C" fn qb_log_get_min_level() -> i32 {
    logging::get_min_level()
}
