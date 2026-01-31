//! C FFI for HTTP client (libqb_http_*).
//!
//! Matches QB64pe libqb http.h API: init/stop, open/close, connected,
//! get_length, get_content_length, get_status_code, get_url, get, get_fixed.

use std::ffi::CStr;
use std::os::raw::{c_char, c_int};

use crate::http;

// ============================================================================
// Init / Stop
// ============================================================================

/// Initialize the HTTP system. Call before any other libqb_http_* function.
#[no_mangle]
pub extern "C" fn libqb_http_init() {
    http::http_init();
}

/// Shut down the HTTP system and close all handles.
#[no_mangle]
pub extern "C" fn libqb_http_stop() {
    http::http_stop();
}

// ============================================================================
// Open / Close / Connected
// ============================================================================

/// Open a URL with the given handle id. Performs a blocking GET and buffers
/// the response. Returns 0 on success, -1 on error.
///
/// # Safety
/// `url` must be a valid null-terminated C string for the duration of the call.
#[no_mangle]
pub unsafe extern "C" fn libqb_http_open(url: *const c_char, handle: c_int) -> c_int {
    if url.is_null() {
        return -1;
    }
    let url_str = match CStr::from_ptr(url).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };
    if http::http_open(url_str, handle) {
        0
    } else {
        -1
    }
}

/// Close the handle and free its resources. Returns 0 on success, -1 if
/// handle was invalid.
#[no_mangle]
pub extern "C" fn libqb_http_close(handle: c_int) -> c_int {
    if http::http_close(handle) {
        0
    } else {
        -1
    }
}

/// Returns 1 if handle is open and connected, 0 if closed, -1 if invalid handle.
#[no_mangle]
pub extern "C" fn libqb_http_connected(handle: c_int) -> c_int {
    http::http_connected(handle)
}

// ============================================================================
// Length / Status / URL
// ============================================================================

/// Get number of bytes available to read. Returns 0 on success, -1 on error.
/// On success, `*length` is set to the byte count.
///
/// # Safety
/// `length` must be a valid pointer to a size_t.
#[no_mangle]
pub unsafe extern "C" fn libqb_http_get_length(handle: c_int, length: *mut libc::size_t) -> c_int {
    if length.is_null() {
        return -1;
    }
    match http::http_get_length(handle) {
        Some(n) => {
            *length = n;
            0
        }
        None => -1,
    }
}

/// Get Content-Length from response header. Returns 0 on success, -1 if
/// header not present or invalid handle. On success, `*length` is set.
///
/// # Safety
/// `length` must be a valid pointer to uint64_t.
#[no_mangle]
pub unsafe extern "C" fn libqb_http_get_content_length(handle: c_int, length: *mut u64) -> c_int {
    if length.is_null() {
        return -1;
    }
    match http::http_get_content_length(handle) {
        Some(n) => {
            *length = n;
            0
        }
        None => -1,
    }
}

/// Get HTTP status code. Returns the code (e.g. 200) or -1 if invalid handle.
#[no_mangle]
pub extern "C" fn libqb_http_get_status_code(handle: c_int) -> c_int {
    http::http_get_status_code(handle)
}

/// Get effective URL (after redirects). Returns pointer valid until handle
/// is closed, or NULL if invalid handle or no URL. Caller must not free.
#[no_mangle]
pub extern "C" fn libqb_http_get_url(handle: c_int) -> *const c_char {
    http::http_get_url_ptr(handle)
}

// ============================================================================
// Read
// ============================================================================

/// Read up to `*length` bytes into `buf`. On return, `*length` is set to
/// the number of bytes actually read. Returns 0 on success, -1 on error.
///
/// # Safety
/// `buf` must point to at least `*length` bytes of writable memory.
/// `length` must be a valid pointer to size_t.
#[no_mangle]
pub unsafe extern "C" fn libqb_http_get(
    handle: c_int,
    buf: *mut c_char,
    length: *mut libc::size_t,
) -> c_int {
    if buf.is_null() || length.is_null() {
        return -1;
    }
    let max_len = *length;
    let slice = std::slice::from_raw_parts_mut(buf as *mut u8, max_len);
    let mut bytes_read = 0usize;
    if http::http_get(handle, slice, &mut bytes_read) {
        *length = bytes_read;
        0
    } else {
        -1
    }
}

/// Read exactly `length` bytes into `buf`. Returns 0 on success, -1 if
/// fewer bytes available or invalid handle.
///
/// # Safety
/// `buf` must point to at least `length` bytes of writable memory.
#[no_mangle]
pub unsafe extern "C" fn libqb_http_get_fixed(
    handle: c_int,
    buf: *mut c_char,
    length: libc::size_t,
) -> c_int {
    if buf.is_null() {
        return -1;
    }
    let slice = std::slice::from_raw_parts_mut(buf as *mut u8, length);
    if http::http_get_fixed(handle, slice, length) {
        0
    } else {
        -1
    }
}
