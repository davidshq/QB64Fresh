//! QB64Fresh String Implementation
//!
//! BASIC strings are dynamic, variable-length, and can be freely copied and
//! concatenated. This module provides a reference-counted string type that
//! is compatible with C code.
//!
//! # Memory Management
//!
//! Each `QbString` is a pointer to a heap-allocated structure containing:
//! - Reference count
//! - Length
//! - Character data (null-terminated for C compatibility)
//!
//! When a string is "copied" in BASIC, we just increment the reference count.
//! When a string goes out of scope, we decrement the count and free when it hits zero.
//!
//! # Thread Safety
//!
//! The current implementation is NOT thread-safe. BASIC programs are traditionally
//! single-threaded, so this is acceptable for now.

use std::alloc::{alloc, dealloc, Layout};
use std::ffi::CStr;
use std::os::raw::c_char;
use std::ptr;
use std::slice;

/// Internal string header stored before the character data.
#[repr(C)]
struct QbStringHeader {
    /// Reference count (starts at 1)
    ref_count: usize,
    /// Length in bytes (not including null terminator)
    len: usize,
    /// Capacity (allocated bytes, not including header)
    capacity: usize,
}

/// Opaque string handle passed to/from C code.
///
/// This is an opaque type - the pointer IS the string data pointer directly,
/// with the header stored immediately before it. There's no extra wrapper
/// struct allocation; the QbString "type" is just a marker for the pointer.
///
/// Memory layout: [QbStringHeader][character data...][null]
///                                 ^-- pointer points here
///
/// This design eliminates the double indirection that would occur if
/// we boxed a wrapper struct containing the data pointer.
#[repr(C)]
pub struct QbString {
    // Opaque type - never instantiated, just used as a pointer target
    _opaque: [u8; 0],
}

/// Gets the header for a string pointer.
#[inline]
unsafe fn get_header(data_ptr: *const c_char) -> &'static QbStringHeader {
    let header_ptr = (data_ptr as *const QbStringHeader).offset(-1);
    &*header_ptr
}

/// Gets the mutable header for a string pointer.
#[inline]
unsafe fn get_header_mut(data_ptr: *mut c_char) -> &'static mut QbStringHeader {
    let header_ptr = (data_ptr as *mut QbStringHeader).offset(-1);
    &mut *header_ptr
}

/// Calculate the layout for a string allocation.
fn string_layout(capacity: usize) -> Layout {
    // Header + capacity + null terminator
    let size = std::mem::size_of::<QbStringHeader>() + capacity + 1;
    Layout::from_size_align(size, std::mem::align_of::<QbStringHeader>()).unwrap()
}

/// Create a new string from a C string literal.
///
/// # Safety
/// - `s` must be a valid null-terminated C string
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_string_new(s: *const c_char) -> *mut QbString {
    if s.is_null() {
        return qb_string_empty();
    }

    let cstr = CStr::from_ptr(s);
    let bytes = cstr.to_bytes();
    qb_string_from_bytes(bytes.as_ptr(), bytes.len())
}

/// Create an empty string.
///
/// # Safety
/// The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_string_empty() -> *mut QbString {
    unsafe { qb_string_from_bytes(ptr::null(), 0) }
}

/// Create a string from raw bytes.
///
/// # Safety
/// - If `data` is not null, it must point to at least `len` valid bytes
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_string_from_bytes(data: *const u8, len: usize) -> *mut QbString {
    let capacity = len.max(16); // Minimum capacity for small string optimization
    let layout = string_layout(capacity);

    let ptr = alloc(layout);
    if ptr.is_null() {
        // Allocation failed - in a real runtime we'd handle this better
        std::process::abort();
    }

    // Initialize header
    let header = ptr as *mut QbStringHeader;
    (*header).ref_count = 1;
    (*header).len = len;
    (*header).capacity = capacity;

    // Get data pointer (right after header)
    let data_ptr = header.offset(1) as *mut c_char;

    // Copy string data
    if !data.is_null() && len > 0 {
        ptr::copy_nonoverlapping(data, data_ptr as *mut u8, len);
    }

    // Null-terminate
    *data_ptr.add(len) = 0;

    // Return data pointer directly as *mut QbString
    // (QbString is opaque - the pointer IS the string handle)
    data_ptr as *mut QbString
}

/// Increment the reference count of a string.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_string_retain(s: *mut QbString) -> *mut QbString {
    if s.is_null() {
        return s;
    }

    let data_ptr = s as *mut c_char;
    get_header_mut(data_ptr).ref_count += 1;
    s
}

/// Decrement the reference count and free if it reaches zero.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
/// - After calling this, `s` should not be used unless retained elsewhere
#[no_mangle]
pub unsafe extern "C" fn qb_string_release(s: *mut QbString) {
    if s.is_null() {
        return;
    }

    let data_ptr = s as *mut c_char;
    let header = get_header_mut(data_ptr);

    // Guard against underflow - if ref_count is already 0, this is a double-free bug
    if header.ref_count == 0 {
        // Already freed or corrupted - don't double-free
        // In debug builds, this would indicate a bug in the calling code
        #[cfg(debug_assertions)]
        panic!("qb_string_release: ref_count already 0 (double-free detected)");
        #[cfg(not(debug_assertions))]
        return;
    }

    header.ref_count -= 1;
    if header.ref_count == 0 {
        // Free the string allocation (header + data in one block)
        let capacity = header.capacity;
        let layout = string_layout(capacity);
        let header_ptr = (data_ptr as *mut QbStringHeader).offset(-1);
        dealloc(header_ptr as *mut u8, layout);
        // No extra Box to free - we return data pointer directly
    }
}

/// Get the length of a string.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_string_len(s: *const QbString) -> usize {
    if s.is_null() {
        return 0;
    }
    get_header(s as *const c_char).len
}

/// Get a pointer to the string's character data (null-terminated).
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
/// - The returned pointer is valid until the string is modified or released
#[no_mangle]
pub unsafe extern "C" fn qb_string_data(s: *const QbString) -> *const c_char {
    if s.is_null() {
        return b"\0".as_ptr() as *const c_char;
    }
    // The QbString pointer IS the data pointer
    s as *const c_char
}

/// Concatenate two strings, returning a new string.
///
/// # Safety
/// - Both `a` and `b` must be valid QbString pointers or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_string_concat(a: *const QbString, b: *const QbString) -> *mut QbString {
    let a_len = qb_string_len(a);
    let b_len = qb_string_len(b);
    let new_len = a_len + b_len;

    let capacity = new_len.max(16);
    let layout = string_layout(capacity);

    let ptr = alloc(layout);
    if ptr.is_null() {
        std::process::abort();
    }

    // Initialize header
    let header = ptr as *mut QbStringHeader;
    (*header).ref_count = 1;
    (*header).len = new_len;
    (*header).capacity = capacity;

    // Get data pointer
    let data_ptr = header.offset(1) as *mut u8;

    // Copy first string
    if a_len > 0 {
        ptr::copy_nonoverlapping(qb_string_data(a) as *const u8, data_ptr, a_len);
    }

    // Copy second string
    if b_len > 0 {
        ptr::copy_nonoverlapping(qb_string_data(b) as *const u8, data_ptr.add(a_len), b_len);
    }

    // Null-terminate
    *data_ptr.add(new_len) = 0;

    // Return data pointer directly as *mut QbString
    data_ptr as *mut c_char as *mut QbString
}

/// Compare two strings.
///
/// Returns:
/// - < 0 if a < b
/// - 0 if a == b
/// - > 0 if a > b
///
/// # Safety
/// - Both `a` and `b` must be valid QbString pointers or null
#[no_mangle]
pub unsafe extern "C" fn qb_string_compare(a: *const QbString, b: *const QbString) -> i32 {
    let a_data = qb_string_data(a);
    let b_data = qb_string_data(b);

    libc::strcmp(a_data, b_data)
}

/// Create a string containing a single character.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_chr(code: i32) -> *mut QbString {
    let c = code as u8;
    unsafe { qb_string_from_bytes(&c, 1) }
}

/// Get the ASCII code of the first character in a string.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_asc(s: *const QbString) -> i32 {
    if s.is_null() || qb_string_len(s) == 0 {
        return 0;
    }
    *qb_string_data(s) as u8 as i32
}

/// Get the leftmost n characters of a string.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_left(s: *const QbString, n: i32) -> *mut QbString {
    if s.is_null() || n <= 0 {
        return qb_string_empty();
    }

    let len = qb_string_len(s);
    let take = (n as usize).min(len);
    qb_string_from_bytes(qb_string_data(s) as *const u8, take)
}

/// Get the rightmost n characters of a string.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_right(s: *const QbString, n: i32) -> *mut QbString {
    if s.is_null() || n <= 0 {
        return qb_string_empty();
    }

    let len = qb_string_len(s);
    let take = (n as usize).min(len);
    let start = len - take;
    qb_string_from_bytes((qb_string_data(s) as *const u8).add(start), take)
}

/// Get a substring from a string.
///
/// # Arguments
/// * `s` - Source string
/// * `start` - 1-based start position
/// * `length` - Number of characters to extract (-1 for rest of string)
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_mid(s: *const QbString, start: i32, length: i32) -> *mut QbString {
    if s.is_null() || start < 1 {
        return qb_string_empty();
    }

    let len = qb_string_len(s);
    let start_idx = (start as usize).saturating_sub(1); // Convert to 0-based

    if start_idx >= len {
        return qb_string_empty();
    }

    let remaining = len - start_idx;
    let take = if length < 0 {
        remaining
    } else {
        (length as usize).min(remaining)
    };

    qb_string_from_bytes((qb_string_data(s) as *const u8).add(start_idx), take)
}

/// Find a substring within a string.
///
/// # Arguments
/// * `haystack` - String to search in
/// * `needle` - String to search for
/// * `start` - 1-based start position (0 or 1 starts from beginning)
///
/// Returns: 1-based position of needle, or 0 if not found
///
/// # Safety
/// - Both strings must be valid QbString pointers or null
#[no_mangle]
pub unsafe extern "C" fn qb_instr(
    start: i32,
    haystack: *const QbString,
    needle: *const QbString,
) -> i32 {
    if haystack.is_null() || needle.is_null() {
        return 0;
    }

    let h_len = qb_string_len(haystack);
    let n_len = qb_string_len(needle);

    if n_len == 0 {
        return if start <= 1 { 1 } else { start };
    }

    if h_len == 0 || n_len > h_len {
        return 0;
    }

    let start_idx = if start <= 1 { 0 } else { (start - 1) as usize };
    if start_idx >= h_len {
        return 0;
    }

    let h_data = slice::from_raw_parts(qb_string_data(haystack) as *const u8, h_len);
    let n_data = slice::from_raw_parts(qb_string_data(needle) as *const u8, n_len);

    // Use saturating_sub to avoid potential underflow panic in debug mode
    // (Even though we checked n_len <= h_len above, this is defensive coding)
    let search_end = h_len.saturating_sub(n_len);

    // Simple substring search
    for i in start_idx..=search_end {
        if &h_data[i..i + n_len] == n_data {
            return (i + 1) as i32; // Return 1-based position
        }
    }

    0
}

/// Convert a string to uppercase.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_ucase(s: *const QbString) -> *mut QbString {
    if s.is_null() {
        return qb_string_empty();
    }

    let len = qb_string_len(s);
    if len == 0 {
        return qb_string_empty();
    }

    let src = slice::from_raw_parts(qb_string_data(s) as *const u8, len);
    let upper: Vec<u8> = src.iter().map(|&c| c.to_ascii_uppercase()).collect();

    qb_string_from_bytes(upper.as_ptr(), len)
}

/// Convert a string to lowercase.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_lcase(s: *const QbString) -> *mut QbString {
    if s.is_null() {
        return qb_string_empty();
    }

    let len = qb_string_len(s);
    if len == 0 {
        return qb_string_empty();
    }

    let src = slice::from_raw_parts(qb_string_data(s) as *const u8, len);
    let lower: Vec<u8> = src.iter().map(|&c| c.to_ascii_lowercase()).collect();

    qb_string_from_bytes(lower.as_ptr(), len)
}

/// Trim leading spaces from a string.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_ltrim(s: *const QbString) -> *mut QbString {
    if s.is_null() {
        return qb_string_empty();
    }

    let len = qb_string_len(s);
    if len == 0 {
        return qb_string_empty();
    }

    let src = slice::from_raw_parts(qb_string_data(s) as *const u8, len);
    let start = src.iter().position(|&c| c != b' ').unwrap_or(len);

    qb_string_from_bytes(src.as_ptr().add(start), len - start)
}

/// Trim trailing spaces from a string.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_rtrim(s: *const QbString) -> *mut QbString {
    if s.is_null() {
        return qb_string_empty();
    }

    let len = qb_string_len(s);
    if len == 0 {
        return qb_string_empty();
    }

    let src = slice::from_raw_parts(qb_string_data(s) as *const u8, len);
    let end = src.iter().rposition(|&c| c != b' ').map_or(0, |p| p + 1);

    qb_string_from_bytes(src.as_ptr(), end)
}

/// Create a string of n spaces.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_space(n: i32) -> *mut QbString {
    if n <= 0 {
        return qb_string_empty();
    }

    let spaces = vec![b' '; n as usize];
    unsafe { qb_string_from_bytes(spaces.as_ptr(), n as usize) }
}

/// Create a string by repeating a character n times.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_string_fill(n: i32, char_code: i32) -> *mut QbString {
    if n <= 0 {
        return qb_string_empty();
    }

    let chars = vec![char_code as u8; n as usize];
    unsafe { qb_string_from_bytes(chars.as_ptr(), n as usize) }
}

/// Convert a number to its string representation.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_str_int(n: i64) -> *mut QbString {
    let s = if n >= 0 {
        format!(" {}", n) // BASIC adds leading space for positive numbers
    } else {
        format!("{}", n)
    };
    unsafe { qb_string_from_bytes(s.as_ptr(), s.len()) }
}

/// Convert a floating-point number to its string representation.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_str_float(n: f64) -> *mut QbString {
    let s = if n >= 0.0 {
        format!(" {}", n)
    } else {
        format!("{}", n)
    };
    unsafe { qb_string_from_bytes(s.as_ptr(), s.len()) }
}

/// Convert a string to a number (VAL function).
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
#[no_mangle]
pub unsafe extern "C" fn qb_val(s: *const QbString) -> f64 {
    if s.is_null() {
        return 0.0;
    }

    let len = qb_string_len(s);
    if len == 0 {
        return 0.0;
    }

    let data = slice::from_raw_parts(qb_string_data(s) as *const u8, len);
    let s = String::from_utf8_lossy(data);
    let trimmed = s.trim();

    // Try to parse as float
    trimmed.parse::<f64>().unwrap_or(0.0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_new_and_len() {
        unsafe {
            let s = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            assert_eq!(qb_string_len(s), 5);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_string_empty() {
        unsafe {
            let s = qb_string_empty();
            assert_eq!(qb_string_len(s), 0);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_string_concat() {
        unsafe {
            let a = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let b = qb_string_new(b" World\0".as_ptr() as *const c_char);
            let c = qb_string_concat(a, b);

            assert_eq!(qb_string_len(c), 11);

            qb_string_release(a);
            qb_string_release(b);
            qb_string_release(c);
        }
    }

    #[test]
    fn test_string_refcount() {
        unsafe {
            let s = qb_string_new(b"Test\0".as_ptr() as *const c_char);
            let s2 = qb_string_retain(s);

            // Both point to the same data
            assert_eq!(qb_string_data(s), qb_string_data(s2));

            qb_string_release(s);
            // s2 should still be valid
            assert_eq!(qb_string_len(s2), 4);

            qb_string_release(s2);
        }
    }

    #[test]
    fn test_chr_and_asc() {
        unsafe {
            let s = qb_chr(65); // 'A'
            assert_eq!(qb_string_len(s), 1);
            assert_eq!(qb_asc(s), 65);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_left_right_mid() {
        unsafe {
            let s = qb_string_new(b"Hello World\0".as_ptr() as *const c_char);

            let left = qb_left(s, 5);
            assert_eq!(qb_string_len(left), 5);

            let right = qb_right(s, 5);
            assert_eq!(qb_string_len(right), 5);

            let mid = qb_mid(s, 7, 5); // "World"
            assert_eq!(qb_string_len(mid), 5);

            qb_string_release(s);
            qb_string_release(left);
            qb_string_release(right);
            qb_string_release(mid);
        }
    }

    #[test]
    fn test_instr() {
        unsafe {
            let haystack = qb_string_new(b"Hello World\0".as_ptr() as *const c_char);
            let needle = qb_string_new(b"World\0".as_ptr() as *const c_char);

            assert_eq!(qb_instr(1, haystack, needle), 7);

            qb_string_release(haystack);
            qb_string_release(needle);
        }
    }

    #[test]
    fn test_instr_edge_cases() {
        unsafe {
            let short = qb_string_new(b"Hi\0".as_ptr() as *const c_char);
            let long = qb_string_new(b"Hello World\0".as_ptr() as *const c_char);
            let empty = qb_string_empty();

            // Needle longer than haystack - should return 0, not panic
            assert_eq!(qb_instr(1, short, long), 0);

            // Empty needle - should return start position
            assert_eq!(qb_instr(1, short, empty), 1);
            assert_eq!(qb_instr(5, short, empty), 5);

            // Start position past haystack length
            assert_eq!(qb_instr(100, short, short), 0);

            // Both empty
            assert_eq!(qb_instr(1, empty, empty), 1);

            qb_string_release(short);
            qb_string_release(long);
            qb_string_release(empty);
        }
    }

    #[test]
    fn test_ucase_lcase() {
        unsafe {
            let s = qb_string_new(b"Hello\0".as_ptr() as *const c_char);

            let upper = qb_ucase(s);
            let lower = qb_lcase(s);

            // Check lengths
            assert_eq!(qb_string_len(upper), 5);
            assert_eq!(qb_string_len(lower), 5);

            qb_string_release(s);
            qb_string_release(upper);
            qb_string_release(lower);
        }
    }

    #[test]
    fn test_trim() {
        unsafe {
            let s = qb_string_new(b"  Hello  \0".as_ptr() as *const c_char);

            let ltrimmed = qb_ltrim(s);
            assert_eq!(qb_string_len(ltrimmed), 7); // "Hello  "

            let rtrimmed = qb_rtrim(s);
            assert_eq!(qb_string_len(rtrimmed), 7); // "  Hello"

            qb_string_release(s);
            qb_string_release(ltrimmed);
            qb_string_release(rtrimmed);
        }
    }

    #[test]
    fn test_str_and_val() {
        unsafe {
            let s = qb_str_int(42);
            assert!(qb_string_len(s) > 0);

            let v = qb_val(s);
            assert_eq!(v, 42.0);

            qb_string_release(s);
        }
    }
}
