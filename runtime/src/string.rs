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
/// This is actually a pointer to the character data, with the header
/// stored immediately before it. This layout allows C code to access
/// the string data directly while Rust manages the header.
#[repr(transparent)]
pub struct QbString {
    /// Pointer to the first character (header is before this)
    data: *mut c_char,
}

impl QbString {
    /// Gets the header for this string.
    #[inline]
    fn header(&self) -> &QbStringHeader {
        unsafe {
            let header_ptr = (self.data as *mut QbStringHeader).offset(-1);
            &*header_ptr
        }
    }

    /// Gets the mutable header for this string.
    #[inline]
    fn header_mut(&mut self) -> &mut QbStringHeader {
        unsafe {
            let header_ptr = (self.data as *mut QbStringHeader).offset(-1);
            &mut *header_ptr
        }
    }

    /// Returns true if this is the only reference to the string.
    /// (Reserved for future copy-on-write optimization)
    #[inline]
    #[allow(dead_code)]
    fn is_unique(&self) -> bool {
        self.header().ref_count == 1
    }
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

    // Return as QbString pointer (we're returning a pointer to the struct on the heap)
    // Actually, we need to box the QbString itself
    let qb_str = Box::new(QbString { data: data_ptr });
    Box::into_raw(qb_str)
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

    let str_ref = &mut *s;
    str_ref.header_mut().ref_count += 1;
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

    let str_ref = &mut *s;
    let header = str_ref.header_mut();

    header.ref_count -= 1;
    if header.ref_count == 0 {
        // Free the string data
        let capacity = header.capacity;
        let layout = string_layout(capacity);
        let header_ptr = (str_ref.data as *mut QbStringHeader).offset(-1);
        dealloc(header_ptr as *mut u8, layout);

        // Free the QbString struct itself
        drop(Box::from_raw(s));
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
    (*s).header().len
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
    (*s).data
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

    let qb_str = Box::new(QbString {
        data: data_ptr as *mut c_char,
    });
    Box::into_raw(qb_str)
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

    // Simple substring search
    for i in start_idx..=(h_len - n_len) {
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
