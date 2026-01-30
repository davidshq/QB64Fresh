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

/// Debug logging for string operations (leak detection).
///
/// When enabled via `QB64FRESH_STRING_DEBUG` environment variable,
/// logs all string allocations, releases, and reference count changes.
/// This helps detect memory leaks and double-free bugs.
#[cfg(debug_assertions)]
fn debug_log_string_op(op: &str, ptr: *const QbString, ref_count: usize, len: usize) {
    use std::env;
    if env::var("QB64FRESH_STRING_DEBUG").is_ok() {
        eprintln!(
            "[STRING_DEBUG] {}: ptr={:p}, ref_count={}, len={}",
            op, ptr, ref_count, len
        );
    }
}

#[cfg(not(debug_assertions))]
fn debug_log_string_op(_op: &str, _ptr: *const QbString, _ref_count: usize, _len: usize) {
    // Debug logging disabled in release builds
}

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

/// Convert a C string to QbString (alias for qb_string_new).
///
/// This function is used for fixed-length string conversions.
///
/// # Safety
/// - `s` must be a valid null-terminated C string or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_str_from_c(s: *const c_char) -> *mut QbString {
    qb_string_new(s)
}

/// Wrapper to make raw pointer Send+Sync for the singleton.
///
/// # Safety
/// This is safe because:
/// 1. BASIC programs are single-threaded
/// 2. The singleton is only initialized once and never freed
/// 3. The pointer is only read (never written) after initialization
struct EmptyStringPtr(*mut QbString);
unsafe impl Send for EmptyStringPtr {}
unsafe impl Sync for EmptyStringPtr {}

/// Static empty string singleton - never freed.
/// This is used for empty string comparisons and to avoid allocating
/// multiple empty strings.
static EMPTY_STRING_SINGLETON: std::sync::OnceLock<EmptyStringPtr> = std::sync::OnceLock::new();

/// Create an empty string.
///
/// Returns a singleton empty string that has very high refcount and will never be freed.
/// This is important for use as a global empty string constant.
///
/// # Safety
/// The returned string should NOT be released - it's a static singleton.
#[no_mangle]
pub extern "C" fn qb_string_empty() -> *mut QbString {
    EMPTY_STRING_SINGLETON
        .get_or_init(|| {
            unsafe {
                let ptr = qb_string_from_bytes(ptr::null(), 0);
                // Set refcount very high so it's never freed
                if !ptr.is_null() {
                    let header = get_header_mut(ptr as *mut c_char);
                    header.ref_count = usize::MAX / 2; // Very high but won't overflow on increment
                }
                EmptyStringPtr(ptr)
            }
        })
        .0
}

/// Create a QbString from a byte array.
///
/// # Safety
/// - `data` must be valid for reads of `len` bytes, or null if `len` is 0
/// - The returned string must be released with `qb_string_release`
///
/// # Returns
/// - A pointer to the newly allocated QbString, or NULL if allocation fails
/// - Returns NULL if `len` exceeds MAX_STRING_SIZE (100MB)
///
/// # Error Handling
/// This function returns NULL on allocation failure instead of aborting,
/// allowing callers to handle errors gracefully. Callers should check for NULL
/// before using the returned pointer.
#[no_mangle]
pub unsafe extern "C" fn qb_string_from_bytes(data: *const u8, len: usize) -> *mut QbString {
    // Check for reasonable size limit to prevent huge allocations
    // 100MB should be more than enough for any legitimate string
    const MAX_STRING_SIZE: usize = 100 * 1024 * 1024;
    if len > MAX_STRING_SIZE {
        // Return NULL for oversized strings instead of aborting
        return std::ptr::null_mut();
    }

    let capacity = len.max(16); // Minimum capacity for small string optimization
    let layout = string_layout(capacity);

    let ptr = alloc(layout);
    if ptr.is_null() {
        // Allocation failed - return NULL instead of aborting
        // Callers should check for NULL and handle gracefully
        return std::ptr::null_mut();
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
    let result = data_ptr as *mut QbString;

    // Debug logging for leak detection
    debug_log_string_op("ALLOC", result, 1, len);

    result
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
    let header = get_header_mut(data_ptr);
    let new_ref_count = header.ref_count + 1;
    header.ref_count = new_ref_count;

    // Debug logging for leak detection
    debug_log_string_op("RETAIN", s, new_ref_count, header.len);

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

    let len = header.len;
    header.ref_count -= 1;

    if header.ref_count == 0 {
        // Debug logging before free
        debug_log_string_op("FREE", s, 0, len);

        // Free the string allocation (header + data in one block)
        let capacity = header.capacity;
        let layout = string_layout(capacity);
        let header_ptr = (data_ptr as *mut QbStringHeader).offset(-1);
        dealloc(header_ptr as *mut u8, layout);
        // No extra Box to free - we return data pointer directly
    } else {
        // Debug logging for reference count decrement (not freed yet)
        debug_log_string_op("RELEASE", s, header.ref_count, len);
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

    // Additional validation: check pointer alignment and detect obviously bad pointers
    let s_addr = s as usize;
    if s_addr < 0x1000 || (s_addr & (std::mem::align_of::<usize>() - 1)) != 0 {
        eprintln!(
            "qb_string_len: suspicious pointer {:?} (low address or misaligned)",
            s
        );
        return 0;
    }

    let len = get_header(s as *const c_char).len;
    // Sanity check: if length is unreasonably large, treat as invalid/empty
    // This helps catch uninitialized or garbage pointers
    const MAX_REASONABLE_LEN: usize = 1024 * 1024 * 1024; // 1GB
    if len > MAX_REASONABLE_LEN {
        eprintln!(
            "qb_string_len: suspicious length {} at {:?}, treating as 0",
            len, s
        );
        return 0;
    }
    len
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

    // Sanity check: catch obviously invalid lengths that would cause allocation issues
    // Maximum reasonable string size is 1GB - anything larger is likely garbage data
    const MAX_REASONABLE_LEN: usize = 1024 * 1024 * 1024;
    if a_len > MAX_REASONABLE_LEN || b_len > MAX_REASONABLE_LEN {
        eprintln!(
            "qb_string_concat: invalid string length detected (a_len={}, b_len={})",
            a_len, b_len
        );
        eprintln!("  a pointer: {:?}, b pointer: {:?}", a, b);
        // Return empty string instead of crashing
        return qb_string_empty();
    }

    let new_len = match a_len.checked_add(b_len) {
        Some(len) => len,
        None => {
            eprintln!("qb_string_concat: length overflow");
            return qb_string_empty();
        }
    };

    let capacity = new_len.max(16);
    let layout = match std::alloc::Layout::from_size_align(
        std::mem::size_of::<QbStringHeader>() + capacity + 1,
        std::mem::align_of::<QbStringHeader>(),
    ) {
        Ok(l) => l,
        Err(e) => {
            eprintln!(
                "qb_string_concat: layout error: {} (capacity={})",
                e, capacity
            );
            return qb_string_empty();
        }
    };

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

    // Additional validation: check pointer alignment and try to detect garbage
    let s_addr = s as usize;
    if s_addr < 0x1000 || (s_addr & (std::mem::align_of::<usize>() - 1)) != 0 {
        eprintln!(
            "qb_rtrim: suspicious pointer {:?} (low address or misaligned)",
            s
        );
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

/// HEX$ - Convert number to hexadecimal string.
///
/// Treats the value as unsigned for formatting (matches QB64 behavior).
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_hex(n: i64) -> *mut QbString {
    let s = format!("{:X}", n as u64);
    unsafe { qb_string_from_bytes(s.as_ptr(), s.len()) }
}

/// OCT$ - Convert number to octal string.
///
/// Treats the value as unsigned for formatting (matches QB64 behavior).
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_oct(n: i64) -> *mut QbString {
    let s = format!("{:o}", n as u64);
    unsafe { qb_string_from_bytes(s.as_ptr(), s.len()) }
}

/// _BIN$ - Convert number to binary string.
///
/// Treats the value as unsigned for formatting (matches QB64 behavior).
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_bin(n: i64) -> *mut QbString {
    let s = format!("{:b}", n as u64);
    unsafe { qb_string_from_bytes(s.as_ptr(), s.len()) }
}

/// TRIM$ / _TRIM$ - Trim whitespace from both ends of a string.
///
/// # Safety
/// - `s` must be a valid QbString pointer or null
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub unsafe extern "C" fn qb_trim(s: *const QbString) -> *mut QbString {
    if s.is_null() {
        return qb_string_empty();
    }
    let len = qb_string_len(s);
    if len == 0 {
        return qb_string_empty();
    }
    let src = slice::from_raw_parts(qb_string_data(s) as *const u8, len);
    let start = src
        .iter()
        .position(|&c| c != b' ' && c != b'\t')
        .unwrap_or(len);
    let end = src
        .iter()
        .rposition(|&c| c != b' ' && c != b'\t')
        .map_or(0, |p| p + 1);
    if start >= end {
        return qb_string_empty();
    }
    qb_string_from_bytes((qb_string_data(s) as *const u8).add(start), end - start)
}

/// _INSTRREV(source, search) - Find last occurrence of search in source.
///
/// Returns 1-based position, or 0 if not found.
///
/// # Safety
/// - Both `source` and `search` must be valid QbString pointers or null
#[no_mangle]
pub unsafe extern "C" fn qb_instrrev(source: *const QbString, search: *const QbString) -> i32 {
    if source.is_null() || search.is_null() {
        return 0;
    }
    let s_len = qb_string_len(source);
    let n_len = qb_string_len(search);
    if n_len == 0 || n_len > s_len {
        return 0;
    }
    let s_data = slice::from_raw_parts(qb_string_data(source) as *const u8, s_len);
    let n_data = slice::from_raw_parts(qb_string_data(search) as *const u8, n_len);
    for i in (0..=s_len - n_len).rev() {
        if &s_data[i..i + n_len] == n_data {
            return (i + 1) as i32;
        }
    }
    0
}

/// _INSTRREV(s, sub, start) - Find last occurrence of sub in s from start.
///
/// `start` is 1-based; if &lt; 1 or &gt; len, search from end. Returns 1-based position or 0.
///
/// # Safety
/// - Both `s` and `sub` must be valid QbString pointers or null
#[no_mangle]
pub unsafe extern "C" fn qb_instrrev3(s: *const QbString, sub: *const QbString, start: i32) -> i32 {
    if s.is_null() || sub.is_null() {
        return 0;
    }
    let s_len = qb_string_len(s);
    let n_len = qb_string_len(sub);
    if n_len == 0 {
        return start;
    }
    if s_len < n_len {
        return 0;
    }
    let search_start = if start < 1 || start as usize > s_len {
        s_len
    } else {
        start as usize
    };
    let s_data = slice::from_raw_parts(qb_string_data(s) as *const u8, s_len);
    let n_data = slice::from_raw_parts(qb_string_data(sub) as *const u8, n_len);
    let from = search_start.saturating_sub(n_len);
    for i in (0..=from).rev() {
        if &s_data[i..i + n_len] == n_data {
            return (i + 1) as i32;
        }
    }
    0
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

/// _TOSTR$ - convert number to string without leading space.
///
/// Integer-valued doubles in the safe integer range are formatted without
/// decimals; others use %.14g. Used by `--runtime external` when the generated
/// C calls `qb_tostr` instead of an inline definition.
///
/// # Safety
/// - The returned string must be released with `qb_string_release`
#[no_mangle]
pub extern "C" fn qb_tostr(n: f64) -> *mut QbString {
    let s = if n.trunc() == n && n >= -9007199254740992.0 && n <= 9007199254740992.0 {
        format!("{:.0}", n)
    } else {
        // %.14g-like: compact representation, avoid trailing zeros
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

    // ========================================================================
    // NULL POINTER HANDLING TESTS
    // ========================================================================

    #[test]
    fn test_null_string_len() {
        unsafe {
            assert_eq!(qb_string_len(std::ptr::null()), 0);
        }
    }

    #[test]
    fn test_null_string_data() {
        unsafe {
            // Should return empty string, not crash
            let data = qb_string_data(std::ptr::null());
            assert!(!data.is_null());
            // The returned string should be empty (null-terminated)
            assert_eq!(*data, 0);
        }
    }

    #[test]
    fn test_null_string_retain() {
        unsafe {
            // Retain on null should return null, not crash
            let result = qb_string_retain(std::ptr::null_mut());
            assert!(result.is_null());
        }
    }

    #[test]
    fn test_null_string_release() {
        unsafe {
            // Release on null should not crash
            qb_string_release(std::ptr::null_mut());
        }
    }

    #[test]
    fn test_null_string_concat_both() {
        unsafe {
            let result = qb_string_concat(std::ptr::null(), std::ptr::null());
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_null_string_concat_first() {
        unsafe {
            let b = qb_string_new(b"World\0".as_ptr() as *const c_char);
            let result = qb_string_concat(std::ptr::null(), b);
            assert_eq!(qb_string_len(result), 5);
            qb_string_release(b);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_null_string_concat_second() {
        unsafe {
            let a = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let result = qb_string_concat(a, std::ptr::null());
            assert_eq!(qb_string_len(result), 5);
            qb_string_release(a);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_null_string_compare() {
        unsafe {
            // Comparing nulls should not crash
            let result = qb_string_compare(std::ptr::null(), std::ptr::null());
            assert_eq!(result, 0); // Both empty = equal
        }
    }

    #[test]
    fn test_null_asc() {
        unsafe {
            assert_eq!(qb_asc(std::ptr::null()), 0);
        }
    }

    #[test]
    fn test_null_left() {
        unsafe {
            let result = qb_left(std::ptr::null(), 5);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_null_right() {
        unsafe {
            let result = qb_right(std::ptr::null(), 5);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_null_mid() {
        unsafe {
            let result = qb_mid(std::ptr::null(), 1, 5);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_null_instr() {
        unsafe {
            assert_eq!(qb_instr(1, std::ptr::null(), std::ptr::null()), 0);
        }
    }

    #[test]
    fn test_null_ucase() {
        unsafe {
            let result = qb_ucase(std::ptr::null());
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_null_lcase() {
        unsafe {
            let result = qb_lcase(std::ptr::null());
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_null_ltrim() {
        unsafe {
            let result = qb_ltrim(std::ptr::null());
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_null_rtrim() {
        unsafe {
            let result = qb_rtrim(std::ptr::null());
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_null_val() {
        unsafe {
            assert_eq!(qb_val(std::ptr::null()), 0.0);
        }
    }

    // ========================================================================
    // REFERENCE COUNTING EDGE CASES
    // ========================================================================

    #[test]
    fn test_multiple_retain_release() {
        unsafe {
            let s = qb_string_new(b"Test\0".as_ptr() as *const c_char);

            // Retain multiple times
            qb_string_retain(s);
            qb_string_retain(s);
            qb_string_retain(s);

            // Should still be valid after multiple retains
            assert_eq!(qb_string_len(s), 4);

            // Release all references
            qb_string_release(s);
            qb_string_release(s);
            qb_string_release(s);
            qb_string_release(s); // Original reference
        }
    }

    #[test]
    fn test_refcount_after_operations() {
        unsafe {
            let a = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let b = qb_string_new(b"World\0".as_ptr() as *const c_char);

            // Operations should create new strings, not modify originals
            let concat = qb_string_concat(a, b);
            let upper = qb_ucase(a);
            let lower = qb_lcase(a);
            let left = qb_left(a, 3);
            let right = qb_right(a, 3);
            let mid = qb_mid(a, 2, 3);

            // Original strings should still be valid
            assert_eq!(qb_string_len(a), 5);
            assert_eq!(qb_string_len(b), 5);

            // Clean up all strings
            qb_string_release(a);
            qb_string_release(b);
            qb_string_release(concat);
            qb_string_release(upper);
            qb_string_release(lower);
            qb_string_release(left);
            qb_string_release(right);
            qb_string_release(mid);
        }
    }

    // ========================================================================
    // EDGE CASE VALUE TESTS
    // ========================================================================

    #[test]
    fn test_left_negative() {
        unsafe {
            let s = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let result = qb_left(s, -1);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_left_zero() {
        unsafe {
            let s = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let result = qb_left(s, 0);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_left_exceeds_length() {
        unsafe {
            let s = qb_string_new(b"Hi\0".as_ptr() as *const c_char);
            let result = qb_left(s, 100);
            assert_eq!(qb_string_len(result), 2); // Should only return what's available
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_right_negative() {
        unsafe {
            let s = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let result = qb_right(s, -1);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_right_zero() {
        unsafe {
            let s = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let result = qb_right(s, 0);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_right_exceeds_length() {
        unsafe {
            let s = qb_string_new(b"Hi\0".as_ptr() as *const c_char);
            let result = qb_right(s, 100);
            assert_eq!(qb_string_len(result), 2);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_mid_start_zero() {
        unsafe {
            let s = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let result = qb_mid(s, 0, 3);
            assert_eq!(qb_string_len(result), 0); // Invalid start
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_mid_start_negative() {
        unsafe {
            let s = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let result = qb_mid(s, -1, 3);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_mid_start_past_end() {
        unsafe {
            let s = qb_string_new(b"Hi\0".as_ptr() as *const c_char);
            let result = qb_mid(s, 10, 3);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_mid_length_negative_means_rest() {
        unsafe {
            let s = qb_string_new(b"Hello World\0".as_ptr() as *const c_char);
            let result = qb_mid(s, 7, -1); // Should return "World"
            assert_eq!(qb_string_len(result), 5);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_instr_start_negative() {
        unsafe {
            let haystack = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let needle = qb_string_new(b"l\0".as_ptr() as *const c_char);
            // Negative start should be treated as 1
            let result = qb_instr(-5, haystack, needle);
            assert_eq!(result, 3); // First 'l' is at position 3
            qb_string_release(haystack);
            qb_string_release(needle);
        }
    }

    #[test]
    fn test_space_negative() {
        let result = qb_space(-5);
        unsafe {
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_space_zero() {
        let result = qb_space(0);
        unsafe {
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_space_positive() {
        let result = qb_space(5);
        unsafe {
            assert_eq!(qb_string_len(result), 5);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_string_fill_negative() {
        let result = qb_string_fill(-5, 65);
        unsafe {
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_string_fill_zero() {
        let result = qb_string_fill(0, 65);
        unsafe {
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_string_fill_positive() {
        let result = qb_string_fill(5, 65); // 5 'A's
        unsafe {
            assert_eq!(qb_string_len(result), 5);
            qb_string_release(result);
        }
    }

    // ========================================================================
    // LARGE STRING TESTS
    // ========================================================================

    #[test]
    fn test_large_string_creation() {
        unsafe {
            // Create a large string (1MB)
            let size = 1024 * 1024;
            let data = vec![b'A'; size];
            let s = qb_string_from_bytes(data.as_ptr(), size);

            assert_eq!(qb_string_len(s), size);

            qb_string_release(s);
        }
    }

    #[test]
    fn test_large_string_concat() {
        unsafe {
            // Create two medium strings and concat them
            let size = 100_000;
            let data = vec![b'A'; size];
            let a = qb_string_from_bytes(data.as_ptr(), size);
            let b = qb_string_from_bytes(data.as_ptr(), size);

            let concat = qb_string_concat(a, b);
            assert_eq!(qb_string_len(concat), size * 2);

            qb_string_release(a);
            qb_string_release(b);
            qb_string_release(concat);
        }
    }

    #[test]
    fn test_many_small_strings() {
        unsafe {
            // Create and release many small strings to test memory management
            for i in 0..1000 {
                let data = format!("String {}", i);
                let s = qb_string_from_bytes(data.as_ptr(), data.len());
                assert_eq!(qb_string_len(s), data.len());
                qb_string_release(s);
            }
        }
    }

    // ========================================================================
    // BINARY DATA TESTS
    // ========================================================================

    #[test]
    fn test_string_with_embedded_nulls() {
        unsafe {
            // BASIC strings can contain embedded null bytes
            let data = b"Hello\0World";
            let s = qb_string_from_bytes(data.as_ptr(), data.len());

            // Length should include the embedded null
            assert_eq!(qb_string_len(s), 11);

            qb_string_release(s);
        }
    }

    #[test]
    fn test_string_with_high_bytes() {
        unsafe {
            // Test with high ASCII/binary values
            let data: [u8; 5] = [0xFF, 0xFE, 0x00, 0x01, 0x80];
            let s = qb_string_from_bytes(data.as_ptr(), data.len());

            assert_eq!(qb_string_len(s), 5);

            qb_string_release(s);
        }
    }

    // ========================================================================
    // STRING CONVERSION TESTS
    // ========================================================================

    #[test]
    fn test_str_int_negative() {
        let s = qb_str_int(-42);
        unsafe {
            // Negative numbers should not have leading space
            let data = qb_string_data(s);
            assert_eq!(*data as u8, b'-');
            qb_string_release(s);
        }
    }

    #[test]
    fn test_str_int_zero() {
        let s = qb_str_int(0);
        unsafe {
            assert!(qb_string_len(s) > 0);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_str_int_max() {
        let s = qb_str_int(i64::MAX);
        unsafe {
            assert!(qb_string_len(s) > 0);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_str_int_min() {
        let s = qb_str_int(i64::MIN);
        unsafe {
            assert!(qb_string_len(s) > 0);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_str_float_negative() {
        let s = qb_str_float(-3.14);
        unsafe {
            let data = qb_string_data(s);
            assert_eq!(*data as u8, b'-');
            qb_string_release(s);
        }
    }

    #[test]
    fn test_str_float_positive_has_space() {
        let s = qb_str_float(3.14);
        unsafe {
            let data = qb_string_data(s);
            // Positive floats should have leading space
            assert_eq!(*data as u8, b' ');
            qb_string_release(s);
        }
    }

    #[test]
    fn test_val_whitespace() {
        unsafe {
            let s = qb_string_new(b"  42  \0".as_ptr() as *const c_char);
            assert_eq!(qb_val(s), 42.0);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_val_invalid_returns_zero() {
        unsafe {
            let s = qb_string_new(b"not a number\0".as_ptr() as *const c_char);
            assert_eq!(qb_val(s), 0.0);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_val_empty_string() {
        unsafe {
            let s = qb_string_empty();
            assert_eq!(qb_val(s), 0.0);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_val_float() {
        unsafe {
            let s = qb_string_new(b"3.14159\0".as_ptr() as *const c_char);
            let v = qb_val(s);
            assert!((v - 3.14159).abs() < 0.0001);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_val_negative() {
        unsafe {
            let s = qb_string_new(b"-123\0".as_ptr() as *const c_char);
            assert_eq!(qb_val(s), -123.0);
            qb_string_release(s);
        }
    }

    // ========================================================================
    // CHR$ EDGE CASES
    // ========================================================================

    #[test]
    fn test_chr_null_byte() {
        let s = qb_chr(0);
        unsafe {
            assert_eq!(qb_string_len(s), 1);
            let data = qb_string_data(s);
            assert_eq!(*data as u8, 0);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_chr_max_byte() {
        let s = qb_chr(255);
        unsafe {
            assert_eq!(qb_string_len(s), 1);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_chr_overflow() {
        // Values > 255 should wrap around
        let s = qb_chr(256);
        unsafe {
            assert_eq!(qb_string_len(s), 1);
            let data = qb_string_data(s);
            assert_eq!(*data as u8, 0); // 256 mod 256 = 0
            qb_string_release(s);
        }
    }

    // ========================================================================
    // ASC EDGE CASES
    // ========================================================================

    #[test]
    fn test_asc_empty_string() {
        unsafe {
            let s = qb_string_empty();
            assert_eq!(qb_asc(s), 0);
            qb_string_release(s);
        }
    }

    #[test]
    fn test_asc_high_byte() {
        unsafe {
            let data: [u8; 1] = [255];
            let s = qb_string_from_bytes(data.as_ptr(), 1);
            assert_eq!(qb_asc(s), 255);
            qb_string_release(s);
        }
    }

    // ========================================================================
    // CASE CONVERSION EDGE CASES
    // ========================================================================

    #[test]
    fn test_ucase_empty() {
        unsafe {
            let s = qb_string_empty();
            let result = qb_ucase(s);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_lcase_empty() {
        unsafe {
            let s = qb_string_empty();
            let result = qb_lcase(s);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_ucase_already_upper() {
        unsafe {
            let s = qb_string_new(b"HELLO\0".as_ptr() as *const c_char);
            let result = qb_ucase(s);
            assert_eq!(qb_string_len(result), 5);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_lcase_already_lower() {
        unsafe {
            let s = qb_string_new(b"hello\0".as_ptr() as *const c_char);
            let result = qb_lcase(s);
            assert_eq!(qb_string_len(result), 5);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    // ========================================================================
    // TRIM EDGE CASES
    // ========================================================================

    #[test]
    fn test_ltrim_empty() {
        unsafe {
            let s = qb_string_empty();
            let result = qb_ltrim(s);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_rtrim_empty() {
        unsafe {
            let s = qb_string_empty();
            let result = qb_rtrim(s);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_ltrim_all_spaces() {
        unsafe {
            let s = qb_string_new(b"     \0".as_ptr() as *const c_char);
            let result = qb_ltrim(s);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_rtrim_all_spaces() {
        unsafe {
            let s = qb_string_new(b"     \0".as_ptr() as *const c_char);
            let result = qb_rtrim(s);
            assert_eq!(qb_string_len(result), 0);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_ltrim_no_leading_spaces() {
        unsafe {
            let s = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let result = qb_ltrim(s);
            assert_eq!(qb_string_len(result), 5);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    #[test]
    fn test_rtrim_no_trailing_spaces() {
        unsafe {
            let s = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let result = qb_rtrim(s);
            assert_eq!(qb_string_len(result), 5);
            qb_string_release(s);
            qb_string_release(result);
        }
    }

    // ========================================================================
    // STRING COMPARISON EDGE CASES
    // ========================================================================

    #[test]
    fn test_compare_empty_strings() {
        unsafe {
            let a = qb_string_empty();
            let b = qb_string_empty();
            assert_eq!(qb_string_compare(a, b), 0);
            qb_string_release(a);
            qb_string_release(b);
        }
    }

    #[test]
    fn test_compare_same_strings() {
        unsafe {
            let a = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let b = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            assert_eq!(qb_string_compare(a, b), 0);
            qb_string_release(a);
            qb_string_release(b);
        }
    }

    #[test]
    fn test_compare_different_strings() {
        unsafe {
            let a = qb_string_new(b"Apple\0".as_ptr() as *const c_char);
            let b = qb_string_new(b"Banana\0".as_ptr() as *const c_char);
            assert!(qb_string_compare(a, b) < 0); // Apple < Banana
            assert!(qb_string_compare(b, a) > 0); // Banana > Apple
            qb_string_release(a);
            qb_string_release(b);
        }
    }

    #[test]
    fn test_compare_prefix_string() {
        unsafe {
            let a = qb_string_new(b"Hello\0".as_ptr() as *const c_char);
            let b = qb_string_new(b"Hello World\0".as_ptr() as *const c_char);
            assert!(qb_string_compare(a, b) < 0); // "Hello" < "Hello World"
            qb_string_release(a);
            qb_string_release(b);
        }
    }
}
