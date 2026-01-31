//! Path utilities (LIBQB filepath.h compatibility).
//!
//! Provides filename/extension extraction, extension matching, path separator
//! normalization, and split/join. Behavior matches QB64pe's filepath.cpp
//! (derived from miniaudio.h). Returned strings are QbString*; caller must
//! call `qb_string_release` when done.

use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use crate::string::{qb_string_from_bytes, qb_string_new, QbString};

/// Returns the filename portion of a path (last component after `/` or `\`).
///
/// Returns a new QbString; caller must call `qb_string_release`. Returns empty
/// string if path is null or has no filename (e.g. trailing slash only).
///
/// # Safety
/// - `path` must be a valid null-terminated C string or null
#[no_mangle]
pub unsafe extern "C" fn filepath_get_filename(path: *const c_char) -> *mut QbString {
    if path.is_null() {
        return qb_string_new(std::ptr::null());
    }
    let s = match CStr::from_ptr(path).to_str() {
        Ok(x) => x,
        Err(_) => return qb_string_new(std::ptr::null()),
    };
    let bytes = s.as_bytes();
    let mut last_slash = None::<usize>;
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'/' || b == b'\\' {
            last_slash = Some(i);
        }
    }
    let start = match last_slash {
        Some(i) => {
            let mut j = i + 1;
            while j < bytes.len() && (bytes[j] == b'/' || bytes[j] == b'\\') {
                j += 1;
            }
            j
        }
        None => 0,
    };
    let slice = &bytes[start..];
    if slice.is_empty() {
        return qb_string_new(std::ptr::null());
    }
    qb_string_from_bytes(slice.as_ptr(), slice.len())
}

/// Returns the extension part of a path (text after the last `.` in the filename).
///
/// Returns a new QbString; caller must call `qb_string_release`. If there is
/// no extension, returns empty string.
///
/// # Safety
/// - `path` must be a valid null-terminated C string or null
#[no_mangle]
pub unsafe extern "C" fn filepath_get_extension(path: *const c_char) -> *mut QbString {
    if path.is_null() {
        return qb_string_new(std::ptr::null());
    }
    let filename = filepath_get_filename(path);
    let data = crate::string::qb_string_data(filename);
    let len = crate::string::qb_string_len(filename);
    let mut last_dot = None::<usize>;
    for i in 0..len {
        if *data.add(i) == b'.' as c_char {
            last_dot = Some(i);
        }
    }
    let result = match last_dot {
        Some(i) => {
            let start = i + 1;
            if start < len {
                qb_string_from_bytes(data.add(start) as *const u8, len - start)
            } else {
                qb_string_new(std::ptr::null())
            }
        }
        None => qb_string_new(std::ptr::null()),
    };
    crate::string::qb_string_release(filename);
    result
}

/// Returns whether the path has the given extension (case-insensitive).
///
/// # Safety
/// - `path` and `extension` must be valid null-terminated C strings or null
#[no_mangle]
pub unsafe extern "C" fn filepath_has_extension(
    path: *const c_char,
    extension: *const c_char,
) -> i32 {
    if path.is_null() || extension.is_null() {
        return 0;
    }
    let ext_cstr = CStr::from_ptr(extension);
    let ext_bytes = ext_cstr.to_bytes();
    let path_ext = filepath_get_extension(path);
    let path_ext_data = crate::string::qb_string_data(path_ext);
    let path_ext_len = crate::string::qb_string_len(path_ext);
    let eq = if path_ext_len == ext_bytes.len() {
        ext_bytes
            .iter()
            .zip(std::slice::from_raw_parts(path_ext_data, path_ext_len))
            .all(|(a, b)| to_ascii_lowercase(*a) == to_ascii_lowercase(*b as u8))
    } else {
        false
    };
    crate::string::qb_string_release(path_ext);
    if eq {
        1
    } else {
        0
    }
}

#[inline]
fn to_ascii_lowercase(b: u8) -> u8 {
    if b >= b'A' && b <= b'Z' {
        b + (b'a' - b'A')
    } else {
        b
    }
}

/// Normalizes path separators in place: on Windows `'/'` → `'\\'`, elsewhere `'\\'` → `'/'`.
///
/// # Safety
/// - `path` must point to a valid null-terminated buffer that may be modified
#[no_mangle]
pub unsafe extern "C" fn filepath_fix_directory(path: *mut c_char) -> *const c_char {
    if path.is_null() {
        return std::ptr::null();
    }
    let mut p = path;
    while *p != 0 {
        let c = *p as u8;
        #[cfg(windows)]
        if c == b'/' {
            *p = b'\\' as c_char;
        }
        #[cfg(not(windows))]
        if c == b'\\' {
            *p = b'/' as c_char;
        }
        p = p.add(1);
    }
    path
}

/// Returns a new path string with separators normalized (does not modify input).
///
/// Caller must call `qb_string_release` on the result.
///
/// # Safety
/// - `path` must be a valid null-terminated C string or null
#[no_mangle]
pub unsafe extern "C" fn filepath_fix_directory_copy(path: *const c_char) -> *mut QbString {
    if path.is_null() {
        return qb_string_new(std::ptr::null());
    }
    let s = match CStr::from_ptr(path).to_str() {
        Ok(x) => x,
        Err(_) => return qb_string_new(std::ptr::null()),
    };
    let mut out: Vec<u8> = s.as_bytes().to_vec();
    #[cfg(windows)]
    for b in &mut out {
        if *b == b'/' {
            *b = b'\\';
        }
    }
    #[cfg(not(windows))]
    for b in &mut out {
        if *b == b'\\' {
            *b = b'/';
        }
    }
    qb_string_from_bytes(out.as_ptr(), out.len())
}

/// Splits a path into directory and filename. Writes new QbString* into `dir_out` and `file_out`.
///
/// Caller must call `qb_string_release` on both outputs. Either pointer may be null to skip that part.
///
/// # Safety
/// - `path` must be a valid null-terminated C string or null
/// - `dir_out` and `file_out` must be valid pointers to QbString* or null
#[no_mangle]
pub unsafe extern "C" fn filepath_split(
    path: *const c_char,
    dir_out: *mut *mut QbString,
    file_out: *mut *mut QbString,
) {
    if path.is_null() {
        if !dir_out.is_null() {
            *dir_out = qb_string_new(std::ptr::null());
        }
        if !file_out.is_null() {
            *file_out = qb_string_new(std::ptr::null());
        }
        return;
    }
    let s = match CStr::from_ptr(path).to_str() {
        Ok(x) => x,
        Err(_) => {
            if !dir_out.is_null() {
                *dir_out = qb_string_new(std::ptr::null());
            }
            if !file_out.is_null() {
                *file_out = qb_string_new(std::ptr::null());
            }
            return;
        }
    };
    let bytes = s.as_bytes();
    let last_slash = bytes.iter().rposition(|&b| b == b'/' || b == b'\\');
    let (dir, file) = match last_slash {
        Some(i) => (
            &bytes[..i + 1], // include trailing separator
            &bytes[i + 1..],
        ),
        None => (&bytes[..0], bytes),
    };
    if !dir_out.is_null() {
        *dir_out = if dir.is_empty() {
            qb_string_new(std::ptr::null())
        } else {
            qb_string_from_bytes(dir.as_ptr(), dir.len())
        };
    }
    if !file_out.is_null() {
        *file_out = if file.is_empty() {
            qb_string_new(std::ptr::null())
        } else {
            qb_string_from_bytes(file.as_ptr(), file.len())
        };
    }
}

/// Joins directory and filename into a single path, adding a separator if needed.
///
/// Returns a new QbString; caller must call `qb_string_release`.
///
/// # Safety
/// - `directory` and `filename` must be valid null-terminated C strings or null
#[no_mangle]
pub unsafe extern "C" fn filepath_join(
    directory: *const c_char,
    filename: *const c_char,
) -> *mut QbString {
    let dir = if directory.is_null() {
        ""
    } else {
        match CStr::from_ptr(directory).to_str() {
            Ok(x) => x,
            Err(_) => "",
        }
    };
    let file = if filename.is_null() {
        ""
    } else {
        match CStr::from_ptr(filename).to_str() {
            Ok(x) => x,
            Err(_) => "",
        }
    };
    let need_sep = !dir.is_empty() && !dir.ends_with('/') && !dir.ends_with('\\');
    let sep = if cfg!(windows) { '\\' } else { '/' };
    let joined = if need_sep {
        format!("{}{}{}", dir, sep, file)
    } else {
        format!("{}{}", dir, file)
    };
    let cstr = match CString::new(joined) {
        Ok(c) => c,
        Err(_) => return qb_string_new(std::ptr::null()),
    };
    qb_string_new(cstr.as_ptr())
}
