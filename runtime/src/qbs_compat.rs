//! libqb qbs.h compatibility — wrapper types for old code
//!
//! QB64Fresh uses opaque `QbString*` internally. This module provides
//! `struct qbs` and `struct qbs_field` with the same layout as QB64pe
//! libqb/include/qbs.h so code that expects qbs can compile and run.
//! Use `qbs_from_qb_string()` to wrap QbString* as qbs*; `qbs_free()`
//! releases the underlying QbString.

use std::collections::HashMap;
use std::sync::Mutex;

use crate::string::{qb_string_data, qb_string_len, qb_string_release, qb_string_retain, QbString};

/// C-compatible qbs_field layout (QB64pe libqb/include/qbs.h).
#[repr(C)]
pub struct QbsField {
    pub fileno: i32,
    pub fileid: i64,
    pub size: i64,
    pub offset: i64,
}

/// C-compatible qbs layout (QB64pe libqb/include/qbs.h).
#[repr(C)]
pub struct Qbs {
    pub chr: *mut u8,
    pub len: i32,
    pub in_cmem: u8,
    pub cmem_descriptor: *mut u16,
    pub cmem_descriptor_offset: u16,
    pub listi: u32,
    pub tmp: u8,
    pub tmplisti: u32,
    pub fixed: u8,
    pub readonly: u8,
    pub field: *mut QbsField,
}

/// Maps qbs* wrappers to their underlying QbString* for qbs_free / qb_string_from_qbs.
/// Pointers are not Send/Sync; wrapper is safe because BASIC runtime is single-threaded.
struct QbsWrapperMap(Mutex<Option<HashMap<*mut Qbs, *mut QbString>>>);
unsafe impl Send for QbsWrapperMap {}
unsafe impl Sync for QbsWrapperMap {}
static QBS_WRAPPER_MAP: QbsWrapperMap = QbsWrapperMap(Mutex::new(None));

fn ensure_map() {
    let mut guard = QBS_WRAPPER_MAP.0.lock().unwrap();
    if guard.is_none() {
        *guard = Some(HashMap::new());
    }
}

/// Wrap QbString* as qbs*; chr/len point into the string. Call qbs_free when done.
///
/// # Safety
/// - `s` must be a valid QbString pointer (or null; returns null).
/// - Do not modify the string through the returned qbs->chr; QbString may move.
#[no_mangle]
pub unsafe extern "C" fn qbs_from_qb_string(s: *mut QbString) -> *mut Qbs {
    if s.is_null() {
        return std::ptr::null_mut();
    }
    ensure_map();
    let data = qb_string_data(s);
    let len = qb_string_len(s);
    let chr = data as *const u8 as *mut u8;
    let qbs = Box::into_raw(Box::new(Qbs {
        chr,
        len: len as i32,
        in_cmem: 0,
        cmem_descriptor: std::ptr::null_mut(),
        cmem_descriptor_offset: 0,
        listi: 0,
        tmp: 0,
        tmplisti: 0,
        fixed: 0,
        readonly: 1,
        field: std::ptr::null_mut(),
    }));
    qb_string_retain(s);
    QBS_WRAPPER_MAP
        .0
        .lock()
        .unwrap()
        .as_mut()
        .unwrap()
        .insert(qbs, s);
    qbs
}

/// Release a qbs* created by qbs_from_qb_string (releases the underlying QbString).
///
/// # Safety
/// - `q` must be a pointer returned by qbs_from_qb_string (or null; no-op).
#[no_mangle]
pub unsafe extern "C" fn qbs_free(q: *mut Qbs) {
    if q.is_null() {
        return;
    }
    let mut guard = QBS_WRAPPER_MAP.0.lock().unwrap();
    if let Some(ref mut map) = *guard {
        if let Some(s) = map.remove(&q) {
            qb_string_release(s);
        }
    }
    let _ = Box::from_raw(q);
}

/// Get QbString* from a qbs* created by qbs_from_qb_string (retains; caller must release).
///
/// # Safety
/// - `q` must be a pointer returned by qbs_from_qb_string (or null; returns null).
#[no_mangle]
pub unsafe extern "C" fn qb_string_from_qbs(q: *mut Qbs) -> *mut QbString {
    if q.is_null() {
        return std::ptr::null_mut();
    }
    ensure_map();
    let guard = QBS_WRAPPER_MAP.0.lock().unwrap();
    if let Some(ref map) = *guard {
        if let Some(&s) = map.get(&q) {
            qb_string_retain(s);
            return s;
        }
    }
    std::ptr::null_mut()
}
