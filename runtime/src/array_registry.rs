//! Array bounds registry for LBOUND/UBOUND (external runtime).
//!
//! Tracks lower/upper bounds per dimension for arrays so that LBOUND and UBOUND
//! return correct values. Generated code calls `qb_array_register` / `qb_array_register_md`
//! on DIM, and `qb_array_update` on REDIM; LBOUND/UBOUND look up by array pointer.
//!
//! Matches the ABI of the inline runtime in `src/codegen/c_backend/runtime/arrays.rs`.

use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::{LazyLock, Mutex};

/// Maximum number of dimensions (matches codegen QBA_MAX_DIMS).
const MAX_DIMS: usize = 8;

/// Per-array metadata: dimension count and lower/upper bounds per dimension.
#[derive(Clone)]
struct ArrayMeta {
    num_dims: i32,
    lower: [i32; MAX_DIMS],
    upper: [i32; MAX_DIMS],
}

impl Default for ArrayMeta {
    fn default() -> Self {
        ArrayMeta {
            num_dims: 0,
            lower: [0; MAX_DIMS],
            upper: [0; MAX_DIMS],
        }
    }
}

/// Registry key: array pointer as usize (Send + Sync for use in static Mutex).
fn ptr_key(ptr: *mut c_void) -> usize {
    ptr as usize
}

static REGISTRY: LazyLock<Mutex<HashMap<usize, ArrayMeta>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Register 1D array bounds (called from generated code on DIM).
///
/// # Safety
/// - `ptr` must be a valid array pointer for the lifetime of the program or until
///   `qb_array_erase` is called. The runtime does not take ownership.
#[no_mangle]
pub unsafe extern "C" fn qb_array_register(ptr: *mut c_void, lower: i32, upper: i32) {
    if ptr.is_null() {
        return;
    }
    if let Ok(mut guard) = REGISTRY.lock() {
        guard.insert(
            ptr_key(ptr),
            ArrayMeta {
                num_dims: 1,
                lower: [lower, 0, 0, 0, 0, 0, 0, 0],
                upper: [upper, 0, 0, 0, 0, 0, 0, 0],
            },
        );
    }
}

/// Register multi-dimensional array bounds (called from generated code on DIM).
///
/// # Safety
/// - `ptr` must be a valid array pointer for the lifetime of the program or until
///   `qb_array_erase` is called.
/// - `lowers` and `uppers` must point to at least `num_dims` valid `int32_t` values.
#[no_mangle]
pub unsafe extern "C" fn qb_array_register_md(
    ptr: *mut c_void,
    num_dims: i32,
    lowers: *const i32,
    uppers: *const i32,
) {
    if ptr.is_null()
        || num_dims <= 0
        || num_dims as usize > MAX_DIMS
        || lowers.is_null()
        || uppers.is_null()
    {
        return;
    }
    let nd = num_dims as usize;
    let mut meta = ArrayMeta::default();
    meta.num_dims = num_dims;
    for d in 0..nd {
        meta.lower[d] = *lowers.add(d);
        meta.upper[d] = *uppers.add(d);
    }
    if let Ok(mut guard) = REGISTRY.lock() {
        guard.insert(ptr_key(ptr), meta);
    }
}

/// Update array bounds after REDIM (pointer may change due to realloc).
///
/// # Safety
/// - `old_ptr` and `new_ptr` must be valid; see `qb_array_register`.
#[no_mangle]
pub unsafe extern "C" fn qb_array_update(
    old_ptr: *mut c_void,
    new_ptr: *mut c_void,
    lower: i32,
    upper: i32,
) {
    if let Ok(mut guard) = REGISTRY.lock() {
        if old_ptr == new_ptr {
            if let Some(meta) = guard.get_mut(&ptr_key(old_ptr)) {
                meta.num_dims = 1;
                meta.lower[0] = lower;
                meta.upper[0] = upper;
            } else {
                drop(guard);
                qb_array_register(new_ptr, lower, upper);
            }
        } else {
            guard.remove(&ptr_key(old_ptr));
            drop(guard);
            qb_array_register(new_ptr, lower, upper);
        }
    }
}

/// LBOUND(array) - return lower bound of first dimension.
///
/// # Safety
/// - `arr` is an array pointer; may be null (returns 0).
#[no_mangle]
pub unsafe extern "C" fn qb_lbound(arr: *mut c_void) -> i32 {
    if arr.is_null() {
        return 0;
    }
    if let Ok(guard) = REGISTRY.lock() {
        if let Some(meta) = guard.get(&ptr_key(arr)) {
            return meta.lower[0];
        }
    }
    0
}

/// LBOUND(array, dimension) - return lower bound of given dimension.
///
/// # Safety
/// - `arr` is an array pointer; may be null (returns 0).
#[no_mangle]
pub unsafe extern "C" fn qb_lbound2(arr: *mut c_void, dim: i32) -> i32 {
    if arr.is_null() || dim < 1 || dim as usize > MAX_DIMS {
        return 0;
    }
    if let Ok(guard) = REGISTRY.lock() {
        if let Some(meta) = guard.get(&ptr_key(arr)) {
            if dim <= meta.num_dims {
                return meta.lower[(dim - 1) as usize];
            }
        }
    }
    0
}

/// UBOUND(array) - return upper bound of first dimension.
///
/// # Safety
/// - `arr` is an array pointer; may be null (returns 0).
#[no_mangle]
pub unsafe extern "C" fn qb_ubound(arr: *mut c_void) -> i32 {
    if arr.is_null() {
        return 0;
    }
    if let Ok(guard) = REGISTRY.lock() {
        if let Some(meta) = guard.get(&ptr_key(arr)) {
            return meta.upper[0];
        }
    }
    0
}

/// UBOUND(array, dimension) - return upper bound of given dimension.
///
/// # Safety
/// - `arr` is an array pointer; may be null (returns 0).
#[no_mangle]
pub unsafe extern "C" fn qb_ubound2(arr: *mut c_void, dim: i32) -> i32 {
    if arr.is_null() || dim < 1 || dim as usize > MAX_DIMS {
        return 0;
    }
    if let Ok(guard) = REGISTRY.lock() {
        if let Some(meta) = guard.get(&ptr_key(arr)) {
            if dim <= meta.num_dims {
                return meta.upper[(dim - 1) as usize];
            }
        }
    }
    0
}

/// Clear array metadata (called on ERASE).
///
/// # Safety
/// - `arr` is an array pointer; may be null (no-op).
#[no_mangle]
pub unsafe extern "C" fn qb_array_erase(arr: *mut c_void) {
    if arr.is_null() {
        return;
    }
    if let Ok(mut guard) = REGISTRY.lock() {
        guard.remove(&ptr_key(arr));
    }
}
