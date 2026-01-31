//! libqb mem.h compatibility — _MEM lock lifecycle (new_mem_lock, free_mem_lock).
//!
//! Provides the lock pool and globals expected by code that uses QB64pe's
//! mem_block/mem_lock API. Structures and constants match
//! QB64pe internal/c/libqb/include/mem.h.

use std::ffi::c_void;
use std::ptr::null_mut;
use std::sync::LazyLock;
use std::sync::Mutex;

/// C ABI compatible lock record. Layout must match `struct mem_lock` in qb64fresh_rt.h.
#[repr(C)]
pub struct MemLock {
    pub id: i64,
    pub type_: i32,
    pub offset: *mut c_void,
}

const MEM_LOCK_MAX: usize = 10000;
const MEM_LOCK_ID_INIT: u64 = 1073741823;

/// Globals exported to C (mem_lock_id, mem_lock_tmp, mem_lock_base).
#[no_mangle]
pub static mut mem_lock_id: u64 = MEM_LOCK_ID_INIT;

#[no_mangle]
pub static mut mem_lock_tmp: *mut MemLock = null_mut();

#[no_mangle]
pub static mut mem_lock_base: *mut MemLock = null_mut();

/// Pool of mem_lock records. Leaked at first use so pointers stay valid.
static mut POOL: Option<&'static mut [MemLock; MEM_LOCK_MAX]> = None;
static mut MEM_LOCK_NEXT: usize = 0;
static FREED: LazyLock<Mutex<Vec<usize>>> = LazyLock::new(|| Mutex::new(Vec::new()));

fn ensure_pool() {
    unsafe {
        if POOL.is_none() {
            let b = Box::new(core::array::from_fn(|_| MemLock {
                id: 0,
                type_: 0,
                offset: null_mut(),
            }));
            let leaked = Box::leak(b);
            let ptr = leaked.as_mut_ptr();
            POOL = Some(leaked);
            mem_lock_base = ptr;
        }
    }
}

/// Allocate or reuse a lock, set mem_lock_tmp to it, increment mem_lock_id.
/// Called from C as new_mem_lock().
#[no_mangle]
pub extern "C" fn new_mem_lock() {
    ensure_pool();
    unsafe {
        if mem_lock_base.is_null() {
            return;
        }
        let idx = if let Ok(mut guard) = FREED.lock() {
            if let Some(i) = guard.pop() {
                i
            } else if MEM_LOCK_NEXT < MEM_LOCK_MAX {
                let i = MEM_LOCK_NEXT;
                MEM_LOCK_NEXT += 1;
                i
            } else {
                return;
            }
        } else {
            return;
        };
        let slot = mem_lock_base.add(idx);
        mem_lock_id += 1;
        (*slot).id = mem_lock_id as i64;
        (*slot).type_ = 0;
        (*slot).offset = null_mut();
        mem_lock_tmp = slot;
    }
}

/// Invalidate the lock and free malloc'd memory if type is MEM_TYPE_MALLOC.
/// Called from C as free_mem_lock(lock).
///
/// # Safety
/// `lock` must be a pointer to a MemLock that was returned from this pool
/// (i.e. previously set as mem_lock_tmp by new_mem_lock).
#[no_mangle]
pub unsafe extern "C" fn free_mem_lock(lock: *mut MemLock) {
    if lock.is_null() || mem_lock_base.is_null() {
        return;
    }
    let base = mem_lock_base;
    let size = std::mem::size_of::<MemLock>();
    let base_usize = base as usize;
    let lock_usize = lock as usize;
    if lock_usize < base_usize {
        return;
    }
    let offset = lock_usize - base_usize;
    if offset % size != 0 || offset / size >= MEM_LOCK_MAX {
        return;
    }
    let idx = offset / size;
    (*lock).id = 0;
    if (*lock).type_ == 1 && !(*lock).offset.is_null() {
        libc::free((*lock).offset);
        (*lock).offset = null_mut();
    }
    if let Ok(mut guard) = FREED.lock() {
        guard.push(idx);
    }
}
