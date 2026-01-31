//! C FFI for mutex (libqb_mutex_*).
//!
//! Matches QB64pe libqb mutex.h API: opaque mutex, new/free, lock/unlock.
//! For RAII in C use the LIBQB_MUTEX_GUARD(m) macro; in C++ the header
//! can define a class that locks on construction and unlocks on destruction.

use crate::mutex;

/// Allocates and initializes a new mutex. Returns null on failure.
/// Mutex is created unlocked. Caller must call `libqb_mutex_free` when done.
#[no_mangle]
pub extern "C" fn libqb_mutex_new() -> *mut mutex::LibqbMutex {
    mutex::libqb_mutex_new()
}

/// Destroys and frees a mutex. No-op if `m` is null. Mutex must not be locked.
#[no_mangle]
pub extern "C" fn libqb_mutex_free(m: *mut mutex::LibqbMutex) {
    mutex::libqb_mutex_free(m);
}

/// Locks the mutex, blocking until it is available.
///
/// # Safety
/// `m` must be a valid pointer from `libqb_mutex_new` and not yet freed.
#[no_mangle]
pub unsafe extern "C" fn libqb_mutex_lock(m: *mut mutex::LibqbMutex) {
    if m.is_null() {
        return;
    }
    (*m).lock();
}

/// Unlocks the mutex. Caller must hold the lock.
///
/// # Safety
/// `m` must be a valid pointer from `libqb_mutex_new` and not yet freed.
#[no_mangle]
pub unsafe extern "C" fn libqb_mutex_unlock(m: *mut mutex::LibqbMutex) {
    if m.is_null() {
        return;
    }
    (*m).unlock();
}
