//! C FFI for condition variable (libqb_condvar_*).
//!
//! Matches QB64pe libqb condvar.h API: opaque condvar, new/free, wait,
//! signal, broadcast. Use with libqb_mutex: lock mutex, check predicate,
//! call libqb_condvar_wait(cond, mutex) in a loop, then signal/broadcast
//! when the condition becomes true.

use crate::condvar;

/// Allocates and initializes a new condition variable. Returns null on failure.
/// Caller must call `libqb_condvar_free` when done.
#[no_mangle]
pub extern "C" fn libqb_condvar_new() -> *mut condvar::LibqbCondvar {
    condvar::libqb_condvar_new()
}

/// Destroys and frees a condition variable. No-op if `c` is null. Must not have waiters.
#[no_mangle]
pub extern "C" fn libqb_condvar_free(c: *mut condvar::LibqbCondvar) {
    condvar::libqb_condvar_free(c);
}

/// Waits on the condition variable. Caller must hold the lock on `mutex`;
/// the mutex is released while waiting and reacquired before return.
/// Spurious wakeups are possible; re-check the predicate after return.
///
/// # Safety
/// `c` must be a valid pointer from `libqb_condvar_new` and not yet freed.
/// `mutex` must be a valid pointer from `libqb_mutex_new` and the calling thread must hold the lock.
#[no_mangle]
pub unsafe extern "C" fn libqb_condvar_wait(
    c: *mut condvar::LibqbCondvar,
    mutex: *mut crate::mutex::LibqbMutex,
) {
    if c.is_null() {
        return;
    }
    (*c).wait(mutex);
}

/// Signals one waiter. Caller should hold the lock on the associated mutex when signaling.
///
/// # Safety
/// `c` must be a valid pointer from `libqb_condvar_new` and not yet freed.
#[no_mangle]
pub unsafe extern "C" fn libqb_condvar_signal(c: *mut condvar::LibqbCondvar) {
    if c.is_null() {
        return;
    }
    (*c).signal();
}

/// Wakes all waiters. Caller should hold the lock on the associated mutex when broadcasting.
///
/// # Safety
/// `c` must be a valid pointer from `libqb_condvar_new` and not yet freed.
#[no_mangle]
pub unsafe extern "C" fn libqb_condvar_broadcast(c: *mut condvar::LibqbCondvar) {
    if c.is_null() {
        return;
    }
    (*c).broadcast();
}
