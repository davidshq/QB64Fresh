//! One-shot completion for thread synchronization (libqb completion.h compatibility).
//!
//! A completion is a one-shot signal: waiters block until `completion_finish` is
//! called, then never block again. Callers allocate `struct completion` (stack or
//! heap), call `completion_init`, then use `completion_wait` and `completion_finish`
//! from different threads; finally call `completion_clear` to release resources.

use crate::condvar;
use crate::mutex;

// Use FFI symbols for condvar wait/broadcast (defined in condvar_ffi, re-exported from crate root).
use crate::libqb_condvar_broadcast;
use crate::libqb_condvar_wait;

/// C-compatible completion structure (libqb completion.h).
///
/// Callers allocate this (e.g. `struct completion comp;`) and pass `&comp` to
/// `completion_init`, then use `completion_wait` / `completion_finish`, and
/// finally `completion_clear`.
#[repr(C)]
pub struct Completion {
    /// 1 when finished has been called, 0 otherwise.
    pub finished: std::ffi::c_int,
    /// Mutex protecting `finished` and used with the condition variable.
    pub mutex: *mut mutex::LibqbMutex,
    /// Condition variable signalled when finished becomes 1.
    pub var: *mut condvar::LibqbCondvar,
}

/// Initializes a completion. Sets finished=0 and allocates mutex and condvar.
/// Call this before any wait/finish; the struct must be valid (e.g. stack or heap).
///
/// # Safety
/// `comp` must be a valid pointer to a `Completion` (or C `struct completion`).
#[no_mangle]
pub unsafe extern "C" fn completion_init(comp: *mut Completion) {
    if comp.is_null() {
        return;
    }
    let c = &mut *comp;
    c.finished = 0;
    c.mutex = mutex::libqb_mutex_new();
    c.var = condvar::libqb_condvar_new();
}

/// Clears a completion: frees the mutex and condvar. Does not free the struct itself.
/// Call after all waiters and finishers are done. Do not call wait/finish after clear.
///
/// # Safety
/// `comp` must be a valid pointer from a previous `completion_init`, and no other
/// thread may be in `completion_wait` or `completion_finish`.
#[no_mangle]
pub unsafe extern "C" fn completion_clear(comp: *mut Completion) {
    if comp.is_null() {
        return;
    }
    let c = &mut *comp;
    if !c.mutex.is_null() {
        mutex::libqb_mutex_free(c.mutex);
        c.mutex = std::ptr::null_mut();
    }
    if !c.var.is_null() {
        condvar::libqb_condvar_free(c.var);
        c.var = std::ptr::null_mut();
    }
}

/// Blocks until the completion is finished (until `completion_finish` has been called).
/// Returns immediately if already finished.
///
/// # Safety
/// `comp` must be a valid pointer that was initialized with `completion_init`.
#[no_mangle]
pub unsafe extern "C" fn completion_wait(comp: *mut Completion) {
    if comp.is_null() {
        return;
    }
    let c = &*comp;
    if c.mutex.is_null() || c.var.is_null() {
        return;
    }
    let mtx = c.mutex.as_ref().unwrap();
    mtx.lock();
    while (*comp).finished == 0 {
        libqb_condvar_wait(c.var, c.mutex);
    }
    mtx.unlock();
}

/// Marks the completion as finished and unblocks all current waiters.
/// Future calls to `completion_wait` return immediately.
///
/// # Safety
/// `comp` must be a valid pointer that was initialized with `completion_init`.
#[no_mangle]
pub unsafe extern "C" fn completion_finish(comp: *mut Completion) {
    if comp.is_null() {
        return;
    }
    let c = &mut *comp;
    if c.mutex.is_null() || c.var.is_null() {
        return;
    }
    c.mutex.as_ref().unwrap().lock();
    c.finished = 1;
    libqb_condvar_broadcast(c.var);
    c.mutex.as_ref().unwrap().unlock();
}
