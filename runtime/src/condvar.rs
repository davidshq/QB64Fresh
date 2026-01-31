//! Condition variable for thread synchronization (libqb_condvar compatibility).
//!
//! Provides an opaque condition variable type with create/destroy, wait,
//! signal, and broadcast, matching QB64pe's libqb condvar.h API. Uses the
//! system pthread condition variable so that C code can pair it with
//! libqb_mutex for wait/signal. Used for producer-consumer and
//! event-wait patterns; exposed via FFI for C/C++ code.

use std::alloc::{alloc, dealloc, Layout};

use libc::{
    pthread_cond_broadcast, pthread_cond_destroy, pthread_cond_init, pthread_cond_signal,
    pthread_cond_t, pthread_cond_wait,
};

use crate::mutex::LibqbMutex;

/// Opaque condition variable. Layout wraps a single `pthread_cond_t` for FFI.
/// For dynamic allocation use `libqb_condvar_new` / `libqb_condvar_free`.
#[repr(C)]
pub struct LibqbCondvar {
    inner: pthread_cond_t,
}

impl LibqbCondvar {
    /// Waits on this condition variable. Caller must hold the lock on `mutex`;
    /// the mutex is released while waiting and reacquired before return.
    /// Spurious wakeups are possible; re-check the predicate after return.
    pub fn wait(&self, mutex: *mut LibqbMutex) {
        if mutex.is_null() {
            return;
        }
        unsafe {
            let cond = &self.inner as *const _ as *mut _;
            let mtx = (*mutex).as_pthread_ptr();
            let _ = pthread_cond_wait(cond, mtx);
        }
    }

    /// Signals one waiter. Caller should hold the lock when signaling.
    pub fn signal(&self) {
        unsafe {
            let _ = pthread_cond_signal(&self.inner as *const _ as *mut _);
        }
    }

    /// Wakes all waiters. Caller should hold the lock when broadcasting.
    pub fn broadcast(&self) {
        unsafe {
            let _ = pthread_cond_broadcast(&self.inner as *const _ as *mut _);
        }
    }

    /// Destroys the condition variable. Must not have any waiters. Call before freeing.
    pub fn destroy(&mut self) {
        unsafe {
            let _ = pthread_cond_destroy(&mut self.inner);
        }
    }
}

/// Allocates and initializes a new condition variable. Returns null on allocation or init failure.
///
/// Caller must call `libqb_condvar_free` when done.
pub fn libqb_condvar_new() -> *mut LibqbCondvar {
    let layout = Layout::new::<LibqbCondvar>();
    let ptr = unsafe { alloc(layout) } as *mut LibqbCondvar;
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        std::ptr::write(ptr, std::mem::zeroed());
        if pthread_cond_init(&mut (*ptr).inner, std::ptr::null()) == 0 {
            ptr
        } else {
            dealloc(ptr as *mut u8, layout);
            std::ptr::null_mut()
        }
    }
}

/// Destroys and frees a condition variable. No-op if `condvar` is null. Must not have waiters.
pub fn libqb_condvar_free(condvar: *mut LibqbCondvar) {
    if condvar.is_null() {
        return;
    }
    let layout = Layout::new::<LibqbCondvar>();
    unsafe {
        (*condvar).destroy();
        dealloc(condvar as *mut u8, layout);
    }
}
