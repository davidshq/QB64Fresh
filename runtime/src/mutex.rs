//! Mutex for thread-safe access (libqb_mutex compatibility).
//!
//! Provides an opaque mutex type with create/destroy and lock/unlock,
//! matching QB64pe's libqb mutex.h API. Uses the system pthread mutex
//! so that C code can call lock/unlock independently (no guard required).
//! Used internally by the runtime (e.g. HTTP, font, audio) and exposed
//! via FFI for C/C++ code.

use std::alloc::{alloc, dealloc, Layout};

use libc::{
    pthread_mutex_destroy, pthread_mutex_init, pthread_mutex_lock, pthread_mutex_t,
    pthread_mutex_unlock,
};

/// Opaque mutex. Layout matches a single `pthread_mutex_t` for FFI.
/// For dynamic allocation use `libqb_mutex_new` / `libqb_mutex_free`.
#[repr(C)]
pub struct LibqbMutex {
    inner: pthread_mutex_t,
}

impl LibqbMutex {
    /// Locks the mutex, blocking until it is available.
    pub fn lock(&self) {
        unsafe {
            let _ = pthread_mutex_lock(&self.inner as *const _ as *mut _);
        }
    }

    /// Unlocks the mutex. Caller must hold the lock.
    pub fn unlock(&self) {
        unsafe {
            let _ = pthread_mutex_unlock(&self.inner as *const _ as *mut _);
        }
    }

    /// Destroys the mutex. Must not be locked. Call before freeing.
    pub fn destroy(&mut self) {
        unsafe {
            let _ = pthread_mutex_destroy(&mut self.inner);
        }
    }

    /// Returns a mutable pointer to the inner `pthread_mutex_t` for use with pthread_cond_wait.
    /// Used by the condvar module; not part of the public C API.
    pub(crate) fn as_pthread_ptr(&self) -> *mut pthread_mutex_t {
        &self.inner as *const _ as *mut _
    }
}

/// Allocates and initializes a new mutex. Returns null on allocation or init failure.
///
/// Caller must call `libqb_mutex_free` when done.
pub fn libqb_mutex_new() -> *mut LibqbMutex {
    let layout = Layout::new::<LibqbMutex>();
    let ptr = unsafe { alloc(layout) } as *mut LibqbMutex;
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        // Dynamically allocated mutexes must be initialized with pthread_mutex_init,
        // not PTHREAD_MUTEX_INITIALIZER. Use zeroed storage then init.
        std::ptr::write(ptr, std::mem::zeroed());
        if pthread_mutex_init(&mut (*ptr).inner, std::ptr::null()) == 0 {
            ptr
        } else {
            dealloc(ptr as *mut u8, layout);
            std::ptr::null_mut()
        }
    }
}

/// Destroys and frees a mutex. No-op if `mutex` is null. Mutex must not be locked.
pub fn libqb_mutex_free(mutex: *mut LibqbMutex) {
    if mutex.is_null() {
        return;
    }
    let layout = Layout::new::<LibqbMutex>();
    unsafe {
        (*mutex).destroy();
        dealloc(mutex as *mut u8, layout);
    }
}
