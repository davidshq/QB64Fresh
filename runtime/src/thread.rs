//! Thread API (libqb thread.h compatibility).
//!
//! Opaque thread handle with create/start/join/free. Used by QB64pe for
//! main loop, timer, and HTTP worker threads. Implemented with `std::thread`.

use std::cell::RefCell;
use std::ffi::c_void;
use std::thread::JoinHandle;

/// Wraps a raw pointer so it can be sent to another thread (C callback arg).
/// Safety: caller guarantees the pointer is only used from the new thread.
struct SendPtr(*mut c_void);
unsafe impl Send for SendPtr {}

/// Payload passed into the spawned thread (func + arg).
struct ThreadStartPayload {
    func: extern "C" fn(*mut c_void),
    arg: SendPtr,
}
unsafe impl Send for ThreadStartPayload {}

/// Runs the C callback in the spawned thread (avoids projecting .0 in the closure for Send).
fn run_thread_payload(payload: ThreadStartPayload) {
    (payload.func)(payload.arg.0);
}

/// Opaque thread handle (libqb thread.h `struct libqb_thread`).
///
/// Created with `libqb_thread_new`, started with `libqb_thread_start`,
/// joined with `libqb_thread_join`, then freed with `libqb_thread_free`.
/// Thread must be joined before free.
pub struct LibqbThread {
    join_handle: RefCell<Option<JoinHandle<()>>>,
}

/// Allocates a new thread handle. Thread is not running until `libqb_thread_start` is called.
///
/// Caller must call `libqb_thread_join` before `libqb_thread_free`.
#[no_mangle]
pub extern "C" fn libqb_thread_new() -> *mut LibqbThread {
    let t = LibqbThread {
        join_handle: RefCell::new(None),
    };
    Box::into_raw(Box::new(t))
}

/// Frees a thread handle. Thread must already be stopped/joined.
///
/// # Safety
/// `t` must be a valid pointer from `libqb_thread_new` and must have been joined.
#[no_mangle]
pub unsafe extern "C" fn libqb_thread_free(t: *mut LibqbThread) {
    if t.is_null() {
        return;
    }
    let _ = Box::from_raw(t);
}

/// Starts the thread running `start_func(start_func_arg)` in a new OS thread.
///
/// # Safety
/// `t` must be a valid pointer from `libqb_thread_new` and not yet started, or
/// must have been joined after a previous start. `start_func` must be a valid
/// C function that accepts one `void*` argument; it will be called with `start_func_arg`.
#[no_mangle]
pub unsafe extern "C" fn libqb_thread_start(
    t: *mut LibqbThread,
    start_func: extern "C" fn(*mut c_void),
    start_func_arg: *mut c_void,
) {
    if t.is_null() {
        return;
    }
    let payload = ThreadStartPayload {
        func: start_func,
        arg: SendPtr(start_func_arg),
    };
    let handle = std::thread::spawn(move || run_thread_payload(payload));
    (*t).join_handle.replace(Some(handle));
}

/// Blocks until the thread has finished. Must be called before `libqb_thread_free`.
///
/// # Safety
/// `t` must be a valid pointer from `libqb_thread_new` and must have been started.
#[no_mangle]
pub unsafe extern "C" fn libqb_thread_join(t: *mut LibqbThread) {
    if t.is_null() {
        return;
    }
    if let Some(handle) = (*t).join_handle.take() {
        let _ = handle.join();
    }
}
