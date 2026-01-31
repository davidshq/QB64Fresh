//! OpenGL FFI for QB64Fresh (optional; built only with feature `opengl`).
//!
//! Exposes `sub_gl_called` so that C wrappers (gl_wrappers.c) can check that
//! _GL* commands are only used inside SUB _GL. The runtime sets this to 1
//! before invoking the user's SUB _GL and clears it after return.
//!
//! The generated C registers its SUB _GL via `qb_gl_register_sub_gl`; the
//! runtime invokes it each frame when _GLRENDER is active.

use std::sync::atomic::{AtomicPtr, Ordering};

/// Flag set by the runtime when invoking SUB _GL; C wrappers (call_gl*)
/// check this and raise error 270 if _GL* is used outside SUB _GL.
#[no_mangle]
pub static mut sub_gl_called: i32 = 0;

/// Registered SUB _GL callback (from generated C via qb_gl_register_sub_gl).
/// Null when the program does not use OpenGL or has not registered yet.
static SUB_GL_CALLBACK: AtomicPtr<()> = AtomicPtr::new(std::ptr::null_mut());

/// Registers the SUB _GL callback. Called by generated C at startup when
/// the program uses OpenGL (defines SUB _GL). Pass the address of `qb_sub__gl`.
///
/// # Safety
/// The pointer must remain valid for the lifetime of the program and must
/// be a C function with signature `void (*)(void)`.
#[no_mangle]
pub unsafe extern "C" fn qb_gl_register_sub_gl(cb: *mut std::ffi::c_void) {
    SUB_GL_CALLBACK.store(cb as *mut (), Ordering::Relaxed);
}

/// Invokes the registered SUB _GL callback if non-null. Sets `sub_gl_called`
/// to 1 before the call and 0 after. Used by the graphics backend each frame
/// when _GLRENDER mode is active.
pub fn invoke_sub_gl() {
    let cb = SUB_GL_CALLBACK.load(Ordering::Relaxed);
    if cb.is_null() {
        return;
    }
    let f: extern "C" fn() = unsafe { std::mem::transmute(cb) };
    unsafe {
        sub_gl_called = 1;
        f();
        sub_gl_called = 0;
    }
}
