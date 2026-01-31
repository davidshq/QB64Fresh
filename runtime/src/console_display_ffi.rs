//! Console/display FFI stubs for libqb.h compatibility.
//!
//! These symbols match libqb's main header (console/display) so that
//! generated code or QB64pe-style code can link when using `--runtime external`.
//!
//! - `qb_printimage` / `validatepage` / `qbg_sub_view_print` — stubs (no-op or minimal)
//! - `qb_view_print` / `qb_view_print_reset` — text viewport (VIEW PRINT)
//! - `makefit` / `lprint_makefit` — fit text to width (PRINT/LPRINT)
//! - `port60h_event` / `port60h_events` — keyboard port 0x60 queue
//! - `window_exists` / `no_control_characters2` — window/control state

use std::os::raw::c_void;

// VIEW PRINT state
static mut VIEW_PRINT_TOP: i32 = 1;
static mut VIEW_PRINT_BOTTOM: i32 = 25;

/// _PRINTIMAGE - print image to console/printer. Stub: no-op.
#[no_mangle]
pub extern "C" fn qb_printimage(i: i32) {
    let _ = i;
}

/// Validate graphics page. Stub: no-op.
#[no_mangle]
pub extern "C" fn validatepage(n: i32) {
    let _ = n;
}

/// VIEW PRINT top,bottom - set text viewport.
#[no_mangle]
pub extern "C" fn qb_view_print(top: i32, bottom: i32) {
    unsafe {
        VIEW_PRINT_TOP = top;
        VIEW_PRINT_BOTTOM = bottom;
    }
}

/// VIEW PRINT with no args - reset to full screen.
#[no_mangle]
pub extern "C" fn qb_view_print_reset() {
    unsafe {
        VIEW_PRINT_TOP = 1;
        VIEW_PRINT_BOTTOM = 25;
    }
}

/// VIEW PRINT with three args (libqb qbg_sub_view_print). passed!=0: set; passed==0: reset.
#[no_mangle]
pub extern "C" fn qbg_sub_view_print(topline: i32, bottomline: i32, passed: i32) {
    if passed != 0 {
        qb_view_print(topline, bottomline);
    } else {
        qb_view_print_reset();
    }
}

/// Fit text to width: if current line + text would exceed width, newline.
/// Stub: no-op (console width not tracked in external runtime).
#[no_mangle]
pub unsafe extern "C" fn makefit(_text: *const c_void) {
    // Would use QbString* to check length and emit newline if needed
}

/// LPRINT width fit. Stub: no-op.
#[no_mangle]
pub unsafe extern "C" fn lprint_makefit(_text: *const c_void) {
    // LPRINT device width fit; not implemented
}

// Keyboard port 0x60 queue (libqb port60h_event / port60h_events)
#[no_mangle]
pub static mut port60h_event: [u8; 256] = [0u8; 256];

#[no_mangle]
pub static mut port60h_events: i32 = 0;

// Window/control state (libqb)
#[no_mangle]
pub static mut window_exists: i32 = 1;

#[no_mangle]
pub static mut no_control_characters2: i32 = 0;
