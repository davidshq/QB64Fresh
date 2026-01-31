//! game_controller.h FFI — Gamepad / Keyboard / Mouse devices (libqb compatibility).
//!
//! Provides structures, globals, and functions for device event handling:
//! - `device_struct` / `onstrig_struct` — device and ON STRIG state
//! - Globals: `device_last`, `device_max`, `devices`, `onstrig`, `onstrig_inprogress`
//! - Event accessors: get/set button, axis, wheel in a device event
//! - Device/event lifecycle: `setupDevice`, `createDeviceEvent`, `commitDeviceEvent`
//!
//! Event layout (per QB64pe qbx.cpp): each event is
//! `[axis floats: lastaxis*4][wheel floats: lastwheel*4][button bytes: lastbutton][padding to 8-byte][int64 index]`.

use std::ptr::{null_mut, NonNull};

// Constants matching game_controller.h
const QUEUED_EVENTS_LIMIT: i32 = 1024;

/// C-compatible device structure (must match game_controller.h).
#[repr(C)]
pub struct DeviceStruct {
    pub used: i32,
    pub type_: i32,
    pub name: *mut libc::c_char,
    pub connected: i32,
    pub lastbutton: i32,
    pub lastaxis: i32,
    pub lastwheel: i32,
    pub max_events: i32,
    pub queued_events: i32,
    pub events: *mut u8,
    pub event_size: i32,
    pub strig_button_pressed: [u8; 256],
    pub handle_pointer: *mut libc::c_void,
    pub handle_int: i64,
    pub description: *const libc::c_char,
    pub product_id: i64,
    pub vendor_id: i64,
    pub buttons: i32,
    pub axes: i32,
    pub balls: i32,
    pub hats: i32,
}

/// C-compatible ON STRIG state (must match game_controller.h).
#[repr(C)]
pub struct OnstrigStruct {
    pub id: u32,
    pub pass: i64,
    pub active: u8,
    pub state: u8,
}

// Globals (internal allocation; C sees device_last, devices, etc. via no_mangle below)
const DEVICE_MAX_INIT: i32 = 1000;
const ONSTRIG_COUNT: usize = 65536;

static mut DEVICES_PTR: *mut DeviceStruct = null_mut();
static mut ONSTRIG_PTR: *mut OnstrigStruct = null_mut();
static mut DEVICE_EVENT_INDEX: i64 = 0;

/// Allocate and expose device/onstrig arrays. Call from qb_runtime_init.
pub fn game_controller_init() {
    unsafe {
        if DEVICES_PTR.is_null() {
            let n = (DEVICE_MAX_INIT as usize) + 1;
            DEVICES_PTR = libc::calloc(n, std::mem::size_of::<DeviceStruct>()) as *mut DeviceStruct;
        }
        if ONSTRIG_PTR.is_null() {
            ONSTRIG_PTR = libc::calloc(ONSTRIG_COUNT, std::mem::size_of::<OnstrigStruct>())
                as *mut OnstrigStruct;
        }
        device_last = 0;
        device_max = DEVICE_MAX_INIT;
        devices = DEVICES_PTR;
        onstrig = ONSTRIG_PTR;
        onstrig_inprogress = 0;
    }
}

// C-visible globals (point to our allocated buffers)
#[no_mangle]
pub static mut device_last: i32 = 0;

#[no_mangle]
pub static mut device_max: i32 = DEVICE_MAX_INIT;

#[no_mangle]
pub static mut devices: *mut DeviceStruct = null_mut();

#[no_mangle]
pub static mut onstrig: *mut OnstrigStruct = null_mut();

#[no_mangle]
pub static mut onstrig_inprogress: i32 = 0;

#[no_mangle]
pub extern "C" fn getDeviceEventButtonValue(
    device: *mut DeviceStruct,
    event_index: i32,
    object_index: i32,
) -> u8 {
    let Some(d) = NonNull::new(device) else {
        return 0;
    };
    let d = unsafe { &*d.as_ptr() };
    if d.events.is_null() || d.event_size <= 0 {
        return 0;
    }
    let base = d.lastaxis * 4 + d.lastwheel * 4;
    let offset = event_index * d.event_size + base + object_index;
    if offset < 0 {
        return 0;
    }
    let ptr = unsafe { d.events.add(offset as usize) };
    unsafe { *ptr }
}

#[no_mangle]
pub extern "C" fn setDeviceEventButtonValue(
    device: *mut DeviceStruct,
    event_index: i32,
    object_index: i32,
    value: u8,
) {
    let Some(d) = NonNull::new(device) else {
        return;
    };
    let d = unsafe { &*d.as_ptr() };
    if d.events.is_null() || d.event_size <= 0 {
        return;
    }
    let base = d.lastaxis * 4 + d.lastwheel * 4;
    let offset = event_index * d.event_size + base + object_index;
    if offset < 0 {
        return;
    }
    let ptr = unsafe { d.events.add(offset as usize) };
    unsafe { *ptr = value };
}

#[no_mangle]
pub extern "C" fn getDeviceEventAxisValue(
    device: *mut DeviceStruct,
    event_index: i32,
    object_index: i32,
) -> f32 {
    let Some(d) = NonNull::new(device) else {
        return 0.0;
    };
    let d = unsafe { &*d.as_ptr() };
    if d.events.is_null() || d.event_size <= 0 {
        return 0.0;
    }
    let offset = (event_index * d.event_size + object_index * 4) as usize;
    if offset + 4 > (d.event_size as usize * d.max_events as usize).min(usize::MAX) {
        return 0.0;
    }
    let ptr = unsafe { d.events.add(offset) as *const f32 };
    unsafe { *ptr }
}

#[no_mangle]
pub extern "C" fn setDeviceEventAxisValue(
    device: *mut DeviceStruct,
    event_index: i32,
    object_index: i32,
    value: f32,
) {
    let Some(d) = NonNull::new(device) else {
        return;
    };
    let d = unsafe { &*d.as_ptr() };
    if d.events.is_null() || d.event_size <= 0 {
        return;
    }
    let offset = (event_index * d.event_size + object_index * 4) as usize;
    let ptr = unsafe { d.events.add(offset) as *mut f32 };
    unsafe {
        *ptr = value;
    }
}

#[no_mangle]
pub extern "C" fn getDeviceEventWheelValue(
    device: *mut DeviceStruct,
    event_index: i32,
    object_index: i32,
) -> f32 {
    let Some(d) = NonNull::new(device) else {
        return 0.0;
    };
    let d = unsafe { &*d.as_ptr() };
    if d.events.is_null() || d.event_size <= 0 {
        return 0.0;
    }
    let base = d.lastaxis * 4;
    let offset = (event_index * d.event_size + base + object_index * 4) as usize;
    let ptr = unsafe { d.events.add(offset) as *const f32 };
    unsafe { *ptr }
}

#[no_mangle]
pub extern "C" fn setDeviceEventWheelValue(
    device: *mut DeviceStruct,
    event_index: i32,
    object_index: i32,
    value: f32,
) {
    let Some(d) = NonNull::new(device) else {
        return;
    };
    let d = unsafe { &*d.as_ptr() };
    if d.events.is_null() || d.event_size <= 0 {
        return;
    }
    let base = d.lastaxis * 4;
    let offset = (event_index * d.event_size + base + object_index * 4) as usize;
    let ptr = unsafe { d.events.add(offset) as *mut f32 };
    unsafe {
        *ptr = value;
    }
}

/// Compute event size and allocate device->events (matches QB64pe setupDevice).
#[no_mangle]
pub extern "C" fn setupDevice(device: *mut DeviceStruct) {
    let Some(d) = NonNull::new(device) else {
        return;
    };
    let d = unsafe { &*d.as_ptr() };
    let mut size = d.lastaxis * 4 + d.lastwheel * 4 + d.lastbutton;
    size += 8; // appended ordering index
    size += 7;
    size -= size & 7; // align to 8-byte boundary
    let size = size as usize;
    let ptr = unsafe { libc::calloc(2, size) as *mut u8 };
    if ptr.is_null() {
        return;
    }
    unsafe {
        let d_mut = &mut *device;
        if !d_mut.events.is_null() {
            libc::free(d_mut.events as *mut libc::c_void);
        }
        d_mut.event_size = size as i32;
        d_mut.events = ptr;
        d_mut.max_events = 2;
        d_mut.queued_events = 2;
        d_mut.connected = 1;
        d_mut.used = 1;
    }
}

/// Create a new device event slot; may grow buffer. Returns new event index (matches QB64pe).
#[no_mangle]
pub extern "C" fn createDeviceEvent(device: *mut DeviceStruct) -> i32 {
    let Some(d) = NonNull::new(device) else {
        return 0;
    };
    let d = unsafe { &*d.as_ptr() };
    let event_size = d.event_size as usize;
    if event_size == 0 {
        return 0;
    }

    unsafe {
        let d_mut = &mut *device;
        let queued = d_mut.queued_events;
        let max_ev = d_mut.max_events;

        if queued == max_ev {
            if max_ev >= QUEUED_EVENTS_LIMIT {
                // discard oldest
                let src = d_mut.events.add(event_size);
                libc::memmove(
                    d_mut.events as *mut libc::c_void,
                    src as *const libc::c_void,
                    (queued - 1) as usize * event_size,
                );
                d_mut.queued_events -= 1;
            } else {
                let new_max = max_ev * 2;
                let new_ptr = libc::calloc(new_max as usize, event_size) as *mut u8;
                if new_ptr.is_null() {
                    return 0;
                }
                libc::memcpy(
                    new_ptr as *mut libc::c_void,
                    d_mut.events as *const libc::c_void,
                    queued as usize * event_size,
                );
                libc::free(d_mut.events as *mut libc::c_void);
                d_mut.events = new_ptr;
                d_mut.max_events = new_max;
            }
        }

        let queued = d_mut.queued_events; // re-read after possible decrement/grow
                                          // copy previous event into new slot
        let src = d_mut.events.add((queued - 1) as usize * event_size);
        let dst = d_mut.events.add(queued as usize * event_size);
        libc::memmove(
            dst as *mut libc::c_void,
            src as *const libc::c_void,
            event_size,
        );
        // set global event index at end of event
        let idx_ptr = d_mut
            .events
            .add(queued as usize * event_size + event_size - 8) as *mut i64;
        DEVICE_EVENT_INDEX += 1;
        *idx_ptr = DEVICE_EVENT_INDEX;
        d_mut.queued_events
    }
}

#[no_mangle]
pub extern "C" fn commitDeviceEvent(device: *mut DeviceStruct) {
    if let Some(d) = NonNull::new(device) {
        unsafe {
            let d_mut = &mut *d.as_ptr();
            d_mut.queued_events += 1;
        }
    }
}
