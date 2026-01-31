# Session 109 — game_controller.h (libqb compatibility)

**Date:** 2026-01-31

## Summary

Implemented **game_controller.h** API (LIBQB_FUNCTIONALITY.md §38): structures, constants, globals, and the nine device/event functions for gamepad/keyboard/mouse devices.

## Accomplished

1. **C header (`runtime/include/qb64fresh_rt.h`)**
   - Added `QUEUED_EVENTS_LIMIT`, `DEVICETYPE_CONTROLLER`, `DEVICETYPE_KEYBOARD`, `DEVICETYPE_MOUSE`.
   - Defined `struct device_struct` and `struct onstrig_struct` (layout matches QB64pe).
   - Declared extern globals: `device_last`, `device_max`, `devices`, `onstrig`, `onstrig_inprogress`.
   - Declared: `getDeviceEventButtonValue`, `setDeviceEventButtonValue`, `getDeviceEventAxisValue`, `setDeviceEventAxisValue`, `getDeviceEventWheelValue`, `setDeviceEventWheelValue`, `setupDevice`, `createDeviceEvent`, `commitDeviceEvent`.

2. **Runtime (`runtime/src/game_controller_ffi.rs`)**
   - New module with `#[repr(C)]` `DeviceStruct` and `OnstrigStruct` matching the header.
   - `game_controller_init()`: allocates `devices` (1001 slots) and `onstrig` (65536 slots) via `libc::calloc`, sets C-visible globals; called from `qb_runtime_init()`.
   - Event layout (per QB64pe qbx.cpp): `[axis floats][wheel floats][button bytes][padding][int64 index]`.
   - `setupDevice`: computes event size (lastaxis*4 + lastwheel*4 + lastbutton + 8, 8-byte aligned), allocates initial event buffer (2 slots).
   - `createDeviceEvent`: grows buffer or discards oldest when at limit; copies previous event into new slot; sets global event index; returns new event index.
   - `commitDeviceEvent`: increments `queued_events`.
   - Get/set functions: index into `device->events` using `event_size`, `lastaxis`, `lastwheel` offsets.

3. **LIBQB_FUNCTIONALITY.md**
   - §38 table: all nine functions and device/event setup marked ✅.

## Notes

- No codegen changes: generated C uses the header when `--runtime external`; no new BASIC keywords or built-ins added.
- QB64_GAMEPAD_INIT/POLL/SHUTDOWN (libstem_gamepad) are not implemented; device enumeration and input still come from existing joystick/graphics code. This adds the **data structures and event API** so generated code that uses `devices`, `onstrig`, and the get/set/commit functions links and runs.
