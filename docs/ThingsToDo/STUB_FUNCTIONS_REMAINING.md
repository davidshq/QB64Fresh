# QB64Fresh - Remaining Stub/Incomplete Functions

*Updated: 2026-01-24*

This document lists functions that are **not fully implemented** - either stub-only, intentionally disabled, or obsolete.

For implemented functions, see [STUB_FUNCTIONS_FULL.md](STUB_FUNCTIONS_FULL.md).

---

## Summary

| Status | Count | Description |
|--------|-------|-------------|
| ⚠️ Legacy stubs | ~4 | ERDEV, device error functions |
| ⚠️ Event handler stubs | ~7 | ON COM, ON UEVENT, ON SIGNAL |
| ❌ Obsolete hardware | ~3 | Light pen only |
| **Total Remaining** | **~7** | Out of 419 registered functions |

**Note:** Functions that throw compile errors (FRE, SETMEM, IOCTL$, FILEATTR) are documented in [STUB_FUNCTIONS_FULL.md](STUB_FUNCTIONS_FULL.md) as they match QB64pe's intended behavior.

---

## Joystick Functions - FULLY IMPLEMENTED ✅

The joystick/gamepad system is **fully implemented** in the external runtime (SDL2 mode).

| Function | External Runtime | Inline Runtime | QB64pe | Notes |
|----------|-----------------|----------------|--------|-------|
| `STICK(n)` | ✅ Full | ⚠️ Stub (127) | ✅ Full | Axis position 0-254 |
| `STRIG(n)` | ✅ Full | ⚠️ Stub (0) | ✅ Full | Button state -1/0 |
| `STRIG(n, controller)` | ✅ Full | ⚠️ Stub (0) | ✅ Full | QB64 extension |
| `ON STRIG(n) GOSUB` | ✅ Full | ⚠️ Stub | ⚠️ Ignored | **Fresh exceeds QB64pe** |
| `STRIG(n) ON/OFF/STOP` | ✅ Full | ⚠️ Stub | ⚠️ Ignored | Event control |

**Note:** QB64pe parses but ignores `ON STRIG` handlers. Fresh has a complete event system with pending event queues and STOP state support.

The inline runtime returns stub values because joystick hardware requires SDL2.

---

## Legacy Functions - Stubs

These functions exist for QB4.5 compatibility but are stubs or have minimal implementation.

### Device Functions

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `ERDEV()` | ⚠️ Stub (0) | ❌ Not registered | Legacy DOS device error |
| `ERDEV$()` | ⚠️ Stub ("") | ❌ Not registered | Legacy DOS device error |
| `IOCTL` | ⚠️ Stub | ⚠️ Stub | Statement form compiles to no-op |

### Event Handlers - Stub Only

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `ON COM` | ⚠️ Stub | ❌ Not registered | Serial port events |
| `COM ON/OFF/STOP` | ⚠️ Stub | ❌ Not registered | Serial port control |
| `ON UEVENT` | ⚠️ Stub | ❌ Not registered | User-defined events |
| `UEVENT ON/OFF/STOP` | ⚠️ Stub | ❌ Not registered | User event control |
| `_UEVENTTRIGGER` | ⚠️ Stub | ❌ Not registered | Trigger user event |
| `ON SIGNAL` | ⚠️ Stub | ❌ Not registered | Signal handler |
| `SIGNAL ON/OFF/STOP` | ⚠️ Stub | ❌ Not registered | Signal control |

**Priority:** Very Low - QB64pe doesn't implement these either.

---

## Obsolete Hardware

These functions are stubs because the hardware no longer exists.

### Light Pen

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `PEN()` | ⛔ Stub (0) | ❌ Not registered | Light pen hardware obsolete |
| `ON PEN` | ⛔ Stub | ❌ Not registered | |
| `PEN ON/OFF/STOP` | ⛔ Stub | ❌ Not registered | |

---

## Will Not Implement

- **Light Pen (PEN)** - Hardware doesn't exist; QB64pe also doesn't implement
- **DOS Device Functions (ERDEV)** - Returns stub values; QB64pe doesn't register these
- **Event Handlers (ON COM, ON UEVENT, ON SIGNAL)** - QB64pe doesn't implement these

---

## Implementation Statistics

**Total Built-in Functions/Subs:** 419

| Category | Count | Percentage |
|----------|-------|------------|
| ✅ Fully Implemented | ~412 | 98.3% |
| ⚠️ Legacy/Event Stubs | ~7 | 1.7% |
| ❌ Obsolete Hardware | ~3 | 0.7% |

The vast majority of QB64 programs will work without issues.
