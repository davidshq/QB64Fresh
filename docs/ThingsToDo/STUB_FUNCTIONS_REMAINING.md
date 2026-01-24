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
| ❌ Obsolete hardware | ~5 | Light pen, joystick events |
| **Total Remaining** | **~9** | Out of 419 registered functions |

**Note:** Functions that throw compile errors (FRE, SETMEM, IOCTL$, FILEATTR) are documented in [STUB_FUNCTIONS_FULL.md](STUB_FUNCTIONS_FULL.md) as they match QB64pe's intended behavior.

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

### Joystick Events

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `STRIG()` | ⛔ Stub | ✅ Full | QB64pe polls controller buttons |
| `ON STRIG` | ⛔ Stub | ⚠️ Ignored | QB64pe parses but ignores |
| `STRIG ON/OFF/STOP` | ⛔ Stub | ⚠️ Ignored | QB64pe parses but ignores |

---

## Potential Future Work

### Could Be Implemented (Low Priority)

1. **STRIG Function** - Joystick button polling
   - QB64pe: Full implementation at `libqb.cpp:25613`
   - SDL2 already provides joystick support
   - Effort: Low-Medium

2. **COM Port Support** - Serial communication
   - Would need cross-platform serial library (e.g., `serialport` crate)
   - QB64pe: Not implemented
   - Effort: Large

### Will Not Implement

- **Light Pen (PEN)** - Hardware doesn't exist; QB64pe also doesn't implement
- **DOS Device Functions (ERDEV)** - Returns stub values; QB64pe doesn't register these
- **Event Handlers (ON COM, ON UEVENT, ON SIGNAL)** - QB64pe doesn't implement these

---

## Implementation Statistics

**Total Built-in Functions/Subs:** 419

| Category | Count | Percentage |
|----------|-------|------------|
| ✅ Fully Implemented | ~410 | 97.8% |
| ⚠️ Legacy/Event Stubs | ~9 | 2.1% |
| ❌ Obsolete Hardware | ~5 | 1.2% |

The vast majority of QB64 programs will work without issues.
