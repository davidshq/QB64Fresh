# QB64Fresh - Remaining Stub/Incomplete Functions

*Updated: 2026-01-24*

This document lists functions that are **not fully implemented** - either stub-only in inline mode with partial external support, or intentionally disabled.

For the complete function reference (including all implemented functions), see [STUB_FUNCTIONS_FULL.md](STUB_FUNCTIONS_FULL.md).

---

## Summary

| Status | Count | Description |
|--------|-------|-------------|
| ⚠️ Legacy stubs | ~4 | ERDEV, device error functions, event handlers |
| ❌ Compile errors | 4 | FRE, SETMEM, IOCTL$, FILEATTR (match QB64pe) |
| ❌ Obsolete hardware | ~5 | Light pen, some joystick events |
| **Total Remaining** | **~13** | Out of 419 registered functions |

**Recent completions:**
- All audio functions (session 040)
- Alpha blending (`_BLEND`, `_DONTBLEND`, `_CLEARCOLOR`)
- Graphics: `_MAPTRIANGLE`, `_COPYPALETTE`, `_DISPLAYORDER`
- Debugger runtime integration (session 041)

---

## Legacy Functions - Stubs

These functions exist for QB4.5 compatibility but are stubs or have minimal implementation.

### Legacy I/O & Memory

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `FRE()` | ❌ Error | ⛔ Stub (error) | Throws compile error matching QB64pe |
| `SETMEM` | ❌ Error | ⛔ Stub (error) | Throws compile error matching QB64pe |
| `FILEATTR()` | ❌ Error | ⛔ Stub (error) | Throws compile error matching QB64pe |


### Device Functions

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `ERDEV()` | ⚠️ Stub (0) | ❌ Not registered | Legacy DOS device error |
| `ERDEV$()` | ⚠️ Stub ("") | ❌ Not registered | Legacy DOS device error |
| `IOCTL$()` | ❌ Error | ⛔ Stub (error) | Throws compile error matching QB64pe |
| `IOCTL` | ⚠️ Stub | ⛔ Stub (error) | Statement form compiles to no-op |

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

**Priority:** Very Low - QB64pe doesn't implement these either. They're rarely used in modern programs.

---

## Intentionally Disabled Functions

These functions are **not implemented for security or obsolescence reasons** in QB64Fresh, though QB64pe implements some of them.

### Port I/O - VGA Palette Emulation ✅

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `INP()` | ✅ Full | ✅ Full | VGA palette (0x3C9) and retrace (0x3DA) |
| `OUT` | ✅ Full | ✅ Full | Palette registers (0x3C7, 0x3C8, 0x3C9) |
| `WAIT` | ✅ Full | ✅ Full | Returns immediately for unsupported ports |

**Supported ports:** 0x3C7 (palette read index), 0x3C8 (palette write index), 0x3C9 (palette RGB), 0x3DA (vertical retrace). Other ports return 0 or no-op (safe defaults).

### System Interrupts - INT 0x33 Mouse Emulation ✅

Emulates INT 0x33 (mouse interrupt) like QB64pe for legacy program compatibility.

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `INTERRUPT` | ✅ Full | ✅ Full | INT 0x33 mouse emulation |
| `INTERRUPTX` | ✅ Full | ✅ Full | Extended version (same emulation) |

**Supported INT 0x33 subfunctions:**
- AX=0: Check mouse installed → returns AX=0xFFFF, BX=2
- AX=1: Show mouse cursor
- AX=2: Hide mouse cursor
- AX=3: Get status → BX=buttons, CX=X, DX=Y

Other interrupts are no-ops (safe defaults).

### Obsolete Hardware

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `PEN()` | ⛔ Stub (0) | ❌ Not registered | Light pen hardware obsolete |
| `ON PEN` | ⛔ Stub | ❌ Not registered | |
| `PEN ON/OFF/STOP` | ⛔ Stub | ❌ Not registered | |

### Joystick Events

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `STRIG()` | ⛔ Stub | ✅ Full (`libqb.cpp:25613`) | QB64pe polls controller buttons |
| `ON STRIG` | ⛔ Stub | ⚠️ Ignored | QB64pe parses but ignores for compatibility |
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
- **DOS Device Functions (ERDEV, IOCTL)** - QB64pe stubs these too
- **Event Handlers (ON COM, ON UEVENT, ON SIGNAL)** - QB64pe doesn't implement these

---

## Implementation Statistics

**Total Built-in Functions/Subs:** 419

| Category | Count | Percentage |
|----------|-------|------------|
| ✅ Fully Implemented | ~409 | 97.6% |
| ⚠️ Legacy Stubs | ~4 | 1% |
| ❌ Compile Errors (match QB64pe) | 4 | 1% |
| ❌ Obsolete | ~5 | 1.2% |

The vast majority of QB64 programs will work without issues. The remaining issues are:
- Obsolete legacy functions throw compile errors (FRE, SETMEM, IOCTL$, FILEATTR) - matches QB64pe
- Obsolete hardware (light pen) - QB64pe also doesn't implement
- Event handlers (ON COM, ON UEVENT, etc.) - QB64pe also doesn't implement
