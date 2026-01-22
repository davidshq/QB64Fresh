# QB64Fresh Runtime Functions Reference

This document lists functions that have **stub implementations in the inline runtime** but are **fully implemented in the external runtime library**.

## Understanding the Two Runtime Modes

QB64Fresh supports two runtime modes:

| Mode | Flag | Use Case | Dependencies |
|------|------|----------|--------------|
| **Inline** (default) | none | Console programs, simple apps | None (self-contained) |
| **External** | `--runtime external` | Graphics, audio, games | SDL2, system libraries |

### Inline Runtime (Stubs)
- Embeds minimal C code directly in the generated program
- Functions return safe defaults (0, empty strings, -1 for invalid handles)
- Logs warnings when graphics/audio functions are called
- **Location:** `src/codegen/c_backend/runtime.rs` (4,141 lines)

### External Runtime (Full Implementation)
- Links against `libqb64fresh_rt.a` static library
- Complete SDL2-based graphics with hardware acceleration
- Full Rodio-based audio with MML parsing and file playback
- **Location:** `runtime/src/` (11,668 lines total)

---

## Implementation Status Overview

| Category | Inline (Stubs) | External Runtime | Notes |
|----------|----------------|------------------|-------|
| **Audio** | ⚠️ No-op | ✅ Full | Rodio backend |
| **Graphics Core** | ⚠️ Returns defaults | ✅ Full | SDL2 backend |
| **Drawing** | ⚠️ No-op | ✅ Full | PSET, LINE, CIRCLE, PAINT |
| **Images** | ⚠️ Returns -1 | ✅ Full | PNG/JPEG/BMP/GIF support |
| **Mouse** | ⚠️ Returns 0 | ✅ Full | SDL2 input |
| **Fonts** | ⚠️ Hardcoded 8×16 | ✅ Full | TrueType support |
| **Clipboard** | ⚠️ Empty | ✅ Full | SDL2 clipboard |
| **File I/O** | ✅ Full | ✅ Full | FIELD/LSET/RSET complete |
| **Dialogs** | ⚠️ stdin fallback | ✅ Full | Native rfd dialogs |
| **Joystick** | ⚠️ Center position | ✅ Full | SDL2 gamepad via joystick.rs |
| **Legacy Hardware** | ⚠️ Safe defaults | ❌ Intentional | Port I/O, light pen obsolete |

---

## Audio Functions (20 functions)

**External Runtime:** `runtime/src/audio/rodio_backend.rs` (543 lines)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_sndbal()` | void | ⚠️ Partial | Set 3D balance (rodio limitation) |
| `qb_sndgetpos()` | 0.0 | ⚠️ Partial | Get playback position |
| `qb_sndsetpos()` | void | ⚠️ Partial | Set playback position |
| `qb_sndopenraw()` | -1 | ⚠️ Partial | Open raw audio stream |
| `qb_sndrawlen()` | 0.0 | ⚠️ Partial | Get raw audio queue length |

---

## Graphics Functions (77 functions)

**External Runtime:** `runtime/src/dialogs.rs` (348 lines)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_inputbox()` | stdin | ⚠️ Basic | Show input dialog |

---

## Legacy/QB4.5 Functions (32 functions)

These are intentionally minimal - they support compatibility with old BASIC programs but many represent obsolete hardware.

### Legacy I/O & Memory (6 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_lpos()` | 1 | ⚠️ Stub only | Get printer carriage position |
| `qb_fileattr()` | 0 | ⚠️ Stub only | Get file attributes |

### Port I/O Functions (3 functions) - Intentionally Disabled

These are **intentionally not implemented** for security. Direct port I/O is not available on protected-mode operating systems.

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_inp()` | 0xFF | ❌ Sandboxed | Read from I/O port |
| `qb_out()` | void | ❌ Sandboxed | Write to I/O port |
| `qb_wait()` | void | ❌ Sandboxed | Wait for port condition |

### Hardware Functions (5 functions) - Obsolete Hardware

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_pen()` | 0 | ❌ Obsolete | Get light pen state |
| `qb_erdev()` | 0 | ⚠️ Stub only | Get device error code |
| `qb_erdev_str()` | "" | ⚠️ Stub only | Get device error name |
| `qb_ioctl()` | void | ⚠️ Stub only | Send device control string |
| `qb_ioctl_str()` | "" | ⚠️ Stub only | Get device status string |

### System Interrupts (2 functions) - Intentionally Disabled

Not supported on modern systems for security reasons.

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_interrupt()` | void (warns) | ❌ Disabled | Call system interrupt |
| `qb_interruptx()` | void (warns) | ❌ Disabled | Extended system interrupt |

### Event Handlers (11 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_on_strig()` | void | ❌ Not yet | ON STRIG (joystick) handler |
| `qb_strig_control()` | void | ❌ Not yet | STRIG ON/OFF/STOP control |
| `qb_on_com()` | void | ⚠️ Stub only | ON COM (serial port) handler |
| `qb_com_control()` | void | ⚠️ Stub only | COM ON/OFF/STOP control |
| `qb_on_pen()` | void | ❌ Obsolete | ON PEN (light pen) handler |
| `qb_pen_control()` | void | ❌ Obsolete | PEN ON/OFF/STOP control |
| `qb_on_uevent()` | void | ⚠️ Stub only | ON UEVENT (user event) handler |
| `qb_uevent_control()` | void | ⚠️ Stub only | UEVENT ON/OFF/STOP control |
| `qb_uevent_trigger()` | void | ⚠️ Stub only | Trigger user-defined event |
| `qb_on_signal()` | void | ⚠️ Stub only | ON SIGNAL handler |
| `qb_signal_control()` | void | ⚠️ Stub only | SIGNAL ON/OFF/STOP control |

---

## Summary

### Implementation Statistics

| Status | Count | Percentage |
|--------|-------|------------|
| ✅ Fully Implemented | 121 | 81% |
| ⚠️ Partial/Stub only | 19 | 13% |
| ❌ Not implemented/Disabled | 10 | 7% |
| **Total** | **150** | |

### What's Still Missing

**Low Priority (rarely used):**
1. Serial port (COM) support
2. User-defined events (UEVENT)
3. Input box dialog (currently uses stdin)

**Intentionally Not Implemented:**
- Port I/O (INP, OUT, WAIT) - security
- System interrupts (INTERRUPT, INTERRUPTX) - security
- Light pen (PEN) - obsolete hardware

---

## Runtime Source Files Reference

| File | Lines | Purpose |
|------|-------|---------|
| `src/codegen/c_backend/runtime.rs` | 4,141 | Inline runtime (stubs & core functions) |
| `runtime/src/graphics/sdl2.rs` | 2,144 | SDL2 graphics backend |
| `runtime/src/audio/rodio_backend.rs` | 543 | Rodio audio backend |
| `runtime/src/dialogs.rs` | 348 | Native file dialogs (rfd) |
| `runtime/src/joystick.rs` | 306 | Gamepad/joystick support |
| **External runtime total** | **11,668** | All runtime/src/ files |

---

## How to Use Full Functionality

```bash
# Compile with external runtime for graphics/audio/dialogs
qb64fresh myprogram.bas --runtime external

# Default inline mode for console programs (file I/O works!)
qb64fresh myprogram.bas
```

The external runtime requires SDL2 to be installed on the system.
