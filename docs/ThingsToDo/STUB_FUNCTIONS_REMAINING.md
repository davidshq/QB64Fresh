# QB64Fresh - Remaining Stub/Incomplete Functions

*Updated: 2026-01-23*

This document lists functions that are **not fully implemented** - either stub-only in inline mode with partial external support, or intentionally disabled.

For the complete function reference (including all implemented functions), see [STUB_FUNCTIONS_FULL.md](STUB_FUNCTIONS_FULL.md).

---

## Summary

| Status | Count | Description |
|--------|-------|-------------|
| ⚠️ Partial (External) | ~6 | Audio functions with rodio limitations |
| ⚠️ Stub only | ~12 | Legacy functions (no external implementation) |
| ❌ Not implemented | ~10 | Intentionally disabled (security/obsolete) |
| **Total Remaining** | **~28** | Out of 373 registered functions |

---

## Audio Functions - Partial Implementation

**Issue:** Rodio audio library has some limitations compared to QB64pe's OpenAL backend.

| Function | Inline Returns | External Status | Issue |
|----------|----------------|-----------------|-------|
| `_SNDOPENRAW()` | -1 | ⚠️ Partial | Raw audio stream support limited |
| `_SNDBAL()` | void | ⚠️ Partial | 3D balance not fully supported by rodio |
| `_SNDGETPOS()` | 0.0 | ⚠️ Partial | Position tracking limited |
| `_SNDSETPOS()` | void | ⚠️ Partial | Seeking limited in some formats |
| `_SNDRAW()` | void | ⚠️ Partial | Raw sample writing limited |
| `_SNDRAWLEN()` | 0.0 | ⚠️ Partial | Raw queue length tracking |

**Priority:** Low - Most audio programs work fine with the implemented features.

---

## Legacy Functions - Stub Only

These functions exist for QB4.5 compatibility but have no meaningful implementation on modern systems.

### Legacy I/O & Memory

| Function | Inline Returns | Purpose | Notes |
|----------|----------------|---------|-------|
| `LPOS()` | 1 | Printer carriage position | Printers don't work this way anymore |
| `FRE()` | Large number | Free memory | Returns fake value; use system APIs instead |
| `PEEK()` | 0 | Read memory byte | No direct memory access in protected mode |
| `POKE` | void | Write memory byte | No direct memory access in protected mode |

### Device Functions

| Function | Inline Returns | Purpose | Notes |
|----------|----------------|---------|-------|
| `ERDEV()` | 0 | Device error code | DOS device error codes obsolete |
| `ERDEV$()` | "" | Device error name | DOS device names obsolete |
| `IOCTL$()` | "" | Device status string | DOS IOCTL obsolete |
| `IOCTL` | void | Send device control | DOS IOCTL obsolete |

### Event Handlers - Stub Only

| Function | Inline Returns | Purpose | Notes |
|----------|----------------|---------|-------|
| `ON COM` | void | Serial port handler | COM port support not implemented |
| `COM ON/OFF/STOP` | void | Serial control | COM port support not implemented |
| `ON UEVENT` | void | User event handler | Minimal stub |
| `UEVENT ON/OFF/STOP` | void | User event control | Minimal stub |
| `_UEVENTTRIGGER` | void | Trigger user event | Minimal stub |
| `ON SIGNAL` | void | Signal handler | Minimal stub |
| `SIGNAL ON/OFF/STOP` | void | Signal control | Minimal stub |

**Priority:** Very Low - These are rarely used in modern programs.

---

## Intentionally Disabled Functions

These functions are **not implemented for security or obsolescence reasons**.

### Port I/O (Security)

Direct port I/O is not available on protected-mode operating systems and would be a security risk.

| Function | Inline Returns | Purpose |
|----------|----------------|---------|
| `INP()` | 0xFF | Read from I/O port |
| `OUT` | void | Write to I/O port |
| `WAIT` | void | Wait for port condition |

### System Interrupts (Security)

Direct interrupt calls are not supported on modern systems.

| Function | Inline Returns | Purpose |
|----------|----------------|---------|
| `INTERRUPT` | void (warns) | Call system interrupt |
| `INTERRUPTX` | void (warns) | Extended interrupt |

### Obsolete Hardware

| Function | Inline Returns | Purpose |
|----------|----------------|---------|
| `PEN()` | 0 | Light pen state |
| `ON PEN` | void | Light pen handler |
| `PEN ON/OFF/STOP` | void | Light pen control |

### Joystick Events (Not Yet)

| Function | Inline Returns | Purpose | Notes |
|----------|----------------|---------|-------|
| `ON STRIG` | void | Joystick trigger handler | Could be implemented |
| `STRIG ON/OFF/STOP` | void | Trigger control | Could be implemented |

**Priority:** Not planned - Security restrictions or obsolete hardware.

---

## Potential Future Work

### Could Be Implemented (Low Priority)

1. **ON STRIG / STRIG ON/OFF/STOP** - Joystick event handlers
   - SDL2 already provides joystick support
   - Would need event loop integration
   - Effort: Medium

2. **COM Port Support** - Serial communication
   - Would need cross-platform serial library
   - Effort: Large

3. **Raw Audio Streaming** - _SNDRAW improvements
   - Rodio limitation; may need different backend
   - Effort: Medium-Large

### Will Not Implement

- **Port I/O (INP, OUT, WAIT)** - Security risk, OS doesn't allow
- **System Interrupts** - Security risk, OS doesn't allow
- **Light Pen** - Hardware doesn't exist
- **DOS Device Functions** - DOS doesn't exist

---

## Implementation Statistics

**Total Built-in Functions:** 373

| Category | Count | Percentage |
|----------|-------|------------|
| ✅ Fully Implemented | ~345 | 92% |
| ⚠️ Partial/Stub | ~18 | 5% |
| ❌ Disabled/Obsolete | ~10 | 3% |

The vast majority of QB64 programs will work without issues. The remaining stubs are for:
- Obscure legacy features (PEEK/POKE, LPOS, device control)
- Security-restricted operations (port I/O, interrupts)
- Obsolete hardware (light pen)
- Partial audio features (raw streaming, seeking)
