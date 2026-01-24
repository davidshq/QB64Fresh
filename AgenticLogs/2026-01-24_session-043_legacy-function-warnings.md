# Session 043: Legacy Function Runtime Warnings

**Date:** 2026-01-24
**Focus:** Adding runtime warnings for unsupported legacy DOS functions

## Summary

Added one-time runtime warnings to legacy DOS functions that are stubs on modern systems. Also enhanced INT 0x33 mouse emulation for better QB64PE compatibility.

## Changes Made

### Runtime Warnings Added (`src/codegen/c_backend/runtime/legacy.rs`)

Functions that now emit one-time warnings to stderr on first call:

| Function | Warning Message |
|----------|-----------------|
| `PEN()` | "PEN() is not supported on modern systems (light pens are obsolete hardware)" |
| `ERDEV/ERDEV$` | "ERDEV/ERDEV$ are not supported (DOS device error functions)" |
| `IOCTL/IOCTL$` | "IOCTL/IOCTL$ are not supported (DOS device control functions)" |
| `ON COM` | "ON COM is not implemented (serial port event trapping)" |
| `ON PEN` | "ON PEN is not implemented (light pen event trapping)" |
| `ON UEVENT` | "ON UEVENT is not implemented (user event trapping)" |
| `ON SIGNAL` | "ON SIGNAL is not implemented (BASIC signal trapping)" |
| `INTERRUPT/INTERRUPTX` (non-0x33) | "INTERRUPT/INTERRUPTX only supports INT 0x33 (mouse). Other interrupts (0x%02X) are ignored." |

### INT 0x33 Mouse Emulation Enhanced

Added support for additional mouse interrupt functions:

- **AX=4**: Set mouse position (no-op for compatibility)
- **AX=5,6**: Button press/release info (returns 0)

Existing functions already implemented:
- AX=0: Check mouse installed
- AX=1: Show cursor
- AX=2: Hide cursor
- AX=3: Get position and buttons
- AX=7,8: Set min/max range (no-op)

### Implementation Pattern

Used static flags to ensure warnings only print once:

```c
static int _qb_warned_pen = 0;

int qb_pen(int64_t n) {
    (void)n;
    if (!_qb_warned_pen) {
        _qb_warned_pen = 1;
        fprintf(stderr, "QB64Fresh: PEN() is not supported...\n");
    }
    return 0;
}
```

### Documentation Updated

- Updated module documentation in `legacy.rs` with comprehensive list of:
  - Supported functions
  - Runtime warnings
  - INT 0x33 emulation details

## Context

This follows the research into how QB64PE handles legacy functions:
- QB64PE similarly has many legacy functions as stubs
- INT 0x33 is the only interrupt with partial implementation
- Most legacy DOS functions just silently do nothing

The warnings help users understand when their code uses features that won't work on modern systems, without breaking program execution.

## Files Modified

- `src/codegen/c_backend/runtime/legacy.rs` - Added warnings and enhanced INT 0x33

## Verification

- Build succeeds
- Codegen tests pass
- Generated C code contains warning infrastructure
- Warning flags prevent repeated warnings
