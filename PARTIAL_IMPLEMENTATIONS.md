# Partial Implementations Audit

This document catalogs all functionality that is only partially implemented across the QB64Fresh codebase. Generated on 2026-01-26.

## Categories

1. [Runtime Stubs (Inline Mode)](#runtime-stubs-inline-mode)
2. [Runtime Library Partial Implementations](#runtime-library-partial-implementations)
3. [Debugger Infrastructure](#debugger-infrastructure)
4. [Graphics Features](#graphics-features)
5. [System Features](#system-features)
6. [File I/O Features](#file-io-features)
7. [Memory Features](#memory-features)
8. [Joystick Features](#joystick-features)

---

## Runtime Stubs (Inline Mode)

When using `--runtime inline`, many functions are stubs that allow compilation but don't provide full functionality.

### Graphics Stubs (`src/codegen/c_backend/runtime/graphics.rs`)

**Status:** All graphics operations are stubs in inline mode.

- All graphics commands (SCREEN, LINE, CIRCLE, PSET, etc.) are stubs
- Print warning on first use
- Return safe default values
- Frame limiting prevents infinite loops (default 1000 frames)

**Note:** Full graphics support requires `--runtime external` with `libqb64fresh_rt`.

### System Stubs (`src/codegen/c_backend/runtime/system.rs`)

**Font Functions:**
- `_FONT`, `_FREEFONT`, `_LOADFONT` - Stubs (no-op)
- `_MAPUNICODE` - **Fully functional** (Code Page 437 mapping)

**Window Functions:**
- `_TITLE` - Stub (no-op)
- `_SCREENMOVE` - Stub (no-op)
- `_SCREENSHOW` - Stub (no-op)
- `_ICON` - Stub (no-op)

**Network Functions:**
- All network functions are stubs (no actual network support)
- `_OPENHOST`, `_OPENCONNECTION`, `_OPENCLIENT` - Return 0
- `_CONNECTED` - Returns 0 (not connected)
- `_STATUSCODE` - Returns 200 (stub)
- Network I/O functions (`qb_net_get`, `qb_net_put`, etc.) - Return 0/empty

**Drag and Drop Functions:**
- `_TOTALDROPPEDFILES` - Returns 0
- `_DROPPEDFILE$` - Returns empty string
- `_FINISHDROP`, `_ACCEPTFILEDROP` - No-ops

**Dialog Functions:**
- `_MESSAGEBOX` - Stub (returns 1)
- `_SAVEDIALOG$` - Stub (returns empty string)
- `_OPENFILEDIALOG$` - Stub (returns empty string)

**Compression Functions:**
- `_DEFLATE$` - Stub (returns empty string)
- `_MD5$` - Stub (returns "00000000000000000000000000000000")

**Array Metadata:**
- `qb_array_register` - Stub (no-op, doesn't track array bounds)
- `qb_array_register_md` - Stub (no-op, doesn't track multi-dimensional arrays)
- `qb_ubound` - Stub (returns 0)
- `qb_ubound2` - Stub (returns 0)
- `qb_lbound` - Stub (returns 0)

**File I/O:**
- `qb_file_get_string` - Stub (no-op, requires runtime library support for opaque strings)

**Console Control:**
- `qb_echo` - Stub (echo is always on, no control)

### Legacy Functions (`src/codegen/c_backend/runtime/legacy.rs`)

**Joystick:**
- `STICK`, `STRIG` - Stubs (return center position / not pressed)

**Light Pen:**
- `PEN` - Stub with runtime warning (obsolete hardware)

**Serial I/O:**
- `ERDEV`, `ERDEV$`, `IOCTL`, `IOCTL$` - Stubs with runtime warning

**Note:** These are intentionally stub implementations for legacy DOS-era features.

---

## Runtime Library Partial Implementations

### Graphics FFI (`runtime/src/graphics_ffi.rs`)

**Per-Image Palettes:**
- Line 1032: `qb_palettecolor_get` - TODO: Support per-image palettes
- Line 1062: `qb_palettecolor` - TODO: Support per-image palettes with handle
- Currently ignores handle parameter and uses current palette

**Graphics Position Tracking:**
- Line 1683: TODO: Track last graphics position for proper STEP behavior
- STEP modifier in graphics commands needs last position tracking

**OpenGL Functions:**
- `_GLRENDER` - No-op stub (raw `_GL*` excluded per ADR-0014)
- `_GLCOMPAT` - No-op stub (returns 0)

### Graphics SDL2 Backend (`runtime/src/graphics/sdl2.rs`)

**TrueType Font Support:**
- Lines 1419-1454: TTF functions are stubs when `graphics-sdl2-ttf` feature is disabled
- `load_font` - Returns 0 (TTF not supported)
- `set_font` - No-op (returns 0)
- `free_font` - No-op

**Console Scrolling:**
- Line 1736: TODO: scroll if needed
- Line 1765: TODO: implement actual scrolling
- Console text scrolling not yet implemented

### I/O Functions (`runtime/src/io.rs`)

**FIELD Statement:**
- Line 2356: `qb_field_start` - Stub (FIELD statement not yet fully implemented)
- Line 2362: `qb_field_add` - Stub (FIELD statement not yet fully implemented)
- FIELD statement for random file I/O is not complete

**LSET/RSET:**
- Lines 2371-2398: Simplified implementations
- Only copy strings, don't properly pad/truncate to field width

**COMMAND$ Function:**
- Line 2583: Currently returns empty string (stub implementation)
- Should return command-line arguments

### Memory Functions (`runtime/src/memory.rs`)

**Image Memory:**
- Line 226: `_MEMIMAGE` - Stub (returns empty QbMem, no image in external yet)
- Returns default/empty memory block

**Sound Memory:**
- Line 232: `_MEMSOUND` - Stub (returns empty QbMem)
- Returns default/empty memory block

### Joystick Functions (`runtime/src/joystick.rs`)

**Device Enumeration:**
- Line 218: TODO: Actually enumerate SDL2 joysticks
- `qb_devices()` currently returns 2 (keyboard + mouse) without enumerating actual joysticks

---

## Debugger Infrastructure

The debugger infrastructure is complete but requires runtime integration to be functional.

### Execution Control (`tools/debug/src/lib.rs`)

**Status:** All execution control methods are stubs awaiting runtime integration.

- Line 464: `run()` - TODO: Implement actual execution control
- Line 472: `pause()` - TODO: Implement actual pause
- Line 479: `step_over()` - TODO: Implement step over
- Line 486: `step_into()` - TODO: Implement step into
- Line 493: `step_out()` - TODO: Implement step out
- Line 500: `stop()` - TODO: Implement actual stop

**Current State:** Methods update internal state but don't actually control program execution.

### Watch Expression Evaluation (`tools/debug/src/watch.rs`)

**Status:** Expression parsing works, but evaluation requires runtime state.

- Line 257: `IndexExpr::evaluate()` - TODO: Look up variable value in runtime state
- Line 374: `WatchManager::evaluate_all()` - TODO: Implement actual evaluation with runtime state
- Line 401: `WatchManager::evaluate()` - TODO: Implement actual evaluation with runtime state

**Current State:** Can parse expressions and validate variable names exist, but cannot evaluate values.

### Source File Management (`tools/debug/src/sources.rs`)

**$INCLUDE Scanning:**
- Line 233: TODO: Scan for $INCLUDE directives and load those files too
- Currently only loads the main source file, not included files

### Debug Adapter Protocol (`tools/debug/src/server.rs`)

**Attach Mode:**
- Line 343: Attach mode not implemented (returns error: "Use launch instead")

**Expression Evaluation:**
- Line 814: TODO: Implement expression evaluation
- `evaluate` request handler returns error message instead of evaluating

---

## Graphics Features

### Inline Runtime Graphics

All graphics operations in inline runtime mode are stubs. See [Runtime Stubs (Inline Mode)](#runtime-stubs-inline-mode) above.

### External Runtime Graphics

Most graphics features are fully implemented in the external runtime, with these exceptions:

1. **Per-Image Palettes** - Not yet supported (uses global palette)
2. **STEP Position Tracking** - Last graphics position not tracked for STEP modifier
3. **TrueType Fonts** - Only available when `graphics-sdl2-ttf` feature is enabled
4. **Console Scrolling** - Not yet implemented

---

## System Features

### Network Support

**Status:** No network support implemented.

All network functions are stubs:
- `_OPENHOST`, `_OPENCONNECTION`, `_OPENCLIENT` - Return 0
- `_CONNECTED` - Returns 0
- `_STATUSCODE` - Returns 200 (stub)
- Network I/O functions return empty/zero

### Dialog Functions

**Status:** All dialog functions are stubs.

- `_MESSAGEBOX` - Returns 1 (no actual dialog)
- `_SAVEDIALOG$` - Returns empty string
- `_OPENFILEDIALOG$` - Returns empty string

### Compression

**Status:** Compression functions are stubs.

- `_DEFLATE$` - Returns empty string
- `_MD5$` - Returns fixed stub value "00000000000000000000000000000000"

### Drag and Drop

**Status:** All drag-and-drop functions are stubs.

- `_TOTALDROPPEDFILES` - Returns 0
- `_DROPPEDFILE$` - Returns empty string
- `_FINISHDROP`, `_ACCEPTFILEDROP` - No-ops

---

## File I/O Features

### FIELD Statement

**Status:** Partially implemented (stubs).

- `qb_field_start()` - Stub (no-op)
- `qb_field_add()` - Stub (no-op)
- FIELD statement for random file I/O not fully implemented

### LSET/RSET

**Status:** Simplified implementation.

- Only copies strings, doesn't properly pad/truncate to field width
- Should respect field width for alignment

### COMMAND$ Function

**Status:** Stub implementation.

- Returns empty string instead of command-line arguments
- Should parse and return actual command-line arguments

---

## Memory Features

### Image Memory Access

**Status:** Stub.

- `_MEMIMAGE` - Returns empty QbMem block
- Cannot access image pixel data through memory interface

### Sound Memory Access

**Status:** Stub.

- `_MEMSOUND` - Returns empty QbMem block
- Cannot access sound data through memory interface

---

## Joystick Features

### Device Enumeration

**Status:** Partial implementation.

- `qb_devices()` returns 2 (keyboard + mouse) without enumerating actual joysticks
- TODO: Actually enumerate SDL2 joysticks
- Joystick input functions may work if joysticks are manually configured, but automatic detection is incomplete

---

## Summary by Priority

### High Priority (Core Functionality)

1. **Array Metadata Tracking** - UBOUND/LBOUND return 0 (stubs)
2. **COMMAND$ Function** - Returns empty string (should return command-line args)
3. **FIELD Statement** - Not implemented (needed for random file I/O)
4. **LSET/RSET** - Simplified (should respect field width)

### Medium Priority (Useful Features)

1. **Per-Image Palettes** - Graphics feature enhancement
2. **STEP Position Tracking** - Graphics feature enhancement
3. **Console Scrolling** - Text output enhancement
4. **$INCLUDE Scanning in Debugger** - Debugger completeness
5. **Joystick Enumeration** - Input device support

### Low Priority (Nice to Have)

1. **Network Support** - Entire subsystem is stubs
2. **Dialog Functions** - Platform-specific UI
3. **Compression Functions** - Utility features
4. **Drag and Drop** - Platform-specific feature
5. **TrueType Fonts** - Optional feature (requires feature flag)

### Debugger Integration (Separate Effort)

All debugger execution control and evaluation features require runtime integration:
- Execution control (run, pause, step)
- Watch expression evaluation
- Variable value lookup
- Expression evaluation in DAP

These are infrastructure-complete but need runtime hooks to be functional.

---

## Notes

- Many stubs are **intentional** for inline runtime mode (graphics, network, dialogs)
- Full functionality requires `--runtime external` with `libqb64fresh_rt`
- Some stubs are **temporary** and need implementation (array metadata, FIELD statement)
- Debugger infrastructure is **complete** but needs runtime integration
- Graphics features are **mostly complete** in external runtime, with minor enhancements needed
