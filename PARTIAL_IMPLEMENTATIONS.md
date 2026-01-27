# Partial Implementations Audit

This document catalogs all functionality that is only partially implemented across the QB64Fresh codebase. Last updated: 2026-01-27.

## Legend

**Status Icons:**
- 🟢 = Complete/fully functional
- ⚠️ = Stub/partial implementation (code exists but limited functionality, needs work)
- 🟣 = Limited implementation (intentionally limited, acceptable as-is by design)
- 🔴 = Missing/incomplete (needs implementation work)
- 🚫 = Intentionally missing (won't be implemented, by design)

**Intentional Status:**
- **Yes** = Intentionally stub/missing (by design, acceptable as-is)
- **No** = Needs implementation (temporary, should be fixed)
- **Partial** = Partially intentional (e.g., feature flag, conditional availability)
- **-** = Not applicable (feature is complete)

## Categories

1. [Graphics Features](#graphics-features)
2. [System Features](#system-features)
3. [File I/O Features](#file-io-features)
4. [Debugger Infrastructure](#debugger-infrastructure)
5. [Legacy Features](#legacy-features)

---

## Graphics Features

| Feature | Inline | External Runtime | Intentional? | Notes |
|---------|--------|------------------|--------------|-------|
| **Core Graphics Commands** | | | | |
| SCREEN, LINE, CIRCLE, PSET, etc. | 🟣 | 🟢 | Yes | Inline mode: stubs with warnings, frame limiting. External: full SDL2 implementation |
| STEP position tracking | 🔴 | 🟢 | No | Inline: not tracked. External: fully implemented in both mock and SDL2 backends |
| **Font Functions** | | | | |
| `_FONT`, `_FREEFONT`, `_LOADFONT` | 🟣 | 🟣 | Partial | Stubs in inline. External: requires `graphics-sdl2-ttf` feature flag (SDL2_ttf dependency) |
| `_MAPUNICODE` | 🟢 | 🟢 | - | Fully functional (Code Page 437 mapping) |
| **Window Functions** | | | | |
| `_TITLE` | 🔴 | 🟢 | No | Inline: not implemented. External: fully functional |
| `_SCREENMOVE` | 🔴 | 🟢 | No | Inline: not implemented. External: fully functional |
| `_SCREENSHOW` | 🔴 | 🟢 | No | Inline: not implemented. External: fully functional |
| `_ICON` | 🔴 | ⚠️ | No | Inline: not implemented. External: FFI and trait methods exist, but icon setting from image handle is TODO (requires converting image buffer to SDL2 surface) |
| **Palette Functions** | | | | |
| Per-image palettes | 🟢 | 🟢 | - | Fully implemented in both mock and SDL2 backends. Handle 0 uses screen palette, other handles use image-specific palettes |
| **OpenGL Functions** | | | | |
| `_GLRENDER`, `_GLCOMPAT` | 🚫 | 🚫 | Yes | Excluded per ADR-0014 (using SDL2/winit, not raw OpenGL) |
| **Console Scrolling** | 🟢 | 🟢 | - | Fully implemented: `scroll_text_up()` method shifts pixel rows and clears bottom line

---

## System Features

| Feature | Inline | External Runtime | Intentional? | Notes |
|---------|--------|------------------|--------------|-------|
| **Network Functions** | | | | |
| `_OPENHOST`, `_OPENCONNECTION`, `_OPENCLIENT` | 🚫 | 🚫 | Yes | No network support planned |
| `_CONNECTED` | 🚫 | 🚫 | Yes | Returns 0 (not connected) |
| `_STATUSCODE` | 🚫 | 🚫 | Yes | Returns 200 (stub) |
| Network I/O (`qb_net_get`, `qb_net_put`, etc.) | 🚫 | 🚫 | Yes | Return 0/empty |
| **Drag and Drop** | | | | |
| `_TOTALDROPPEDFILES` | 🚫 | 🚫 | Yes | Returns 0 |
| `_DROPPEDFILE$` | 🚫 | 🚫 | Yes | Returns empty string |
| `_FINISHDROP`, `_ACCEPTFILEDROP` | 🚫 | 🚫 | Yes | No-ops |
| **File I/O Support** | | | | |
| `qb_file_get_string` | ⚠️ | 🟢 | No | Inline: stub (requires runtime library for opaque strings). External: fully functional |
| **Console Control** | | | | |
| `qb_echo` | ⚠️ | ⚠️ | No | Echo is always on, no control implemented |

---

## File I/O Features

| Feature | Inline | External Runtime | Intentional? | Notes |
|---------|--------|------------------|--------------|-------|
| **FIELD Statement** | | | | |
| `qb_field_start()` | ⚠️ | ⚠️ | No | Stub in `runtime/src/io.rs` line 2408. Code generation exists but runtime incomplete |
| `qb_field_add()` | ⚠️ | ⚠️ | No | Stub in `runtime/src/io.rs` line 2417. Multi-dimensional array handling TODO at `src/codegen/c_backend/stmt/io.rs` line 89 |
| **LSET/RSET** | | | | |
| LSET/RSET string operations | ⚠️ | ⚠️ | No | Simplified: only copies strings, doesn't pad/truncate to field width. Located in `runtime/src/io.rs` lines 2421-2448 |

---

## Debugger Infrastructure

| Feature | Inline | External Runtime | Intentional? | Notes |
|---------|--------|------------------|--------------|-------|
| **Execution Control** | | | | |
| `run()`, `pause()`, `step_over()`, `step_into()`, `step_out()`, `stop()` | 🟢 | 🟢 | - | Infrastructure complete. Requires runtime integration hooks to be functional |
| `process_events()`, `handle_event()` | 🟢 | 🟢 | - | Event processing implemented. Needs runtime to emit events |
| **Watch Expressions** | | | | |
| Variable evaluation (`x`, `arr(i)`, `player.x`) | 🟢 | 🟢 | - | Expression parsing and evaluation complete. Needs runtime state access |
| **Debug Adapter Protocol** | | | | |
| Attach mode | 🟢 | 🟢 | - | Connects to existing process via pipe path or process ID |
| Expression evaluation | 🟢 | 🟢 | - | Parses expressions and requests variable values from debuggee |

**Note:** All debugger features are infrastructure-complete but require runtime integration (debug info emission, breakpoint hooks, memory access protocol) to be functional.

---

## Legacy Features

| Feature | Inline | External Runtime | Intentional? | Notes |
|---------|--------|------------------|--------------|-------|
| **Joystick** | | | | |
| `STICK`, `STRIG` | 🟣 | 🟣 | Yes | Stubs return center position / not pressed. Legacy DOS-era hardware support |
| **Light Pen** | | | | |
| `PEN` | 🟣 | 🟣 | Yes | Stub with runtime warning. Obsolete hardware |
| **Serial I/O** | | | | |
| `ERDEV`, `ERDEV$`, `IOCTL`, `IOCTL$` | 🟣 | 🟣 | Yes | Stubs with runtime warning. Legacy DOS-era features |

---

## Summary by Priority

### High Priority (Core Functionality - Needs Implementation)

| Feature | Status | Intentional? |
|---------|--------|--------------|
| FIELD Statement | ⚠️ Both modes | No |
| LSET/RSET | ⚠️ Both modes | No |
| Console Scrolling | 🟢 Both modes | - |
| `qb_file_get_string` (inline) | ⚠️ Inline only | No |
| `qb_echo` | ⚠️ Both modes | No |

### Medium Priority (Useful Features)

| Feature | Status | Intentional? |
|---------|--------|--------------|
| STEP position tracking (inline) | 🔴 Inline only | No |
| Window functions (`_TITLE`, `_SCREENMOVE`, etc.) (inline) | 🔴 Inline only | No |
| TrueType Fonts | 🟣 Both modes | Partial | Requires `graphics-sdl2-ttf` feature flag |

### Low Priority / Intentionally Missing

| Feature | Status | Intentional? |
|---------|--------|--------------|
| Network Support | 🚫 Both modes | Yes |
| Drag and Drop | 🚫 Both modes | Yes |
| OpenGL Functions (`_GL*`) | 🚫 Both modes | Yes | Excluded per ADR-0014 |
| Legacy Hardware (Joystick, Light Pen, Serial I/O) | 🟣 Both modes | Yes |

### Complete Features

| Feature | Status |
|---------|--------|
| Array Metadata Tracking (external) | 🟢 External |
| Per-Image Palettes | 🟢 Both modes |
| STEP Position Tracking (external) | 🟢 External |
| Core Graphics Commands (external) | 🟢 External |
| Console Scrolling | 🟢 Both modes |
| Debugger Infrastructure | 🟢 Both modes | (Requires runtime integration hooks) |

---

## Notes

**Runtime Modes:**
- **Inline mode** (`--runtime inline`): Many features are intentionally stubbed to allow compilation without external dependencies. Graphics operations are stubs with frame limiting to prevent infinite loops.
- **External runtime** (`--runtime external`): Full functionality requires linking against `libqb64fresh_rt`. Most features are fully implemented in external mode.

**Implementation Status:**
- Array metadata tracking is fully implemented in external runtime (see `src/codegen/c_backend/runtime/arrays.rs`). Inline runtime has stub for `qb_array_register_md` which is acceptable since inline mode doesn't need full array tracking.
- Per-image palettes and STEP position tracking are fully implemented in both mock and SDL2 backends.
- Debugger infrastructure is complete but requires runtime integration hooks (debug info emission, breakpoint support, memory access protocol) to be functional.

**Intentional vs Temporary:**
- Features marked as **Intentional: Yes** are by design (e.g., network support not planned, OpenGL excluded per ADR-0014, legacy hardware stubs).
- Features marked as **Intentional: No** need implementation work (e.g., FIELD statement, LSET/RSET, console scrolling).
- Features marked as **Intentional: Partial** are conditionally available (e.g., TrueType fonts require feature flag).

## Recent Changes

### 2026-01-27
- ✅ **Console Scrolling**: Fully implemented
  - Added `scroll_text_up()` method to SDL2Backend that shifts pixel rows up by one text line
  - Updated `print()` method to call scrolling when cursor exceeds bottom row
  - Handles both newline characters and line wrapping scenarios
  - Clears bottom line with background color after scrolling
- ✅ **Per-Image Palettes**: Fully implemented
  - Added `get_palette_for_image` and `set_palette_for_image` methods to `GraphicsBackend` trait
  - Implemented per-image palette support in both SDL2 and mock backends
  - Updated `qb_palettecolor_get` and `qb_palettecolor` FFI functions to use per-image palettes
  - Handle 0 uses screen palette, other handles use image-specific palettes
- ✅ **STEP Position Tracking**: Verified complete implementation
  - Both `mock.rs` and `sdl2.rs` backends track `last_x` and `last_y`
  - All STEP variants properly resolve relative coordinates
  - Removed outdated TODO from documentation

### 2026-01-26
- ✅ **Array Metadata Tracking**: Fully implemented with hash table registry system
  - `qb_array_register` / `qb_array_register_md` track array bounds
  - `qb_ubound` / `qb_ubound2` and `qb_lbound` / `qb_lbound2` now return correct values
  - `qb_array_update` handles REDIM pointer changes
  - `qb_array_erase` clears metadata
  - Implementation in `src/codegen/c_backend/runtime/arrays.rs`
  - **Note:** Inline runtime has stub for `qb_array_register_md`, but external runtime has full implementation
