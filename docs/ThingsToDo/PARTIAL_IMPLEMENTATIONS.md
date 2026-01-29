# Partial Implementations Audit & Runtime Implementation Plan

This document catalogs all functionality that is only partially implemented across the QB64Fresh codebase and provides a comprehensive overview of the runtime implementation status. Last updated: 2026-01-28 (verified against codebase).

Per-function status and will-not-implement: [STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md).

## Dual-Runtime Architecture

QB64Fresh supports two runtime modes, selected via `--runtime` when emitting C:

| Mode      | Source | Output | Graphics | Audio | Use case |
|-----------|--------|--------|-----------|-------|----------|
| **inline**  | `src/codegen/c_backend/runtime/` | Self-contained `.c` | Stubs + frame limit | Stubs | CI, headless, bootstrap, portable C |
| **external** | `runtime/src/` | `.c` + `libqb64fresh_rt.a` | SDL2 (real) | Rodio (real) | Graphical apps, full QB64 behavior |

- **Inline:** The compiler *emits* C implementations (types, strings, io, file, keyboard, memory, timing, arrays, math, error, graphics stubs, audio stubs, legacy, system, debug). Graphics/audio are stubs; `QB64FRESH_MAX_FRAMES` (default 1000) limits frames to avoid infinite loops.
- **External:** C code includes `qb64fresh_rt.h` and links `libqb64fresh_rt`. The contract is `runtime/include/qb64fresh_rt.h`. Graphics use the `GraphicsBackend` trait (SDL2, Mock); audio uses `AudioBackend` (Rodio, Mock).

See [RUNTIME_ARCHITECTURE_PERSPECTIVES.md](RUNTIME_ARCHITECTURE_PERSPECTIVES.md) for trade-offs, conformance, and when to use each mode.

## Current Implementation Status (2026-01-28)

- **~409 of 419** built-in functions/subs fully implemented (~97.6%); see [STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md).
- **~7** legacy/event stubs (ERDEV, ON COM, ON UEVENT, ON SIGNAL, etc.) and **~3** obsolete (e.g. PEN) are **will-not-implement** (stub-only).
- **Joystick:** Full in external runtime (STICK, STRIG, _DEVICES, _AXIS, _BUTTON, ON STRIG, STRIG ON/OFF/STOP); inline uses stubs (127/0) — hardware requires SDL2.
- **Test Coverage:** 1,500+ tests total (405 unit, 727 integration, 210 runtime, 10 golden, 19 property-based, 27 execution)
- **QB64pe Compatibility:** 99.1% (114/115 test files passing)

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

**vs QB64pe (parity):**
- **✓** = Parity: QB64pe has it and we have it (inline and/or external), or **QB64pe does not implement it and we don't (or we match their non‑implementation)**.
- **~** = Partial: QB64pe has full impl and we have only stub in both modes (or we're close but not equivalent).
- **✗** = No parity: QB64pe has it and we don't (or only a non‑equivalent stub).
- **—** = N/A or unknown QB64pe behavior.

## Architecture Overview

### Inline Runtime (emits C)

```
src/codegen/c_backend/runtime/
├── mod.rs       - Header emission, runtime mode switch, forward decls, debug support
├── types.rs     - qb_string layout, type-size helpers
├── strings.rs   - LEN, LEFT$, RIGHT$, MID$, INSTR, CHR$, ASC, STR$, VAL, temp pool (QB64FRESH_DEBUG_STRING_POOL for overflow warning), compare
├── io.rs        - PRINT, INPUT, LINE INPUT, CLS, LOCATE, COLOR
├── file.rs      - OPEN, CLOSE, PRINT #, INPUT #, LINE INPUT #, WRITE #, GET, PUT, SEEK, LOC, LOF, EOF, FREEFILE, FIELD, LSET, RSET
├── keyboard.rs  - INKEY$, _KEYHIT, _KEYDOWN, _KEYCLEAR
├── memory.rs    - _MEMNEW, _MEMFREE, _MEM*, PEEK, POKE, VARPTR, etc.
├── timing.rs    - TIMER, SLEEP, _DELAY, _LIMIT, DATE$, TIME$, RND, RANDOMIZE
├── arrays.rs    - LBOUND, UBOUND, REDIM
├── math.rs      - ABS, SGN, INT, FIX, CINT, CLNG, SQR, LOG, EXP, SIN, COS, TAN, ATN, _ATAN2, _PI, etc.
├── error.rs     - ON ERROR, RESUME, ERR, ERL, ERROR
├── graphics.rs  - Stubs: SCREEN, CLS, PSET, LINE, CIRCLE, PAINT, COLOR, etc.; frame limiting (QB64FRESH_MAX_FRAMES)
├── audio.rs     - Stubs: BEEP, SOUND, _SND*
├── legacy.rs    - DEF SEG, PEEK/POKE, OUT, INP, PALETTE, GOSUB stack
├── system.rs    - KILL, NAME, CHDIR, MKDIR, RMDIR, _FILEEXISTS, _DIREXISTS, ENVIRON$, COMMAND$, SHELL, SYSTEM, END, _OS$, _CWD$, _STARTDIR$, etc.
└── debug.rs     - qb_dbg_line, qb_dbg_enter_proc/exit_proc; breakpoint/stepping hooks when compiled with --debug. IPC for debugger (tools/debug) infrastructure complete.
```

### External Runtime (Rust → libqb64fresh_rt)

```
runtime/src/
├── lib.rs           - Crate root, qb_runtime_init/shutdown, qb_end, qb_stop, qb_init_args, qb_init_startdir
├── array_registry.rs - LBOUND/UBOUND array bounds; qb_array_register, qb_array_register_md, qb_array_update, qb_array_erase
├── string.rs        - QbString, qb_string_*, qb_chr, qb_asc, qb_left, qb_right, qb_mid, qb_instr, qb_ucase, qb_lcase, qb_ltrim, qb_rtrim, qb_space, qb_string_fill, qb_str_int, qb_str_float, qb_val, qb_tostr, qb_hex, qb_oct, qb_bin, qb_trim, qb_instrrev, qb_instrrev3
├── io.rs            - PRINT, INPUT, LINE INPUT, qb_iif, qb_iif_str, CLS, LOCATE, COLOR, file open/close, PRINT #, INPUT #, WRITE #, GET, PUT, SEEK, EOF, LOF, LOC, FREEFILE, KILL, NAME, CHDIR, MKDIR, RMDIR, _FILEEXISTS, _DIREXISTS, etc.
├── math.rs          - abs, sgn, int, fix, cint, clng, trig, log, exp, sqr, rnd, randomize, d2r, r2d, pi, min/max
├── graphics/        - GraphicsBackend trait; SDL2 and Mock implementations
│   ├── mod.rs
│   ├── sdl2.rs
│   ├── mock.rs
│   ├── font.rs
│   └── error.rs
├── graphics_ffi.rs  - qb_gfx_* C bindings (init, cls, pset, line, circle, paint, view, window, palette, images, GET/PUT, mouse, fonts, etc.)
├── audio/           - AudioBackend trait; Rodio and Mock
│   ├── mod.rs
│   ├── rodio_backend.rs
│   ├── mock.rs
│   └── error.rs
├── audio_ffi.rs     - qb_beep, qb_sound, qb_play, qb_snd*
├── dialogs.rs       - qb_messagebox_ex, qb_openfiledialog, qb_savefiledialog, qb_selectfolderdialog
├── font_ffi.rs      - qb_loadfont, qb_font, qb_freefont, qb_fontheight, qb_fontwidth, qb_printwidth, qb_printstring, etc.
├── font_manager.rs  - FreeType-based font manager (feature-gated)
├── joystick.rs      - qb_stick, qb_strig, qb_devices, qb_axis, qb_button
├── memory.rs        - qb_mem, qb_memnew, qb_memfree, qb_memget, qb_memput, qb_memcopy, qb_memfill, qb_offset, qb_mem_of, qb_memexists, qb_memelement, qb_memimage, qb_memsound
```

**Note:** `qb_glrender` and `qb_glcompat` (no-op stubs for `_GLRENDER`/`_GLCOMPAT`) are in `graphics_ffi.rs` and `qb64fresh_rt.h`; inline runtime stubs in `src/codegen/c_backend/runtime/graphics.rs`.

**C API contract:** [runtime/include/qb64fresh_rt.h](../../runtime/include/qb64fresh_rt.h)

## Categories

1. [Graphics Features](#graphics-features)
2. [System Features](#system-features)
3. [File I/O Features](#file-io-features)
4. [Debugger Infrastructure](#debugger-infrastructure)
5. [Legacy Features](#legacy-features)
6. [Phase-by-Phase Implementation Status](#phase-by-phase-implementation-status)

---

## Graphics Features

| Feature | Inline | External Runtime | Intentional? | Notes |
|---------|--------|------------------|--------------|-------|
| **Core Graphics Commands** | | | | |
| SCREEN, LINE, CIRCLE, PSET, etc. | 🟣 | 🟢 | Yes | Inline mode: stubs with warnings, frame limiting. External: full SDL2 implementation |
| STEP position tracking | 🟣 | 🟢 | Yes | Inline: not tracked (no-op). External: fully implemented in both mock and SDL2 backends (tracks `last_x` and `last_y`). Inline mode has no graphics window, so STEP tracking is not needed |
| **Font Functions** | | | | |
| `_FONT`, `_FREEFONT`, `_LOADFONT` | 🟣 | 🟣 | Partial | Stubs in inline. External: requires `graphics-sdl2-ttf` feature flag (SDL2_ttf dependency) |
| `_MAPUNICODE` | 🟢 | 🟢 | - | Fully functional (Code Page 437 mapping) |
| **Window Functions** | | | | |
| `_TITLE` | 🟣 | 🟢 | Yes | Inline: not implemented (no-op). External: fully functional via `set_title()` in SDL2 backend. UI-only feature - no window in inline mode |
| `_SCREENMOVE` | 🟣 | 🟢 | Yes | Inline: not implemented (no-op). External: fully functional via `screen_move()` in SDL2 backend. UI-only feature - no window in inline mode |
| `_SCREENSHOW` | 🟣 | 🟢 | Yes | Inline: not implemented (no-op). External: fully functional via `screen_show()` in SDL2 backend. UI-only feature - no window in inline mode |
| `_ICON` | 🟣 | 🟢 | - | Inline: not implemented (no-op). External: fully implemented - converts image buffer ARGB pixels to SDL2 Surface RGBA format and sets window icon |
| **Palette Functions** | | | | |
| Per-image palettes | 🟢 | 🟢 | - | Fully implemented in both mock and SDL2 backends. Handle 0 uses screen palette, other handles use image-specific palettes |
| **OpenGL Functions** | | | | |
| `_GLRENDER`, `_GLCOMPAT` | 🚫 | 🚫 | Yes | Excluded per ADR-0014 (using SDL2/winit, not raw OpenGL) |
| **Console Scrolling** | 🟣 | 🟢 | No | Inline: not implemented (no graphics window). External: fully implemented via `scroll_text_up()` method in SDL2Backend that shifts pixel rows and clears bottom line

---

## System Features

| Feature | Inline | External Runtime | Intentional? | Notes |
|---------|--------|------------------|--------------|-------|
| **Network Functions** | | | | |
| `_OPENHOST`, `_OPENCONNECTION`, `_OPENCLIENT` | 🟣 | 🟣 | Partial | Implemented in external runtime but API differs from QB64pe (see [Phase 6: Networking](#phase-6-networking)). Inline: stubs |
| `_CONNECTED` | 🟣 | 🟣 | Partial | Implemented in external runtime. Inline: stub returns 0 |
| `_STATUSCODE` | 🚫 | 🚫 | Yes | Returns 200 (stub) |
| Network I/O (`qb_net_get`, `qb_net_put`, etc.) | 🟣 | 🟣 | Partial | Implemented in external runtime. Inline: stubs return 0/empty |
| **Drag and Drop** | | | | |
| `_TOTALDROPPEDFILES` | 🚫 | 🚫 | Yes | Returns 0 |
| `_DROPPEDFILE$` | 🚫 | 🚫 | Yes | Returns empty string |
| `_FINISHDROP`, `_ACCEPTFILEDROP` | 🚫 | 🚫 | Yes | No-ops |
| **Console Control** | | | | |
| `qb_echo` | 🟢 | 🟢 | Yes | Implemented. Outputs text to console followed by newline |

---

## File I/O Features

| Feature | Inline | External Runtime | Intentional? | Notes |
|---------|--------|------------------|--------------|-------|
| **FIELD Statement** | | | | |
| `qb_field_start()` | 🟢 | 🟢 | - | Fully implemented in `runtime/src/io.rs`. Allocates field buffer based on file record length |
| `qb_field_add()` | 🟢 | 🟢 | - | Fully implemented in `runtime/src/io.rs`. Creates fixed-length strings for field variables |
| **LSET/RSET** | | | | |
| LSET/RSET string operations | 🟢 | 🟢 | - | Fully implemented in `runtime/src/io.rs`. Properly pads/truncates strings to field width with left/right alignment |

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

## Phase-by-Phase Implementation Status

### Phase 1: Core Essentials

#### 1.1 Memory Management

**Inline:** `src/codegen/c_backend/runtime/memory.rs` — full implementation for core ops; typed get/put/fill; `_MEMELEMENT`. `_MEMIMAGE` and `_MEMSOUND` return empty (stub).  
**External:** `_MEM*` in `qb64fresh_rt.h` and `runtime/src/memory.rs` (qb_mem, qb_memnew, qb_memfree, qb_memget, qb_memput, qb_memcopy, qb_memfill, qb_offset, qb_mem_of, qb_memexists, qb_memelement, qb_memimage, qb_memsound). Typed helpers (qb_memget_byte, etc.) exist only in inline; codegen uses the generic names.

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `_MEMNEW(size)` | 🟢 | ✓ | Allocate memory block |
| `_MEMFREE(block)` | 🟢 | ✓ | Free memory block |
| `_MEMEXISTS(block)` | 🟢 | ✓ | Check if block is valid |
| `_MEMCOPY(src, dest, size)` | 🟢 | ✓ | Copy memory |
| `_MEMGET(block, offset, type)` | 🟢 | ✓ | Read typed value |
| `_MEMPUT(block, offset, value)` | 🟢 | ✓ | Write typed value |
| `_MEMFILL(block, offset, size, value)` | 🟢 | ✓ | Fill memory |
| `_MEM(var)` | 🟢 | ✓ | Get memory block for variable (qb_mem_of, etc.) |
| `_MEMELEMENT(block, index)` | 🟢 | ✓ | Element offset |
| `_MEMSOUND(handle)` | ⚠️ | ~ | Stub (returns empty) |
| `_MEMIMAGE(handle)` | ⚠️ | ~ | Stub (returns empty) |

#### 1.2 String System

**External:** `runtime/src/string.rs`  
**Inline:** `src/codegen/c_backend/runtime/strings.rs`

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `LEN(str)` | 🟢 | ✓ | String length |
| `LEFT$(str, n)` | 🟢 | ✓ | Left substring |
| `RIGHT$(str, n)` | 🟢 | ✓ | Right substring |
| `MID$(str, start, len)` | 🟢 | ✓ | Middle substring |
| `INSTR(start, str, search)` | 🟢 | ✓ | Find substring |
| `_INSTRREV(start, str, search)` | 🟢 | ✓ | Find from right (qb_instrrev, qb_instrrev3) |
| `UCASE$(str)` | 🟢 | ✓ | Uppercase |
| `LCASE$(str)` | 🟢 | ✓ | Lowercase |
| `LTRIM$(str)` | 🟢 | ✓ | Trim left |
| `RTRIM$(str)` | 🟢 | ✓ | Trim right |
| `_TRIM$(str)` | 🟢 | ✓ | Trim both (qb_trim; same as TRIM$) |
| `SPACE$(n)` | 🟢 | ✓ | Create spaces |
| `STRING$(n, char)` | 🟢 | ✓ | Create repeated char |
| `CHR$(n)` | 🟢 | ✓ | ASCII to char |
| `ASC(str, pos)` | 🟢 | ✓ | Char to ASCII |
| `STR$(num)` | 🟢 | ✓ | Number to string |
| `VAL(str)` | 🟢 | ✓ | String to number |
| `HEX$(num)` | 🟢 | ✓ | Number to hex (qb_hex) |
| `OCT$(num)` | 🟢 | ✓ | Number to octal (qb_oct) |
| `_BIN$(num)` | 🟢 | ✓ | Number to binary (qb_bin) |

#### 1.3 Math Functions

**External:** `runtime/src/math.rs`  
**Inline:** `src/codegen/c_backend/runtime/math.rs`

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `ABS(x)` | 🟢 | ✓ | Absolute value |
| `SGN(x)` | 🟢 | ✓ | Sign |
| `INT(x)` | 🟢 | ✓ | Floor |
| `FIX(x)` | 🟢 | ✓ | Truncate toward zero |
| `CINT(x)` | 🟢 | ✓ | Convert to integer |
| `CLNG(x)` | 🟢 | ✓ | Convert to long |
| `CSNG(x)` | 🟢 | ✓ | Convert to single |
| `CDBL(x)` | 🟢 | ✓ | Convert to double |
| `SQR(x)` | 🟢 | ✓ | Square root |
| `LOG(x)` | 🟢 | ✓ | Natural log |
| `EXP(x)` | 🟢 | ✓ | Exponential |
| `SIN(x)` | 🟢 | ✓ | Sine |
| `COS(x)` | 🟢 | ✓ | Cosine |
| `TAN(x)` | 🟢 | ✓ | Tangent |
| `ATN(x)` | 🟢 | ✓ | Arctangent |
| `_ASIN(x)` | 🟢 | ✓ | Arcsine |
| `_ACOS(x)` | 🟢 | ✓ | Arccosine |
| `_ATAN2(y, x)` | 🟢 | ✓ | Two-argument arctangent |
| `_SINH(x)` | 🟢 | ✓ | Hyperbolic sine |
| `_COSH(x)` | 🟢 | ✓ | Hyperbolic cosine |
| `_TANH(x)` | 🟢 | ✓ | Hyperbolic tangent |
| `_PI(mult)` | 🟢 | ✓ | Pi constant |
| `RND(n)` | 🟢 | ✓ | Random number |
| `RANDOMIZE(seed)` | 🟢 | ✓ | Seed RNG |
| `_D2R(x)` | 🟢 | ✓ | Degrees to radians |
| `_R2D(x)` | 🟢 | ✓ | Radians to degrees |

#### 1.4 File I/O

**External:** `runtime/src/io.rs` (file operations; KILL, NAME, CHDIR, MKDIR, RMDIR, _FILEEXISTS, _DIREXISTS).  
**Inline:** `src/codegen/c_backend/runtime/file.rs` (OPEN, PRINT #, WRITE #, GET, PUT, etc.); `src/codegen/c_backend/runtime/system.rs` (KILL, NAME, CHDIR, MKDIR, RMDIR, _FILEEXISTS, _DIREXISTS).

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `OPEN file FOR mode AS #n` | 🟢 | ✓ | Open file |
| `CLOSE #n` | 🟢 | ✓ | Close file |
| `PRINT #n, data` | 🟢 | ✓ | Write to file |
| `INPUT #n, vars` | 🟢 | ✓ | Read from file |
| `LINE INPUT #n, str$` | 🟢 | ✓ | Read line |
| `WRITE #n, data` | 🟢 | ✓ | Write CSV format (qb_file_write_*; file.rs, file_io.rs) |
| `GET #n, pos, var` | 🟢 | ✓ | Binary read |
| `PUT #n, pos, var` | 🟢 | ✓ | Binary write |
| `SEEK #n, pos` | 🟢 | ✓ | Set position |
| `SEEK(n)` | 🟢 | ✓ | Get position |
| `LOC(n)` | 🟢 | ✓ | Current record |
| `LOF(n)` | 🟢 | ✓ | File length |
| `EOF(n)` | 🟢 | ✓ | End of file |
| `FREEFILE` | 🟢 | ✓ | Next free file number |
| `KILL file` | 🟢 | ✓ | Delete file |
| `NAME old AS new` | 🟢 | ✓ | Rename file |
| `CHDIR path` | 🟢 | ✓ | Change directory (inline: system.rs; external: io.rs) |
| `MKDIR path` | 🟢 | ✓ | Create directory |
| `RMDIR path` | 🟢 | ✓ | Remove directory |
| `_FILEEXISTS(file)` | 🟢 | ✓ | Check file exists |
| `_DIREXISTS(path)` | 🟢 | ✓ | Check directory exists (inline: system; external: io) |

**Note (external):** `qb_chdir`, `qb_mkdir`, `qb_rmdir`, and `qb_dir_exists` are declared in `runtime/include/qb64fresh_rt.h` and implemented in `runtime/src/io.rs`.

### Phase 2: Graphics System

**External:** `runtime/src/graphics/` (GraphicsBackend trait, SDL2Backend, MockBackend), `runtime/src/graphics_ffi.rs`  
**Inline:** `src/codegen/c_backend/runtime/graphics.rs` (stubs; `QB64FRESH_MAX_FRAMES` frame limiting, default 1000 frames)

See [../GRAPHICS.md](../GRAPHICS.md) for architecture, backends (SDL2, Mock), and stub behavior.

#### 2.1 Screen Management

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `SCREEN mode` | ⚠️ | ✓ | Set screen mode |
| `_NEWIMAGE(w, h, mode)` | ⚠️ | ✓ | Create image buffer |
| `_FREEIMAGE handle` | ⚠️ | ✓ | Free image |
| `_DEST handle` | ⚠️ | ✓ | Set draw destination |
| `_SOURCE handle` | ⚠️ | ✓ | Set read source |
| `_DISPLAY` | ⚠️ | ✓ | Refresh screen |
| `_SCREENHIDE` | ⚠️ | ✓ | Hide window |
| `_SCREENSHOW` | ⚠️ | ✓ | Show window |
| `_SCREENMOVE x, y` | ⚠️ | ✓ | Move window |
| `_FULLSCREEN mode` | ⚠️ | ✓ | Fullscreen toggle |
| `_TITLE text$` | ⚠️ | ✓ | Set window title |
| `_WIDTH` | ⚠️ | ✓ | Screen width |
| `_HEIGHT` | ⚠️ | ✓ | Screen height |
| `_DESKTOPWIDTH` | ⚠️ | ✓ | Desktop width |
| `_DESKTOPHEIGHT` | ⚠️ | ✓ | Desktop height |

#### 2.2 Drawing Primitives

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `CLS` | ⚠️ | ✓ | Clear screen |
| `PSET (x, y), color` | ⚠️ | ✓ | Set pixel |
| `PRESET (x, y)` | ⚠️ | ✓ | Set pixel to background |
| `POINT(x, y)` | ⚠️ | ✓ | Get pixel color |
| `LINE (x1,y1)-(x2,y2), color, style` | ⚠️ | ✓ | Draw line |
| `CIRCLE (x, y), r, color, start, end, aspect` | ⚠️ | ✓ | Draw circle/ellipse/arc |
| `PAINT (x, y), fill, border` | ⚠️ | ✓ | Flood fill |
| `DRAW commands$` | ⚠️ | ✓ | Draw language |
| `VIEW (x1,y1)-(x2,y2)` | ⚠️ | ✓ | Set viewport |
| `WINDOW (x1,y1)-(x2,y2)` | ⚠️ | ✓ | World coordinates |

#### 2.3 Color and Image Operations

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `COLOR fg, bg` | ⚠️ | ✓ | Set colors |
| `_RGB`, `_RGBA`, `_RGB32`, `_RGBA32` | ⚠️ | ✓ | Create colors |
| `_RED`, `_GREEN`, `_BLUE`, `_ALPHA` (and 32-bit) | ⚠️ | ✓ | Extract components |
| `_PUTIMAGE` | ⚠️ | ✓ | Copy/scale image |
| `GET (x1,y1)-(x2,y2), array` | ⚠️ | ✓ | Capture to array |
| `PUT (x, y), array, action` | ⚠️ | ✓ | Draw from array |

#### 2.4 Text and Fonts

**External:** `runtime/src/font_ffi.rs`, `runtime/src/font_manager.rs` (optional FreeType via `graphics-sdl2-freetype` feature)

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `PRINT expressions` | 🟢 | ✓ | Print text |
| `LOCATE row, col` | 🟢 | ✓ | Set cursor position |
| `CSRLIN`, `POS(0)` | 🟢 | ✓ | Cursor row/column |
| `_PRINTSTRING (x, y), text$` | ⚠️ | ✓ | Print at pixel position |
| `_LOADFONT`, `_FONT`, `_FREEFONT` | ⚠️ | ✓ | Font load/set/free |
| `_FONTHEIGHT`, `_FONTWIDTH`, `_PRINTWIDTH` | ⚠️ | ✓ | Font metrics |

### Phase 3: Input System

#### 3.1 Keyboard

**External:** `runtime/src/io.rs` (keyboard functions: qb_inkey, qb_keyhit, qb_keydown, qb_keyclear)  
**Inline:** `src/codegen/c_backend/runtime/keyboard.rs`

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `INKEY$` | 🟢 | ✓ | Get key (non-blocking) |
| `INPUT prompt; vars` | 🟢 | ✓ | Prompted input |
| `LINE INPUT prompt; str$` | 🟢 | ✓ | Line input |
| `_KEYHIT` | 🟢 | ✓ | Get key code (non-blocking) |
| `_KEYDOWN(code)` | 🟢 | ✓ | Check if key pressed |
| `_KEYCLEAR` | 🟢 | ✓ | Clear keyboard buffer |

#### 3.2 Mouse

**External:** `runtime/src/graphics_ffi.rs` (qb_mouse_* functions); **full** in external runtime via SDL2.  
**Inline:** stubs in `src/codegen/c_backend/runtime/graphics.rs` (return 0/defaults).

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `_MOUSEINPUT` | 🟢 ext / ⚠️ inline | ✓ | Poll mouse events |
| `_MOUSEX`, `_MOUSEY` | 🟢 ext / ⚠️ inline | ✓ | Mouse position |
| `_MOUSEBUTTON(n)` | 🟢 ext / ⚠️ inline | ✓ | Button state |
| `_MOUSEWHEEL` | 🟢 ext / ⚠️ inline | ✓ | Wheel delta |
| `_MOUSESHOW`, `_MOUSEHIDE` | 🟢 ext / ⚠️ inline | ✓ | Cursor visibility |
| `_MOUSEMOVE x, y` | 🟢 ext / ⚠️ inline | ✓ | Move cursor |

#### 3.3 Game Controller

**External:** `runtime/src/joystick.rs` (full).  
**Inline:** Stubs (STICK/STRIG return 127/0).

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `STICK(n)` | 🟢 ext / ⚠️ inline | ✓ | Joystick axis |
| `STRIG(n)` | 🟢 ext / ⚠️ inline | ✓ | Joystick button |
| `_DEVICES`, `_DEVICE$(n)` | 🟢 ext / ⚠️ inline | ✓ | Device count/name (qb_devices; _DEVICE$ in header/io) |
| `_AXIS(n)`, `_BUTTON(n)` | 🟢 ext / ⚠️ inline | ✓ | Axis/button (qb_axis, qb_button in `runtime/include/qb64fresh_rt.h`) |

### Phase 4: Audio System

**External:** `runtime/src/audio/` (AudioBackend trait, RodioBackend, MockBackend), `runtime/src/audio_ffi.rs` (Rodio) — **full**.  
**Inline:** `src/codegen/c_backend/runtime/audio.rs` (stubs; no-op implementations).

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `BEEP` | 🟢 ext / ⚠️ inline | ✓ | System beep |
| `SOUND freq, duration` | 🟢 ext / ⚠️ inline | ✓ | Generate tone |
| `PLAY command$` | 🟢 ext / ⚠️ inline | ✓ | Play music string |
| `_SNDOPEN`, `_SNDCLOSE` | 🟢 ext / ⚠️ inline | ✓ | Open/close sound file |
| `_SNDPLAY`, `_SNDPAUSE`, `_SNDSTOP` | 🟢 ext / ⚠️ inline | ✓ | Playback control |
| `_SNDLOOP`, `_SNDVOL`, `_SNDBAL` | 🟢 ext / ⚠️ inline | ✓ | Loop, volume, balance |
| `_SNDPLAYING`, `_SNDLEN`, `_SNDGETPOS`, `_SNDSETPOS` | 🟢 ext / ⚠️ inline | ✓ | Queries |

### Phase 5: System Integration

**Inline:** `src/codegen/c_backend/runtime/system.rs`, `src/codegen/c_backend/runtime/timing.rs`, `src/codegen/c_backend/runtime/error.rs`  
**External:** `runtime/src/io.rs` (env, command, shell), `runtime/src/dialogs.rs` (native dialogs via rfd crate)

#### 5.1 Timing

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `TIMER` | 🟢 | ✓ | Seconds since midnight |
| `_DELAY seconds` | 🟢 | ✓ | Delay execution |
| `SLEEP seconds` | 🟢 | ✓ | Sleep |
| `_LIMIT fps` | 🟢 | ✓ | Frame rate limit |
| `DATE$` | 🟢 | ✓ | Current date |
| `TIME$` | 🟢 | ✓ | Current time |

#### 5.2 Environment and Shell

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `ENVIRON$(name)` | 🟢 | ✓ | Get env variable |
| `ENVIRON "name=value"` | 🟢 | ✓ | Set env variable |
| `COMMAND$` | 🟢 | ✓ | Command line |
| `_OS$` | 🟢 | ✓ | Operating system |
| `_SHELL(cmd$)` | 🟢 | ✓ | Execute command |
| `SHELL cmd$` | 🟢 | ✓ | Execute (no return) |
| `SYSTEM`, `END` | 🟢 | ✓ | Exit program |

#### 5.3 Dialogs and Clipboard

**External:** `runtime/src/dialogs.rs` (rfd crate for native dialogs); clipboard in `runtime/src/graphics_ffi.rs` (qb_clipboard_get/set). **Full** in external.

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `_MESSAGEBOX` | 🟢 ext / ⚠️ inline | ✓ | Message box (qb_messagebox_ex in header) |
| `_OPENFILEDIALOG$`, `_SAVEFILEDIALOG$` | 🟢 ext / ⚠️ inline | ✓ | File dialogs |
| `_SELECTFOLDERDIALOG$` | 🟢 ext / ⚠️ inline | ✓ | Folder dialog |
| `_CLIPBOARD$` | 🟢 ext / ⚠️ inline | ✓ | Get/set clipboard (qb_clipboard_get/set) |

#### 5.4 Error Handling

**Inline:** `src/codegen/c_backend/runtime/error.rs`

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `ON ERROR GOTO label` | 🟢 | ✓ | Set error handler |
| `ON ERROR GOTO _NEWHANDLER label` | 🟢 | ✓ | QB64 scoped error handler (parser + codegen) |
| `RESUME`, `RESUME NEXT`, `RESUME label` | 🟢 | ✓ | Resume after error |
| `ERR`, `ERL` | 🟢 | ✓ | Error code/line |
| `_ERRORMESSAGE$` | 🟢 | ✓ | Error message (qb_errormessage; error.rs) |
| `ERROR n` | 🟢 | ✓ | Generate error |

### Phase 6: Networking

**External:** `runtime/src/io.rs` implements `qb_net_*`; `runtime/include/qb64fresh_rt.h` declares `qb_net_openhost`, `qb_net_openconnection`, `qb_net_openclient`, `qb_net_connected`, `qb_net_close`, `qb_net_get`, `qb_net_put`, `qb_net_get_string`, `qb_net_put_string`, `qb_net_eof`, `qb_net_lof`.  
**Inline:** `src/codegen/c_backend/runtime/system.rs` emits stubs.

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `_OPENHOST(port)` | 🟢 ext impl / ⚠️ inline / 🟢 in header | ~ | Open TCP server (`qb_net_openhost`) |
| `_OPENCONNECTION(host)` | 🟢 ext impl / ⚠️ inline / 🟢 in header | ~ | Accept connection |
| `_OPENCLIENT("TCP/IP:port:addr")` | 🟢 ext impl / ⚠️ inline / 🟢 in header | ~ | Connect to server |
| `_CONNECTED(handle)` | 🟢 ext impl / ⚠️ inline / 🟢 in header | ~ | Check connected |
| `_CLOSEHOST` (CLOSE #) | 🟢 ext impl / 🟢 in header | ~ | Close handle (`qb_net_close`) |

**Note on networking parity:** Our implementation uses numeric ports for `_OPENHOST` (e.g., `_OPENHOST(8080)`), while QB64pe uses a string format (e.g., `_OPENHOST("TCP/IP:8080")`). The semantic analyzer correctly expects a string, but the runtime implementation currently takes a numeric port. This is a known API difference that should be addressed for full parity. The external runtime implementation is functional but uses a different API signature than QB64pe.

---

## Recommended Crates (External Runtime)

| Category | Crate | Purpose |
|----------|-------|---------|
| Graphics | `sdl2` | Window, rendering, input |
| Audio | `rodio` | Sound playback (replaces miniaudio in original plan) |
| Images | `image` | Image loading/saving (_LOADIMAGE) |
| Fonts | `freetype-rs` (optional) | FreeType-based _LOADFONT, Unicode (via `graphics-sdl2-freetype` feature) |
| Dialogs | `rfd` | Native file/folder/message dialogs |
| Time | `std::time`, `chrono` (if needed) | TIMER, DATE$, TIME$ |

## Implementation Order (Suggested)

1. **Core I/O (done for bootstrap):** Strings, console I/O, file I/O, keyboard, math. 🟢
2. **Graphics foundation (done in external):** SDL2 window, screen modes, CLS, COLOR, PSET, LINE, CIRCLE, VIEW, WINDOW, GET/PUT, _PUTIMAGE, alpha blending, PCOPY, screen pages. 🟢
3. **Graphics extended (done):** Images, fonts, _LOADFONT, _PRINTSTRING, Unicode; _MAPTRIANGLE, _COPYPALETTE, _DISPLAYORDER. 🟢
4. **Audio (done in external):** BEEP, SOUND, _SNDOPEN/_SNDPLAY family, PLAY, rodio backend. 🟢
5. **Input (done in external):** Mouse, game controller/joystick (STICK, STRIG, _DEVICES, _AXIS, _BUTTON, ON STRIG). 🟢
6. **System (done in external):** Dialogs, clipboard. **Networking:** `qb_net_*` implemented in `runtime/src/io.rs` and declared in `runtime/include/qb64fresh_rt.h`. **Directory ops:** `qb_chdir`, `qb_mkdir`, `qb_rmdir`, `qb_dir_exists` in header and `io.rs`. 🟢

## Success Criteria

1. **Bootstrap:** QB64pe compiles itself with QB64Fresh and runs to completion (resolve startup/IDE init hang; see Perspectives doc).
2. **Test suite:** qbasic_testcases (and equivalent) pass on both inline and external where applicable.
3. **Conformance:** Non-graphics behavior matches between inline and external (see Perspectives doc).
4. **Feature parity:** All QB64pe functions either implemented or explicitly documented as stub/unsupported (see function catalog in Related Documentation).
5. **Performance:** Comparable to QB64pe for typical programs.

## Related Documentation

- **Perspectives doc** — [RUNTIME_ARCHITECTURE_PERSPECTIVES.md](RUNTIME_ARCHITECTURE_PERSPECTIVES.md): dual-runtime trade-offs, when to use each, action items
- **Graphics** — [../GRAPHICS.md](../GRAPHICS.md): inline stubs, `QB64FRESH_MAX_FRAMES`, SDL2/Mock backends
- **C API** — [runtime/include/qb64fresh_rt.h](../../runtime/include/qb64fresh_rt.h): external runtime FFI declarations
- **Function catalog** — [STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md): per-function status, will-not-implement

---

## Legacy Features

| Feature | Inline | External Runtime | Intentional? | Notes |
|---------|--------|------------------|--------------|-------|
| **Light Pen** | | | | |
| `PEN` | 🟣 | 🟣 | Yes | Stub with runtime warning. Obsolete hardware |
| **Serial I/O** | | | | |
| `ERDEV`, `ERDEV$`, `IOCTL`, `IOCTL$` | 🟣 | 🟣 | Yes | Stubs with runtime warning. Legacy DOS-era features |

**Note:** Joystick support (STICK, STRIG, _DEVICES, _AXIS, _BUTTON) is fully implemented in external runtime via SDL2. See [Phase 3.3: Game Controller](#33-game-controller) for details.

---

## Notes

**Runtime Modes:**
- **Inline mode** (`--runtime inline`): Many features are intentionally stubbed to allow compilation without external dependencies. Graphics operations are stubs with frame limiting to prevent infinite loops.
- **External runtime** (`--runtime external`): Full functionality requires linking against `libqb64fresh_rt`. Most features are fully implemented in external mode.

**Implementation Status:**
- Array metadata tracking: inline runtime implements it in `src/codegen/c_backend/runtime/arrays.rs` (embedded C); external runtime implements it in `runtime/src/array_registry.rs`. Both provide `qb_array_register`, `qb_array_register_md`, `qb_array_update`, `qb_array_erase`.
- Debugger infrastructure is complete but requires runtime integration hooks (debug info emission, breakpoint support, memory access protocol) to be functional.

**Intentional vs Temporary:**
- Features marked as **Intentional: Yes** are by design (e.g., OpenGL excluded per ADR-0014, legacy hardware stubs).
- Features marked as **Intentional: No** need implementation work (e.g., console scrolling in inline mode).
- Features marked as **Intentional: Partial** are conditionally available (e.g., TrueType fonts require feature flag, networking has API differences from QB64pe).

## Recent Changes

### 2026-01-28
- 📝 **Documentation Update**: Verified against current codebase. External runtime tree now includes `array_registry.rs`. Array metadata note corrected (inline: `src/codegen/c_backend/runtime/arrays.rs`; external: `runtime/src/array_registry.rs`).

### 2026-01-27
- 🟢 **FIELD Statement**: Fully implemented (see [File I/O Features](#file-io-features))
- 🟢 **LSET/RSET**: Fully implemented (see [File I/O Features](#file-io-features))
- 🟢 **Graphics Features**: Console Scrolling, Per-Image Palettes, STEP Position Tracking, and Window Functions verified/implemented (see [Graphics Features](#graphics-features))

### 2026-01-26
- 🟢 **Array Metadata Tracking**: Hash table registry; `qb_array_register` / `qb_array_register_md`, `qb_array_update`, `qb_array_erase`; LBOUND/UBOUND correct. Paths in Notes above.
