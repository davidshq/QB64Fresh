# QB64Fresh Runtime Implementation Plan

This document outlines the runtime implementation for QB64Fresh. It describes the **dual-runtime architecture**, the **actual layout** of both runtimes, and a **phase roadmap** toward QB64pe feature parity.

*Last updated: 2026-01-25*

For per-function status and will-not-implement: [STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md). Phase tables include a **vs QB64pe** column for feature parity; parity counts as ✓ when QB64pe implements and we do (any runtime), or when **neither** implements it (e.g. will-not-implement / stub-only on both).

---

## Dual-Runtime Architecture

QB64Fresh supports two runtime modes, selected via `--runtime` when emitting C:

| Mode      | Source | Output | Graphics | Audio | Use case |
|-----------|--------|--------|-----------|-------|----------|
| **inline**  | `src/codegen/c_backend/runtime/` | Self-contained `.c` | Stubs + frame limit | Stubs | CI, headless, bootstrap, portable C |
| **external** | `runtime/src/` | `.c` + `libqb64fresh_rt.a` | SDL2 (real) | Rodio (real) | Graphical apps, full QB64 behavior |

- **Inline:** The compiler *emits* C implementations (types, strings, io, file, keyboard, memory, timing, arrays, math, error, graphics stubs, audio stubs, legacy, system, debug). Graphics/audio are stubs; `QB64FRESH_MAX_FRAMES` (default 1000) limits frames to avoid infinite loops.
- **External:** C code includes `qb64fresh_rt.h` and links `libqb64fresh_rt`. The contract is `runtime/include/qb64fresh_rt.h`. Graphics use the `GraphicsBackend` trait (SDL2, Mock); audio uses `AudioBackend` (Rodio, Mock).

See [RUNTIME_ARCHITECTURE_PERSPECTIVES.md](RUNTIME_ARCHITECTURE_PERSPECTIVES.md) for trade-offs, conformance, and when to use each mode.

---

## Current Implementation Status (2026-01)

- **~409 of 419** built-in functions/subs fully implemented (~97.6%); see [STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md).
- **~7** legacy/event stubs (ERDEV, ON COM, ON UEVENT, ON SIGNAL, etc.) and **~3** obsolete (e.g. PEN) are **will-not-implement** (stub-only).
- **Joystick:** Full in external runtime (STICK, STRIG, _DEVICES, _AXIS, _BUTTON, ON STRIG, STRIG ON/OFF/STOP); inline uses stubs (127/0) — hardware requires SDL2.

---

## Architecture Overview

### Inline Runtime (emits C)

```
src/codegen/c_backend/runtime/
├── mod.rs       - Header emission, runtime mode switch, forward decls
├── types.rs     - qb_string layout, type-size helpers
├── strings.rs   - LEN, LEFT$, RIGHT$, MID$, INSTR, CHR$, ASC, STR$, VAL, temp pool, compare
├── io.rs        - PRINT, INPUT, LINE INPUT, CLS, LOCATE, COLOR
├── file.rs      - OPEN, CLOSE, PRINT #, INPUT #, LINE INPUT #, WRITE #, GET, PUT, SEEK, LOC, LOF, EOF, FREEFILE, FIELD, LSET, RSET
├── keyboard.rs  - INKEY$, _KEYHIT, _KEYDOWN, _KEYCLEAR
├── memory.rs    - _MEMNEW, _MEMFREE, _MEM*, PEEK, POKE, VARPTR, etc.
├── timing.rs    - TIMER, SLEEP, _DELAY, _LIMIT, DATE$, TIME$, RND, RANDOMIZE
├── arrays.rs    - LBOUND, UBOUND, REDIM
├── math.rs      - ABS, SGN, INT, FIX, CINT, CLNG, SQR, LOG, EXP, SIN, COS, TAN, ATN, _ATAN2, _PI, etc.
├── error.rs     - ON ERROR, RESUME, ERR, ERL, ERROR
├── graphics.rs  - Stubs: SCREEN, CLS, PSET, LINE, CIRCLE, PAINT, COLOR, etc.; frame limiting
├── audio.rs     - Stubs: BEEP, SOUND, _SND*
├── legacy.rs    - DEF SEG, PEEK/POKE, OUT, INP, PALETTE, GOSUB stack
├── system.rs    - KILL, NAME, CHDIR, MKDIR, RMDIR, _FILEEXISTS, _DIREXISTS, ENVIRON$, COMMAND$, SHELL, SYSTEM, END, _OS$, _CWD$, _STARTDIR$, etc.
└── debug.rs     - qb_dbg_line, qb_dbg_enter_proc/exit_proc; breakpoint/stepping hooks when compiled with --debug. IPC for debugger (tools/debug) in progress.
```

### External Runtime (Rust → libqb64fresh_rt)

```
runtime/src/
├── lib.rs         - Crate root, qb_runtime_init/shutdown, qb_end, qb_stop, qb_init_args, qb_init_startdir
├── string.rs      - QbString, qb_string_*, qb_chr, qb_asc, qb_left, qb_right, qb_mid, qb_instr, qb_ucase, qb_lcase, qb_ltrim, qb_rtrim, qb_space, qb_string_fill, qb_str_int, qb_str_float, qb_val, qb_tostr, qb_hex, qb_oct, qb_bin, qb_trim, qb_instrrev, qb_instrrev3
├── io.rs          - PRINT, INPUT, LINE INPUT, qb_iif, qb_iif_str, CLS, LOCATE, COLOR, file open/close, PRINT #, INPUT #, WRITE #, GET, PUT, SEEK, EOF, LOF, LOC, FREEFILE, KILL, NAME, CHDIR, MKDIR, RMDIR, _FILEEXISTS, _DIREXISTS, etc.
├── math.rs        - abs, sgn, int, fix, cint, clng, trig, log, exp, sqr, rnd, randomize, d2r, r2d, pi, min/max
├── graphics/       - GraphicsBackend trait; SDL2 and Mock implementations
│   ├── mod.rs
│   ├── sdl2.rs
│   ├── mock.rs
│   ├── font.rs
│   └── error.rs
├── graphics_ffi.rs - qb_gfx_* C bindings (init, cls, pset, line, circle, paint, view, window, palette, images, GET/PUT, mouse, fonts, etc.)
├── audio/          - AudioBackend trait; Rodio and Mock
│   ├── mod.rs
│   ├── rodio_backend.rs
│   ├── mock.rs
│   └── error.rs
├── audio_ffi.rs    - qb_beep, qb_sound, qb_play, qb_snd*
├── dialogs.rs      - qb_messagebox_ex, qb_openfiledialog, qb_savefiledialog, qb_selectfolderdialog
├── font_ffi.rs     - qb_loadfont, qb_font, qb_freefont, qb_fontheight, qb_fontwidth, qb_printwidth, qb_printstring, etc.
├── font_manager.rs - FreeType-based font manager (feature-gated)
├── joystick.rs     - qb_stick, qb_strig, qb_devices, qb_axis, qb_button
├── memory.rs       - qb_mem, qb_memnew, qb_memfree, qb_memget, qb_memput, qb_memcopy, qb_memfill, qb_offset, qb_mem_of, qb_memexists, qb_memelement, qb_memimage, qb_memsound
```

**Note:** `qb_glrender` and `qb_glcompat` (no-op stubs for `_GLRENDER`/`_GLCOMPAT`) are in `graphics_ffi.rs` and `qb64fresh_rt.h`; inline runtime stubs in `src/codegen/c_backend/runtime/graphics.rs`.

**C API contract:** [runtime/include/qb64fresh_rt.h](../../runtime/include/qb64fresh_rt.h)

---

## Implementation Status Legend

**Status:** [x] = done; [~] = stub in inline and/or partial; [ ] = not implemented. External runtime is often [x] where inline is [~]; see [STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md) for the full catalog.

**vs QB64pe (parity):**

| Symbol | Meaning |
|--------|---------|
| **✓** | Parity: QB64pe has it and we have it (inline and/or external), or **QB64pe does not implement it and we don’t (or we match their non‑implementation)**. |
| **~** | Partial: QB64pe has full impl and we have only stub in both modes (or we’re close but not equivalent). |
| **✗** | No parity: QB64pe has it and we don’t (or only a non‑equivalent stub). |
| **—** | N/A or unknown QB64pe behavior. |

---

## Phase 1: Core Essentials

### 1.1 Memory Management

**Inline:** `src/codegen/c_backend/runtime/memory.rs` — full implementation for core ops; typed get/put/fill; `_MEMELEMENT`. `_MEMIMAGE` and `_MEMSOUND` return empty (stub).  
**External:** `_MEM*` in `qb64fresh_rt.h` and `runtime/src/memory.rs` (qb_mem, qb_memnew, qb_memfree, qb_memget, qb_memput, qb_memcopy, qb_memfill, qb_offset, qb_mem_of, qb_memexists, qb_memelement, qb_memimage, qb_memsound). Typed helpers (qb_memget_byte, etc.) exist only in inline; codegen uses the generic names.

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `_MEMNEW(size)` | [x] | ✓ | Allocate memory block |
| `_MEMFREE(block)` | [x] | ✓ | Free memory block |
| `_MEMEXISTS(block)` | [x] | ✓ | Check if block is valid |
| `_MEMCOPY(src, dest, size)` | [x] | ✓ | Copy memory |
| `_MEMGET(block, offset, type)` | [x] | ✓ | Read typed value |
| `_MEMPUT(block, offset, value)` | [x] | ✓ | Write typed value |
| `_MEMFILL(block, offset, size, value)` | [x] | ✓ | Fill memory |
| `_MEM(var)` | [x] | ✓ | Get memory block for variable (qb_mem_of, etc.) |
| `_MEMELEMENT(block, index)` | [x] | ✓ | Element offset |
| `_MEMSOUND(handle)` | [~] | ~ | Stub (returns empty) |
| `_MEMIMAGE(handle)` | [~] | ~ | Stub (returns empty) |

### 1.2 String System

**External:** `runtime/src/string.rs`  
**Inline:** `src/codegen/c_backend/runtime/strings.rs`

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `LEN(str)` | [x] | ✓ | String length |
| `LEFT$(str, n)` | [x] | ✓ | Left substring |
| `RIGHT$(str, n)` | [x] | ✓ | Right substring |
| `MID$(str, start, len)` | [x] | ✓ | Middle substring |
| `INSTR(start, str, search)` | [x] | ✓ | Find substring |
| `_INSTRREV(start, str, search)` | [x] | ✓ | Find from right (qb_instrrev, qb_instrrev3) |
| `UCASE$(str)` | [x] | ✓ | Uppercase |
| `LCASE$(str)` | [x] | ✓ | Lowercase |
| `LTRIM$(str)` | [x] | ✓ | Trim left |
| `RTRIM$(str)` | [x] | ✓ | Trim right |
| `_TRIM$(str)` | [x] | ✓ | Trim both (qb_trim; same as TRIM$) |
| `SPACE$(n)` | [x] | ✓ | Create spaces |
| `STRING$(n, char)` | [x] | ✓ | Create repeated char |
| `CHR$(n)` | [x] | ✓ | ASCII to char |
| `ASC(str, pos)` | [x] | ✓ | Char to ASCII |
| `STR$(num)` | [x] | ✓ | Number to string |
| `VAL(str)` | [x] | ✓ | String to number |
| `HEX$(num)` | [x] | ✓ | Number to hex (qb_hex) |
| `OCT$(num)` | [x] | ✓ | Number to octal (qb_oct) |
| `_BIN$(num)` | [x] | ✓ | Number to binary (qb_bin) |

### 1.3 Math Functions

**External:** `runtime/src/math.rs`  
**Inline:** `src/codegen/c_backend/runtime/math.rs`

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `ABS(x)` | [x] | ✓ | Absolute value |
| `SGN(x)` | [x] | ✓ | Sign |
| `INT(x)` | [x] | ✓ | Floor |
| `FIX(x)` | [x] | ✓ | Truncate toward zero |
| `CINT(x)` | [x] | ✓ | Convert to integer |
| `CLNG(x)` | [x] | ✓ | Convert to long |
| `CSNG(x)` | [x] | ✓ | Convert to single |
| `CDBL(x)` | [x] | ✓ | Convert to double |
| `SQR(x)` | [x] | ✓ | Square root |
| `LOG(x)` | [x] | ✓ | Natural log |
| `EXP(x)` | [x] | ✓ | Exponential |
| `SIN(x)` | [x] | ✓ | Sine |
| `COS(x)` | [x] | ✓ | Cosine |
| `TAN(x)` | [x] | ✓ | Tangent |
| `ATN(x)` | [x] | ✓ | Arctangent |
| `_ASIN(x)` | [x] | ✓ | Arcsine |
| `_ACOS(x)` | [x] | ✓ | Arccosine |
| `_ATAN2(y, x)` | [x] | ✓ | Two-argument arctangent |
| `_SINH(x)` | [x] | ✓ | Hyperbolic sine |
| `_COSH(x)` | [x] | ✓ | Hyperbolic cosine |
| `_TANH(x)` | [x] | ✓ | Hyperbolic tangent |
| `_PI(mult)` | [x] | ✓ | Pi constant |
| `RND(n)` | [x] | ✓ | Random number |
| `RANDOMIZE(seed)` | [x] | ✓ | Seed RNG |
| `_D2R(x)` | [x] | ✓ | Degrees to radians |
| `_R2D(x)` | [x] | ✓ | Radians to degrees |

### 1.4 File I/O

**External:** `runtime/src/io.rs` (file operations; KILL, NAME, CHDIR, MKDIR, RMDIR, _FILEEXISTS, _DIREXISTS).  
**Inline:** `src/codegen/c_backend/runtime/file.rs` (OPEN, PRINT #, WRITE #, GET, PUT, etc.); `system.rs` (KILL, NAME, CHDIR, MKDIR, RMDIR, _FILEEXISTS, _DIREXISTS).

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `OPEN file FOR mode AS #n` | [x] | ✓ | Open file |
| `CLOSE #n` | [x] | ✓ | Close file |
| `PRINT #n, data` | [x] | ✓ | Write to file |
| `INPUT #n, vars` | [x] | ✓ | Read from file |
| `LINE INPUT #n, str$` | [x] | ✓ | Read line |
| `WRITE #n, data` | [x] | ✓ | Write CSV format (qb_file_write_*; file.rs, file_io.rs) |
| `GET #n, pos, var` | [x] | ✓ | Binary read |
| `PUT #n, pos, var` | [x] | ✓ | Binary write |
| `SEEK #n, pos` | [x] | ✓ | Set position |
| `SEEK(n)` | [x] | ✓ | Get position |
| `LOC(n)` | [x] | ✓ | Current record |
| `LOF(n)` | [x] | ✓ | File length |
| `EOF(n)` | [x] | ✓ | End of file |
| `FREEFILE` | [x] | ✓ | Next free file number |
| `KILL file` | [x] | ✓ | Delete file |
| `NAME old AS new` | [x] | ✓ | Rename file |
| `CHDIR path` | [x] | ✓ | Change directory (inline: system.rs; external: io.rs) |
| `MKDIR path` | [x] | ✓ | Create directory |
| `RMDIR path` | [x] | ✓ | Remove directory |
| `_FILEEXISTS(file)` | [x] | ✓ | Check file exists |
| `_DIREXISTS(path)` | [x] | ✓ | Check directory exists (inline: system; external: io) |

**Note (external):** `qb_chdir`, `qb_mkdir`, `qb_rmdir`, and `qb_dir_exists` are declared in `qb64fresh_rt.h` and implemented in `runtime/src/io.rs`.

---

## Phase 2: Graphics System

**External:** `runtime/src/graphics/`, `runtime/src/graphics_ffi.rs`  
**Inline:** `src/codegen/c_backend/runtime/graphics.rs` (stubs; `QB64FRESH_MAX_FRAMES` frame limiting)

See [../GRAPHICS.md](../GRAPHICS.md) for architecture, backends (SDL2, Mock), and stub behavior.

### 2.1 Screen Management

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `SCREEN mode` | [~] | ✓ | Set screen mode |
| `_NEWIMAGE(w, h, mode)` | [~] | ✓ | Create image buffer |
| `_FREEIMAGE handle` | [~] | ✓ | Free image |
| `_DEST handle` | [~] | ✓ | Set draw destination |
| `_SOURCE handle` | [~] | ✓ | Set read source |
| `_DISPLAY` | [~] | ✓ | Refresh screen |
| `_SCREENHIDE` | [~] | ✓ | Hide window |
| `_SCREENSHOW` | [~] | ✓ | Show window |
| `_SCREENMOVE x, y` | [~] | ✓ | Move window |
| `_FULLSCREEN mode` | [~] | ✓ | Fullscreen toggle |
| `_TITLE text$` | [~] | ✓ | Set window title |
| `_WIDTH` | [~] | ✓ | Screen width |
| `_HEIGHT` | [~] | ✓ | Screen height |
| `_DESKTOPWIDTH` | [~] | ✓ | Desktop width |
| `_DESKTOPHEIGHT` | [~] | ✓ | Desktop height |

### 2.2 Drawing Primitives

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `CLS` | [~] | ✓ | Clear screen |
| `PSET (x, y), color` | [~] | ✓ | Set pixel |
| `PRESET (x, y)` | [~] | ✓ | Set pixel to background |
| `POINT(x, y)` | [~] | ✓ | Get pixel color |
| `LINE (x1,y1)-(x2,y2), color, style` | [~] | ✓ | Draw line |
| `CIRCLE (x, y), r, color, start, end, aspect` | [~] | ✓ | Draw circle/ellipse/arc |
| `PAINT (x, y), fill, border` | [~] | ✓ | Flood fill |
| `DRAW commands$` | [~] | ✓ | Draw language |
| `VIEW (x1,y1)-(x2,y2)` | [~] | ✓ | Set viewport |
| `WINDOW (x1,y1)-(x2,y2)` | [~] | ✓ | World coordinates |

### 2.3 Color and Image Operations

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `COLOR fg, bg` | [~] | ✓ | Set colors |
| `_RGB`, `_RGBA`, `_RGB32`, `_RGBA32` | [~] | ✓ | Create colors |
| `_RED`, `_GREEN`, `_BLUE`, `_ALPHA` (and 32-bit) | [~] | ✓ | Extract components |
| `_PUTIMAGE` | [~] | ✓ | Copy/scale image |
| `GET (x1,y1)-(x2,y2), array` | [~] | ✓ | Capture to array |
| `PUT (x, y), array, action` | [~] | ✓ | Draw from array |

### 2.4 Text and Fonts

**External:** `runtime/src/font_ffi.rs`, `runtime/src/font_manager.rs` (optional FreeType)

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `PRINT expressions` | [x] | ✓ | Print text |
| `LOCATE row, col` | [x] | ✓ | Set cursor position |
| `CSRLIN`, `POS(0)` | [x] | ✓ | Cursor row/column |
| `_PRINTSTRING (x, y), text$` | [~] | ✓ | Print at pixel position |
| `_LOADFONT`, `_FONT`, `_FREEFONT` | [~] | ✓ | Font load/set/free |
| `_FONTHEIGHT`, `_FONTWIDTH`, `_PRINTWIDTH` | [~] | ✓ | Font metrics |

---

## Phase 3: Input System

### 3.1 Keyboard

**External:** `runtime/src/io.rs` (keyboard)  
**Inline:** `src/codegen/c_backend/runtime/keyboard.rs`

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `INKEY$` | [x] | ✓ | Get key (non-blocking) |
| `INPUT prompt; vars` | [x] | ✓ | Prompted input |
| `LINE INPUT prompt; str$` | [x] | ✓ | Line input |
| `_KEYHIT` | [x] | ✓ | Get key code (non-blocking) |
| `_KEYDOWN(code)` | [x] | ✓ | Check if key pressed |
| `_KEYCLEAR` | [x] | ✓ | Clear keyboard buffer |

### 3.2 Mouse

**External:** `runtime/src/graphics_ffi.rs` (qb_mouse_*); **full** in external.  
**Inline:** stubs in `graphics.rs`.

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `_MOUSEINPUT` | [x] ext / [~] inline | ✓ | Poll mouse events |
| `_MOUSEX`, `_MOUSEY` | [x] ext / [~] inline | ✓ | Mouse position |
| `_MOUSEBUTTON(n)` | [x] ext / [~] inline | ✓ | Button state |
| `_MOUSEWHEEL` | [x] ext / [~] inline | ✓ | Wheel delta |
| `_MOUSESHOW`, `_MOUSEHIDE` | [x] ext / [~] inline | ✓ | Cursor visibility |
| `_MOUSEMOVE x, y` | [x] ext / [~] inline | ✓ | Move cursor |

### 3.3 Game Controller

**External:** `runtime/src/joystick.rs` (full).  
**Inline:** Stubs (STICK/STRIG return 127/0).

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `STICK(n)` | [x] ext / [~] inline | ✓ | Joystick axis |
| `STRIG(n)` | [x] ext / [~] inline | ✓ | Joystick button |
| `_DEVICES`, `_DEVICE$(n)` | [x] ext / [~] inline | ✓ | Device count/name (qb_devices; _DEVICE$ in header/io) |
| `_AXIS(n)`, `_BUTTON(n)` | [x] ext / [~] inline | ✓ | Axis/button (qb_axis, qb_button in `runtime/include/qb64fresh_rt.h`) |

---

## Phase 4: Audio System

**External:** `runtime/src/audio/`, `runtime/src/audio_ffi.rs` (Rodio) — **full**.  
**Inline:** `src/codegen/c_backend/runtime/audio.rs` (stubs).

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `BEEP` | [x] ext / [~] inline | ✓ | System beep |
| `SOUND freq, duration` | [x] ext / [~] inline | ✓ | Generate tone |
| `PLAY command$` | [x] ext / [~] inline | ✓ | Play music string |
| `_SNDOPEN`, `_SNDCLOSE` | [x] ext / [~] inline | ✓ | Open/close sound file |
| `_SNDPLAY`, `_SNDPAUSE`, `_SNDSTOP` | [x] ext / [~] inline | ✓ | Playback control |
| `_SNDLOOP`, `_SNDVOL`, `_SNDBAL` | [x] ext / [~] inline | ✓ | Loop, volume, balance |
| `_SNDPLAYING`, `_SNDLEN`, `_SNDGETPOS`, `_SNDSETPOS` | [x] ext / [~] inline | ✓ | Queries |

---

## Phase 5: System Integration

**Inline:** `src/codegen/c_backend/runtime/system.rs`, `timing.rs`, `error.rs`  
**External:** `runtime/src/io.rs` (env, command, shell), `runtime/src/dialogs.rs`

### 5.1 Timing

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `TIMER` | [x] | ✓ | Seconds since midnight |
| `_DELAY seconds` | [x] | ✓ | Delay execution |
| `SLEEP seconds` | [x] | ✓ | Sleep |
| `_LIMIT fps` | [x] | ✓ | Frame rate limit |
| `DATE$` | [x] | ✓ | Current date |
| `TIME$` | [x] | ✓ | Current time |

### 5.2 Environment and Shell

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `ENVIRON$(name)` | [x] | ✓ | Get env variable |
| `ENVIRON "name=value"` | [~] | ~ | Set env variable |
| `COMMAND$` | [x] | ✓ | Command line |
| `_OS$` | [x] | ✓ | Operating system |
| `_SHELL(cmd$)` | [x] | ✓ | Execute command |
| `SHELL cmd$` | [x] | ✓ | Execute (no return) |
| `SYSTEM`, `END` | [x] | ✓ | Exit program |

### 5.3 Dialogs and Clipboard

**External:** `runtime/src/dialogs.rs` (rfd); clipboard in `graphics_ffi.rs`. **Full** in external.

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `_MESSAGEBOX` | [x] ext / [~] inline | ✓ | Message box (qb_messagebox_ex in header) |
| `_OPENFILEDIALOG$`, `_SAVEFILEDIALOG$` | [x] ext / [~] inline | ✓ | File dialogs |
| `_SELECTFOLDERDIALOG$` | [x] ext / [~] inline | ✓ | Folder dialog |
| `_CLIPBOARD$` | [x] ext / [~] inline | ✓ | Get/set clipboard (qb_clipboard_get/set) |

### 5.4 Error Handling

**Inline:** `src/codegen/c_backend/runtime/error.rs`

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `ON ERROR GOTO label` | [x] | ✓ | Set error handler |
| `RESUME`, `RESUME NEXT`, `RESUME label` | [x] | ✓ | Resume after error |
| `ERR`, `ERL` | [x] | ✓ | Error code/line |
| `_ERRORMESSAGE$` | [x] | ✓ | Error message (qb_errormessage; error.rs) |
| `ERROR n` | [x] | ✓ | Generate error |

---

## Phase 6: Networking

**External:** `runtime/src/io.rs` implements `qb_net_*`; `qb64fresh_rt.h` declares `qb_net_openhost`, `qb_net_openconnection`, `qb_net_openclient`, `qb_net_connected`, `qb_net_close`, `qb_net_get`, `qb_net_put`, `qb_net_get_string`, `qb_net_put_string`, `qb_net_eof`, `qb_net_lof`.  
**Inline:** `src/codegen/c_backend/runtime/system.rs` emits stubs.

| Function | Status | vs QB64pe | Description |
|----------|--------|-----------|-------------|
| `_OPENHOST(port)` | [x] ext impl / [~] inline / [x] in header | ~ | Open TCP server (`qb_net_openhost`) |
| `_OPENCONNECTION(host)` | [x] ext impl / [~] inline / [x] in header | ~ | Accept connection |
| `_OPENCLIENT("TCP/IP:port:addr")` | [x] ext impl / [~] inline / [x] in header | ~ | Connect to server |
| `_CONNECTED(handle)` | [x] ext impl / [~] inline / [x] in header | ~ | Check connected |
| `_CLOSEHOST` (CLOSE #) | [x] ext impl / [x] in header | ~ | Close handle (`qb_net_close`) |

---

## Recommended Crates (External Runtime)

| Category | Crate | Purpose |
|----------|-------|---------|
| Graphics | `sdl2` | Window, rendering, input |
| Audio | `rodio` | Sound playback (replaces miniaudio in original plan) |
| Images | `image` | Image loading/saving (_LOADIMAGE) |
| Fonts | `freetype-rs` (optional) | FreeType-based _LOADFONT, Unicode |
| Dialogs | `rfd` | Native file/folder/message dialogs |
| Time | `std::time`, `chrono` (if needed) | TIMER, DATE$, TIME$ |

---

## Implementation Order (Suggested)

1. **Core I/O (done for bootstrap):** Strings, console I/O, file I/O, keyboard, math.
2. **Graphics foundation (done in external):** SDL2 window, screen modes, CLS, COLOR, PSET, LINE, CIRCLE, VIEW, WINDOW, GET/PUT, _PUTIMAGE, alpha blending, PCOPY, screen pages.
3. **Graphics extended (done):** Images, fonts, _LOADFONT, _PRINTSTRING, Unicode; _MAPTRIANGLE, _COPYPALETTE, _DISPLAYORDER.
4. **Audio (done in external):** BEEP, SOUND, _SNDOPEN/_SNDPLAY family, PLAY, rodio backend.
5. **Input (done in external):** Mouse, game controller/joystick (STICK, STRIG, _DEVICES, _AXIS, _BUTTON, ON STRIG).
6. **System (done in external):** Dialogs, clipboard. **Networking:** `qb_net_*` implemented in `runtime/src/io.rs` and declared in `qb64fresh_rt.h`. **Directory ops:** `qb_chdir`, `qb_mkdir`, `qb_rmdir`, `qb_dir_exists` in header and `io.rs`.

---

## Success Criteria

1. **Bootstrap:** QB64pe compiles itself with QB64Fresh and runs to completion (resolve startup/IDE init hang; see [RUNTIME_ARCHITECTURE_PERSPECTIVES.md](RUNTIME_ARCHITECTURE_PERSPECTIVES.md)).
2. **Test suite:** qbasic_testcases (and equivalent) pass on both inline and external where applicable.
3. **Conformance:** Non-graphics behavior matches between inline and external; see Perspectives doc.
4. **Feature parity:** All QB64pe functions either implemented or explicitly documented as stub/unsupported; see [STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md).
5. **Performance:** Comparable to QB64pe for typical programs.

---

## Related Documentation

- [RUNTIME_ARCHITECTURE_PERSPECTIVES.md](RUNTIME_ARCHITECTURE_PERSPECTIVES.md) — dual-runtime trade-offs, when to use each, action items
- [../GRAPHICS.md](../GRAPHICS.md) — graphics architecture, inline stubs, `QB64FRESH_MAX_FRAMES`, SDL2/Mock backends
- [runtime/include/qb64fresh_rt.h](../../runtime/include/qb64fresh_rt.h) — C API contract for external runtime
- [STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md) — full function catalog, implemented and will-not-implement
