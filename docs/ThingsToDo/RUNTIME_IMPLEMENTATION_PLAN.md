# QB64Fresh Runtime Implementation Plan

This document outlines the runtime implementation for QB64Fresh. It describes the **dual-runtime architecture**, the **actual layout** of both runtimes, and a **phase roadmap** toward QB64pe feature parity.

*Last updated: 2026-01-25*

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

- **~412 of 419** built-in functions/subs are fully implemented (~98.3%).
- **~7** are legacy/event stubs (ERDEV, ON COM, ON UEVENT, ON SIGNAL, etc.); **~3** obsolete (e.g. PEN).
- **Joystick:** Full in external runtime; inline uses stubs (STICK/STRIG return 127/0) because hardware needs SDL2.

**References:**
- [ThingsToDo/STUB_FUNCTIONS_REMAINING.md](ThingsToDo/STUB_FUNCTIONS_REMAINING.md) — stub-only and incomplete functions
- [ThingsToDo/STUB_FUNCTIONS_FULL.md](ThingsToDo/STUB_FUNCTIONS_FULL.md) — full catalog and compile-time rejections

---

## Architecture Overview

### Inline Runtime (emits C)

```
src/codegen/c_backend/runtime/
├── mod.rs       - Header emission, runtime mode switch, forward decls
├── types.rs     - qb_string layout, type-size helpers
├── strings.rs   - LEN, LEFT$, RIGHT$, MID$, INSTR, CHR$, ASC, STR$, VAL, temp pool, compare
├── io.rs        - PRINT, INPUT, LINE INPUT, CLS, LOCATE, COLOR
├── file.rs      - OPEN, CLOSE, PRINT #, INPUT #, LINE INPUT #, GET, PUT, SEEK, LOC, LOF, EOF, FREEFILE, KILL, NAME, CHDIR, MKDIR, RMDIR
├── keyboard.rs  - INKEY$, _KEYHIT, _KEYDOWN, _KEYCLEAR
├── memory.rs    - _MEMNEW, _MEMFREE, _MEM*, PEEK, POKE, VARPTR, etc.
├── timing.rs    - TIMER, SLEEP, _DELAY, _LIMIT, DATE$, TIME$, RND, RANDOMIZE
├── arrays.rs    - LBOUND, UBOUND, REDIM
├── math.rs      - ABS, SGN, INT, FIX, CINT, CLNG, SQR, LOG, EXP, SIN, COS, TAN, ATN, _ATAN2, _PI, etc.
├── error.rs     - ON ERROR, RESUME, ERR, ERL, ERROR
├── graphics.rs  - Stubs: SCREEN, CLS, PSET, LINE, CIRCLE, PAINT, COLOR, etc.; frame limiting
├── audio.rs     - Stubs: BEEP, SOUND, _SND*
├── legacy.rs    - DEF SEG, PEEK/POKE, OUT, INP, PALETTE, GOSUB stack
├── system.rs    - ENVIRON$, COMMAND$, SHELL, SYSTEM, END, _OS$, _CWD$, _STARTDIR$, etc.
└── debug.rs     - Optional: breakpoints, stepping, IPC for debugger
```

### External Runtime (Rust → libqb64fresh_rt)

```
runtime/src/
├── lib.rs         - Crate root, qb_runtime_init/shutdown, qb_end, qb_stop, qb_init_args, qb_init_startdir
├── string.rs      - QbString, qb_string_*, qb_chr, qb_asc, qb_left, qb_right, qb_mid, qb_instr, qb_ucase, qb_lcase, qb_ltrim, qb_rtrim, qb_space, qb_string_fill, qb_str_int, qb_str_float, qb_val
├── io.rs          - PRINT, INPUT, LINE INPUT, CLS, LOCATE, COLOR, file open/close, PRINT #, INPUT #, GET, PUT, SEEK, EOF, LOF, LOC, FREEFILE, KILL, NAME, etc.
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
```

**C API contract:** [runtime/include/qb64fresh_rt.h](../runtime/include/qb64fresh_rt.h)

---

## Implementation Status Legend

- `[ ]` Not started
- `[~]` Partial or stub-only
- `[x]` Complete

**Note:** For up-to-date per-function status, see [ThingsToDo/STUB_FUNCTIONS_REMAINING.md](ThingsToDo/STUB_FUNCTIONS_REMAINING.md) and [ThingsToDo/STUB_FUNCTIONS_FULL.md](ThingsToDo/STUB_FUNCTIONS_FULL.md).

---

## Phase 1: Core Essentials

### 1.1 Memory Management

**Inline:** `src/codegen/c_backend/runtime/memory.rs`  
**External:** Not a separate module; `_MEM*` and related may be inlined or TBD.

| Function | Status | Description |
|----------|--------|-------------|
| `_MEMNEW(size)` | [~] | Allocate memory block |
| `_MEMFREE(block)` | [~] | Free memory block |
| `_MEMEXISTS(block)` | [~] | Check if block is valid |
| `_MEMCOPY(src, dest, size)` | [~] | Copy memory |
| `_MEMGET(block, offset, type)` | [~] | Read typed value |
| `_MEMPUT(block, offset, value)` | [~] | Write typed value |
| `_MEMFILL(block, offset, size, value)` | [~] | Fill memory |
| `_MEM(var)` | [~] | Get memory block for variable |
| `_MEMSOUND(handle)` | [~] | Get memory block for sound |
| `_MEMIMAGE(handle)` | [~] | Get memory block for image |

### 1.2 String System

**External:** `runtime/src/string.rs`  
**Inline:** `src/codegen/c_backend/runtime/strings.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `LEN(str)` | [x] | String length |
| `LEFT$(str, n)` | [x] | Left substring |
| `RIGHT$(str, n)` | [x] | Right substring |
| `MID$(str, start, len)` | [x] | Middle substring |
| `INSTR(start, str, search)` | [x] | Find substring |
| `_INSTRREV(start, str, search)` | [~] | Find from right |
| `UCASE$(str)` | [x] | Uppercase |
| `LCASE$(str)` | [x] | Lowercase |
| `LTRIM$(str)` | [x] | Trim left |
| `RTRIM$(str)` | [x] | Trim right |
| `_TRIM$(str)` | [~] | Trim both |
| `SPACE$(n)` | [x] | Create spaces |
| `STRING$(n, char)` | [x] | Create repeated char |
| `CHR$(n)` | [x] | ASCII to char |
| `ASC(str, pos)` | [x] | Char to ASCII |
| `STR$(num)` | [x] | Number to string |
| `VAL(str)` | [x] | String to number |
| `HEX$(num)` | [~] | Number to hex |
| `OCT$(num)` | [~] | Number to octal |
| `_BIN$(num)` | [~] | Number to binary |

### 1.3 Math Functions

**External:** `runtime/src/math.rs`  
**Inline:** `src/codegen/c_backend/runtime/math.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `ABS(x)` | [x] | Absolute value |
| `SGN(x)` | [x] | Sign |
| `INT(x)` | [x] | Floor |
| `FIX(x)` | [x] | Truncate toward zero |
| `CINT(x)` | [x] | Convert to integer |
| `CLNG(x)` | [x] | Convert to long |
| `CSNG(x)` | [~] | Convert to single |
| `CDBL(x)` | [~] | Convert to double |
| `SQR(x)` | [x] | Square root |
| `LOG(x)` | [x] | Natural log |
| `EXP(x)` | [x] | Exponential |
| `SIN(x)` | [x] | Sine |
| `COS(x)` | [x] | Cosine |
| `TAN(x)` | [x] | Tangent |
| `ATN(x)` | [x] | Arctangent |
| `_ASIN(x)` | [x] | Arcsine |
| `_ACOS(x)` | [x] | Arccosine |
| `_ATAN2(y, x)` | [x] | Two-argument arctangent |
| `_SINH(x)` | [x] | Hyperbolic sine |
| `_COSH(x)` | [x] | Hyperbolic cosine |
| `_TANH(x)` | [x] | Hyperbolic tangent |
| `_PI(mult)` | [x] | Pi constant |
| `RND(n)` | [x] | Random number |
| `RANDOMIZE(seed)` | [x] | Seed RNG |
| `_D2R(x)` | [x] | Degrees to radians |
| `_R2D(x)` | [x] | Radians to degrees |

### 1.4 File I/O

**External:** `runtime/src/io.rs` (file operations)  
**Inline:** `src/codegen/c_backend/runtime/file.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `OPEN file FOR mode AS #n` | [x] | Open file |
| `CLOSE #n` | [x] | Close file |
| `PRINT #n, data` | [x] | Write to file |
| `INPUT #n, vars` | [x] | Read from file |
| `LINE INPUT #n, str$` | [x] | Read line |
| `WRITE #n, data` | [~] | Write CSV format |
| `GET #n, pos, var` | [x] | Binary read |
| `PUT #n, pos, var` | [x] | Binary write |
| `SEEK #n, pos` | [x] | Set position |
| `SEEK(n)` | [x] | Get position |
| `LOC(n)` | [x] | Current record |
| `LOF(n)` | [x] | File length |
| `EOF(n)` | [x] | End of file |
| `FREEFILE` | [x] | Next free file number |
| `KILL file` | [x] | Delete file |
| `NAME old AS new` | [x] | Rename file |
| `CHDIR path` | [~] | Change directory |
| `MKDIR path` | [~] | Create directory |
| `RMDIR path` | [~] | Remove directory |
| `_FILEEXISTS(file)` | [x] | Check file exists |

---

## Phase 2: Graphics System

**External:** `runtime/src/graphics/`, `runtime/src/graphics_ffi.rs`  
**Inline:** `src/codegen/c_backend/runtime/graphics.rs` (stubs; `QB64FRESH_MAX_FRAMES` frame limiting)

See [GRAPHICS.md](GRAPHICS.md) for architecture, backends (SDL2, Mock), and stub behavior.

### 2.1 Screen Management

| Function | Status | Description |
|----------|--------|-------------|
| `SCREEN mode` | [~] | Set screen mode |
| `_NEWIMAGE(w, h, mode)` | [~] | Create image buffer |
| `_FREEIMAGE handle` | [~] | Free image |
| `_DEST handle` | [~] | Set draw destination |
| `_SOURCE handle` | [~] | Set read source |
| `_DISPLAY` | [~] | Refresh screen |
| `_SCREENHIDE` | [~] | Hide window |
| `_SCREENSHOW` | [~] | Show window |
| `_SCREENMOVE x, y` | [~] | Move window |
| `_FULLSCREEN mode` | [~] | Fullscreen toggle |
| `_TITLE text$` | [~] | Set window title |
| `_WIDTH` | [~] | Screen width |
| `_HEIGHT` | [~] | Screen height |
| `_DESKTOPWIDTH` | [~] | Desktop width |
| `_DESKTOPHEIGHT` | [~] | Desktop height |

### 2.2 Drawing Primitives

| Function | Status | Description |
|----------|--------|-------------|
| `CLS` | [~] | Clear screen |
| `PSET (x, y), color` | [~] | Set pixel |
| `PRESET (x, y)` | [~] | Set pixel to background |
| `POINT(x, y)` | [~] | Get pixel color |
| `LINE (x1,y1)-(x2,y2), color, style` | [~] | Draw line |
| `CIRCLE (x, y), r, color, start, end, aspect` | [~] | Draw circle/ellipse/arc |
| `PAINT (x, y), fill, border` | [~] | Flood fill |
| `DRAW commands$` | [~] | Draw language |
| `VIEW (x1,y1)-(x2,y2)` | [~] | Set viewport |
| `WINDOW (x1,y1)-(x2,y2)` | [~] | World coordinates |

### 2.3 Color and Image Operations

| Function | Status | Description |
|----------|--------|-------------|
| `COLOR fg, bg` | [~] | Set colors |
| `_RGB`, `_RGBA`, `_RGB32`, `_RGBA32` | [~] | Create colors |
| `_RED`, `_GREEN`, `_BLUE`, `_ALPHA` (and 32-bit) | [~] | Extract components |
| `_PUTIMAGE` | [~] | Copy/scale image |
| `GET (x1,y1)-(x2,y2), array` | [~] | Capture to array |
| `PUT (x, y), array, action` | [~] | Draw from array |

### 2.4 Text and Fonts

**External:** `runtime/src/font_ffi.rs`, `runtime/src/font_manager.rs` (optional FreeType)

| Function | Status | Description |
|----------|--------|-------------|
| `PRINT expressions` | [x] | Print text |
| `LOCATE row, col` | [x] | Set cursor position |
| `CSRLIN`, `POS(0)` | [x] | Cursor row/column |
| `_PRINTSTRING (x, y), text$` | [~] | Print at pixel position |
| `_LOADFONT`, `_FONT`, `_FREEFONT` | [~] | Font load/set/free |
| `_FONTHEIGHT`, `_FONTWIDTH`, `_PRINTWIDTH` | [~] | Font metrics |

---

## Phase 3: Input System

### 3.1 Keyboard

**External:** `runtime/src/io.rs` (keyboard)  
**Inline:** `src/codegen/c_backend/runtime/keyboard.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `INKEY$` | [x] | Get key (non-blocking) |
| `INPUT prompt; vars` | [x] | Prompted input |
| `LINE INPUT prompt; str$` | [x] | Line input |
| `_KEYHIT` | [x] | Get key code (non-blocking) |
| `_KEYDOWN(code)` | [x] | Check if key pressed |
| `_KEYCLEAR` | [x] | Clear keyboard buffer |

### 3.2 Mouse

**External:** `runtime/src/graphics_ffi.rs` (qb_mouse_*)

| Function | Status | Description |
|----------|--------|-------------|
| `_MOUSEINPUT` | [~] | Poll mouse events |
| `_MOUSEX`, `_MOUSEY` | [~] | Mouse position |
| `_MOUSEBUTTON(n)` | [~] | Button state |
| `_MOUSEWHEEL` | [~] | Wheel delta |
| `_MOUSESHOW`, `_MOUSEHIDE` | [~] | Cursor visibility |
| `_MOUSEMOVE x, y` | [~] | Move cursor |

### 3.3 Game Controller

**External:** `runtime/src/joystick.rs` (full).  
**Inline:** Stubs (STICK/STRIG return 127/0).

| Function | Status | Description |
|----------|--------|-------------|
| `STICK(n)` | [x] ext / [~] inline | Joystick axis |
| `STRIG(n)` | [x] ext / [~] inline | Joystick button |
| `_DEVICES`, `_DEVICE$(n)` | [~] | Device count/name |
| `_AXIS(n)`, `_BUTTON(n)` | [~] | Axis/button state |

---

## Phase 4: Audio System

**External:** `runtime/src/audio/`, `runtime/src/audio_ffi.rs` (Rodio)  
**Inline:** `src/codegen/c_backend/runtime/audio.rs` (stubs)

| Function | Status | Description |
|----------|--------|-------------|
| `BEEP` | [~] | System beep |
| `SOUND freq, duration` | [~] | Generate tone |
| `PLAY command$` | [~] | Play music string |
| `_SNDOPEN`, `_SNDCLOSE` | [~] | Open/close sound file |
| `_SNDPLAY`, `_SNDPAUSE`, `_SNDSTOP` | [~] | Playback control |
| `_SNDLOOP`, `_SNDVOL`, `_SNDBAL` | [~] | Loop, volume, balance |
| `_SNDPLAYING`, `_SNDLEN`, `_SNDGETPOS`, `_SNDSETPOS` | [~] | Queries |

---

## Phase 5: System Integration

**Inline:** `src/codegen/c_backend/runtime/system.rs`, `timing.rs`, `error.rs`  
**External:** `runtime/src/io.rs` (env, command, shell), `runtime/src/dialogs.rs`

### 5.1 Timing

| Function | Status | Description |
|----------|--------|-------------|
| `TIMER` | [x] | Seconds since midnight |
| `_DELAY seconds` | [x] | Delay execution |
| `SLEEP seconds` | [x] | Sleep |
| `_LIMIT fps` | [x] | Frame rate limit |
| `DATE$` | [x] | Current date |
| `TIME$` | [x] | Current time |

### 5.2 Environment and Shell

| Function | Status | Description |
|----------|--------|-------------|
| `ENVIRON$(name)` | [x] | Get env variable |
| `ENVIRON "name=value"` | [~] | Set env variable |
| `COMMAND$` | [x] | Command line |
| `_OS$` | [x] | Operating system |
| `_SHELL(cmd$)` | [x] | Execute command |
| `SHELL cmd$` | [x] | Execute (no return) |
| `SYSTEM`, `END` | [x] | Exit program |

### 5.3 Dialogs and Clipboard

**External:** `runtime/src/dialogs.rs` (rfd); clipboard in graphics_ffi.

| Function | Status | Description |
|----------|--------|-------------|
| `_MESSAGEBOX` | [~] | Message box |
| `_OPENFILEDIALOG$`, `_SAVEFILEDIALOG$` | [~] | File dialogs |
| `_SELECTFOLDERDIALOG$` | [~] | Folder dialog |
| `_CLIPBOARD$` | [~] | Get/set clipboard text |

### 5.4 Error Handling

**Inline:** `src/codegen/c_backend/runtime/error.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `ON ERROR GOTO label` | [x] | Set error handler |
| `RESUME`, `RESUME NEXT`, `RESUME label` | [x] | Resume after error |
| `ERR`, `ERL` | [x] | Error code/line |
| `_ERRORMESSAGE$` | [~] | Error message |
| `ERROR n` | [x] | Generate error |

---

## Phase 6: Networking

| Function | Status | Description |
|----------|--------|-------------|
| `_OPENCLIENT("HTTP:url")` | [ ] | Open HTTP connection |
| `_OPENHOST("TCP/IP:port")` | [ ] | Open server |
| `_OPENCLIENT("TCP/IP:port:addr")` | [ ] | Connect to server |
| `_CONNECTED(handle)` | [ ] | Check connected |

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
2. **Graphics foundation:** SDL2 window, screen modes, CLS, COLOR, PSET, LINE, CIRCLE.
3. **Graphics extended:** Images, _PUTIMAGE, GET/PUT, text/fonts, viewport/window.
4. **Audio:** BEEP, SOUND, _SNDOPEN/_SNDPLAY family, PLAY command.
5. **Input:** Mouse, game controller (external already has joystick).
6. **System:** Dialogs, clipboard, networking; polish and conformance tests.

---

## Success Criteria

1. **Bootstrap:** QB64pe compiles itself with QB64Fresh and runs to completion (resolve startup/IDE init hang; see [RUNTIME_ARCHITECTURE_PERSPECTIVES.md](RUNTIME_ARCHITECTURE_PERSPECTIVES.md)).
2. **Test suite:** qbasic_testcases (and equivalent) pass on both inline and external where applicable.
3. **Conformance:** Non-graphics behavior matches between inline and external; see Perspectives doc.
4. **Feature parity:** All QB64pe functions either implemented or explicitly documented as stub/unsupported (STUB_FUNCTIONS_*).
5. **Performance:** Comparable to QB64pe for typical programs.

---

## Related Documentation

- [RUNTIME_ARCHITECTURE_PERSPECTIVES.md](RUNTIME_ARCHITECTURE_PERSPECTIVES.md) — dual-runtime trade-offs, when to use each, action items
- [GRAPHICS.md](GRAPHICS.md) — graphics architecture, inline stubs, `QB64FRESH_MAX_FRAMES`, SDL2/Mock backends
- [runtime/include/qb64fresh_rt.h](../runtime/include/qb64fresh_rt.h) — C API contract for external runtime
- [ThingsToDo/STUB_FUNCTIONS_REMAINING.md](ThingsToDo/STUB_FUNCTIONS_REMAINING.md) — stub-only and incomplete functions
- [ThingsToDo/STUB_FUNCTIONS_FULL.md](ThingsToDo/STUB_FUNCTIONS_FULL.md) — full function catalog
