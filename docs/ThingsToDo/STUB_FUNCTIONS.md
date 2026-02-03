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
- **Location:** `src/codegen/c_backend/runtime.rs` (~4,500 lines)
- Core functions (string, math, I/O) are fully implemented inline

### External Runtime (Full Implementation)
- Links against `libqb64fresh_rt.a` static library
- Complete SDL2-based graphics with hardware acceleration
- Full Rodio-based audio with MML parsing and file playback
- **Location:** `runtime/src/` (~16,000+ lines total)

---

## Implementation Status Overview

| Category | Inline (Stubs) | External Runtime | Notes |
|----------|----------------|------------------|-------|
| **String Functions** | ✅ Full | ✅ Full | LEN, CHR$, ASC, LEFT$, RIGHT$, MID$, etc. |
| **Math Functions** | ✅ Full | ✅ Full | SIN, COS, TAN, LOG, EXP, RND, etc. |
| **File I/O** | ✅ Full | ✅ Full | OPEN, CLOSE, GET, PUT, EOF, LOF, etc. |
| **Console I/O** | ✅ Full | ✅ Full | PRINT, INPUT, CLS, LOCATE, COLOR |
| **Audio** | ⚠️ No-op | ✅ Full | Rodio backend with file playback |
| **Graphics Core** | ⚠️ Returns defaults | ✅ Full | SDL2 backend |
| **Drawing** | ⚠️ No-op | ✅ Full | PSET, LINE, CIRCLE, PAINT |
| **Images** | ⚠️ Returns -1 | ✅ Full | PNG/JPEG/BMP/GIF support |
| **Mouse** | ⚠️ Returns 0 | ✅ Full | SDL2 input |
| **Fonts** | ⚠️ Hardcoded 8×16 | ✅ Full | TrueType support |
| **Clipboard** | ⚠️ Empty | ✅ Full | SDL2 clipboard |
| **Dialogs** | ⚠️ stdin fallback | ✅ Full | Native rfd dialogs |
| **Joystick** | ⚠️ Center position | ✅ Full | SDL2 gamepad via joystick.rs |
| **Networking** | ⚠️ Stub | ✅ Full | TCP/IP client/server |
| **Memory Operations** | ⚠️ Stub | ✅ Full | _MEMNEW, _MEMGET, _MEMPUT, etc. |
| **Legacy Hardware** | ⚠️ Safe defaults | ❌ Intentional | Port I/O, light pen obsolete |

---

## Function Categories

### String Functions (~20 functions) - ✅ Fully Implemented (Both Modes)

**Inline & External Runtime:** Core string operations work in both modes

| Function | Purpose |
|---------|---------|
| `LEN()` | Get string length or UDT size |
| `CHR$()` | Convert ASCII code to character |
| `ASC()` | Get ASCII code of character |
| `LEFT$()` | Extract left portion of string |
| `RIGHT$()` | Extract right portion of string |
| `MID$()` | Extract substring |
| `INSTR()` | Find substring position |
| `UCASE$()` | Convert to uppercase |
| `LCASE$()` | Convert to lowercase |
| `LTRIM$()` | Remove leading spaces |
| `RTRIM$()` | Remove trailing spaces |
| `TRIM$()` | Remove leading/trailing spaces |
| `STR$()` | Convert number to string |
| `VAL()` | Convert string to number |
| `STRING$()` | Repeat character |
| `SPACE$()` | Create string of spaces |
| `HEX$()` | Convert to hexadecimal |
| `OCT$()` | Convert to octal |
| `_BIN$()` | Convert to binary |
| `_INSTRREV()` | Reverse string search |

**External Runtime:** `runtime/src/string.rs` (~600 lines)

---

### Math Functions (~50 functions) - ✅ Fully Implemented (Both Modes)

**Inline & External Runtime:** Core math operations work in both modes

| Category | Functions |
|----------|-----------|
| **Basic** | ABS, SGN, INT, FIX, CINT, CLNG, CSNG, CDBL |
| **Trigonometry** | SIN, COS, TAN, ATN, _ASIN, _ACOS, _ATAN2 |
| **Hyperbolic** | _SINH, _COSH, _TANH, _ASINH, _ACOSH, _ATANH |
| **Reciprocal Trig** | _SEC, _CSC, _COT, _SECH, _CSCH, _COTH |
| **Inverse Reciprocal** | _ARCSEC, _ARCCSC, _ARCCOT, _ARCSECH, _ARCCSCH, _ARCCOTH |
| **Exponential/Log** | SQR, LOG, EXP, _PI, _E |
| **Min/Max/Clamp** | _MIN, _MAX, _CLAMP |
| **Rounding** | _CEIL, _ROUND |
| **Angle Conversion** | _D2R, _R2D, _D2G, _G2D, _G2R, _R2G |
| **Random** | RND, RANDOMIZE |
| **Timer** | TIMER, SLEEP, DELAY |

**External Runtime:** `runtime/src/math.rs` (~320 lines)

---

### File I/O Functions (~10 functions) - ✅ Fully Implemented (Both Modes)

**Inline & External Runtime:** File operations work in both modes

| Function | Purpose |
|---------|---------|
| `EOF()` | Check end of file |
| `LOF()` | Get file length |
| `LOC()` | Get file position |
| `SEEK()` | Get file position |
| `FREEFILE()` | Get next free file number |
| `_FILEEXISTS()` | Check if file exists |
| `_DIREXISTS()` | Check if directory exists |
| `_DIR$()` | Get directory listing |
| `_READFILE$()` | Read entire file |
| `_WRITEFILE()` | Write entire file (statement) |

---

### Console I/O Functions (~15 functions) - ✅ Fully Implemented (Both Modes)

**Inline & External Runtime:** Console operations work in both modes

| Function | Purpose |
|---------|---------|
| `PRINT` | Output text/numbers (statement) |
| `INPUT` | Read user input (statement) |
| `CLS` | Clear screen (statement) |
| `LOCATE` | Set cursor position (statement) |
| `COLOR` | Set text colors (statement) |
| `TAB()` | Tab formatting function |
| `SPC()` | Space formatting function |
| `POS()` | Get cursor column |
| `CSRLIN` | Get cursor row |
| `SCREEN()` | Read screen character/attribute |
| `INKEY$()` | Read keyboard character |
| `INPUT$()` | Read n characters |
| `_KEYHIT()` | Check for key press |
| `_KEYDOWN()` | Check if key is held |
| `_CINP()` | Read character without echo |

**External Runtime:** `runtime/src/io.rs` (~850 lines)

---

### Audio Functions (~20 functions)

**External Runtime:** `runtime/src/audio/rodio_backend.rs` (543 lines)  
**External Runtime FFI:** `runtime/src/audio_ffi.rs` (~420 lines)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `BEEP` | void (no-op) | ✅ Full | Play system beep (statement) |
| `SOUND` | void (no-op) | ✅ Full | Play tone (statement) |
| `PLAY` | void (no-op) | ✅ Full | Play MML music (statement) |
| `_SNDOPEN()` | -1 | ✅ Full | Open audio file |
| `_SNDOPENRAW()` | -1 | ⚠️ Partial | Open raw audio stream |
| `_SNDCLOSE()` | void | ✅ Full | Close audio handle |
| `_SNDPLAY()` | void | ✅ Full | Start playback (statement) |
| `_SNDSTOP()` | void | ✅ Full | Stop playback (statement) |
| `_SNDPAUSE()` | void | ✅ Full | Pause playback (statement) |
| `_SNDRESUME()` | void | ✅ Full | Resume playback (statement) |
| `_SNDLOOP()` | void | ✅ Full | Set loop mode (statement) |
| `_SNDVOL()` | void | ✅ Full | Set volume (statement) |
| `_SNDBAL()` | void | ⚠️ Partial | Set 3D balance (rodio limitation) |
| `_SNDLEN()` | 0.0 | ✅ Full | Get audio length |
| `_SNDGETPOS()` | 0.0 | ⚠️ Partial | Get playback position |
| `_SNDSETPOS()` | void | ⚠️ Partial | Set playback position |
| `_SNDPLAYING()` | 0 | ✅ Full | Check if playing |
| `_SNDPAUSED()` | 0 | ✅ Full | Check if paused |
| `_SNDCOPY()` | -1 | ✅ Full | Copy audio handle |
| `_SNDRAW()` | void | ⚠️ Partial | Write raw audio sample |
| `_SNDRAWLEN()` | 0.0 | ⚠️ Partial | Get raw audio queue length |

---

### Graphics Functions (~80 functions)

**External Runtime:** `runtime/src/graphics/sdl2.rs` (2,144 lines)  
**External Runtime FFI:** `runtime/src/graphics_ffi.rs` (~1,600 lines)

#### Core Graphics

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `SCREEN` | void | ✅ Full | Set screen mode (statement) |
| `_NEWIMAGE()` | -1 | ✅ Full | Create image buffer |
| `_LOADIMAGE()` | -1 | ✅ Full | Load image from file |
| `_COPYIMAGE()` | -1 | ✅ Full | Copy image |
| `_WIDTH()` | 0 | ✅ Full | Get image/screen width |
| `_HEIGHT()` | 0 | ✅ Full | Get image/screen height |
| `_FULLSCREEN()` | 0 | ✅ Full | Toggle fullscreen |
| `_SCREENHIDE()` | 0 | ✅ Full | Hide window |
| `_SCREENSHOW()` | 0 | ✅ Full | Show window |
| `_SCREENMOVE()` | 0 | ✅ Full | Move window |
| `_SCREENCLICK()` | 0 | ✅ Full | Enable click-through |

#### Drawing Functions

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `PSET` | void (no-op) | ✅ Full | Set pixel (statement) |
| `LINE` | void (no-op) | ✅ Full | Draw line/box (statement) |
| `CIRCLE` | void (no-op) | ✅ Full | Draw circle (statement) |
| `PAINT` | void (no-op) | ✅ Full | Fill area (statement) |
| `POINT()` | 0 | ✅ Full | Get pixel color |
| `PMAP()` | 0.0 | ✅ Full | Coordinate mapping |
| `GET` | void (no-op) | ✅ Full | Copy screen to array (statement) |
| `PUT` | void (no-op) | ✅ Full | Copy array to screen (statement) |
| `PCOPY` | void (no-op) | ✅ Full | Copy page (statement) |
| `DISPLAY` | void (no-op) | ✅ Full | Update screen (statement) |

#### Color Functions

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `RGB()` | 0 | ✅ Full | Create RGB color |
| `RGBA()` | 0 | ✅ Full | Create RGBA color |
| `RGB32()` | 0 | ✅ Full | Create 32-bit RGB |
| `RGBA32()` | 0 | ✅ Full | Create 32-bit RGBA |
| `PALETTE` | void | ✅ Full | Set palette color (statement) |

#### View/Window Functions

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `VIEW` | void | ✅ Full | Set viewport (statement) |
| `WINDOW` | void | ✅ Full | Set coordinate system (statement) |
| `VIEW PRINT` | void | ✅ Full | Set text viewport (statement) |

---

### Mouse Functions (~8 functions)

**External Runtime:** `runtime/src/graphics_ffi.rs` (mouse functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `_MOUSEX()` | 0 | ✅ Full | Get mouse X position |
| `_MOUSEY()` | 0 | ✅ Full | Get mouse Y position |
| `_MOUSEBUTTON()` | 0 | ✅ Full | Check button state |
| `_MOUSEINPUT()` | 0 | ✅ Full | Check for mouse events |
| `_MOUSEMOVEMENTX()` | 0 | ✅ Full | Get X movement delta |
| `_MOUSEMOVEMENTY()` | 0 | ✅ Full | Get Y movement delta |
| `_MOUSEWHEEL()` | 0 | ✅ Full | Get wheel delta |
| `_MOUSEHIDE()` / `_MOUSESHOW()` | void | ✅ Full | Hide/show cursor |

---

### Font Functions (~7 functions)

**External Runtime:** `runtime/src/graphics/sdl2.rs` (font support)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `_LOADFONT()` | -1 | ✅ Full | Load TrueType font |
| `_FONT()` | -1 | ✅ Full | Set current font |
| `_FREEFONT()` | 0 | ✅ Full | Free font handle |
| `_FONTHEIGHT()` | 16 | ✅ Full | Get font height |
| `_FONTWIDTH()` | 8 | ✅ Full | Get font width |
| `_PRINTWIDTH()` | 0 | ✅ Full | Get text pixel width |

---

### Clipboard Functions (~2 functions)

**External Runtime:** `runtime/src/graphics_ffi.rs` (clipboard support)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `_CLIPBOARD$()` | "" | ✅ Full | Get clipboard text |
| `_CLIPBOARD$` | void | ✅ Full | Set clipboard text (statement) |

---

### Dialog Functions (~5 functions)

**External Runtime:** `runtime/src/dialogs.rs` (348 lines)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `_MESSAGEBOX()` | 1 | ✅ Full | Show message box |
| `_INPUTBOX$()` | "" | ✅ Full | Show input dialog |
| `_OPENFILEDIALOG$()` | "" | ✅ Full | Open file dialog |
| `_SAVEFILEDIALOG$()` | "" | ✅ Full | Save file dialog |
| `_SELECTFOLDERDIALOG$()` | "" | ✅ Full | Select folder dialog |

---

### Joystick Functions (~6 functions)

**External Runtime:** `runtime/src/joystick.rs` (306 lines)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `STICK()` | 0 | ✅ Full | Get joystick position (QB4.5) |
| `STRIG()` | 0 | ✅ Full | Get trigger state (QB4.5) |
| `_DEVICES()` | 0 | ✅ Full | Get device count |
| `_AXIS()` | 0.0 | ✅ Full | Get axis value |
| `_BUTTON()` | 0 | ✅ Full | Get button state |

---

### Networking Functions (~5 functions)

**External Runtime:** `runtime/src/io.rs` (networking support)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `_OPENHOST()` | -1 | ✅ Full | Open server socket |
| `_OPENCONNECTION()` | -1 | ✅ Full | Accept connection |
| `_OPENCLIENT()` | -1 | ✅ Full | Connect to server |
| `_CONNECTED()` | 0 | ✅ Full | Check connection status |
| `_CLOSEHOST()` | void | ✅ Full | Close socket |

---

### Memory Functions (~8 functions)

**External Runtime:** Core memory operations

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `_MEMNEW()` | NULL | ✅ Full | Allocate memory |
| `_MEMFREE()` | void | ✅ Full | Free memory |
| `_MEMGET()` | 0 | ✅ Full | Read from memory |
| `_MEMPUT()` | void | ✅ Full | Write to memory |
| `_MEMCOPY()` | void | ✅ Full | Copy memory |
| `_MEMFILL()` | void | ✅ Full | Fill memory |
| `_MEM()` | NULL | ✅ Full | Get variable memory |
| `_OFFSET()` | 0 | ✅ Full | Get variable offset |

---

### Legacy/QB4.5 Functions (~15 functions)

These are intentionally minimal - they support compatibility with old BASIC programs but many represent obsolete hardware.

#### Legacy I/O & Memory

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `LPOS()` | 1 | ⚠️ Stub only | Get printer carriage position |
| `FRE()` | Large number | ⚠️ Stub only | Get free memory |
| `PEEK()` | 0 | ⚠️ Stub only | Read memory byte |
| `POKE` | void | ⚠️ Stub only | Write memory byte (statement) |

#### Port I/O Functions (3 functions) - Intentionally Disabled

These are **intentionally not implemented** for security. Direct port I/O is not available on protected-mode operating systems.

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `INP()` | 0xFF | ❌ Sandboxed | Read from I/O port |
| `OUT` | void | ❌ Sandboxed | Write to I/O port (statement) |
| `WAIT` | void | ❌ Sandboxed | Wait for port condition (statement) |

#### Hardware Functions - Obsolete Hardware

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `PEN()` | 0 | ❌ Obsolete | Get light pen state |
| `ERDEV()` | 0 | ⚠️ Stub only | Get device error code |
| `ERDEV$()` | "" | ⚠️ Stub only | Get device error name |
| `IOCTL$()` | "" | ⚠️ Stub only | Get device status string |
| `IOCTL` | void | ⚠️ Stub only | Send device control (statement) |

#### System Interrupts (2 functions) - Intentionally Disabled

Not supported on modern systems for security reasons.

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `INTERRUPT` | void (warns) | ❌ Disabled | Call system interrupt (statement) |
| `INTERRUPTX` | void (warns) | ❌ Disabled | Extended interrupt (statement) |

#### Event Handlers (~11 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `ON STRIG` | void | ❌ Not yet | Joystick trigger handler (statement) |
| `STRIG ON/OFF/STOP` | void | ❌ Not yet | Trigger control (statement) |
| `ON COM` | void | ⚠️ Stub only | Serial port handler (statement) |
| `COM ON/OFF/STOP` | void | ⚠️ Stub only | Serial control (statement) |
| `ON PEN` | void | ❌ Obsolete | Light pen handler (statement) |
| `PEN ON/OFF/STOP` | void | ❌ Obsolete | Light pen control (statement) |
| `ON UEVENT` | void | ⚠️ Stub only | User event handler (statement) |
| `UEVENT ON/OFF/STOP` | void | ⚠️ Stub only | User event control (statement) |
| `_UEVENTTRIGGER` | void | ⚠️ Stub only | Trigger user event (statement) |
| `ON SIGNAL` | void | ⚠️ Stub only | Signal handler (statement) |
| `SIGNAL ON/OFF/STOP` | void | ⚠️ Stub only | Signal control (statement) |

---

## Summary

### Implementation Statistics

**Total Built-in Functions Registered:** ~420 (in semantic analyzer)

| Status | Count | Percentage | Notes |
|--------|-------|------------|-------|
| ✅ Fully Implemented (Both Modes) | ~150 | 36% | String, math, file I/O, console |
| ✅ Fully Implemented (External Only) | ~200 | 48% | Graphics, audio, dialogs, networking |
| ⚠️ Partial/Stub only | ~50 | 12% | Some audio features, legacy functions |
| ❌ Not implemented/Disabled | ~20 | 5% | Port I/O, interrupts, obsolete hardware |

### What's Still Missing

**Low Priority (rarely used):**
1. Serial port (COM) support - ON COM handlers
2. User-defined events (UEVENT) - full implementation
3. Some audio features - raw audio streaming limitations

**Intentionally Not Implemented:**
- Port I/O (INP, OUT, WAIT) - security restrictions
- System interrupts (INTERRUPT, INTERRUPTX) - security restrictions
- Light pen (PEN) - obsolete hardware

---

## Runtime Source Files Reference

| File | Lines | Purpose |
|------|-------|---------|
| `src/codegen/c_backend/runtime.rs` | ~4,500 | Inline runtime (stubs & core functions) |
| `runtime/src/string.rs` | ~600 | String operations |
| `runtime/src/math.rs` | ~320 | Math functions |
| `runtime/src/io.rs` | ~850 | Console I/O, file I/O, networking |
| `runtime/src/graphics/sdl2.rs` | 2,144 | SDL2 graphics backend |
| `runtime/src/graphics_ffi.rs` | ~1,600 | Graphics C FFI layer |
| `runtime/src/audio/rodio_backend.rs` | 543 | Rodio audio backend |
| `runtime/src/audio_ffi.rs` | ~420 | Audio C FFI layer |
| `runtime/src/dialogs.rs` | 348 | Native file dialogs (rfd) |
| `runtime/src/joystick.rs` | 306 | Gamepad/joystick support |
| **External runtime total** | **~16,000+** | All runtime/src/ files |

---

## How to Use Full Functionality

```bash
# Compile with external runtime for graphics/audio/dialogs
qb64fresh myprogram.bas --runtime external

# Default inline mode for console programs (file I/O works!)
qb64fresh myprogram.bas
```

The external runtime requires SDL2 to be installed on the system.
