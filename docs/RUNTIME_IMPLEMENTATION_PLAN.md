# QB64Fresh Runtime Implementation Plan

This document outlines the full Rust runtime implementation needed for QB64Fresh to achieve feature parity with QB64pe.

## Architecture Overview

```
QB64Fresh Runtime (Rust)
├── core/           - Core types, memory management
├── graphics/       - SDL2-based graphics system
├── audio/          - miniaudio-based audio system
├── input/          - Keyboard, mouse, joystick
├── fileio/         - File and console I/O
├── strings/        - String operations
├── math/           - Math functions
├── network/        - HTTP/TCP networking
├── system/         - OS integration, timing, clipboard
└── ffi/            - C bindings for generated code
```

## Implementation Status Legend

- [ ] Not started
- [~] Partial/stub implementation
- [x] Complete

---

## Phase 1: Core Essentials

### 1.1 Memory Management
**File:** `runtime/src/memory.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `_MEMNEW(size)` | [ ] | Allocate memory block |
| `_MEMFREE(block)` | [ ] | Free memory block |
| `_MEMEXISTS(block)` | [ ] | Check if block is valid |
| `_MEMCOPY(src, dest, size)` | [ ] | Copy memory |
| `_MEMGET(block, offset, type)` | [ ] | Read typed value |
| `_MEMPUT(block, offset, value)` | [ ] | Write typed value |
| `_MEMFILL(block, offset, size, value)` | [ ] | Fill memory |
| `_MEM(var)` | [ ] | Get memory block for variable |
| `_MEMSOUND(handle)` | [ ] | Get memory block for sound |
| `_MEMIMAGE(handle)` | [ ] | Get memory block for image |

**Implementation Notes:**
- Use Rust's allocator for memory management
- Track blocks with unique IDs for safety
- Support element size and type information

### 1.2 String System
**File:** `runtime/src/strings.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `LEN(str)` | [~] | String length |
| `LEFT$(str, n)` | [~] | Left substring |
| `RIGHT$(str, n)` | [~] | Right substring |
| `MID$(str, start, len)` | [~] | Middle substring |
| `INSTR(start, str, search)` | [~] | Find substring |
| `_INSTRREV(start, str, search)` | [ ] | Find from right |
| `UCASE$(str)` | [~] | Uppercase |
| `LCASE$(str)` | [~] | Lowercase |
| `LTRIM$(str)` | [~] | Trim left |
| `RTRIM$(str)` | [~] | Trim right |
| `_TRIM$(str)` | [~] | Trim both |
| `SPACE$(n)` | [~] | Create spaces |
| `STRING$(n, char)` | [~] | Create repeated char |
| `CHR$(n)` | [~] | ASCII to char |
| `ASC(str, pos)` | [~] | Char to ASCII |
| `STR$(num)` | [~] | Number to string |
| `VAL(str)` | [~] | String to number |
| `HEX$(num)` | [~] | Number to hex |
| `OCT$(num)` | [~] | Number to octal |
| `_BIN$(num)` | [ ] | Number to binary |
| `_STRCMP(a, b)` | [ ] | Case-sensitive compare |
| `_STRICMP(a, b)` | [ ] | Case-insensitive compare |

### 1.3 Math Functions
**File:** `runtime/src/math.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `ABS(x)` | [~] | Absolute value |
| `SGN(x)` | [~] | Sign |
| `INT(x)` | [~] | Floor |
| `FIX(x)` | [~] | Truncate toward zero |
| `CINT(x)` | [~] | Convert to integer |
| `CLNG(x)` | [~] | Convert to long |
| `CSNG(x)` | [~] | Convert to single |
| `CDBL(x)` | [~] | Convert to double |
| `SQR(x)` | [~] | Square root |
| `LOG(x)` | [~] | Natural log |
| `EXP(x)` | [~] | Exponential |
| `SIN(x)` | [~] | Sine |
| `COS(x)` | [~] | Cosine |
| `TAN(x)` | [~] | Tangent |
| `ATN(x)` | [~] | Arctangent |
| `_ASIN(x)` | [~] | Arcsine |
| `_ACOS(x)` | [~] | Arccosine |
| `_ATAN2(y, x)` | [~] | Two-argument arctangent |
| `_SINH(x)` | [ ] | Hyperbolic sine |
| `_COSH(x)` | [ ] | Hyperbolic cosine |
| `_TANH(x)` | [ ] | Hyperbolic tangent |
| `_SEC(x)` | [ ] | Secant |
| `_CSC(x)` | [ ] | Cosecant |
| `_COT(x)` | [ ] | Cotangent |
| `_D2R(x)` | [ ] | Degrees to radians |
| `_R2D(x)` | [ ] | Radians to degrees |
| `_PI(mult)` | [~] | Pi constant |
| `RND(n)` | [~] | Random number |
| `RANDOMIZE(seed)` | [~] | Seed RNG |
| `_CLAMP(val, min, max)` | [ ] | Clamp value |
| `_MIN(a, b)` | [ ] | Minimum |
| `_MAX(a, b)` | [ ] | Maximum |
| `_ROUND(x, digits)` | [ ] | Round to digits |

### 1.4 File I/O
**File:** `runtime/src/fileio.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `OPEN file FOR mode AS #n` | [~] | Open file |
| `CLOSE #n` | [~] | Close file |
| `PRINT #n, data` | [~] | Write to file |
| `INPUT #n, vars` | [~] | Read from file |
| `LINE INPUT #n, str$` | [~] | Read line |
| `WRITE #n, data` | [ ] | Write CSV format |
| `GET #n, pos, var` | [~] | Binary read |
| `PUT #n, pos, var` | [~] | Binary write |
| `SEEK #n, pos` | [~] | Set position |
| `SEEK(n)` | [~] | Get position |
| `LOC(n)` | [~] | Current record |
| `LOF(n)` | [~] | File length |
| `EOF(n)` | [~] | End of file |
| `FREEFILE` | [~] | Next free file number |
| `KILL file` | [~] | Delete file |
| `NAME old AS new` | [~] | Rename file |
| `CHDIR path` | [~] | Change directory |
| `MKDIR path` | [~] | Create directory |
| `RMDIR path` | [~] | Remove directory |
| `FILES pattern` | [ ] | List files |
| `_FILEEXISTS(file)` | [~] | Check file exists |
| `_DIREXISTS(dir)` | [~] | Check dir exists |
| `_CWD$` | [~] | Current directory |
| `_STARTDIR$` | [~] | Start directory |
| `_DIR$(filter)` | [ ] | Directory listing |
| `_FULLPATH$(path)` | [ ] | Absolute path |

---

## Phase 2: Graphics System

### 2.1 Screen Management
**File:** `runtime/src/graphics/screen.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `SCREEN mode` | [~] | Set screen mode |
| `_NEWIMAGE(w, h, mode)` | [~] | Create image buffer |
| `_FREEIMAGE handle` | [~] | Free image |
| `_COPYIMAGE(src, mode)` | [ ] | Copy image |
| `_DEST handle` | [~] | Set draw destination |
| `_DEST` | [~] | Get draw destination |
| `_SOURCE handle` | [~] | Set read source |
| `_SOURCE` | [~] | Get read source |
| `_DISPLAY` | [~] | Refresh screen |
| `_AUTODISPLAY` | [ ] | Auto refresh mode |
| `_SCREENHIDE` | [~] | Hide window |
| `_SCREENSHOW` | [~] | Show window |
| `_SCREENEXISTS` | [~] | Check window exists |
| `_SCREENMOVE x, y` | [~] | Move window |
| `_SCREENX` | [ ] | Window X position |
| `_SCREENY` | [ ] | Window Y position |
| `_FULLSCREEN mode` | [~] | Fullscreen toggle |
| `_ALLOWFULLSCREEN` | [ ] | Allow fullscreen |
| `_TITLE text$` | [~] | Set window title |
| `_TITLE$` | [ ] | Get window title |
| `_ICON handle` | [ ] | Set window icon |
| `_SCREENICON` | [ ] | Minimize window |
| `_WIDTH` | [~] | Screen width |
| `_HEIGHT` | [~] | Screen height |
| `_PIXELSIZE` | [ ] | Bytes per pixel |
| `_RESIZE` | [ ] | Check for resize |
| `_RESIZEWIDTH` | [ ] | New width after resize |
| `_RESIZEHEIGHT` | [ ] | New height after resize |
| `_DESKTOPWIDTH` | [~] | Desktop width |
| `_DESKTOPHEIGHT` | [~] | Desktop height |

### 2.2 Drawing Primitives
**File:** `runtime/src/graphics/draw.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `CLS` | [~] | Clear screen |
| `PSET (x, y), color` | [~] | Set pixel |
| `PRESET (x, y)` | [~] | Set pixel to background |
| `POINT(x, y)` | [~] | Get pixel color |
| `LINE (x1,y1)-(x2,y2), color, style` | [~] | Draw line |
| `CIRCLE (x, y), r, color, start, end, aspect` | [~] | Draw circle/ellipse/arc |
| `PAINT (x, y), fill, border` | [~] | Flood fill |
| `DRAW commands$` | [ ] | Draw language |
| `VIEW (x1,y1)-(x2,y2)` | [~] | Set viewport |
| `VIEW PRINT top TO bottom` | [~] | Text viewport |
| `WINDOW (x1,y1)-(x2,y2)` | [~] | World coordinates |

### 2.3 Color Functions
**File:** `runtime/src/graphics/color.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `COLOR fg, bg` | [~] | Set colors |
| `_RGB(r, g, b)` | [~] | Create RGB color |
| `_RGBA(r, g, b, a)` | [~] | Create RGBA color |
| `_RGB32(r, g, b)` | [~] | Create 32-bit RGB |
| `_RGBA32(r, g, b, a)` | [~] | Create 32-bit RGBA |
| `_RED(color)` | [~] | Extract red |
| `_GREEN(color)` | [~] | Extract green |
| `_BLUE(color)` | [~] | Extract blue |
| `_ALPHA(color)` | [~] | Extract alpha |
| `_RED32(color)` | [~] | Extract red (32-bit) |
| `_GREEN32(color)` | [~] | Extract green (32-bit) |
| `_BLUE32(color)` | [~] | Extract blue (32-bit) |
| `_ALPHA32(color)` | [~] | Extract alpha (32-bit) |
| `PALETTE attr, color` | [~] | Set palette entry |
| `_PALETTECOLOR(attr, img)` | [~] | Get/set palette |
| `_CLEARCOLOR color, img` | [ ] | Set transparent color |
| `_SETALPHA alpha, color, img` | [ ] | Set alpha for color |
| `_BLEND img` | [ ] | Enable alpha blending |
| `_DONTBLEND img` | [ ] | Disable blending |
| `_BACKGROUNDCOLOR` | [ ] | Get background color |
| `_DEFAULTCOLOR` | [ ] | Get default color |

### 2.4 Image Operations
**File:** `runtime/src/graphics/image.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `_LOADIMAGE(file$, mode)` | [ ] | Load image file |
| `_SAVEIMAGE(file$, handle)` | [ ] | Save image file |
| `_PUTIMAGE (dx,dy)-(dw,dh), src, dest, (sx,sy)-(sw,sh)` | [~] | Copy/scale image |
| `GET (x1,y1)-(x2,y2), array` | [~] | Capture to array |
| `PUT (x, y), array, action` | [~] | Draw from array |
| `_MAPTRIANGLE (sx1,sy1)-(sx2,sy2)-(sx3,sy3), src TO (dx1,dy1)-(dx2,dy2)-(dx3,dy3), dest` | [ ] | Texture map triangle |
| `_PRINTMODE mode` | [ ] | Set text render mode |
| `_PRINTWIDTH(text$)` | [ ] | Get text width |
| `_FONTHEIGHT` | [ ] | Get font height |
| `_FONTWIDTH` | [ ] | Get font width |
| `_LOADFONT(file$, size, style$)` | [ ] | Load font |
| `_FREEFONT handle` | [ ] | Free font |
| `_FONT handle` | [ ] | Set current font |

### 2.5 Text Output
**File:** `runtime/src/graphics/text.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `PRINT expressions` | [~] | Print text |
| `PRINT USING format; values` | [ ] | Formatted print |
| `LOCATE row, col` | [~] | Set cursor position |
| `CSRLIN` | [~] | Get cursor row |
| `POS(0)` | [~] | Get cursor column |
| `TAB(col)` | [~] | Tab to column |
| `SPC(n)` | [~] | Print spaces |
| `WIDTH cols, rows` | [~] | Set text dimensions |
| `_PRINTSTRING (x, y), text$` | [ ] | Print at pixel position |
| `_CONTROLCHR ON/OFF` | [ ] | Control character mode |

---

## Phase 3: Input System

### 3.1 Keyboard Input
**File:** `runtime/src/input/keyboard.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `INKEY$` | [~] | Get key (non-blocking) |
| `INPUT$(n)` | [ ] | Get n characters |
| `INPUT prompt; vars` | [~] | Prompted input |
| `LINE INPUT prompt; str$` | [~] | Line input |
| `_KEYHIT` | [ ] | Get key code (non-blocking) |
| `_KEYDOWN(code)` | [ ] | Check if key pressed |
| `_KEYCLEAR` | [ ] | Clear keyboard buffer |
| `_SCREENPRINT text$` | [ ] | Simulate typing |

**Key Codes to Support:**
- Function keys (F1-F12)
- Arrow keys
- Home, End, Page Up/Down, Insert, Delete
- Modifier keys (Shift, Ctrl, Alt)
- Numpad keys
- All printable ASCII

### 3.2 Mouse Input
**File:** `runtime/src/input/mouse.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `_MOUSEINPUT` | [ ] | Poll mouse events |
| `_MOUSEX` | [ ] | Mouse X position |
| `_MOUSEY` | [ ] | Mouse Y position |
| `_MOUSEBUTTON(n)` | [ ] | Button state |
| `_MOUSEWHEEL` | [ ] | Wheel delta |
| `_MOUSEMOVE x, y` | [ ] | Move mouse cursor |
| `_MOUSESHOW` | [ ] | Show cursor |
| `_MOUSEHIDE` | [ ] | Hide cursor |
| `_MOUSEMOVEMENTX` | [ ] | Relative X movement |
| `_MOUSEMOVEMENTY` | [ ] | Relative Y movement |

### 3.3 Game Controller
**File:** `runtime/src/input/gamepad.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `STICK(n)` | [~] | Joystick axis |
| `STRIG(n)` | [~] | Joystick button |
| `_DEVICES` | [ ] | Number of input devices |
| `_DEVICE$(n)` | [ ] | Device name |
| `_DEVICEINPUT(n)` | [ ] | Poll device |
| `_LASTBUTTON(n)` | [ ] | Last button on device |
| `_LASTAXIS(n)` | [ ] | Last axis on device |
| `_LASTWHEEL(n)` | [ ] | Last wheel on device |
| `_BUTTON(n)` | [ ] | Button state |
| `_BUTTONCHANGE(n)` | [ ] | Button changed |
| `_AXIS(n)` | [ ] | Axis value |
| `_WHEEL(n)` | [ ] | Wheel delta |

---

## Phase 4: Audio System

### 4.1 Basic Audio
**File:** `runtime/src/audio/basic.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `BEEP` | [~] | System beep |
| `SOUND freq, duration` | [~] | Generate tone |
| `PLAY command$` | [ ] | Play music string |
| `PLAY(voice)` | [ ] | Notes remaining |

### 4.2 Sound Files
**File:** `runtime/src/audio/sound.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `_SNDOPEN(file$)` | [ ] | Open sound file |
| `_SNDCLOSE handle` | [ ] | Close sound |
| `_SNDCOPY(handle)` | [ ] | Copy sound |
| `_SNDPLAY handle` | [ ] | Play sound |
| `_SNDPLAYCOPY(handle, vol)` | [ ] | Play copy |
| `_SNDPLAYFILE file$` | [ ] | Play file directly |
| `_SNDPAUSE handle` | [ ] | Pause sound |
| `_SNDSTOP handle` | [ ] | Stop sound |
| `_SNDLOOP handle` | [ ] | Loop sound |
| `_SNDPLAYING(handle)` | [ ] | Is playing? |
| `_SNDPAUSED(handle)` | [ ] | Is paused? |
| `_SNDVOL handle, vol` | [ ] | Set volume |
| `_SNDBAL handle, bal` | [ ] | Set balance |
| `_SNDLEN(handle)` | [ ] | Sound length |
| `_SNDGETPOS(handle)` | [ ] | Playback position |
| `_SNDSETPOS handle, pos` | [ ] | Set position |
| `_SNDLIMIT handle, seconds` | [ ] | Limit playback |
| `_SNDRATE` | [ ] | Sample rate |

### 4.3 Raw Audio
**File:** `runtime/src/audio/raw.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `_SNDOPENRAW` | [ ] | Open raw audio stream |
| `_SNDRAW left, right` | [ ] | Write raw samples |
| `_SNDRAWBATCH(handle, frames(), chans)` | [ ] | Write batch |
| `_SNDRAWLEN(handle)` | [ ] | Buffer length |
| `_SNDRAWDONE(handle)` | [ ] | Is buffer empty? |
| `_SNDNEW(frames, chans, bits)` | [ ] | Create sound buffer |

---

## Phase 5: System Integration

### 5.1 Timing
**File:** `runtime/src/system/timing.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `TIMER` | [~] | Seconds since midnight |
| `TIMER(accuracy)` | [~] | High-res timer |
| `_DELAY seconds` | [~] | Delay execution |
| `SLEEP seconds` | [~] | Sleep |
| `_LIMIT fps` | [~] | Frame rate limit |
| `DATE$` | [~] | Current date |
| `TIME$` | [~] | Current time |
| `ON TIMER(n) GOSUB label` | [ ] | Timer event |
| `TIMER ON/OFF/STOP` | [ ] | Timer control |

### 5.2 Environment
**File:** `runtime/src/system/environ.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `ENVIRON$(name)` | [~] | Get env variable |
| `ENVIRON$(n)` | [~] | Get nth variable |
| `ENVIRON "name=value"` | [~] | Set env variable |
| `_ENVIRONCOUNT` | [~] | Number of variables |
| `COMMAND$` | [~] | Command line |
| `COMMAND$(n)` | [~] | Nth argument |
| `_COMMANDCOUNT` | [~] | Number of arguments |
| `_OS$` | [~] | Operating system |
| `_SHELL(cmd$)` | [~] | Execute command |
| `_SHELLHIDE(cmd$)` | [ ] | Execute hidden |
| `SHELL cmd$` | [~] | Execute (no return) |
| `SYSTEM` | [~] | Exit program |
| `END` | [~] | End program |

### 5.3 Clipboard
**File:** `runtime/src/system/clipboard.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `_CLIPBOARD$` | [ ] | Get clipboard text |
| `_CLIPBOARD$ = text` | [ ] | Set clipboard text |
| `_CLIPBOARDIMAGE` | [ ] | Get clipboard image |
| `_CLIPBOARDIMAGE = handle` | [ ] | Set clipboard image |

### 5.4 GUI Dialogs
**File:** `runtime/src/system/dialogs.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `_MESSAGEBOX(title, msg, type)` | [ ] | Message box |
| `_INPUTBOX$(title, prompt, default)` | [ ] | Input dialog |
| `_OPENFILEDIALOG$(title, filter)` | [ ] | File open dialog |
| `_SAVEFILEDIALOG$(title, filter)` | [ ] | File save dialog |
| `_SELECTFOLDERDIALOG$(title)` | [ ] | Folder dialog |
| `_COLORCHOOSERDIALOG(default)` | [ ] | Color picker |
| `_NOTIFYPOPUP title, msg, icon` | [ ] | Notification |

### 5.5 Error Handling
**File:** `runtime/src/system/error.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `ON ERROR GOTO label` | [~] | Set error handler |
| `RESUME` | [~] | Resume after error |
| `RESUME NEXT` | [~] | Resume next line |
| `RESUME label` | [~] | Resume at label |
| `ERR` | [~] | Error code |
| `ERL` | [~] | Error line |
| `_ERRORLINE` | [~] | Error line (int64) |
| `_ERRORMESSAGE$` | [~] | Error message |
| `_INCLERRORLINE` | [~] | Include error line |
| `_INCLERRORFILE$` | [~] | Include file name |
| `ERROR n` | [~] | Generate error |

---

## Phase 6: Networking

### 6.1 HTTP
**File:** `runtime/src/network/http.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `_OPENCLIENT("HTTP:url")` | [ ] | Open HTTP connection |
| `_CONNECTED(handle)` | [ ] | Check connected |
| `_STATUSCODE(handle)` | [~] | HTTP status code |
| `GET #handle, , data` | [ ] | Read data |
| `CLOSE #handle` | [ ] | Close connection |

### 6.2 TCP/IP
**File:** `runtime/src/network/tcp.rs`

| Function | Status | Description |
|----------|--------|-------------|
| `_OPENHOST("TCP/IP:port")` | [ ] | Open server |
| `_OPENCONNECTION(host)` | [ ] | Accept connection |
| `_OPENCLIENT("TCP/IP:port:addr")` | [ ] | Connect to server |
| `_CONNECTED(handle)` | [ ] | Check connected |
| `_CONNECTIONADDRESS$(handle)` | [ ] | Client address |
| `GET #handle, , data` | [ ] | Receive data |
| `PUT #handle, , data` | [ ] | Send data |

---

## Recommended Crates

| Category | Crate | Purpose |
|----------|-------|---------|
| Graphics | `sdl2` | Window, rendering, input |
| Audio | `rodio` or `miniaudio` | Sound playback |
| Images | `image` | Image loading/saving |
| Fonts | `rusttype` or `fontdue` | Font rendering |
| HTTP | `ureq` or `reqwest` | HTTP client |
| TCP | `std::net` | TCP networking |
| Dialogs | `rfd` | Native file dialogs |
| Clipboard | `arboard` | Clipboard access |
| Time | `std::time` + `chrono` | Time functions |

---

## Implementation Order

1. **Week 1-2: Core I/O**
   - Real keyboard input (non-blocking INKEY$, _KEYHIT)
   - Real console output
   - Complete file I/O

2. **Week 3-4: Graphics Foundation**
   - SDL2 window management
   - Screen modes (at least mode 0, 12, 13, 32-bit)
   - Basic drawing (PSET, LINE, CIRCLE)
   - CLS, COLOR

3. **Week 5-6: Graphics Complete**
   - Image loading/saving
   - _PUTIMAGE, GET/PUT
   - Text rendering with fonts
   - Alpha blending

4. **Week 7-8: Audio**
   - Basic SOUND/BEEP
   - _SNDOPEN, _SNDPLAY family
   - PLAY command (music strings)

5. **Week 9-10: Input Complete**
   - Mouse input
   - Game controller support
   - Event handling

6. **Week 11-12: System Integration**
   - Clipboard
   - GUI dialogs
   - Networking
   - Polish and testing

---

## Success Criteria

1. **Bootstrap:** QB64pe compiles itself and runs to completion
2. **Test Suite:** All qbasic_testcases pass
3. **Feature Parity:** All QB64pe functions implemented
4. **Performance:** Comparable to QB64pe execution speed
