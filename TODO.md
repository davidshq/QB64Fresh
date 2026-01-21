# QB64Fresh TODO

*Last updated: 2026-01-20 (Session 031)*

A prioritized roadmap for QB64Fresh development. For completed features, see [TODO-completed.md](TODO-completed.md).

---

## Phase 1: Language Completeness - Remaining Items

### Missing Built-in Functions
- [ ] `POKE` (memory write - may be limited/simulated for safety)

---

## Phase 2: Core Extensions ✅ COMPLETE

All Phase 2 items have been completed - see TODO-completed.md.

---

## Phase 3: Graphics System - Remaining Items

### Not Yet Implemented
- [ ] Alpha blending support
- [ ] Hardware acceleration option
- [ ] Multiple screen pages
- [ ] GET/PUT full pixel copying implementation (stubs exist, need actual pixel operations)

---

## Phase 4: Sound System ✅ COMPLETE

All sound features have been implemented - see TODO-completed.md.

---

## Phase 5: Advanced Features - Remaining Items

### C Library Integration ✅ COMPLETE

All C Library Integration items completed - see TODO-completed.md.

### Networking
- [ ] Network stream I/O (PUT/GET with network handles)

### Input Devices
- [ ] Joystick/gamepad support
- [ ] Touch input support

### Multi-threading (QB64 Extension)
- [ ] `_THREAD` support
- [ ] Thread synchronization primitives

---

## Phase 6: Tooling & Ecosystem

### Language Server Protocol
- [ ] Implement full LSP server
- [ ] Go-to-definition
- [ ] Find references
- [ ] Hover information
- [ ] Code completion
- [ ] Diagnostics (real-time error checking)
- [ ] Signature help

### Debugging
- [ ] Source-level debugging support
- [ ] Breakpoints
- [ ] Variable inspection
- [ ] Step execution

### Optimization
- [ ] Dead code elimination
- [ ] Constant folding (expand current)
- [ ] Loop optimization
- [ ] Inline small functions

### Documentation
- [ ] Language reference documentation
- [ ] Migration guide from QB64
- [ ] Tutorial/getting started guide
- [ ] Example programs

### Testing (See TESTING_INFRASTRUCTURE_PLAN.md for details)
- [x] Achieve 80%+ line coverage (**81.63%** achieved!) ✅
- [x] QB4.5 test cases from QB64pe (**114/115 = 99.1%** passing, excluding open_gl) ✅
- [ ] Compatibility tests against QB64 programs (16 fixture files, need more)

---

## Phase 7: Missing Language Features

### High Priority - QB4.5 Core Features

#### Remaining QB4.5 Items
*(All completed - see TODO-completed.md)*

### Medium Priority - QB64 Extensions

#### String Functions
- [ ] `_CV(type, string$)` function - generic convert string to type
- [ ] `_MK$(type, value)` function - generic convert value to string

#### Error Handling (Extended)
- [ ] `_INCLERRORFILE$` / `_INCLERRORLINE` - include file error info
- [x] `_EXIT` statement - exit program with code ✅ (Session 031)

#### Utility Functions
- [ ] `_CAST(type, value)` function - explicit type cast
- [ ] `_DEFINE` statement - define default variable types by letter range
- [ ] `_STATUSCODE` function - status code from last operation

#### Networking (Extended)
- [ ] `_CONNECTIONADDRESS(handle)` function - get connection IP address
- [ ] `_CONNECTIONADDRESS$(handle)` function - get connection IP as string
- [ ] `_DONTWAIT` keyword - non-blocking network operations

### Lower Priority - QB64 Advanced Extensions

#### Desktop/Window Info
- [x] `_SCREENEXISTS` function - check if window exists ✅ (Session 031)
- [ ] `_SCREENPRINT` statement - print screen contents
- [x] `_ICON` statement - set window icon ✅ (Session 031)
- [x] `_HIDE` / `_SHOW` statements - hide/show window ✅ (Session 031)
- [x] `_ONTOP` statement - set window always on top ✅ (Session 031)

#### Unicode Font Support
- [ ] `_UCHARPOS` function - Unicode character position
- [ ] `_UFONTHEIGHT` function - Unicode font height
- [ ] `_ULINESPACING` function - Unicode line spacing
- [ ] `_UPRINTSTRING` statement - Unicode print string
- [ ] `_UPRINTWIDTH` function - Unicode print width
- [ ] `_MAPUNICODE` statement - map Unicode code points

#### Color Functions
- [x] `_RED(color)` / `_GREEN(color)` / `_BLUE(color)` / `_ALPHA(color)` - color components ✅ (Session 031)
- [x] `_RED32` / `_GREEN32` / `_BLUE32` / `_ALPHA32` - 32-bit color components ✅ (Session 031)
- [x] `_PALETTECOLOR(index, color)` statement - set palette entry ✅ (Session 031)
- [x] `_COPYPALETTE` statement - copy palette between images ✅ (Session 031)
- [x] `_DEFAULTCOLOR` / `_BACKGROUNDCOLOR` functions ✅ (Session 031)
- [x] `_SETALPHA` statement - set image alpha ✅ (Session 031)
- [ ] `_HSB32(h, s, b)` / `_HSBA32(h, s, b, a)` - HSB color creation
- [ ] `_HUE32(color)` / `_SATURATION32(color)` / `_BRIGHTNESS32(color)` - HSB components
- [x] `_PIXELSIZE` function - bytes per pixel ✅ (Session 031)

#### Graphics (Extended)
- [ ] `_SAVEIMAGE(file$, handle)` statement - save image to file
- [ ] `_MAPTRIANGLE` statement - 3D triangle mapping
- [x] `_BLEND` / `_DONTBLEND` statements - alpha blending control ✅ (Session 031)
- [x] `_CLEARCOLOR` statement - set transparent color ✅ (Session 031)
- [ ] `_CLIP` keyword - clipping mode for _PUTIMAGE
- [x] `_DEPTHBUFFER` statement - depth buffer control ✅ (Session 031)
- [ ] `_SMOOTH` / `_SMOOTHSHRUNK` / `_SMOOTHSTRETCHED` - image scaling modes
- [ ] `_HARDWARE` / `_HARDWARE1` / `_SOFTWARE` - rendering modes
- [x] `_DISPLAYORDER` statement - set display layer order ✅ (Session 031)
- [ ] `_GLRENDER` statement - OpenGL render mode
- [ ] `_SQUAREPIXELS` keyword - square pixels mode
- [ ] `_SEAMLESS` keyword - seamless image tiling
- [ ] `_STRETCH` keyword - stretch mode for _PUTIMAGE
- [ ] `_ANTICLOCKWISE` / `_CLOCKWISE` keywords - drawing direction
- [ ] `_BEHIND` keyword - draw behind existing content
- [ ] `_KEEPBACKGROUND` keyword - preserve background
- [ ] `_FILLBACKGROUND` keyword - fill background
- [ ] `_ONLYBACKGROUND` keyword - affect only background
- [ ] `_PRINTIMAGE` statement - print to image instead of screen
- [x] `_PRINTMODE` statement - set print mode (fill/keep/only) ✅ (Session 031)

#### Device Input (Gamepad/Joystick)
- [x] `_DEVICES` function - number of input devices ✅ (Session 031)
- [x] `_DEVICE$(n)` function - device name ✅ (Session 031)
- [x] `_DEVICEINPUT` function - check for device input ✅ (Session 031)
- [x] `_LASTAXIS(device)` / `_LASTBUTTON(device)` / `_LASTWHEEL(device)` - device capabilities ✅ (Session 031)
- [x] `_AXIS(device, axis)` function - read axis value ✅ (Session 031)
- [x] `_BUTTON(device, button)` function - read button state ✅ (Session 031)
- [x] `_BUTTONCHANGE(device, button)` function - button state changed ✅ (Session 031)
- [x] `_WHEEL(device, wheel)` function - read wheel value ✅ (Session 031)
- [ ] `_LASTHANDLER` function - get last event handler
- [ ] `_NEWHANDLER` statement - create new event handler

#### Mouse (Extended)
- [x] `_MOUSEHIDDEN` function - check if mouse cursor is hidden ✅ (Session 031)

#### Clipboard (Extended)
- [x] `_CLIPBOARDIMAGE` function - get image from clipboard ✅ (Session 031)

#### Dialog Boxes
- [x] `_COLORCHOOSERDIALOG` function - color picker ✅ (Session 031)
- [x] `_NOTIFYPOPUP` function - system notification ✅ (Session 031)

#### Drag and Drop
- [x] `_ACCEPTFILEDROP` statement - enable file drop ✅ (Session 031)
- [x] `_TOTALDROPPEDFILES` function - count dropped files ✅ (Session 031)
- [x] `_DROPPEDFILE` / `_DROPPEDFILE$` functions - get dropped file ✅ (Session 031)
- [x] `_FINISHDROP` statement - complete drop handling ✅ (Session 031)

#### Hash and Encoding Functions
- [x] `_CRC32(data$)` function - CRC32 checksum ✅ (Session 031)
- [x] `_MD5$(data$)` function - MD5 hash ✅ (Session 031)
- [x] `_ADLER32(data$)` function - Adler32 checksum ✅ (Session 031)
- [x] `_BASE64ENCODE$(data$)` / `_BASE64DECODE$(data$)` - Base64 ✅ (Session 031)
- [x] `_DEFLATE$(data$)` / `_INFLATE$(data$)` - compression ✅ (Session 031)
- [x] `_ENCODEURL$(url$)` / `_DECODEURL$(url$)` - URL encoding ✅ (Session 031)

#### Memory (Extended)
- [x] `_MEMEXISTS(mem)` function - check if memory valid ✅ (Session 031)
- [ ] `_MEMELEMENT(mem, index)` function - get element offset
- [ ] `_MEMIMAGE(handle)` function - get image memory
- [ ] `_MEMSOUND(handle)` function - get sound memory

#### Sound (Extended)
- [x] `_SNDRAWDONE` function - raw buffer done ✅ (Session 031)
- [x] `_SNDLIMIT(handle, seconds)` statement - limit sound length ✅ (Session 031)
- [ ] `_SNDNEW(frames, channels, bits)` function - create sound buffer
- [ ] `_SNDRAWBATCH` statement - batch raw samples
- [ ] `_WAVE` keyword - waveform type for sound synthesis
- [ ] `_MIDISOUNDBANK` statement - set MIDI soundbank

#### Console Mode
- [ ] `$CONSOLE` / `$CONSOLE:ONLY` metacommands
- [x] `_CONSOLECURSOR` statement - console cursor control ✅ (Session 031)
- [x] `_CONSOLEFONT` statement - console font ✅ (Session 031)
- [x] `_CONSOLEINPUT` function - console input available ✅ (Session 031)
- [x] `_CONTROLCHR` statement - control character handling ✅ (Session 031)
- [x] `_ECHO` statement - console output ✅ (Session 031)

#### Logging (QB64 extension)
- [ ] `_LOGTRACE` / `_LOGINFO` / `_LOGWARN` / `_LOGERROR` statements
- [ ] `_LOGMINLEVEL` statement - set minimum log level

#### Resize Events
- [ ] `$RESIZE:ON` / `$RESIZE:OFF` metacommands
- [ ] `$RESIZE:STRETCH` / `$RESIZE:SMOOTH` metacommands
- [x] `_RESIZE` function - check for resize ✅ (Session 031)
- [x] `_RESIZEHEIGHT` / `_RESIZEWIDTH` functions - new dimensions ✅ (Session 031)
- [x] `_SCALEDHEIGHT` / `_SCALEDWIDTH` functions - scaled dimensions ✅ (Session 031)

#### File I/O (Extended)
- [ ] `_EMBEDDED$(name$)` function - get embedded file data
- [ ] `$EMBED:'filename'` metacommand - embed file in executable
- [x] `_FULLPATH$(path$)` function - get full path ✅ (Session 031)
- [ ] `_FILES$(pattern$)` function - file listing iterator

#### Miscellaneous QB64 Features
- [x] `_ANDALSO` / `_ORELSE` operators - short-circuit evaluation ✅ (Session 031)
- [ ] `OPTION _EXPLICIT` / `OPTION _EXPLICITARRAY` - require declarations
- [ ] `SELECT EVERYCASE` - check all cases
- [x] `_FPS` function - current frame rate ✅ (Session 031)
- [x] `_FREETIMER` function - free a timer ✅ (Session 031)
- [ ] `_FREETIMER` function - free a timer

### Metacommands (Not Yet Implemented)
- [ ] `$STATIC` / `$DYNAMIC` - array allocation mode
- [ ] `$COLOR:0` / `$COLOR:32` - color mode
- [ ] `$EXEICON:'file.ico'` - executable icon
- [ ] `$VERSIONINFO:key=value` - version info
- [ ] `$MIDISOUNDFONT:'file.sf2'` - MIDI soundfont
- [ ] `$DEBUG` - enable debug mode
- [ ] `$INCLUDEONCE` - include file only once
- [ ] `$NOPREFIX` - allow keywords without underscore
- [ ] `$UNSTABLE:feature` - enable unstable features
- [ ] `$ERROR message` - compiler error
- [ ] `$FORMAT` - code formatting directive
- [ ] `$SCREENHIDE` / `$SCREENSHOW` - hide/show window on startup
- [ ] `$USELIBRARY` - use external library

### Type Modifiers & Keywords (Not Yet Implemented)
- [ ] `_BIT` type - single bit type modifier
- [ ] `_ALL` keyword - all items/modes modifier
- [ ] `_AUTO` keyword - auto display mode
- [ ] `_BLINK` keyword - text blinking mode
- [ ] `_CLEAR` statement - clear specific resource
- [ ] `_MIDDLE` keyword - middle alignment
- [ ] `_NONE` constant - none/null value
- [ ] `_OFF` keyword - off state for toggles
- [ ] `_ONLY` keyword - exclusive mode modifier
- [ ] `_TOGGLE` statement - toggle a setting

### OpenGL Commands (Intentionally Excluded)

QB64PE includes ~300+ `_GL*` commands (e.g., `_GLBEGIN`, `_GLEND`, `_GLVERTEX3F`, etc.)
for raw OpenGL access. These are **intentionally excluded** from QB64Fresh because:

1. We use SDL2/winit for graphics, not raw OpenGL
2. Raw GL commands expose implementation details that reduce portability
3. The `_MAPTRIANGLE` statement provides 3D capability without raw GL
4. Future WebGL/Vulkan backends would be incompatible with GL commands

If raw OpenGL is needed, users can use `DECLARE LIBRARY` to call OpenGL functions directly.

---

## Known Issues / Technical Debt

- [ ] Recursion: Should work but needs thorough testing
- [ ] Large array handling: Verify stack vs heap allocation
- [ ] Unicode support: Currently ASCII-focused
- [ ] Windows-specific path handling in file I/O
- [ ] **GOSUB uses GCC computed goto extension** - The GOSUB/RETURN implementation uses GCC's
      computed goto extension (`&&label` for label addresses, `goto *ptr` for indirect jumps).
      This works with GCC and Clang but NOT MSVC. For MSVC support, would need a switch-based
      dispatch table alternative. Low priority since most users compile with GCC/MinGW.

---

## Notes

**Dependencies Integrated:**
- SDL2 crate for graphics (feature: `graphics-sdl2`)
- rodio crate for audio (feature: `audio-rodio`)
- Runtime library provides file I/O, graphics, and audio

**Design Decisions Made:**
- Graphics backend: Trait-based abstraction with SDL2 as default, mock for testing
- Sound backend: Trait-based abstraction with rodio as default, mock for testing
- PEEK returns 0 (safe stub) - actual memory access would be unsafe

**Design Decisions Needed:**
- How to handle `POKE` in a safe manner (currently not implemented)
- Memory model for `_MEM` operations
