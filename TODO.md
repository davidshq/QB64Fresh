# QB64Fresh TODO

*Last updated: 2026-01-19*

A prioritized roadmap for QB64Fresh development. For completed features, see [TODO-completed.md](TODO-completed.md).

---

## Phase 1: Language Completeness - Remaining Items

### Error Handling
- [ ] Add error code constants (standard error codes are used)

### Missing Built-in Functions
- [ ] `DEF SEG` statement (legacy, low priority)
- [ ] `PEEK()` and `POKE` (memory access, may be limited/simulated)

### Variable/Scope Enhancements
- [ ] `SHARED` in module-level scope (partially supported via DIM SHARED)
- [ ] `STATIC` arrays in procedures
- [ ] Proper `OPTION BASE` support

### DEF FN Support
- [ ] Implement multi-line `DEF FN` (QB64 extension)

---

## Phase 2: Core Extensions - Remaining Items

### Conditional Compilation
- [ ] `$INCLUDE` - actually read and parse included files
- [ ] Built-in constants: `WIN`, `LINUX`, `MAC`, `32BIT`, `64BIT`

### Date/Time Enhancements
- [ ] `_AUTODISPLAY` / `_DISPLAY` timing

---

## Phase 3: Graphics System - Remaining Items

### Partially Implemented
- [ ] Text rendering - currently 8x8 bitmap font (needs SDL2_ttf for TrueType)
- [ ] `_PRINTWIDTH` function
- [ ] GET/PUT graphics arrays - structure exists, needs testing
- [ ] VIEW PRINT - text viewport (structure exists, needs testing)

### Not Yet Implemented
- [ ] Alpha blending support
- [ ] Hardware acceleration option
- [ ] Multiple screen pages

---

## Phase 4: Sound System - Remaining Items

(All core features complete - see TODO-completed.md)

---

## Phase 5: Advanced Features - Remaining Items

### C Library Integration
- [ ] Automatic header parsing
- [ ] Type marshalling for complex C types
- [ ] Callback support

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
- [ ] Compatibility tests against QB64 programs (16 fixture files, need more)
- [ ] Port QB4.5 test cases from QB64pe
- [ ] Achieve 80%+ line coverage (currently 72.67%)

---

## Phase 7: Missing Language Features

### High Priority - QB4.5 Core Features

#### Print Formatting
- [ ] `LPOS(n)` function - printer position

#### Memory/Legacy
- [ ] `BLOAD` / `BSAVE` statements - binary load/save to memory
- [ ] `VARPTR()` / `VARPTR$()` / `VARSEG()` - memory address functions
- [ ] `SADD()` function - string address
- [ ] `SETMEM` statement - set available memory
- [ ] `SEG` clause - segment for CALL ABSOLUTE

#### File System
- [ ] `FILEATTR()` function - file attributes

#### Type Conversion (Microsoft Binary Format)
- [ ] `CVDMBF()` / `CVSMBF()` functions - convert MBF strings to numbers
- [ ] `MKDMBF$()` / `MKSMBF$()` functions - convert numbers to MBF strings

#### Procedure Calling
- [ ] `ABSOLUTE` clause - call machine language routine
- [ ] `CALLS` statement - call with far pointers
- [ ] `CDECL` clause - C calling convention

#### Event Handling
- [ ] `KEY(n)` function - check key trap status
- [ ] `ON KEY(n) GOSUB` - key event handler (parser ready, needs ON statement integration)
- [ ] `COM` statement - serial port event trapping
- [ ] `ON COM(n) GOSUB` - serial port event handler
- [ ] `PEN` statement - light pen event trapping
- [ ] `ON PEN GOSUB` - light pen event handler
- [ ] `ON STRIG(n) GOSUB` - joystick trigger handler (parser ready, needs ON statement integration)
- [ ] `UEVENT` - user-defined event
- [ ] `ON UEVENT GOSUB` - user event handler
- [ ] `SIGNAL` statement - signal handling

#### Input Devices (QB4.5)
- [ ] `STICK(n)` function - joystick position
- [ ] `STRIG(n)` function - joystick trigger state

#### System Interrupts (Legacy)
- [ ] `INTERRUPT` / `INTERRUPTX` statements - call system interrupts

#### Serial I/O
- [ ] `ERDEV` / `ERDEV$` - error device information
- [ ] `IOCTL` / `IOCTL$` - device control

#### Miscellaneous QB4.5
- [ ] `FRE()` function - free memory
- [ ] `FREE` statement - free string space
- [ ] `WAIT` statement - wait for port
- [ ] `INP()` / `OUT` - port I/O (may need sandboxing)
- [ ] `OFF` keyword - turn off event trapping (KEY OFF, etc.)
- [ ] `ONLY` keyword - exclusive file access (OPEN...FOR...ONLY)
- [ ] `SMOOTH` keyword - graphics smooth mode
- [ ] `STRETCH` keyword - graphics stretch mode
- [ ] `CUSTOMTYPE` - TYPE declaration modifier

### Medium Priority - QB64 Extensions

#### String Functions
- [ ] `_CV(type, string$)` function - generic convert string to type
- [ ] `_MK$(type, value)` function - generic convert value to string

#### Error Handling (Extended)
- [ ] `_INCLERRORFILE$` / `_INCLERRORLINE` - include file error info
- [ ] `_EXIT` statement - exit program with code

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
- [ ] `_SCREENEXISTS` function - check if window exists
- [ ] `_SCREENPRINT` statement - print screen contents
- [ ] `_ICON` statement - set window icon (parser ready, dual-use with function)
- [ ] `_HIDE` / `_SHOW` statements - hide/show window (alias)
- [ ] `_ONTOP` statement - set window always on top

#### Unicode Font Support
- [ ] `_UCHARPOS` function - Unicode character position
- [ ] `_UFONTHEIGHT` function - Unicode font height
- [ ] `_ULINESPACING` function - Unicode line spacing
- [ ] `_UPRINTSTRING` statement - Unicode print string
- [ ] `_UPRINTWIDTH` function - Unicode print width
- [ ] `_MAPUNICODE` statement - map Unicode code points

#### Color Functions
- [ ] `_RED(color)` / `_GREEN(color)` / `_BLUE(color)` / `_ALPHA(color)` - color components
- [ ] `_RED32` / `_GREEN32` / `_BLUE32` / `_ALPHA32` - 32-bit color components
- [ ] `_PALETTECOLOR(index, color)` statement - set palette entry
- [ ] `_COPYPALETTE` statement - copy palette between images
- [ ] `_DEFAULTCOLOR` / `_BACKGROUNDCOLOR` functions
- [ ] `_SETALPHA` statement - set image alpha
- [ ] `_HSB32(h, s, b)` / `_HSBA32(h, s, b, a)` - HSB color creation
- [ ] `_HUE32(color)` / `_SATURATION32(color)` / `_BRIGHTNESS32(color)` - HSB components
- [ ] `_PIXELSIZE` function - bytes per pixel

#### Graphics (Extended)
- [ ] `_SAVEIMAGE(file$, handle)` statement - save image to file
- [ ] `_MAPTRIANGLE` statement - 3D triangle mapping
- [ ] `_BLEND` / `_DONTBLEND` statements - alpha blending control
- [ ] `_CLEARCOLOR` statement - set transparent color
- [ ] `_CLIP` keyword - clipping mode for _PUTIMAGE
- [ ] `_DEPTHBUFFER` statement - depth buffer control
- [ ] `_SMOOTH` / `_SMOOTHSHRUNK` / `_SMOOTHSTRETCHED` - image scaling modes
- [ ] `_HARDWARE` / `_HARDWARE1` / `_SOFTWARE` - rendering modes
- [ ] `_DISPLAYORDER` statement - set display layer order
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
- [ ] `_PRINTMODE` statement - set print mode (fill/keep/only)

#### Device Input (Gamepad/Joystick)
- [ ] `_DEVICES` function - number of input devices
- [ ] `_DEVICE$(n)` function - device name
- [ ] `_DEVICEINPUT` function - check for device input
- [ ] `_LASTAXIS(device)` / `_LASTBUTTON(device)` / `_LASTWHEEL(device)` - device capabilities
- [ ] `_AXIS(device, axis)` function - read axis value
- [ ] `_BUTTON(device, button)` function - read button state
- [ ] `_BUTTONCHANGE(device, button)` function - button state changed
- [ ] `_WHEEL(device, wheel)` function - read wheel value
- [ ] `_LASTHANDLER` function - get last event handler
- [ ] `_NEWHANDLER` statement - create new event handler

#### Mouse (Extended)
- [ ] `_MOUSEHIDDEN` function - check if mouse cursor is hidden

#### Clipboard (Extended)
- [ ] `_CLIPBOARDIMAGE` function - get image from clipboard

#### Dialog Boxes
- [ ] `_COLORCHOOSERDIALOG` function - color picker
- [ ] `_NOTIFYPOPUP` function - system notification

#### Drag and Drop
- [ ] `_ACCEPTFILEDROP` statement - enable file drop
- [ ] `_TOTALDROPPEDFILES` function - count dropped files
- [ ] `_DROPPEDFILE` / `_DROPPEDFILE$` functions - get dropped file
- [ ] `_FINISHDROP` statement - complete drop handling

#### Hash and Encoding Functions
- [ ] `_CRC32(data$)` function - CRC32 checksum
- [ ] `_MD5$(data$)` function - MD5 hash
- [ ] `_ADLER32(data$)` function - Adler32 checksum
- [ ] `_BASE64ENCODE$(data$)` / `_BASE64DECODE$(data$)` - Base64
- [ ] `_DEFLATE$(data$)` / `_INFLATE$(data$)` - compression
- [ ] `_ENCODEURL$(url$)` / `_DECODEURL$(url$)` - URL encoding

#### Memory (Extended)
- [ ] `_MEMEXISTS(mem)` function - check if memory valid
- [ ] `_MEMELEMENT(mem, index)` function - get element offset
- [ ] `_MEMIMAGE(handle)` function - get image memory
- [ ] `_MEMSOUND(handle)` function - get sound memory

#### Sound (Extended)
- [ ] `_SNDCOPY(handle)` function - copy sound
- [ ] `_SNDPLAYCOPY(handle)` statement - play copy
- [ ] `_SNDPLAYFILE(file$)` statement - quick play file
- [ ] `_SNDRAWDONE` function - raw buffer done
- [ ] `_SNDLIMIT(handle, seconds)` statement - limit sound length
- [ ] `_SNDNEW(frames, channels, bits)` function - create sound buffer
- [ ] `_SNDRAWBATCH` statement - batch raw samples
- [ ] `_WAVE` keyword - waveform type for sound synthesis
- [ ] `_MIDISOUNDBANK` statement - set MIDI soundbank

#### Console Mode
- [ ] `$CONSOLE` / `$CONSOLE:ONLY` metacommands
- [ ] `_CONSOLECURSOR` statement - console cursor control
- [ ] `_CONSOLEFONT` statement - console font
- [ ] `_CONSOLEINPUT` function - console input available
- [ ] `_CONTROLCHR` statement - control character handling
- [ ] `_ECHO` statement - console output

#### Logging (QB64 extension)
- [ ] `_LOGTRACE` / `_LOGINFO` / `_LOGWARN` / `_LOGERROR` statements
- [ ] `_LOGMINLEVEL` statement - set minimum log level

#### Resize Events
- [ ] `$RESIZE:ON` / `$RESIZE:OFF` metacommands
- [ ] `$RESIZE:STRETCH` / `$RESIZE:SMOOTH` metacommands
- [ ] `_RESIZE` function - check for resize
- [ ] `_RESIZEHEIGHT` / `_RESIZEWIDTH` functions - new dimensions
- [ ] `_SCALEDHEIGHT` / `_SCALEDWIDTH` functions - scaled dimensions

#### File I/O (Extended)
- [ ] `_EMBEDDED$(name$)` function - get embedded file data
- [ ] `$EMBED:'filename'` metacommand - embed file in executable
- [ ] `_FULLPATH$(path$)` function - get full path
- [ ] `_FILES$(pattern$)` function - file listing iterator

#### Miscellaneous QB64 Features
- [ ] `_ANDALSO` / `_ORELSE` operators - short-circuit evaluation
- [ ] `OPTION _EXPLICIT` / `OPTION _EXPLICITARRAY` - require declarations
- [ ] `SELECT EVERYCASE` - check all cases
- [ ] `_FPS` function - current frame rate
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

**Design Decisions Needed:**
- How to handle `PEEK`/`POKE` in a safe manner
- Memory model for `_MEM` operations
