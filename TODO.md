# QB64Fresh TODO

*Last updated: 2026-01-19*

A prioritized roadmap for QB64Fresh development. Items are ordered from most granular (near-term) to high-level (long-term).

---

## Phase 1: Language Completeness (Near-Term) ✅ COMPLETED
- File I/O (Framework exists, needs code generation)
- Keyboard Input

### Error Handling
- [ ] Add error code constants (standard error codes are used)

### Computed Control Flow

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

## Phase 2: Core Extensions (Medium-Term) ✅ COMPLETED

### Conditional Compilation
- [ ] `$INCLUDE` - actually read and parse included files
- [ ] Built-in constants: `WIN`, `LINUX`, `MAC`, `32BIT`, `64BIT`

- Memory Operations
- String Enhancements

### Date/Time Enhancements
- [ ] `_AUTODISPLAY` / `_DISPLAY` timing

---

## Phase 3: Graphics System (In Progress)

### Graphics Architecture ✅
```
runtime/src/
├── graphics/
│   ├── mod.rs      # GraphicsBackend trait + global instance ✅
│   ├── error.rs    # GraphicsError, GraphicsErrorKind ✅
│   ├── mock.rs     # MockGraphicsBackend for testing ✅
│   └── sdl2.rs     # SDL2Backend (actual rendering) ✅
├── graphics_ffi.rs # C FFI layer (qb_gfx_*, qb_mouse_*, etc.) ✅
```

### Core Graphics ✅
- [x] PSET, POINT - pixel plotting and reading
- [x] LINE - line drawing with STEP support
- [x] BOX - rectangle outline and filled (via LINE B/BF)
- [x] CIRCLE - circle outline and filled (midpoint algorithm)
- [x] PAINT - flood fill (scanline algorithm)
- [x] CLS, COLOR, LOCATE - screen/color management
- [x] _DISPLAY - double-buffered rendering
- [x] SDL2 backend with pixel buffer for fast POINT()
- [x] Mouse input (_MOUSEX, _MOUSEY, _MOUSEBUTTON, _MOUSEINPUT, _MOUSEWHEEL, etc.)

### Extended Graphics ✅
- [x] VIEW - viewport clipping with fill/border colors
- [x] WINDOW - world coordinate system (Cartesian and screen modes)
- [x] DRAW - turtle graphics (U/D/L/R/E/F/G/H/M/B/N/A/T/C/S/P commands)
- [x] Image buffers (_NEWIMAGE, _LOADIMAGE, _PUTIMAGE, _FREEIMAGE)
- [x] _SOURCE, _DEST - image handle selection
- [x] _COPYIMAGE, _SCREENIMAGE - image copying

### Partially Implemented ⚠️
- [ ] Text rendering - currently 8x8 bitmap font (needs SDL2_ttf for TrueType)
- [ ] `_PRINTWIDTH` function
- [ ] GET/PUT graphics arrays - structure exists, needs testing
- [ ] VIEW PRINT - text viewport (structure exists, needs testing)

### Not Yet Implemented
- [ ] Alpha blending support
- [ ] Hardware acceleration option
- [ ] Multiple screen pages

---

## Phase 4: Sound System (In Progress)

### Audio Architecture (mirrors Graphics architecture) ✅

The audio system uses a trait-based backend abstraction, allowing different audio
libraries to be swapped at compile time via Cargo feature flags. This follows the
same pattern as the graphics system.

```
runtime/src/
├── audio/
│   ├── mod.rs           # AudioBackend trait + global instance ✅
│   ├── error.rs         # AudioError, AudioErrorKind ✅
│   ├── mock.rs          # MockAudioBackend for testing ✅
│   └── rodio_backend.rs # RodioBackend (actual audio output) ✅
├── audio_ffi.rs         # C FFI layer (qb_snd_*, qb_beep, etc.) ✅
```

**Backend Selection (Cargo.toml features):**
- `audio-rodio` - Default. Pure Rust audio library via cpal
- `audio-mock` - For headless testing (no actual audio output)

**Why rodio (chosen over miniaudio)?**
- Pure Rust (no C dependencies, better safety)
- Cross-platform (ALSA/PulseAudio/WASAPI/CoreAudio via cpal)
- Good format support (WAV, MP3, OGG, FLAC)
- Simple API, well-maintained
- Note: miniaudio was considered (used by QB64-PE) but rodio integrates better with Rust

### Audio Backend Infrastructure ✅
- [x] Implement `RodioBackend` (actual audio playback) - pure Rust, cross-platform
- [x] Add feature flags to `runtime/Cargo.toml` (`audio-rodio`, `audio-mock`)
- Note: miniaudio backend not needed - rodio provides equivalent functionality

- Classic BASIC Sound (Parser, Semantic, Codegen, FFI complete)
- QB64 Sound Extensions (Parser, Semantic, Codegen, FFI complete)
- Raw Audio Synthesis (FFI complete, backend needs implementation)

### Audio Format Support ✅ (via rodio)
- [x] WAV (PCM)
- [x] MP3
- [x] OGG Vorbis
- [x] FLAC

---

## Phase 5: Advanced Features (Long-Term) - In Progress

### C Library Integration ✅ (Parser/Semantic/Codegen complete)
- [ ] Automatic header parsing
- [ ] Type marshalling for complex C types
- [ ] Callback support

### Networking (QB64 Extensions) ✅ (Parser/Semantic/Codegen/Runtime complete)
- [ ] Network stream I/O (PUT/GET with network handles)

### Input Devices ✅ (Parser/Semantic/Codegen complete - backend integration pending)
- [ ] Joystick/gamepad support
- [ ] Touch input support

- Clipboard ✅ (Parser/Semantic/Codegen complete - backend integration pending)
- System Integration ✅

### Multi-threading (QB64 Extension)
- [ ] `_THREAD` support
- [ ] Thread synchronization primitives

---

## Phase 6: Tooling & Ecosystem (Long-Term)

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
- [x] Expand test suite for all built-ins (272 integration tests) ✅
- [ ] Compatibility tests against QB64 programs (16 fixture files, need more)
- [ ] Port QB4.5 test cases from QB64pe
- [ ] Achieve 80%+ line coverage (currently 72.67%)

---

## Phase 7: Missing Language Features (Discovered 2026-01-18)

The following features were identified by comparing the QB64pe syntax highlighter keyword list
against the current QB64Fresh implementation. Organized by priority and category.

### High Priority - QB4.5 Core Features

#### Timing & Flow Control
- [ ] `RUN` statement - run program
- [ ] `CHAIN` statement - run another program, optionally passing variables
- [ ] `TROFF` / `TRON` statements - debug trace off/on

#### Print Formatting
- [ ] `LPRINT` statement - printer output
- [ ] `LPOS(n)` function - printer position

#### Memory/Legacy
- [ ] `BLOAD` / `BSAVE` statements - binary load/save to memory
- [ ] `CLEAR` statement - clear memory/variables
- [ ] `VARPTR()` / `VARPTR$()` / `VARSEG()` - memory address functions
- [ ] `SADD()` function - string address
- [ ] `SETMEM` statement - set available memory
- [ ] `SEG` clause - segment for CALL ABSOLUTE

#### File System
- [ ] `FILES` statement - directory listing display
- [ ] `FIELD` statement - define record fields for random access
- [ ] `LSET` / `RSET` statements - left/right justify in field
- [ ] `FILEATTR()` function - file attributes
- [ ] `RESET` statement - close all open files

- Arrays

#### Type Conversion (Microsoft Binary Format)
- [ ] `CVDMBF()` / `CVSMBF()` functions - convert MBF strings to numbers
- [ ] `MKDMBF$()` / `MKSMBF$()` functions - convert numbers to MBF strings

#### Procedure Calling
- [ ] `ABSOLUTE` clause - call machine language routine
- [ ] `CALLS` statement - call with far pointers
- [ ] `CDECL` clause - C calling convention

#### Event Handling
- [ ] `KEY` statement - key event trapping (KEY ON/OFF/STOP)
- [ ] `KEY(n)` function - check key trap status
- [ ] `ON KEY(n) GOSUB` - key event handler
- [ ] `COM` statement - serial port event trapping
- [ ] `ON COM(n) GOSUB` - serial port event handler
- [ ] `PEN` statement - light pen event trapping
- [ ] `ON PEN GOSUB` - light pen event handler
- [ ] `STRIG` statement - joystick trigger event trapping
- [ ] `ON STRIG(n) GOSUB` - joystick trigger handler
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
- [x] `PALETTE` statement - set palette colors ✅
- [x] `PCOPY` statement - copy screen page ✅
- [x] `PMAP()` function - coordinate mapping ✅
- [ ] `OFF` keyword - turn off event trapping (KEY OFF, etc.)
- [ ] `ONLY` keyword - exclusive file access (OPEN...FOR...ONLY)
- [ ] `SMOOTH` keyword - graphics smooth mode
- [ ] `STRETCH` keyword - graphics stretch mode
- [ ] `CUSTOMTYPE` - TYPE declaration modifier
- [ ] `ENDIF` - alternative END IF syntax (no space)

### Medium Priority - QB64 Extensions

- Keyboard Input (Important for games)
- Timing Functions
- Math Functions
- Trigonometric (Extended)
- Bitwise Operations ✅

#### String Functions
- [ ] `_CV(type, string$)` function - generic convert string to type
- [ ] `_MK$(type, value)` function - generic convert value to string

#### Error Handling (Extended)
- [ ] `_INCLERRORFILE$` / `_INCLERRORLINE` - include file error info
- [ ] `_ASSERT` statement - assertions
- [ ] `$ASSERTS` metacommand - enable assertions
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
- [ ] `_SCREENICON` function - check if window is minimized
- [ ] `_SCREENPRINT` statement - print screen contents
- [ ] `_ALLOWFULLSCREEN` statement - allow/disallow fullscreen toggle
- [x] `_TITLE$` - get window title (codegen complete, backend pending) ✅
- [ ] `_TITLE` statement - set window title
- [ ] `_ICON` statement - set window icon
- [ ] `_HIDE` / `_SHOW` statements - hide/show window (alias)
- [ ] `_ONTOP` statement - set window always on top
- [x] `_WINDOWHANDLE` function - get native window handle (codegen complete) ✅
- [x] `_WINDOWHASFOCUS` function - check if window has focus (codegen complete) ✅

#### Font Support

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
- [ ] `_CONSOLE` statement - enable console
- [ ] `_CONSOLETITLE` statement - set console title
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
- [ ] `_READFILE$(file$)` function - read entire file
- [ ] `_WRITEFILE(file$, content$)` statement - write entire file
- [ ] `_EMBEDDED$(name$)` function - get embedded file data
- [ ] `$EMBED:'filename'` metacommand - embed file in executable
- [ ] `_FULLPATH$(path$)` function - get full path
- [ ] `_FILES$(pattern$)` function - file listing iterator

#### Miscellaneous QB64 Features
- [ ] `_ANDALSO` / `_ORELSE` operators - short-circuit evaluation
- [ ] `_NEGATE` function - negate value
- [ ] `OPTION _EXPLICIT` / `OPTION _EXPLICITARRAY` - require declarations
- [ ] `SELECT EVERYCASE` - check all cases
- [x] `_TRUE` / `_FALSE` constants ✅
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
- [ ] **Implicit SUB calls without parentheses** - `Greet "World"` doesn't parse; must use
      `CALL Greet("World")` instead. This is a parser limitation (discovered session 021).
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
