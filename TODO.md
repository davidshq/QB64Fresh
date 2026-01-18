# QB64Fresh TODO

A prioritized roadmap for QB64Fresh development. Items are ordered from most granular (near-term) to high-level (long-term).

---

## Phase 1: Language Completeness (Near-Term) ✅ COMPLETED

### File I/O (Framework exists, needs code generation)
- [x] Complete `OPEN` statement code generation (all modes: INPUT, OUTPUT, APPEND, BINARY, RANDOM)
- [x] Implement `CLOSE` statement
- [x] Implement `PRINT #` (file output)
- [x] Implement `INPUT #` (file input)
- [x] Implement `LINE INPUT #`
- [x] Implement `WRITE #` statement
- [x] Implement `GET` and `PUT` for binary/random access
- [x] Implement `SEEK` statement
- [x] Implement `LOF()` function (length of file)
- [x] Implement `EOF()` function (end of file)
- [x] Implement `LOC()` function (current position)
- [x] Implement `FREEFILE` function
- [x] Add file handle tracking to runtime

### Keyboard Input
- [x] Implement `INKEY$` function
- [x] Implement `INPUT$()` function (read n characters)
- [x] Implement `_KEYHIT` function (QB64 extension)
- [x] Implement `_KEYDOWN` function (QB64 extension - stub, always returns 0)
- [x] Implement `_KEYCLEAR` statement (QB64 extension)

### Error Handling
- [x] Implement `ON ERROR GOTO` statement
- [x] Implement `RESUME` statement (RESUME, RESUME NEXT, RESUME label)
- [x] Implement `ERR` variable
- [x] Implement `ERL` variable (error line)
- [x] Implement `ERROR` statement (raise error)
- [ ] Add error code constants (standard error codes are used)

### Computed Control Flow
- [x] Implement `ON n GOTO` statement
- [x] Implement `ON n GOSUB` statement

### Missing Built-in Functions
- [x] `ENVIRON$()` - get environment variable
- [x] `COMMAND$` - command line arguments
- [x] `_OS$` - operating system identifier
- [x] `_CWD$` - current working directory
- [x] `_STARTDIR$` - program start directory
- [x] `SWAP` statement (parser + semantic: requires exact type match)
- [ ] `DEF SEG` statement (legacy, low priority)
- [ ] `PEEK()` and `POKE` (memory access, may be limited/simulated)

### Variable/Scope Enhancements
- [x] `COMMON` statement (shared variables between modules)
- [ ] `SHARED` in module-level scope (partially supported via DIM SHARED)
- [ ] `STATIC` arrays in procedures
- [x] `REDIM` with `_PRESERVE`
- [ ] Proper `OPTION BASE` support

### DEF FN Support
- [x] Implement `DEF FN` single-line functions
- [ ] Implement multi-line `DEF FN` (QB64 extension)

---

## Phase 2: Core Extensions (Medium-Term) ✅ COMPLETED

### Conditional Compilation
- [x] `$IF` / `$ELSE` / `$ELSEIF` / `$END IF` directives
- [x] `$LET` directive for compile-time variables
- [ ] `$INCLUDE` - actually read and parse included files
- [x] `$CHECKING` directive
- [ ] Built-in constants: `WIN`, `LINUX`, `MAC`, `32BIT`, `64BIT`

### Memory Operations
- [x] `_MEM` type full support
- [x] `_MEMNEW` function
- [x] `_MEMFREE` statement
- [x] `_MEMGET` / `_MEMPUT`
- [x] `_MEMCOPY`
- [x] `_MEMFILL`
- [x] `_OFFSET` pointer arithmetic

### String Enhancements
- [x] `_INSTRREV` function
- [x] `TRIM$` / `_TRIM$` functions (both map to same implementation)
- [x] `STRING$()` function
- [x] `MKI$`, `MKL$`, `MKS$`, `MKD$` (pack numbers to strings)
- [x] `CVI`, `CVL`, `CVS`, `CVD` (unpack strings to numbers)

### Date/Time Enhancements
- [x] `DATE$` (classic BASIC format: MM-DD-YYYY)
- [x] `TIME$` (classic BASIC format: HH:MM:SS)
- [x] `_DATE$` (QB64 format)
- [x] `_TIME$` (QB64 format)
- [ ] `_AUTODISPLAY` / `_DISPLAY` timing

---

## Phase 3: Graphics System (In Progress)

### Graphics Architecture ✅
- [x] Define `GraphicsBackend` trait for pluggable backends
- [x] Implement SDL2Backend (full implementation with all drawing primitives)
- [x] Implement MockBackend for testing (complete)
- [x] Create C FFI layer (`graphics_ffi.rs` with all wrapper functions)
- [x] Implement error handling and type system
- [x] Design C FFI wrapper functions

### Screen Setup
- [x] `SCREEN` statement (text and graphics modes)
- [x] `WIDTH` statement (parser, semantic, codegen, FFI)
- [x] `CLS` statement (clear screen)
- [x] `COLOR` statement
- [x] `LOCATE` statement (cursor positioning)
- [x] `VIEW` statement (viewport) - parser, semantic, codegen, FFI
- [x] `WINDOW` statement (coordinate mapping) - parser, semantic, codegen, FFI

### Basic Drawing
- [x] `PSET` / `PRESET` (plot point)
- [x] `LINE` statement (lines and boxes)
- [x] `CIRCLE` statement
- [x] `PAINT` statement (flood fill)
- [x] `DRAW` statement (turtle graphics) - parser, semantic, codegen, FFI
- [x] `POINT()` function (read pixel) - in runtime FFI

### QB64 Graphics Extensions
- [x] `_NEWIMAGE` function - backend trait method
- [x] `_LOADIMAGE` function - backend trait method
- [x] `_FREEIMAGE` statement - full implementation
- [x] `_PUTIMAGE` statement - full implementation with multiple variants
- [x] `_SOURCE` / `_DEST` statements - full implementation
- [x] `_COPYIMAGE` function - backend trait method
- [x] `_SCREENIMAGE` function - backend trait method
- [x] `_WIDTH` / `_HEIGHT` functions - backend trait methods
- [x] `_PRINTSTRING` statement - full implementation
- [ ] `_PRINTWIDTH` function
- [x] `_RGB` / `_RGBA` functions (in runtime FFI)
- [x] `_RGB32` / `_RGBA32` functions (in runtime FFI)
- [ ] Alpha blending support (requires image buffer implementation)
- [x] `_AUTODISPLAY` statement - full implementation

### Graphics Backend Integration
- [x] Integrate SDL2 for window management
- [x] Implement frame buffer (pixel_buffer for POINT())
- [x] Implement `_DISPLAY` / `_AUTODISPLAY`
- [ ] Hardware acceleration option

---

## Phase 4: Sound System (In Progress)

### Audio Architecture (mirrors Graphics architecture) ✅

The audio system uses a trait-based backend abstraction, allowing different audio
libraries to be swapped at compile time via Cargo feature flags. This follows the
same pattern as the graphics system.

```
runtime/src/
├── audio/
│   ├── mod.rs          # AudioBackend trait + global instance ✅
│   ├── error.rs        # AudioError, AudioErrorKind ✅
│   ├── mock.rs         # MockAudioBackend for testing ✅
│   └── miniaudio.rs    # MiniaudioBackend (TODO - actual audio output)
├── audio_ffi.rs        # C FFI layer (qb_snd_*, qb_beep, etc.) ✅
```

**Backend Selection (Cargo.toml features):**
- `audio-miniaudio` - Default. Single-header C library, zero dependencies, cross-platform
- `audio-mock` - For headless testing (no actual audio output)
- Future: `audio-sdl2`, `audio-rodio`, `audio-webaudio`

**Why miniaudio?**
- Single-header C library (easy to integrate with generated C code)
- Zero external dependencies (no SDL2, no system libs)
- Cross-platform (auto-selects ALSA/PulseAudio/WASAPI/CoreAudio)
- Public domain / MIT-0 license
- Battle-tested (used by QB64-PE)

### Audio Backend Infrastructure ✅
- [x] Define `AudioBackend` trait for pluggable backends
- [x] Implement `AudioError` and `AudioErrorKind` types
- [x] Implement `MockAudioBackend` for headless testing
- [ ] Implement `MiniaudioBackend` (actual audio playback)
- [x] Create C FFI layer (`audio_ffi.rs`)
- [ ] Add feature flags to `runtime/Cargo.toml`

### Classic BASIC Sound (Parser, Semantic, Codegen, FFI complete)
- [x] `BEEP` statement - full compiler + FFI
- [x] `SOUND` statement (frequency, duration) - full compiler + FFI
- [x] `PLAY` statement - full compiler + FFI (MML parser in backend)

### QB64 Sound Extensions (Parser, Semantic, Codegen, FFI complete)
- [x] `_SNDOPEN` function (returns handle) - FFI
- [x] `_SNDCLOSE` statement - full implementation
- [x] `_SNDPLAY` / `_SNDSTOP` statements - full implementation
- [x] `_SNDPAUSE` / `_SNDRESUME` statements - FFI (resume via sndresume)
- [x] `_SNDLOOP` statement - full implementation
- [x] `_SNDVOL` statement (0.0 - 1.0) - full implementation
- [x] `_SNDBAL` statement (stereo balance) - full implementation
- [x] `_SNDLEN` function (duration in seconds) - FFI
- [x] `_SNDGETPOS` / `_SNDSETPOS` (playback position) - FFI
- [x] `_SNDPLAYING` / `_SNDPAUSED` functions - FFI
- [x] `_SNDRATE` function (get sample rate) - FFI

### Raw Audio Synthesis (FFI complete, backend needs implementation)
- [x] `_SNDOPENRAW` function (create raw audio stream) - FFI
- [x] `_SNDRAW` statement (push sample frames) - full implementation
- [x] `_SNDRAWLEN` function (queued samples remaining) - FFI

### Audio Format Support (Requires MiniaudioBackend)
- [ ] WAV (PCM)
- [ ] MP3
- [ ] OGG Vorbis
- [ ] FLAC (nice to have)

---

## Phase 5: Advanced Features (Long-Term) - In Progress

### C Library Integration ✅ (Parser/Semantic/Codegen complete)
- [x] `DECLARE LIBRARY` statement - extern C function declarations
- [x] `DECLARE DYNAMIC LIBRARY` - parsed but runtime loading deferred
- [x] BYVAL parameter passing for C calling convention
- [x] ALIAS clause for name mapping
- [ ] Automatic header parsing
- [ ] Type marshalling for complex C types
- [ ] Callback support

### Networking (QB64 Extensions) ✅ (Parser/Semantic/Codegen/Runtime complete)
- [x] `_OPENHOST` function - TCP server on port
- [x] `_OPENCONNECTION` function - accept incoming connection (non-blocking)
- [x] `_OPENCLIENT` function - connect to TCP server
- [x] `_CONNECTED` function - check connection status
- [ ] Network stream I/O (PUT/GET with network handles)

### Input Devices ✅ (Parser/Semantic/Codegen complete - backend integration pending)
- [x] Mouse support (`_MOUSEX`, `_MOUSEY`, `_MOUSEBUTTON`, etc.) - full pipeline
- [x] `_MOUSEINPUT` - check for mouse events
- [x] `_MOUSEMOVEMENTX` / `_MOUSEMOVEMENTY` - relative movement
- [x] `_MOUSEWHEEL` - scroll wheel
- [x] `_MOUSEHIDE` / `_MOUSESHOW` - cursor visibility
- [x] `_MOUSEMOVE` - position cursor
- [ ] Joystick/gamepad support
- [ ] Touch input support

### Clipboard ✅ (Parser/Semantic/Codegen complete - backend integration pending)
- [x] `_CLIPBOARD$` function (get)
- [x] `_CLIPBOARD$` statement (set)

### System Integration ✅
- [x] `SHELL` statement
- [x] `_SHELLHIDE`
- [x] `KILL` statement (delete file)
- [x] `NAME` statement (rename file)
- [x] `MKDIR` / `RMDIR` / `CHDIR`
- [x] `_DIREXISTS` / `_FILEEXISTS`
- [x] `_DIR$` function (directory listing)

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
- [x] Integration tests for compiled programs (176 tests)
- [x] Golden/snapshot tests for codegen (10 tests)
- [x] Property-based testing with proptest (19 tests)
- [x] Fuzz testing infrastructure (3 targets: lexer, parser, full pipeline)
- [x] Fuzz testing verified (~4.6M inputs, 0 crashes)
- [x] Achieve 60%+ line coverage (currently 59.92%)
- [ ] Expand test suite for all built-ins
- [ ] Compatibility tests against QB64 programs
- [ ] Port QB4.5 test cases from QB64pe
- [ ] Achieve 80%+ line coverage

---

## Phase 7: Missing Language Features (Discovered 2026-01-18)

The following features were identified by comparing the QB64pe syntax highlighter keyword list
against the current QB64Fresh implementation. Organized by priority and category.

### High Priority - QB4.5 Core Features

#### Timing & Flow Control
- [x] `SLEEP` statement - pause execution
- [x] `TIMER` function - seconds since midnight (with millisecond precision)
- [x] `RANDOMIZE` statement - seed random number generator
- [ ] `RUN` statement - run program
- [ ] `CHAIN` statement - run another program, optionally passing variables
- [x] `SYSTEM` statement - exit program to operating system
- [ ] `TROFF` / `TRON` statements - debug trace off/on

#### Print Formatting
- [x] `TAB(n)` function - move to column n in PRINT
- [x] `SPC(n)` function - output n spaces in PRINT
- [x] `USING` clause - formatted PRINT output (PRINT USING)
- [ ] `LPRINT` statement - printer output
- [ ] `LPOS(n)` function - printer position
- [x] `POS(n)` function - current cursor column position
- [x] `CSRLIN` function - current cursor row
- [x] `?` - PRINT alias (question mark)

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

#### Arrays
- [x] `ERASE` statement - clear/deallocate arrays

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
- [ ] `PALETTE` statement - set palette colors
- [ ] `PCOPY` statement - copy screen page
- [ ] `PMAP()` function - coordinate mapping
- [ ] `OFF` keyword - turn off event trapping (KEY OFF, etc.)
- [ ] `ONLY` keyword - exclusive file access (OPEN...FOR...ONLY)
- [ ] `SMOOTH` keyword - graphics smooth mode
- [ ] `STRETCH` keyword - graphics stretch mode
- [ ] `CUSTOMTYPE` - TYPE declaration modifier
- [ ] `ENDIF` - alternative END IF syntax (no space)

### Medium Priority - QB64 Extensions

#### Keyboard Input (Important for games)
- [x] `_KEYHIT` function - get key code without waiting
- [x] `_KEYDOWN(code)` function - check if key pressed (stub - always 0)
- [x] `_KEYCLEAR` statement - clear keyboard buffer
- [x] `_CINP` function - raw console input
- [x] `_CAPSLOCK` / `_NUMLOCK` / `_SCROLLLOCK` - lock key states

#### Timing Functions
- [x] `_DELAY(seconds)` statement - pause execution (float precision)
- [x] `_LIMIT(fps)` statement - limit frame rate

#### Math Functions
- [x] `_CEIL(n)` function - ceiling
- [x] `_ROUND(n)` function - round to nearest
- [x] `_PI` constant - pi (3.14159...)
- [x] `_MIN(a, b)` / `_MAX(a, b)` functions
- [x] `_CLAMP(val, min, max)` function
- [x] `_HYPOT(x, y)` function - hypotenuse
- [ ] `_NEGATE(n)` function - negate value

#### Trigonometric (Extended)
- [ ] `_ACOS(n)` / `_ASIN(n)` functions - arc cosine/sine
- [ ] `_ATAN2(y, x)` function - arc tangent of y/x
- [ ] `_SINH(n)` / `_COSH(n)` / `_TANH(n)` - hyperbolic functions
- [ ] `_ASINH(n)` / `_ACOSH(n)` / `_ATANH(n)` - inverse hyperbolic
- [ ] `_SEC(n)` / `_CSC(n)` / `_COT(n)` - secant/cosecant/cotangent
- [ ] `_SECH(n)` / `_CSCH(n)` / `_COTH(n)` - hyperbolic sec/csc/cot
- [ ] `_ARCSEC(n)` / `_ARCCSC(n)` / `_ARCCOT(n)` - inverse sec/csc/cot
- [ ] `_D2R(degrees)` / `_R2D(radians)` - degree/radian conversion
- [ ] `_D2G(degrees)` / `_G2D(gradians)` / `_G2R(gradians)` / `_R2G(radians)` - gradian conversions

#### Bitwise Operations ✅
- [x] `_SHL(value, bits)` function - shift left
- [x] `_SHR(value, bits)` function - shift right
- [x] `_ROL(value, bits)` function - rotate left
- [x] `_ROR(value, bits)` function - rotate right
- [x] `_READBIT(value, bit)` function - read bit
- [x] `_SETBIT(value, bit)` function - set bit
- [x] `_RESETBIT(value, bit)` function - clear bit
- [x] `_TOGGLEBIT(value, bit)` function - toggle bit

#### String Functions
- [ ] `_STRCMP(a$, b$)` function - case-sensitive compare
- [ ] `_STRICMP(a$, b$)` function - case-insensitive compare
- [ ] `_TOSTR$(n)` function - number to string (no leading space)
- [ ] `_BIN$(n)` function - number to binary string
- [ ] `_CV(type, string$)` function - generic convert string to type
- [ ] `_MK$(type, value)` function - generic convert value to string

#### Error Handling (Extended)
- [ ] `_ERRORLINE` variable - line number of error
- [ ] `_ERRORMESSAGE$` function - error message text
- [ ] `_INCLERRORFILE$` / `_INCLERRORLINE` - include file error info
- [ ] `_ASSERT` statement - assertions
- [ ] `$ASSERTS` metacommand - enable assertions
- [ ] `_EXIT` statement - exit program with code

#### Utility Functions
- [ ] `_IIF(condition, true_val, false_val)` function - inline IF
- [ ] `_CAST(type, value)` function - explicit type cast
- [ ] `_DEFINE` statement - define default variable types by letter range
- [ ] `_COMMANDCOUNT` function - count of command line arguments
- [ ] `_ENVIRONCOUNT` function - count of environment variables
- [ ] `_STATUSCODE` function - status code from last operation

#### Networking (Extended)
- [ ] `_CONNECTIONADDRESS(handle)` function - get connection IP address
- [ ] `_CONNECTIONADDRESS$(handle)` function - get connection IP as string
- [ ] `_DONTWAIT` keyword - non-blocking network operations

### Lower Priority - QB64 Advanced Extensions

#### Desktop/Window Info
- [ ] `_DESKTOPHEIGHT` / `_DESKTOPWIDTH` functions - desktop dimensions
- [ ] `_SCREENX` / `_SCREENY` functions - window position
- [ ] `_SCREENMOVE x, y` statement - move window
- [ ] `_SCREENEXISTS` function - check if window exists
- [ ] `_SCREENHIDE` / `_SCREENSHOW` statements - hide/show window
- [ ] `_SCREENICON` function - check if window is minimized
- [ ] `_SCREENPRINT` statement - print screen contents
- [ ] `_SCREENCLICK` statement - simulate screen click
- [ ] `_FULLSCREEN` statement - toggle fullscreen
- [ ] `_ALLOWFULLSCREEN` statement - allow/disallow fullscreen toggle
- [ ] `_TITLE` / `_TITLE$` - set/get window title
- [ ] `_ICON` statement - set window icon
- [ ] `_HIDE` / `_SHOW` statements - hide/show window (alias)
- [ ] `_ONTOP` statement - set window always on top
- [ ] `_WINDOWHANDLE` function - get native window handle
- [ ] `_WINDOWHASFOCUS` function - check if window has focus

#### Font Support
- [ ] `_LOADFONT(file$, size)` function - load font
- [ ] `_FONT` statement - set current font
- [ ] `_FREEFONT(handle)` statement - release font
- [ ] `_FONTHEIGHT` / `_FONTWIDTH` functions - font dimensions

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
- [ ] `_MESSAGEBOX` function - display message box
- [ ] `_INPUTBOX$` function - input dialog
- [ ] `_OPENFILEDIALOG$` function - file open dialog
- [ ] `_SAVEFILEDIALOG$` function - file save dialog
- [ ] `_SELECTFOLDERDIALOG$` function - folder select dialog
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
- [ ] `_TRUE` / `_FALSE` constants
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
- [ ] Line number support for legacy BASIC (currently labels only)

---

## Notes

**Dependencies Available (Not Yet Integrated):**
- SDL2 crate is in Cargo.toml (for graphics/sound)
- Runtime library has foundations for file I/O

**Design Decisions Made:**
- Graphics backend: Trait-based abstraction with SDL2 as default, mock for testing
- Sound backend: Trait-based abstraction with miniaudio as default (see Phase 4)

**Design Decisions Needed:**
- How to handle `PEEK`/`POKE` in a safe manner
- Memory model for `_MEM` operations
