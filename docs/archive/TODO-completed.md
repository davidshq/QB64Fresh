# QB64Fresh - Completed Features

*A comprehensive record of all completed features, organized by development phase.*

---

## Phase 1: Language Completeness ✅ COMPLETED

### File I/O
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

### Computed Control Flow
- [x] Implement `ON n GOTO` statement
- [x] Implement `ON n GOSUB` statement

### Built-in Functions
- [x] `ENVIRON$()` - get environment variable
- [x] `COMMAND$` - command line arguments
- [x] `_OS$` - operating system identifier
- [x] `_CWD$` - current working directory
- [x] `_STARTDIR$` - program start directory
- [x] `SWAP` statement (parser + semantic: requires exact type match)

### Variable/Scope Enhancements
- [x] `COMMON` statement (shared variables between modules)
- [x] `REDIM` with `_PRESERVE`
- [x] Proper `OPTION BASE` support - full implementation
- [x] `DIM SHARED` at module level - fixed semantic check to allow shared variable declarations
- [x] `STATIC` arrays/variables in procedures - full implementation with parser, semantic, and codegen

### DEF FN Support
- [x] Implement `DEF FN` single-line functions
- [x] Implement multi-line `DEF FN` (QB64 extension) - `DEF FN ... END DEF` block syntax

### Memory Segment Support
- [x] `DEF SEG` statement - runtime implementation for segment tracking

### Error Handling (Extended)
- [x] Add error code constants (standard error codes are used) - added 42 `_ERR_*` constants

---

## Phase 2: Core Extensions ✅ COMPLETED

### Conditional Compilation
- [x] `$IF` / `$ELSE` / `$ELSEIF` / `$END IF` directives
- [x] `$LET` directive for compile-time variables
- [x] `$CHECKING` directive
- [x] `$INCLUDE` - read and parse included files (implemented in preprocessor.rs)
- [x] Built-in platform constants: `_WIN`, `_WINDOWS`, `_LINUX`, `_MAC`, `_32BIT`, `_64BIT`
  - Evaluated at compile time for `$IF` conditional compilation
  - Uses BASIC convention: -1 for TRUE, 0 for FALSE
  - Supports boolean operators: `AND`, `OR`, `NOT`, `XOR`
  - Supports comparisons: `=`, `<>`, `<`, `>`, `<=`, `>=`
  - Renamed to underscore prefix in Session 031 to avoid conflicts with user variables

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

---

## Phase 3: Graphics System ✅ COMPLETED

### Graphics Architecture
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
- [x] `BOX` - rectangle outline and filled (via LINE B/BF)
- [x] `CIRCLE` statement (midpoint algorithm)
- [x] `PAINT` statement (scanline flood fill algorithm)
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
- [x] `_RGB` / `_RGBA` functions (in runtime FFI)
- [x] `_RGB32` / `_RGBA32` functions (in runtime FFI)
- [x] `_AUTODISPLAY` statement - full implementation
- [x] `_DISPLAY` - double-buffered rendering

### Graphics Backend Integration
- [x] Integrate SDL2 for window management
- [x] Implement frame buffer (pixel_buffer for POINT())
- [x] Implement `_DISPLAY` / `_AUTODISPLAY`

### Text & Font Support
- [x] Text rendering - 8x8 bitmap font (CP437 character set)
- [x] `_PRINTWIDTH` function - calculate pixel width of text
- [x] `_FONTHEIGHT` / `_FONTWIDTH` functions - font metrics
- [x] `_LOADFONT` function - TrueType support (optional `graphics-sdl2-ttf` feature)
- [x] `_FONT` function - set/get current font
- [x] `_FREEFONT` function - release loaded font
- [x] VIEW PRINT statement - text viewport (`qb_view_print`, `qb_view_print_reset`)

### Sprite Operations
- [x] GET statement (graphics) - capture screen region to array (runtime stubs)
- [x] PUT statement (graphics) - display array to screen (runtime stubs)
- [x] PUT action modes: XOR, PSET, PRESET, AND, OR

### Alpha Blending & Page Support
- [x] Alpha blending — `_BLEND`, `_DONTBLEND`, `_CLEARCOLOR` in external runtime (SDL2)
- [x] Multiple screen pages — SCREEN active/visual page, `PCOPY`; 4 pages in `runtime/src/graphics/sdl2.rs`
- [x] Hardware acceleration — persistent SDL2 streaming textures; dirty flag optimization; O(1) page switches

### Mouse Input
- [x] `_MOUSEX`, `_MOUSEY`, `_MOUSEBUTTON` - full pipeline
- [x] `_MOUSEINPUT` - check for mouse events
- [x] `_MOUSEMOVEMENTX` / `_MOUSEMOVEMENTY` - relative movement
- [x] `_MOUSEWHEEL` - scroll wheel
- [x] `_MOUSEHIDE` / `_MOUSESHOW` - cursor visibility
- [x] `_MOUSEMOVE` - position cursor

---

## Phase 4: Sound System ✅ COMPLETED

### Audio Architecture
- [x] Define `AudioBackend` trait for pluggable backends
- [x] Implement `AudioError` and `AudioErrorKind` types
- [x] Implement `MockAudioBackend` for headless testing
- [x] Implement `RodioBackend` (actual audio playback) - pure Rust, cross-platform
- [x] Create C FFI layer (`audio_ffi.rs`)
- [x] Add feature flags to `runtime/Cargo.toml` (`audio-rodio`, `audio-mock`)

### Classic BASIC Sound
- [x] `BEEP` statement - full compiler + FFI
- [x] `SOUND` statement (frequency, duration) - full compiler + FFI
- [x] `PLAY` statement - full compiler + FFI (MML parser in backend)

### QB64 Sound Extensions
- [x] `_SNDOPEN` function (returns handle) - FFI
- [x] `_SNDCLOSE` statement - full implementation
- [x] `_SNDPLAY` / `_SNDSTOP` statements - full implementation
- [x] `_SNDPAUSE` / `_SNDRESUME` statements - FFI
- [x] `_SNDLOOP` statement - full implementation
- [x] `_SNDVOL` statement (0.0 - 1.0) - full implementation
- [x] `_SNDBAL` statement (stereo balance) - full implementation
- [x] `_SNDLEN` function (duration in seconds) - FFI
- [x] `_SNDGETPOS` / `_SNDSETPOS` (playback position) - FFI
- [x] `_SNDPLAYING` / `_SNDPAUSED` functions - FFI
- [x] `_SNDRATE` function (get sample rate) - FFI

### Raw Audio Synthesis
- [x] `_SNDOPENRAW` function (create raw audio stream) - FFI
- [x] `_SNDRAW` statement (push sample frames) - full implementation
- [x] `_SNDRAWLEN` function (queued samples remaining) - FFI

### Audio Format Support (via rodio)
- [x] WAV (PCM)
- [x] MP3
- [x] OGG Vorbis
- [x] FLAC

---

## Phase 5: Advanced Features ✅ COMPLETED

### C Library Integration (Parser/Semantic/Codegen complete)
- [x] `DECLARE LIBRARY` statement - extern C function declarations
- [x] `DECLARE DYNAMIC LIBRARY` - parsed but runtime loading deferred
- [x] BYVAL parameter passing for C calling convention
- [x] ALIAS clause for name mapping

### Networking (QB64 Extensions) (Parser/Semantic/Codegen/Runtime complete)
- [x] `_OPENHOST` function - TCP server on port
- [x] `_OPENCONNECTION` function - accept incoming connection (non-blocking)
- [x] `_OPENCLIENT` function - connect to TCP server
- [x] `_CONNECTED` function - check connection status
- [x] Network stream I/O — `PUT #`/`GET #` dispatch to `qb_net_put`/`qb_net_get` when handle < 0
- [x] `EOF()` / `LOF()` for network handles — `qb_net_eof`, `qb_net_lof` in runtime

### Clipboard (Parser/Semantic/Codegen complete - backend integration pending)
- [x] `_CLIPBOARD$` function (get)
- [x] `_CLIPBOARD$` statement (set)

### System Integration
- [x] `SHELL` statement
- [x] `_SHELLHIDE`
- [x] `KILL` statement (delete file)
- [x] `NAME` statement (rename file)
- [x] `MKDIR` / `RMDIR` / `CHDIR`
- [x] `_DIREXISTS` / `_FILEEXISTS`
- [x] `_DIR$` function (directory listing)

### Input Devices (External Runtime) ✅ (2026-01-25)
- [x] Joystick/gamepad full implementation — STICK, STRIG, _DEVICES, _AXIS, _BUTTON, ON STRIG, STRIG ON/OFF/STOP in external runtime (`runtime/src/joystick.rs`, SDL2 gamepad API). Inline: stubs (returns 127/0).

---

## Phase 6: Tooling & Ecosystem (Partial)

### Testing
- [x] Expand test suite for all built-ins (315 integration tests)
- [x] Achieve 80%+ line coverage (81.63% achieved!)
- [x] SHARED variable scope - SUBs/FUNCTIONs can access module-level variables via SHARED statement
- [x] QB4.5 test cases from QB64pe (**114/115 = 99.1%** passing, excluding open_gl)
- [x] Runtime stub tests for File I/O, Graphics, Sound (94 tests added)

---

## Phase 7: Missing Language Features (2026-01-18 - 2026-01-19)

### Timing & Flow Control
- [x] `RUN` statement - run program
- [x] `CHAIN` statement - run another program, optionally passing variables
- [x] `TROFF` / `TRON` statements - debug trace off/on

### Print Formatting
- [x] `LPRINT` statement - printer output
- [x] `PRINT USING` statement - formatted output

### Random Number Generation
- [x] `RND` function - random number
- [x] `RANDOMIZE` statement - seed random number generator

### Memory/Legacy
- [x] `CLEAR` statement - clear memory/variables

### File System
- [x] `FILES` statement - directory listing display
- [x] `FIELD` statement - define record fields for random access
- [x] `LSET` / `RSET` statements - left/right justify in field
- [x] `RESET` statement - close all open files

### Event Handling
- [x] `KEY(n) ON/OFF/STOP` statement - key event trapping
- [x] `STRIG(n) ON/OFF/STOP` statement - joystick trigger event trapping

### Graphics (QB4.5)
- [x] `PALETTE` statement - set palette colors
- [x] `PCOPY` statement - copy screen page
- [x] `PMAP()` function - coordinate mapping

### Syntax Compatibility
- [x] `ENDIF` - alternative END IF syntax (no space)

### Keyboard Input (QB64)
- [x] `_KEYHIT` function
- [x] `_KEYDOWN` function (stub)
- [x] `_KEYCLEAR` statement

### Timing Functions (QB64)
- [x] `SLEEP` statement
- [x] `_DELAY` statement
- [x] `_LIMIT` statement

### Math Functions (QB64)
- [x] `_ROUND()` function
- [x] `_CEIL()` function
- [x] `_PI` constant
- [x] `_HYPOT()` function
- [x] `_ATAN2()` function

### Trigonometric (Extended)
- [x] `_ASIN()` / `_ACOS()` functions - arc sine/cosine
- [x] `_SINH()` / `_COSH()` / `_TANH()` functions - hyperbolic
- [x] `_ASINH()` / `_ACOSH()` / `_ATANH()` functions - inverse hyperbolic
- [x] `_SEC()` / `_CSC()` / `_COT()` functions - reciprocal trig
- [x] `_ARCSEC()` / `_ARCCSC()` / `_ARCCOT()` functions - inverse reciprocal
- [x] `_ARCSECH()` / `_ARCCSCH()` / `_ARCCOTH()` functions - inverse hyperbolic reciprocal
- [x] `_SECH()` / `_CSCH()` / `_COTH()` functions - hyperbolic reciprocal
- [x] `_D2R()` / `_R2D()` functions - degrees/radians conversion
- [x] `_D2G()` / `_G2D()` / `_G2R()` / `_R2G()` functions - gradians conversion
- [x] `_NEGATE(n)` function - negate value

### Bitwise Operations
- [x] `AND`, `OR`, `XOR` operators
- [x] `EQV`, `IMP` operators
- [x] `NOT` operator (unary)
- [x] `_SHL(value, bits)` function - shift left
- [x] `_SHR(value, bits)` function - shift right
- [x] `_ROL(value, bits)` function - rotate left
- [x] `_ROR(value, bits)` function - rotate right
- [x] `_READBIT(value, bit)` function - read bit
- [x] `_SETBIT(value, bit)` function - set bit
- [x] `_RESETBIT(value, bit)` function - clear bit
- [x] `_TOGGLEBIT(value, bit)` function - toggle bit

### Constants
- [x] `_TRUE` / `_FALSE` constants

### Window/Desktop Functions
- [x] `_DESKTOPWIDTH` / `_DESKTOPHEIGHT` functions
- [x] `_SCREENX` / `_SCREENY` functions
- [x] `_SCREENMOVE` statement
- [x] `_SCREENICON` statement - minimize window
- [x] `_ALLOWFULLSCREEN` statement - fullscreen toggle
- [x] `_FULLSCREEN` statement
- [x] `_TITLE` statement (set window title)
- [x] `_TITLE$` function (get window title)
- [x] `_WINDOWHANDLE` function - native window handle
- [x] `_WINDOWHASFOCUS` function - window focus check

### Dialog Functions
- [x] `_MESSAGEBOX` function
- [x] `_INPUTBOX$` function
- [x] `_OPENFILEDIALOG$` function
- [x] `_SAVEFILEDIALOG$` function
- [x] `_SELECTFOLDERDIALOG$` function

### String Comparison
- [x] `_STRCMP()` function - case-sensitive compare
- [x] `_STRICMP()` function - case-insensitive compare

### Control Flow (Extended)
- [x] `_IIF()` function - inline if (numeric)
- [x] `_IIF$()` function - inline if (string)

### File Helpers
- [x] `_READFILE$` function - read entire file
- [x] `_WRITEFILE` statement - write entire file

### Console Control
- [x] `_CONSOLE ON/OFF` statement - console control
- [x] `_CONSOLETITLE` statement - set console title

### Assertions
- [x] `_ASSERT` statement - assertions
- [x] `$ASSERTS` metacommand - enable assertions (parser ready)

### Parser Compatibility
- [x] Implicit SUB calls without parentheses - `Greet "World"` works correctly
- [x] Line number support for legacy BASIC programs

---

## Phase 7: Legacy/Compatibility Features ✅ (Session 028)

### Print Formatting
- [x] `LPOS(n)` function - printer position (stub, returns 1)

### Memory/Legacy Functions
- [x] `BLOAD` / `BSAVE` statements - binary load/save to memory (with BSAVE header format)
- [x] `VARPTR()` function - returns memory address of variable
- [x] `VARPTR$()` function - returns binary string representation of address
- [x] `VARSEG()` function - returns segment address (0 in flat memory model)
- [x] `SADD()` function - returns address of string's data buffer
- [x] `SETMEM` statement - set available memory (no-op in modern systems)
- [x] `SEG` clause - segment for CALL ABSOLUTE (parsed, ignored in flat model)

### File System
- [x] `FILEATTR()` function - file attributes (stub)

### Type Conversion (Microsoft Binary Format)
- [x] `CVSMBF()` function - convert 4-byte MBF string to SINGLE
- [x] `CVDMBF()` function - convert 8-byte MBF string to DOUBLE
- [x] `MKSMBF$()` function - convert SINGLE to 4-byte MBF string
- [x] `MKDMBF$()` function - convert DOUBLE to 8-byte MBF string

### Procedure Calling (Legacy)
- [x] `CALL ABSOLUTE address` statement - call machine language routine (warning stub)
- [x] `CALLS` statement - call with far pointers (treated as regular CALL)
- [x] `CDECL` clause - C calling convention in DECLARE (parsed, ignored)

---

## Design Decisions Completed

- Graphics backend: Trait-based abstraction with SDL2 as default, mock for testing
- Sound backend: Trait-based abstraction with rodio as default (chosen over miniaudio for pure Rust)
- C intermediate representation for code generation (proven, portable)
- Logos for lexing (fast, maintainable)
- Pratt parsing for expressions (elegant, correct precedence)
- Two-pass semantic analysis (enables forward references)
- GOSUB/RETURN uses GCC computed goto extension (works with GCC/Clang)

---

## Statistics

- **Total source code:** ~57,000+ lines of Rust (compiler: ~46k, runtime: ~11k)
- **Unit tests:** 217
- **Integration tests:** 720+
- **Total tests:** 937+
- **Line coverage:** 81.63% ✅
- **Statement types:** 60+
- **Built-in functions:** 240+ (includes QB64 extensions)

---

*Last updated: 2026-01-31*

---

## Phase 6: Tooling & Ecosystem ✅ (Session 038)

### Documentation
- [x] Migration guide from QB64 *(Small - 1-2 sessions)* - see docs/MIGRATION_GUIDE.md

### Testing
- [x] Add comprehensive recursion test suite *(Small - 1 session)* - 32 tests in `recursion` module

---

## Known Issues Resolved ✅ (Session 038)

### Memory Features
- [x] **QB45 memory features: DEF SEG, VARSEG, VARPTR, BLOAD, BSAVE** - All memory segment operations fully implemented and working. Test suite shows 96.5% compatibility (136/141 files passing). thebob/ directory (heavy BLOAD/BSAVE usage) passes 100%.

### Recursion
- [x] **Recursion support** - Thoroughly tested with 32 tests added

---

## Phase 7: QB4.5 Event Handling & Legacy Features ✅ (Session 028)

### Event Handling
- [x] `KEY(n)` function - check key trap status (stub, returns 0)
- [x] `ON KEY(n) GOSUB` statement - key event handler
- [x] `ON TIMER(n) GOSUB` statement - timer event handler
- [x] `ON STRIG(n) GOSUB` statement - joystick trigger handler
- [x] `ON COM(n) GOSUB` statement - serial port event handler (stub)
- [x] `ON PEN GOSUB` statement - light pen event handler (stub)
- [x] `ON UEVENT GOSUB` statement - user-defined event handler (stub)
- [x] `ON SIGNAL(n) GOSUB` statement - signal event handler (stub)
- [x] `KEY(n) ON/OFF/STOP` statement - key event trapping control
- [x] `TIMER ON/OFF/STOP` statement - timer event trapping control
- [x] `STRIG(n) ON/OFF/STOP` statement - joystick trigger event trapping control
- [x] `COM(n) ON/OFF/STOP` statement - serial port event trapping control (stub)
- [x] `PEN ON/OFF/STOP` statement - light pen event trapping control (stub)
- [x] `UEVENT ON/OFF/STOP` statement - user event trapping control (stub)
- [x] `UEVENT` statement - trigger user-defined event (stub)
- [x] `SIGNAL(n) ON/OFF/STOP` statement - signal event trapping control (stub)
- [x] `OFF` keyword - token for event trapping (KEY OFF, etc.)

### Input Devices (QB4.5)
- [x] `STICK(n)` function - joystick position (stub, returns center=127)
- [x] `STRIG(n)` function - joystick trigger state (stub, returns 0)
- [x] `PEN(n)` function - light pen information (stub, returns 0)

### Memory Functions (QB4.5)
- [x] `FRE(n)` function - free memory (returns 64MB on modern systems)
- [x] `FREE` statement - free string space (no-op, GC handles it)

### Port I/O (QB4.5 - Sandboxed)
- [x] `INP(port)` function - read byte from I/O port (stub, returns 0xFF)
- [x] `OUT port, value` statement - write byte to I/O port (stub, no-op)

### System Interrupts (Legacy)
- [x] `INTERRUPT intnum, inregs, outregs` statement - call system interrupt (stub with warning)
- [x] `INTERRUPTX intnum, inregs, outregs` statement - extended interrupt call (stub with warning)

### Serial I/O (QB4.5)
- [x] `ERDEV` function - device error code (stub, returns 0)
- [x] `ERDEV$` function - device error name (stub, returns "")
- [x] `IOCTL [#]filenum, string$` statement - send device control string (stub)
- [x] `IOCTL$(filenum)` function - get device status string (stub, returns "")

### QB4.5 Remaining Items (Session 029)
- [x] `WAIT port, and_mask [, xor_mask]` statement - wait for hardware port condition (stub on modern systems)
- [x] `ONLY` keyword - exclusive file access mode for `OPEN...FOR...ONLY AS #n`
- [x] `SMOOTH` keyword - bilinear interpolation scaling mode for `_PUTIMAGE`
- [x] `STRETCH` keyword - nearest-neighbor scaling mode for `_PUTIMAGE`
- [x] `CUSTOMTYPE` modifier - TYPE declaration modifier for C-compatible (packed) memory layout using `#pragma pack`

---

## Phase 7: QB4.5 Compatibility Fixes ✅ (Session 023)

### Built-in Functions
- [x] `PEEK(address)` function - memory read (stub, returns 0 for safety)
- [x] `MID$(string, start [, length])` - third parameter now optional
- [x] `RND([seed])` - seed parameter now optional
- [x] `INSTR([start,] string, substring)` - start position now optional
- [x] `STRING$(n, char)` - accepts either integer ASCII code or single-character string

### Parser Fixes
- [x] `LINE INPUT "prompt", var$` - accepts comma OR semicolon after prompt
- [x] `DATA` statement - unquoted strings with operators (e.g., PLAY music notation like `o3e-o2b-ge-`)
- [x] `MID$(str$, pos, len) = value$` - MID$ as lvalue for in-place substring replacement

### Semantic Analysis Fixes
- [x] Variable array bounds - `DIM array(1 TO variable)` now allows runtime expressions
- [x] `DIM SHARED` module-level visibility - variables declared with `DIM SHARED` at module level are now visible in SUB/FUNCTION procedures

### Test Results Improvement
- QB4.5 test case compatibility: 56 → 64 files passing (39% → 45%)

---

## Phase 7: Array Passing Support ✅ (Session 024)

### Array Parameters
- [x] Array passing with `array()` syntax - pass entire arrays to SUB/FUNCTION
- [x] `ArrayRef` expression kind in typed IR for array references
- [x] `is_array` field in `ParameterInfo` for parameter tracking
- [x] Array parameters registered as `ArrayVariable` with dynamic dimensions
- [x] `try_check_array_ref()` method to detect array reference arguments
- [x] REDIM now uses `update_or_define_symbol` to properly update array parameters
- [x] Skip dimension validation for dynamic arrays (empty dimensions)
- [x] Code generation for `ArrayRef` (emits array name, decays to pointer in C)

---

## Phase 7: QB4.5 Compatibility Improvements ✅ (Session 025)

### Lexer Fixes
- [x] `INPUT$` function token - Added `InputDollar` token with high priority regex to prevent `INPUT$` being tokenized as `INPUT` keyword + `$` suffix
- [x] Parser support for `INPUT$` as builtin function call

### Semantic Analysis Fixes
- [x] `DIM` can now replace simple variables with arrays - Allows `DIM x(10)` after `FOR x = 1 TO n` without "duplicate variable" error
- [x] `STRING * n` ↔ `STRING` conversion - Fixed-length strings and regular strings are now interconvertible

### Console INPUT Improvements
- [x] `INPUT` statement now accepts array elements - `INPUT p(k)` works correctly
- [x] Changed INPUT statement AST from `variables: Vec<String>` to `targets: Vec<InputTarget>`
- [x] Support for `InputTarget` variants: Variable, ArrayElement, ArrayElementField, Field
- [x] Updated semantic checker, typed IR, and codegen for INPUT with complex targets

### Test Results Improvement
- QB4.5 test case compatibility: **105/141 → 108/141** (74.5% → 76.6%)

---

## Phase 7: QB4.5 Compatibility Improvements ✅ (Session 030)

### Parser Fixes
- [x] LINE statement style pattern - optional 16-bit hex pattern for dashed lines (e.g., `LINE (0,0)-(100,100),,, &HF0F0`)
- [x] SUB calls with parenthesized first argument - `SubName ((expr)), arg2, arg3` pattern
- [x] Empty array dimension syntax - `STATIC arr()` and `DIM arr()` for dynamic arrays
- [x] Single-line IF-THEN-ELSE with colons - `IF x THEN A: B ELSE C` where colon separates statements, not labels

### Sound Functions (QB64 Extensions)
- [x] `_SNDPLAYFILE filename$[, volume!][, x!][, y!][, z!]` statement - play sound file directly
- [x] `_SNDPLAYCOPY handle&[, volume!]` statement - play copy of sound
- [x] `_SNDSETPOS handle&, position!` statement - set playback position
- [x] `_SNDCOPY(handle&)` function - copy sound handle
- [x] `_SNDPLAYING(handle&)` function - check if sound is playing
- [x] `_SNDPAUSED(handle&)` function - check if sound is paused
- [x] `_SNDGETPOS(handle&)` function - get playback position in seconds
- [x] `_SNDLEN(handle&)` function - get sound length in seconds
- [x] `_SNDOPEN(file$[, mode$])` function - now accepts optional mode/requirements string

### Test Results Improvement
- QB4.5 test case compatibility: **108/141 → 117/141** (76.6% → 83.0%)

---

## Phase 7: QB4.5 Compatibility Improvements ✅ (Session 031)

### Platform Constants Renamed
- [x] Platform constants renamed from `WIN`, `LINUX`, `MAC` to `_WIN`, `_LINUX`, `_MAC` etc.
  - Prevents conflicts with user variables (e.g., `win` variable in pongsource.bas)
  - Now uses QB64 convention with underscore prefix

### Parser Fixes
- [x] Label parsing only at line start - labels are now only recognized at the beginning of a logical line, not after colon statement separators
  - Fixes "duplicate label" errors in programs using colon-separated statements
  - Added `at_line_start` tracking to parser state
- [x] `UnterminatedString` handling in DATA statements - strings with missing closing quote are now accepted in DATA
  - Fixes parsing of legacy BASIC programs with this common pattern

### Semantic Analysis Fixes
- [x] `SHARED` statement implicit declaration - SHARED now implicitly declares module-level variables if not already defined
  - Classic BASIC behavior: variables can be declared implicitly by SHARED
- [x] `LEN()` function accepts UDTs - LEN() now works with any type (Unknown), not just strings
  - Returns the size of UDT structures

### Built-in Functions
- [x] `SCREEN(row, col [, flag])` function - read text screen content
  - Returns ASCII value at text position, or attribute if flag is non-zero

### C Library Integration (FFI)
- [x] Type marshalling for STRING ↔ char* at FFI boundary
  - `qb_string_data()` returns `const char*` for C interop
  - `qb_string_from_cstr()` creates QB string from C string
- [x] Callback support via `_PROCPTR(procedureName)` function
  - Returns function pointer address as `_OFFSET`
  - Generates wrapper functions for C library callbacks
- [x] Automatic header parsing (optional feature flag `header-parsing`)
  - Parses C header files to auto-generate DECLARE LIBRARY bindings

### QB64 Extension Functions (Batch Implementation)
- [x] Color component functions: `_RED`, `_GREEN`, `_BLUE`, `_ALPHA`, `_RED32`, `_GREEN32`, `_BLUE32`, `_ALPHA32`
- [x] Screen info: `_PIXELSIZE`, `_SCREENEXISTS`, `_FPS`
- [x] Color defaults: `_DEFAULTCOLOR`, `_BACKGROUNDCOLOR`
- [x] Window control: `_ICON`, `_HIDE`, `_SHOW`, `_ONTOP`
- [x] Hash functions: `_CRC32`, `_MD5$`, `_ADLER32`
- [x] Encoding: `_BASE64ENCODE$`, `_BASE64DECODE$`, `_ENCODEURL$`, `_DECODEURL$`
- [x] Compression: `_DEFLATE$`, `_INFLATE$`
- [x] Path functions: `_FULLPATH$`
- [x] Memory: `_MEMEXISTS`
- [x] Device input: `_DEVICES`, `_DEVICE$`, `_DEVICEINPUT`, `_LASTAXIS`, `_LASTBUTTON`, `_LASTWHEEL`, `_AXIS`, `_BUTTON`, `_BUTTONCHANGE`, `_WHEEL`
- [x] Drag and drop: `_TOTALDROPPEDFILES`, `_DROPPEDFILE`, `_DROPPEDFILE$`
- [x] Resize events: `_RESIZE`, `_RESIZEWIDTH`, `_RESIZEHEIGHT`, `_SCALEDWIDTH`, `_SCALEDHEIGHT`
- [x] Dialogs: `_COLORCHOOSERDIALOG`, `_NOTIFYPOPUP`
- [x] Mouse: `_MOUSEHIDDEN`
- [x] Clipboard: `_CLIPBOARDIMAGE`
- [x] Console: `_CONSOLEINPUT`, `_ECHO`
- [x] Logic operators: `_ANDALSO`, `_ORELSE`
- [x] Timer: `_FREETIMER`
- [x] Sound: `_SNDRAWDONE`
- [x] Exit: `_EXIT` statement

### QB64 Extension Statements (Batch Implementation)
- [x] Drag and drop: `_ACCEPTFILEDROP`, `_FINISHDROP`
- [x] Console: `_CONSOLECURSOR`, `_CONSOLEFONT`, `_CONTROLCHR`
- [x] Graphics alpha: `_SETALPHA`, `_PALETTECOLOR`, `_COPYPALETTE`, `_BLEND`, `_DONTBLEND`, `_CLEARCOLOR`, `_DEPTHBUFFER`, `_DISPLAYORDER`
- [x] Sound: `_SNDLIMIT`
- [x] Print: `_PRINTMODE`

### Test Results Improvement
- QB4.5 test case compatibility: **117/141 → 114/115** (83.0% → 99.1%, excluding open_gl)
  - pete: 42/42 (100%) - up from 62/68
  - thebob: 19/19 (100%) - up from 12/19
  - misc: 45/46 (98%)
  - qb45com: 5/5 (100%)
  - n54: 3/3 (100%)
- Only remaining failure: frog.bas (bug in original code: `SCORE > HISCORE` where HISCORE is a UDT array)
- Integration tests: 409 → 454 tests

---

## Phase 7: QB64 Extension Functions ✅ (Session 032)

### Error Handling Extended
- [x] `_INCLERRORFILE$` function - get include file where error occurred
- [x] `_INCLERRORLINE` function - get line number in include file

### Utility Functions
- [x] `_STATUSCODE` function - get status code from last operation

### Networking Extended
- [x] `_CONNECTIONADDRESS(handle)` function - get connection IP address as numeric
- [x] `_CONNECTIONADDRESS$(handle)` function - get connection IP as string

### HSB Color Functions
- [x] `_HSB32(h, s, b)` function - create 32-bit color from HSB values
- [x] `_HSBA32(h, s, b, a)` function - create 32-bit color from HSBA values
- [x] `_HUE32(color)` function - extract hue component from color
- [x] `_SATURATION32(color)` function - extract saturation component from color
- [x] `_BRIGHTNESS32(color)` function - extract brightness component from color

### Memory Extended
- [x] `_MEMELEMENT(mem, index)` function - get element offset in memory block
- [x] `_MEMIMAGE(handle)` function - get memory block for image
- [x] `_MEMSOUND(handle)` function - get memory block for sound

### Sound Extended
- [x] `_SNDNEW(frames, channels, bits)` function - create new sound buffer

### File I/O Extended
- [x] `_FILES$(pattern$)` function - file listing iterator

### Device Input Extended
- [x] `_LASTHANDLER` function - get last event handler

### Unicode Font Functions
- [x] `_UCHARPOS(text$, pos)` function - get Unicode character position
- [x] `_UFONTHEIGHT(handle)` function - get Unicode font height
- [x] `_ULINESPACING` function - get Unicode line spacing
- [x] `_UPRINTWIDTH(text$)` function - get Unicode print width

### Graphics Extended Statements
- [x] `_SAVEIMAGE filename$, handle` statement - save image to file
- [x] `_SCREENPRINT text$` statement - print screen contents

### Unicode Font Statements
- [x] `_UPRINTSTRING x, y, text$` statement - print Unicode string
- [x] `_MAPUNICODE codepoint, charcode` statement - map Unicode code point

### Logging Statements
- [x] `_LOGTRACE message$` statement - log trace message
- [x] `_LOGINFO message$` statement - log info message
- [x] `_LOGWARN message$` statement - log warning message
- [x] `_LOGERROR message$` statement - log error message
- [x] `_LOGMINLEVEL level` statement - set minimum log level

### Sound Extended Statements
- [x] `_SNDRAWBATCH handle, samples, count` statement - batch raw audio samples
- [x] `_MIDISOUNDBANK filename$` statement - set MIDI soundbank file

### Device Input Extended Statements
- [x] `_NEWHANDLER callback` statement - create new event handler

### Test Results
- Integration tests: 454 → 486 tests (+32 new tests)

---

## Phase 7: QB64 Extension Functions ✅ (Session 033)

### File I/O Extended
- [x] `_EMBEDDED$(name$)` function - get embedded file data

### Graphics Rendering Mode Functions
- [x] `_SMOOTH` function - smooth scaling mode constant
- [x] `_SMOOTHSHRUNK` function - smooth shrunk mode constant
- [x] `_SMOOTHSTRETCHED` function - smooth stretched mode constant
- [x] `_HARDWARE` function - hardware rendering mode constant
- [x] `_HARDWARE1` function - hardware1 rendering mode constant
- [x] `_SOFTWARE` function - software rendering mode constant

### Graphics Direction Constants
- [x] `_ANTICLOCKWISE` function - anticlockwise direction constant
- [x] `_CLOCKWISE` function - clockwise direction constant

### Print Mode Constants
- [x] `_KEEPBACKGROUND` function - keep background mode constant
- [x] `_FILLBACKGROUND` function - fill background mode constant
- [x] `_ONLYBACKGROUND` function - only background mode constant

### Alignment Constants
- [x] `_MIDDLE` function - middle alignment constant

### Display Mode Constants
- [x] `_AUTO` function - auto display mode constant

### Built-in Constants
- [x] `_NONE` constant - null/none value (0)

### Graphics Extended Statements
- [x] `_PRINTIMAGE handle` statement - print to image instead of screen
- [x] `_CLEAR resource` statement - clear specific resource
- [x] `_TOGGLE setting` statement - toggle a setting
- [x] `_MAPTRIANGLE` statement - 3D triangle mapping (12 parameters)
- [x] `_GLRENDER mode` statement - OpenGL render mode (stub)

### Test Results
- Integration tests: 486 → 506 tests (+20 new tests)

---

## Phase 7: QB64 Extension Functions ✅ (Session 034)

### Graphics Keyword Constants
- [x] `_CLIP` function - clipping mode constant for _PUTIMAGE
- [x] `_STRETCH` function - stretch mode constant for _PUTIMAGE
- [x] `_SEAMLESS` function - seamless image tiling constant
- [x] `_SQUAREPIXELS` function - square pixels mode constant
- [x] `_BEHIND` function - draw behind existing content constant

### Type/Mode Keyword Constants
- [x] `_ALL` function - all items/modes modifier constant
- [x] `_BLINK` function - text blinking mode constant
- [x] `_OFF` function - off state for toggles constant
- [x] `_ONLY` function - exclusive mode modifier constant

### Sound/Network Keywords
- [x] `_WAVE` function - waveform type constant for sound synthesis
- [x] `_DONTWAIT` function - non-blocking network operations constant

### Console Functions
- [x] `_CONSOLETITLE$` function - get console window title
- [x] `_CONSOLE` function - get console handle/state

### Environment Functions
- [x] `_SHELLHIDE` function - hidden shell constant

### Graphics Info Functions
- [x] `_GLCOMPAT` function - OpenGL compatibility mode constant

### Debug/Assert Functions
- [x] `_ASSERT(condition)` function - assert condition
- [x] `_ASSERTERROR$` function - get last assertion error message

### Display Extended Functions
- [x] `_FULLSCREENSMOOTH` function - fullscreen smooth mode constant
- [x] `_ALLOWFULLSCREEN` function - get allow fullscreen state
- [x] `_DISPLAYWIDTH` function - get display width in pixels
- [x] `_DISPLAYHEIGHT` function - get display height in pixels

### Console Statements
- [x] `_ECHO text$` statement - echo text to console
- [x] `_CONSOLETITLE title$` statement - set console window title

### Clipboard Statements
- [x] `_CLIPBOARD text$` statement - set clipboard text content

### Timing Statements
- [x] `_DELAY seconds` statement - delay execution

### Test Results
- Integration tests: 506 → 526 tests (+20 new tests)

---

## Phase 7: QB64 Extension Functions ✅ (Session 035)

### Memory Functions
- [x] `_MEMGET(block, offset)` function - get value from memory block
- [x] `_MEMEXISTS(block)` function - check if memory block exists

### String Utility Functions
- [x] `_TRIM$(text$)` function - trim whitespace from both ends

### Console Extended Functions
- [x] `_SCREENBUFFER` function - get console screen buffer handle
- [x] `_SCINKEY$` function - get console keyboard input

### Date/Time Extended Functions
- [x] `_YEAR` function - get current year
- [x] `_MONTH` function - get current month (1-12)
- [x] `_DAY` function - get current day of month (1-31)
- [x] `_WEEKDAY` function - get current day of week (0-6, Sunday=0)
- [x] `_HOUR` function - get current hour (0-23)
- [x] `_MINUTE` function - get current minute (0-59)
- [x] `_SECOND` function - get current second (0-59)

### Image Functions
- [x] `_PIXELSIZE(handle)` function - get pixel size in bytes (already existed, mapped)

### Window Functions
- [x] `_SCREENICON` function - get window minimized state

### Memory Statements
- [x] `_MEMPUT block, offset, value` statement - put value into memory block
- [x] `_MEMFILL block, offset, size, value` statement - fill memory block
- [x] `_MEMCOPY src, srcoff, bytes, dst, dstoff` statement - copy memory block
- [x] `_MEMFREE block` statement - free memory block

### Window Statements
- [x] `_SCREENICON` statement - minimize window

### Test Results
- Integration tests: 526 → 539 tests (+13 new tests)

---

## Phase 7: Previously Implemented (Discovered Session 035)

The following items were discovered to already be implemented during a TODO audit:

### Metacommands
- [x] `$CONSOLE` / `$CONSOLE:ONLY` metacommands - enable console window
- [x] `$SCREENHIDE` / `$SCREENSHOW` metacommands - hide/show window on startup
- [x] `$NOPREFIX` metacommand - allow keywords without underscore prefix
- [x] `$COLOR:0` / `$COLOR:32` metacommand - color mode

### Type Modifiers
- [x] `_BIT` type - single bit type modifier (0 or -1)

---

## Phase 7: Session 036 Additions

### Metacommands
- [x] `$RESIZE:ON` / `$RESIZE:OFF` - enable/disable window resize events
- [x] `$RESIZE:STRETCH` / `$RESIZE:SMOOTH` - resize scaling modes
- [x] `$STATIC` / `$DYNAMIC` - array allocation mode
- [x] `$DEBUG` - enable debug mode
- [x] `$INCLUDEONCE` - include file only once
- [x] `$EXEICON:'file.ico'` - set executable icon
- [x] `$VERSIONINFO:key=value` - set version information
- [x] `$ERROR message` - compiler error directive
- [x] `$EMBED:'filename'` - embed file in executable

### Control Flow
- [x] `OPTION _EXPLICIT` / `OPTION _EXPLICITARRAY` - require explicit declarations
- [x] `SELECT EVERYCASE` - evaluate all matching cases (not just first)

### Type Conversion Functions (QB64)
- [x] `_CV(type, string$)` function - generic convert string bytes to typed value
- [x] `_MK$(type, value)` function - generic convert value to string bytes
- [x] `_CAST(type, value)` function - explicit type conversion

### Variable Type Declaration
- [x] `_DEFINE A-Z AS type` statement - define default type for letter ranges (QB64 extended)

### Test Results
- Integration tests: 550 → 558 tests (+8 new tests)

---

## Phase 7: Session 037 Additions

### Final Metacommands
- [x] `$MIDISOUNDFONT:'file.sf2'` - set MIDI soundfont file for playback
- [x] `$UNSTABLE:feature` - enable unstable/experimental features
- [x] `$FORMAT` - code formatting directive (IDE support, no-op)
- [x] `$USELIBRARY:'library'` - use external library

### Test Results
- Integration tests: 558 → 562 tests (+4 new tests)

---

## Phase 1: Language Completeness ✅ (Discovered Complete Session 037)

### Memory Write Function
- [x] `POKE address, value` - write byte to memory (already implemented, generates `qb_poke()` call)

---

## Phase 6: Tooling & Ecosystem ✅ (Session 039)

### Optimization
- [x] Constant folding expansion - Compile-time evaluation of constant expressions
  - Arithmetic: `10 + 5` → `15`
  - String concatenation: `"Hello" + " World"` → `"Hello World"`
  - Pure built-in functions: `ABS(-42)` → `42`, `LEN("test")` → `4`
  - Trigonometric: `SIN(0)` → `0.0`, `COS(0)` → `1.0`
  - String functions: `UCASE$("hello")` → `"HELLO"`, `CHR$(65)` → `"A"`
  - Bitwise: `_SHL(1, 4)` → `16`, `_SHR(16, 2)` → `4`
  - Comparisons: `5 > 3` → `-1` (TRUE)
  - Implementation: `src/codegen/c_backend/const_fold.rs` (650+ lines)

### Test Results
- Integration tests: 693 → 720 tests (+27 constant folding tests)

---

## Session 039 Additions

### Phase 3: Graphics System
- [x] GET/PUT full pixel copying implementation *(Small - 1 session)* - stubs upgraded to full implementation

### Known Issues Resolved
- [x] Large array handling: Verify stack vs heap allocation *(Small - 1 session)* ✅ **VERIFIED**
      Arrays are correctly heap-allocated via `malloc()` in `emit_dim()` (stmt.rs:3010).
      REDIM uses `realloc()` with optional `_PRESERVE`. Only scalars use stack allocation.

### Documentation
- [x] Add doc comments to 16 undocumented modules *(Small - 1-2 sessions)* ✅ **ALREADY COMPLETE**
      All parser and codegen modules now have module-level documentation (`//!` comments).

---

## Session 040 Additions

### Testing
- [x] Add unit tests for new parser modules (graphics, audio, system, file_io) *(Completed Session 040)*

---

## Session 043 Additions

### Language Server Protocol ✅ (Session 043)
- [x] Implement LSP server core features
  - [x] Go-to-definition
  - [x] Find references
  - [x] Hover information with type details
  - [x] Code completion (keywords + built-in functions)
  - [x] Diagnostics (real-time error checking)
  - [x] Signature help for 50+ built-in functions

### Known Issues Resolved (Session 043)
- [x] **STRING * n in UDTs** - Fixed: The lexer tokenizes `s.PERSON` as a single identifier
      (supporting classic BASIC naming like `player.move`). The semantic analyzer now detects
      dotted names where the first part is a UDT variable and handles them as field assignments.
      Code generation properly uses strncpy for fixed-length string field assignments.

---

## Phase 6: Tooling & Ecosystem ✅ (Session 054 / 2026-01-25)

### Debugging (`tools/debug`) ✅ Complete
Debugger fully implemented with 50+ tests passing. See [ADR-0013](../adrs/ADR-0013-debugger-architecture.md) for architecture details.

**Completed:**
- [x] Debug infrastructure (symbols, values, frames, dap, sources, watch)
- [x] Runtime state capture (debug info in generated C via `--debug` flag)
- [x] Live breakpoint execution (runtime hooks with `qb_dbg_line()`)
- [x] Variable value reading (DAP server pipe communication)
- [x] Step execution (step into/over/out via `qb_dbg_enter_proc()`/`qb_dbg_exit_proc()`)
- [x] DAP server for VS Code/Cursor integration
- [x] Named pipe IPC for debugger ↔ debugee communication

**Infrastructure Details:**
- [x] Debug symbol extraction from AST (`symbols.rs`) — types, variables, scopes, procedures
- [x] Value representation types (`values.rs`) — scalars, arrays, UDTs, display formatting
- [x] Call stack frame structures (`frames.rs`) — stack frames, frame navigation, variable groups
- [x] Debug Adapter Protocol types (`dap.rs`) — full DAP message types for IDE integration
- [x] Multi-file source management (`sources.rs`) — $INCLUDE handling, line mapping
- [x] Watch expression parsing (`watch.rs`) — variables, array indices, UDT member access

### Documentation
- [x] Language reference documentation *(Large - 4-6 sessions)* — **COMPLETED 2026-01-23**
- [x] Tutorial/getting started guide *(Medium - 2-3 sessions)* — **COMPLETED 2026-01-25** — `docs/GETTING_STARTED.md`; Handbook and examples/README updated.

---

## Phase 8: VSCode Extension ✅ (Session 054 / 2026-01-25)

- [x] Formatter integration (qb64fresh-fmt)
- [x] Linter integration (qb64fresh-lint)
- [x] Format on save
- [x] Lint on save / lint on type
- [x] Build error integration (Problems panel)
- [x] Settings validation on startup
- [x] Code actions (quick fixes from linter suggestions)
- [x] Workspace symbol search (Ctrl+T)
- [x] Document symbols outline (Ctrl+Shift+O)
- [x] Rename symbol
- [x] Snippet expansion improvements
- [x] Debugger support (DAP) - Runtime integration complete (Phase 6), VSCode extension needs `launch.json` configuration

---

## Known Issues Resolved ✅ (Session 054 / 2026-01-25)

- [x] Windows-specific path handling in file I/O (fixed type mismatch in declarations)
- [x] **Unicode / QB64pe parity** — UCASE$/LCASE$ match QB64pe (ASCII a-z/A-Z only; other bytes preserved). We also have: _MAPUNICODE, _UCHARPOS, _UFONTHEIGHT, _ULINESPACING, _UPRINTWIDTH, _UPRINTSTRING, UTF-8 char-count helpers, _STRLEN. Full Unicode (e.g. ß→SS) is beyond QB64pe; parity achieved.

---

## QB64pe Bootstrap Progress ✅ (2026-01-28)

### Compilation Pipeline Completion
- [x] **QB64pe preprocessing** - All `$INCLUDE` directives processed (39 files, ~59K lines)
- [x] **QB64pe lexing** - 0 tokenization errors
- [x] **QB64pe parsing** - 0 parse errors
- [x] **QB64pe semantic analysis** - 0 type checking errors
- [x] **QB64pe code generation** - 114,924 lines of C code generated successfully

### Error Reduction Achievements
- [x] **Variable shadowing issue resolved** - Fixed local variable shadowing function parameters (was causing 188 errors)
- [x] **ParseNum UDT struct added** - Added `qbt_ParseNum` struct to runtime header (commit 1183a3a)
- [x] **Type name consistency fixed** - Fixed `QbString*` vs `qb_string*` inconsistency
- [x] **91% error reduction** - Reduced C compilation errors from 807 → 69 (91% reduction)

### Runtime Features Implementation
- [x] **File I/O** - All operations tested in `tests/integration_tests.rs` (OPEN, CLOSE, PRINT#, INPUT#, LINE INPUT#, WRITE#, GET, PUT, SEEK, LOC, LOF, EOF, FREEFILE)
- [x] **Keyboard Input** - Unix and Windows implementations working (INKEY$, _KEYHIT, _KEYCLEAR)
- [x] **String Operations** - All operations tested and working (concatenation, MID$ assignment, fixed-length strings, comparisons, arrays)
- [x] **Array Operations** - All operations tested and working (REDIM _PRESERVE, LBOUND/UBOUND, array parameters, large arrays, array scoping)
- [x] **Command-Line Mode** - Argument parsing works correctly (bootstrapped QB64pe displays help with `-h` flag)
- [x] **Error Handling** - ON ERROR GOTO, RESUME, error reporting implemented with line numbers

---

## Code Quality Improvements ✅ (2026-01-28)

### Error Recovery Tests
- [x] **Error recovery test suite** - 39 tests passing in `tests/error_recovery_tests.rs`
  - Parser continues after errors (doesn't stop at first error)
  - Semantic errors are collected, not just first error
  - Error messages are helpful (not confusing cascades)
  - Error recovery behavior doesn't cause incorrect cascades
  - Comprehensive coverage of parser and semantic error recovery
  - Validates error handling works correctly

---

## QB64pe Bootstrap ✅ **COMPLETE** (Session 064-071 / 2026-01-28)

**Status:** QB64pe compiles successfully and runs

**Achievements:**
- [x] QB64pe compiles without errors (0 parse, 0 semantic, 0 GCC errors)
- [x] Bootstrapped QB64pe runs and displays help (`-h` flag works)
- [x] All runtime features complete (file I/O, keyboard input, string operations, arrays)
- [x] Command-line mode verified
- [x] Error handling implemented (ON ERROR GOTO, RESUME)

**Test:** `cargo test --test bootstrap_tests qb64pe_compiles_successfully`

**Files:** `tests/bootstrap_tests.rs`, `docs/archive/BOOTSTRAP_PLAN_FULL.md` (Bootstrap Validation §7.1)

**Note:** Full execution testing (compiling BASIC programs with bootstrapped QB64pe) is pending but code generation is fully validated.

---

## LSP Incremental Parsing ✅ **COMPLETE** (Session 072 / 2026-01-28)

**Status:** All incremental optimizations implemented

**Working Features:**
- [x] Incremental sync (document change tracking)
- [x] Incremental lexing (token merging)
- [x] Incremental parsing (statement boundary detection)
- [x] Incremental semantic analysis (scope dependency tracking)
- [x] Diagnostics collection (parse and semantic errors)
- [x] Graceful fallback (full re-analysis when needed)

**Files:** `src/lsp/mod.rs`, `src/lsp/analysis/incremental.rs`, `src/lsp/analysis.rs`

**Performance Improvements:**
- Before: Full re-lexing, re-parsing, and re-analysis on every change
- After: Incremental updates only for affected regions, preserving unchanged work

---

## Code-level Completed Items ✅ (2026-01-28)

### Code Generation
- [x] **LINE statement style pattern support** (`stmt/mod.rs:1751-1794`)
  - 16-bit style pattern support for line drawing
  - Pattern applies to box outlines (B), ignored for filled boxes (BF) and plain lines
  - Solid line default (0xFFFF) when no style specified

### Runtime Library
- [x] **Per-image palette support** (`graphics_ffi.rs`)
  - `qb_palettecolor_get` and `qb_palettecolor` implemented with handle support
  - Per-image palette operations working correctly
  - Screen palette used when handle is 0

- [x] **Track last graphics position for STEP behavior** (`graphics_ffi.rs`)
  - `resolve_step_coordinates` implemented
  - STEP coordinates resolved relative to last graphics position
  - Works for LINE, CIRCLE, PSET, and other graphics operations

---

## Code-level Completed Items ✅ (Session 073 / 2026-01-28)

### Code Generation
- [x] **Multi-dimensional array I/O support** (`stmt/io.rs:153,182`, `file_io.rs`)
  - Added `dimensions: Vec<TypedArrayDimension>` field to `TypedInputTarget::ArrayElement` and `ArrayElementField`
  - Updated semantic checker to extract dimensions from symbol table in all locations
  - Updated codegen to use `calculate_array_index()` for proper multi-dimensional index calculation
  - Works for both console I/O (INPUT/PRINT) and file I/O (INPUT#/PRINT#)
  - Files modified: `src/semantic/typed_ir.rs`, `src/semantic/checker/statements/io.rs`, `src/semantic/checker/assignments.rs`, `src/codegen/c_backend/stmt/io.rs`, `src/codegen/c_backend/file_io.rs`

### Runtime Library
- [x] **Text scrolling implementation** (`graphics/sdl2.rs`)
  - Already implemented - `scroll_text_up()` method exists and is called when cursor exceeds bottom row
  - Handles both newline-triggered scrolling and line-wrapping-triggered scrolling
  - Shifts pixel rows up by `FONT_HEIGHT` pixels and clears bottom line

### Testing
- [x] **Golden file comparison test for QB64pe subset** (`bootstrap_tests.rs:279`)
  - Implemented test that compiles representative subset of QB64pe files
  - Compares generated C code against golden file
  - Supports `UPDATE_GOLDEN=1` environment variable for updating golden files
  - Gracefully skips if QB64pe files aren't available
  - Golden file stored at `tests/golden/qb64pe_subset.golden`

- [x] **INSTR 2-argument form support verification** (`integration_tests.rs`)
  - Verified implementation already exists (codegen calls `qb_instr2()` for 2-arg form)
  - Added test `instr_2arg_function()` to verify `INSTR(string, search)` works correctly
  - Implementation locations: `src/codegen/c_backend/expr.rs:157-164`, `src/codegen/c_backend/runtime/strings.rs:500`

### Debugger
- [x] **Watch expression evaluation documentation** (`tools/debug/src/watch.rs`)
  - Added comprehensive status section documenting infrastructure completion
  - Clarified runtime integration requirements (debug info emission, breakpoint hooks, memory access)
  - Infrastructure is complete and ready for runtime integration

---

## Testing Infrastructure (Session 079 / 2026-01-28)

### Graphics backend integration tests
- [x] **Graphics backend integration tests (SDL single-init challenge)**
  - Added `serial_test` to runtime dev-dependencies
  - Created `runtime/tests/graphics_integration.rs` with `#[cfg(feature = "graphics-sdl2")]` and two `#[serial]` tests: `graphics_init_and_shutdown`, `graphics_init_after_shutdown`
  - Both tests pass

### Audio backend integration tests
- [x] **Audio backend integration tests**
  - Created `runtime/tests/audio_integration.rs` with `#[cfg(feature = "audio-rodio")]` and two `#[serial]` tests: `audio_init_and_shutdown`, `audio_init_after_shutdown`
  - Both tests pass

### By-ref parameter codegen
- [x] **By-ref parameter codegen (parameters as pointers not dereferenced)**
  - Already implemented: pointer alias in `definitions.rs` (`int32_t* n = n_ref`), dereference in expr/assignments/control_flow via `current_func_byref_scalar_names`
  - Verified: `integration_tests::byref_scalar_sub_compiles_and_emits_write_through` and `execution_tests::byref_scalar_sub_modifies_caller_variable` both pass

---

## Option B Implementation Plan (2026-01-29)

*Completed items from [TODO_CONSOLIDATED.md](../ThingsToDo/TODO_CONSOLIDATED.md#option-b-complete-runtime-library) (Option B section; formerly OPTION_B_IMPLEMENTATION_PLAN.md / OPTION_B_COMPLETE_RUNTIME.md).*

### Prerequisites
- [x] Generated C compiles and links with runtime (e.g. `qb64pe_fresh.c` + `libqb64fresh_rt`).
- [x] Baseline test: run a trivial BASIC program (PRINT, no errors) with external runtime.

### Success criteria (minimum Option B)
- [x] Runtime builds and links with generated C (`--runtime external`).
- [x] `qb_error_pending()` exists and reflects runtime-set errors; ON ERROR GOTO and RESUME work.
- [x] `qb_evnt(line, incline, file)` exists (no-op or hook); ready for codegen to call.
- [x] String temp pool and cleanup work (inline or in runtime); no string leaks in normal use.

### Steps completed
- [x] **Step 1.1** — Runtime: Error state and `is_error_pending` (qb_error_pending, qb_set_error, qb_clear_error, qb_commit_error).
- [x] **Step 1.2** — Runtime: Set error from runtime code (file I/O, memory, error messages).
- [x] **Step 1.3** — Codegen: Emit `qb_error_pending()` checks after file I/O and system ops when `--runtime external` (emit_error_pending_goto_handler; OPEN, CLOSE, PRINT #, WRITE #, INPUT #, LINE INPUT #, GET, PUT, SEEK; KILL, RENAME, MKDIR, RMDIR, CHDIR, SHELL).
- [x] **Step 4.1** — Runtime: RESUME support (codegen emits qb_clear_error for RESUME NEXT / RESUME label when external).
- [x] **Step 4.2** — Runtime: ERL / ERR / _ERRORLINE / _ERRORMESSAGE$ (qb_err_code, qb_err_line, qb_errorline, qb_errormessage, qb_inclerrorline, qb_inclerrorfile).
- [x] **Step 7.1** — Audit and fill gaps: string-related symbols in qb64pe_fresh.c; all from runtime or inline in codegen; link succeeds; no undefined symbols.

---

## ThingsToDo Docs Moved to Archive (2026-01-30)

Completed or reference-only docs from `docs/ThingsToDo/` were moved to `docs/archive/`:

- [x] **BOOTSTRAP_VALIDATION.md** → merged into `docs/archive/BOOTSTRAP_PLAN_FULL.md` §7.1 (2026-01-31) — Bootstrap compilation fully validated; runtime features and regression tests complete. Optional next steps (QB4.5 compatibility test, self-compilation) documented in that section.
- [x] **QB64PE_INCREMENTAL_TESTING.md** → `docs/archive/QB64PE_INCREMENTAL_TESTING.md` — Full QB64pe compilation succeeds; Phases 1, 2, 4 complete. Summary in [TESTING-COMPLETED.md](TESTING-COMPLETED.md).
- [x] **ARCHITECTURAL_REVIEW_ITEM5_IMPLEMENTATION.md** → merged into [ARCHITECTURAL_REVIEW_COMPLETED.md](ARCHITECTURAL_REVIEW_COMPLETED.md) — Item 5 Phase 1 (Runtime Architecture) complete; Phase 2–3 deferred. Full content now in ARCHITECTURAL_REVIEW_COMPLETED.md §5.

---

## Option B: Runtime Library Completion ✅ (2026-01-30)

**Status:** ~85-90% of runtime functionality complete. Production-ready for most BASIC programs.

*Completed items from [TODO_CONSOLIDATED.md](../ThingsToDo/TODO_CONSOLIDATED.md#option-b-complete-runtime-library) (Option B section).*

### TIER 1: Critical Core ✅ COMPLETE

#### String System
- [x] Reference-counted strings (QbString) - fully functional
- [x] All major string functions (LEFT$, RIGHT$, MID$, INSTR, CHR$, ASC, UCASE$, LCASE$, TRIM$, HEX$, OCT$, _BIN$, STR$, VAL, etc.) - ~40+ functions
- [x] String concatenation, comparison
- [x] String/number conversions
- [x] Temporary string management (uses reference counting instead of pool)

**Note:** QbString is NOT binary-compatible with QB64pe's qbs* structure. For full compatibility, qbs structure would still be needed.

#### Memory Management System
- [x] `qb_memnew` - Allocate memory block
- [x] `qb_memfree` - Free memory block
- [x] `qb_memget` - Read from memory
- [x] `qb_memput` - Write to memory
- [x] `qb_memcopy` - Copy memory
- [x] `qb_memfill` - Fill memory
- [x] `qb_mem_of` - Wrap existing memory (_MEM)
- [x] `qb_memexists` - Check validity (_MEMEXISTS)
- [x] `qb_memelement` - Array element access (_MEMELEMENT)
- [x] `qb_memimage` - Image memory access (_MEMIMAGE)
- [x] `qb_memsound` - Sound memory access (_MEMSOUND)
- [x] `qb_offset` - Get pointer offset (_OFFSET)

#### Error Handling System
- [x] `qb_error_pending` - Check if error is pending
- [x] `qb_set_error` - Set pending error from runtime
- [x] `qb_clear_error` - Clear pending error (RESUME NEXT)
- [x] `qb_commit_error` - Commit pending to ERR/ERL
- [x] `qb_err_code` - ERR function
- [x] `qb_err_line` - ERL function
- [x] `qb_errorline` - _ERRORLINE function
- [x] `qb_errormessage` - _ERRORMESSAGE$ function
- [x] `qb_inclerrorline` - _INCLERRORLINE function
- [x] `qb_inclerrorfile` - _INCLERRORFILE$ function

### TIER 2: Essential Features ✅ COMPLETE

#### File I/O System (GFS Layer)
- [x] `qb_file_open` - OPEN statement (all modes: INPUT, OUTPUT, APPEND, RANDOM, BINARY)
- [x] `qb_file_close` - CLOSE statement
- [x] `qb_file_close_all` - Close all files
- [x] `qb_file_seek` - SEEK statement
- [x] `qb_file_seek_record` - Random access record positioning
- [x] `qb_file_get` - GET statement (binary)
- [x] `qb_file_put` - PUT statement (binary)
- [x] `qb_file_print_*` - PRINT # statement
- [x] `qb_file_write_*` - WRITE # statement
- [x] `qb_file_input_*` - INPUT # statement
- [x] `qb_file_line_input` - LINE INPUT # statement
- [x] `qb_eof` - EOF function
- [x] `qb_lof` - LOF function
- [x] `qb_loc` - LOC function
- [x] `qb_seek` - SEEK function
- [x] `qb_freefile` - FREEFILE function
- [x] `qb_field_start` / `qb_field_add` - FIELD statement support
- [x] `qb_lset` / `qb_rset` - LSET/RSET statements
- [x] `qb_file_kill` - KILL statement
- [x] `qb_file_rename` - NAME statement
- [x] `qb_file_exists` - _FILEEXISTS function
- [x] `qb_chdir` - CHDIR statement
- [x] `qb_mkdir` - MKDIR statement
- [x] `qb_rmdir` - RMDIR statement
- [x] `qb_dir_exists` - _DIREXISTS function
- [x] `qb_dir` - DIR$ function
- [x] `qb_cwd` - _CWD$ function
- [x] `qb_startdir` - _STARTDIR$ function
- [x] `qb_sub_environ` - ENVIRON statement

#### Graphics System (SDL2 Backend)
- [x] `qb_gfx_init` / `qb_gfx_shutdown` - Graphics initialization
- [x] `qb_gfx_screen` - SCREEN statement (modes 0-13)
- [x] `qb_gfx_cls` - CLS statement
- [x] `qb_gfx_color` - COLOR statement
- [x] `qb_gfx_locate` - LOCATE statement
- [x] `qb_gfx_print` - PRINT in graphics mode
- [x] `qb_gfx_pset` - PSET statement
- [x] `qb_gfx_line` - LINE statement
- [x] `qb_gfx_box` - BOX/LINE with BF option
- [x] `qb_gfx_circle` - CIRCLE statement
- [x] `qb_gfx_paint` - PAINT statement
- [x] `qb_gfx_point` - POINT function
- [x] `qb_rgb` / `qb_rgba` - _RGB32, _RGBA32 functions
- [x] `qb_gfx_newimage` - _NEWIMAGE function
- [x] `qb_gfx_loadimage` - _LOADIMAGE function
- [x] `qb_gfx_freeimage` - _FREEIMAGE statement
- [x] `qb_gfx_putimage` - _PUTIMAGE statement
- [x] `qb_gfx_get` / `qb_gfx_put` - GET/PUT array operations
- [x] `qb_gfx_palette` - PALETTE statement
- [x] `qb_palettecolor` - _PALETTECOLOR function/statement
- [x] `qb_gfx_view` - VIEW statement
- [x] `qb_gfx_window` - WINDOW statement
- [x] `qb_gfx_pmap` - PMAP function
- [x] `qb_gfx_pcopy` - PCOPY statement
- [x] `qb_mouse_*` - Mouse functions (_MOUSEX, _MOUSEY, _MOUSEBUTTON, etc.)
- [x] `qb_loadfont` / `qb_font` - Font functions
- [x] `qb_clipboard_get` / `qb_clipboard_set` - Clipboard functions
- [x] `qb_fullscreen` - _FULLSCREEN function/statement
- [x] `qb_screenmove` / `qb_screenshow` / `qb_screenhide` - Window control
- [x] `qb_sub__title` - _TITLE statement
- [x] `qb_maptriangle` - _MAPTRIANGLE statement

#### Audio System (Rodio Backend)
- [x] `qb_audio_init` / `qb_audio_shutdown` - Audio initialization
- [x] `qb_beep` - BEEP statement
- [x] `qb_sound` - SOUND statement
- [x] `qb_play` - PLAY statement (MML)
- [x] `qb_sndopen` - _SNDOPEN function
- [x] `qb_sndclose` - _SNDCLOSE statement
- [x] `qb_sndplay` - _SNDPLAY statement
- [x] `qb_sndstop` - _SNDSTOP statement
- [x] `qb_sndpause` - _SNDPAUSE statement
- [x] `qb_sndresume` - _SNDRESUME statement
- [x] `qb_sndloop` - _SNDLOOP statement
- [x] `qb_sndvol` - _SNDVOL statement
- [x] `qb_sndbal` - _SNDBAL statement
- [x] `qb_sndlen` - _SNDLEN function
- [x] `qb_sndgetpos` - _SNDGETPOS function
- [x] `qb_sndsetpos` - _SNDSETPOS statement
- [x] `qb_sndplaying` - _SNDPLAYING function
- [x] `qb_sndpaused` - _SNDPAUSED function
- [x] `qb_sndcopy` - _SNDCOPY function
- [x] `qb_sndplayfile` - _SNDPLAYFILE function
- [x] `qb_sndopenraw` - _SNDOPENRAW function
- [x] `qb_sndraw` / `qb_sndraw_stereo` - _SNDRAW statement
- [x] `qb_sndrawlen` - _SNDRAWLEN function

### TIER 3: Important Features ✅ COMPLETE

#### Event System
- [x] `qbevent` - Global debug event flag
- [x] `qb_evnt` - Statement-level debug hook
- [x] `qb_on_key` - ON KEY(n) GOSUB registration
- [x] `qb_key_control` - KEY(n) ON/OFF/STOP
- [x] `qb_on_timer` - ON TIMER(n) GOSUB registration
- [x] `qb_timer_control` - TIMER ON/OFF/STOP
- [x] `qb_on_uevent` - ON UEVENT GOSUB registration
- [x] `qb_uevent_control` - UEVENT ON/OFF/STOP
- [x] `qb_uevent_trigger` - UEVENT statement
- [x] `qb_check_key_event` - Check for key events
- [x] `qb_check_timer_event` - Check for timer events
- [x] `qb_check_uevent` - Check for user events
- [x] `qb_keyhit` - _KEYHIT function
- [x] `qb_keydown` - _KEYDOWN function
- [x] `qb_inkey` - INKEY$ function
- [x] `qb_mouse_x` / `qb_mouse_y` - _MOUSEX, _MOUSEY functions
- [x] `qb_mouse_button` - _MOUSEBUTTON function
- [x] `qb_mouse_input` - _MOUSEINPUT function
- [x] `qb_mouse_wheel` - _MOUSEWHEEL function
- [x] `qb_mouse_move` - _MOUSEMOVE statement

#### Date/Time Functions
- [x] `qb_date` - DATE$ function (MM-DD-YYYY)
- [x] `qb_time` - TIME$ function (HH:MM:SS)
- [x] `qb_date64` - _DATE$ function (YYYY-MM-DD)
- [x] `qb_time64` - _TIME$ function (HH:MM:SS)
- [x] `qb_timer` - TIMER function
- [x] `qb_sleep` / `qb_delay` - _DELAY statement
- [x] `qb_cwd` - _CWD$ function
- [x] `qb_startdir` - _STARTDIR$ function
- [x] `qb_os` - _OS$ function

#### Math Functions
- [x] `qb_abs_*` - ABS function (int/float)
- [x] `qb_sgn_*` - SGN function (int/float)
- [x] `qb_int` / `qb_fix` - INT, FIX functions
- [x] `qb_cint` / `qb_clng` - CINT, CLNG functions
- [x] `qb_sin` / `qb_cos` / `qb_tan` - Trigonometric functions
- [x] `qb_atn` / `qb_asin` / `qb_acos` - Inverse trigonometric
- [x] `qb_sinh` / `qb_cosh` / `qb_tanh` - Hyperbolic functions
- [x] `qb_sqr` - SQR function (with error handling)
- [x] `qb_log` / `qb_log10` - LOG, LOG10 functions (with error handling)
- [x] `qb_exp` - EXP function (with error handling)
- [x] `qb_pow` / `qb_pow_int` - Exponentiation
- [x] `qb_randomize` / `qb_rnd` - Random number functions
- [x] `qb_d2r` / `qb_r2d` - Degree/radian conversion
- [x] `qb_pi` / `qb_e` - Mathematical constants
- [x] `qb_min_*` / `qb_max_*` - MIN, MAX functions
- [x] `qb_readbit` / `qb_setbit` / `qb_resetbit` / `qb_togglebit` - Bit manipulation
- [x] `qb_rol` / `qb_ror` - Rotate left/right

### TIER 4: Nice to Have (Partial)

#### Type Conversions
- [x] `qb_hex` - HEX$ function
- [x] `qb_oct` - OCT$ function
- [x] `qb_bin` - _BIN$ function

#### Additional Features
- [x] **Clipboard:** `qb_clipboard_get` / `qb_clipboard_set` - Platform-specific clipboard access
- [x] **Shell:** `qb_shell` - SHELL command execution
- [x] **Environment:** `qb_sub_environ` - ENVIRON statement (partial)
- [x] **Networking:** `qb_net_*` functions - _OPENHOST, _OPENCONNECTION, _OPENCLIENT, GET#/PUT# on network handles
- [x] **Dialogs:** `qb_openfiledialog`, `qb_savefiledialog`, `qb_selectfolderdialog`, `qb_messagebox_ex`
- [x] **Joystick:** `qb_stick`, `qb_strig`, `qb_devices`, `qb_axis`, `qb_button`
- [x] **Interrupt emulation:** `qb_interrupt` / `qb_interruptx` - DOS interrupt emulation (INT 0x33 mouse)

### Implementation Phases ✅ COMPLETE

- [x] **Phase 1:** String System (4-6 weeks) - QbString implemented with ~40+ functions
- [x] **Phase 2:** Memory & Error Handling (2-3 weeks) - All _MEM* functions and error handling complete
- [x] **Phase 3:** File I/O (3-4 weeks) - Full GFS layer with all file operations
- [x] **Phase 4:** Graphics (4-6 weeks) - SDL2 backend with all drawing primitives
- [x] **Phase 5:** Audio (2-3 weeks) - Rodio backend with all sound functions
- [x] **Phase 6:** Polish (2-4 weeks) - Event system, date/time, math functions complete

### Statistics

- **Original Estimated Scope:** 17,000 - 27,000 lines
- **Completed:** ~85-90% of runtime functionality
- **Remaining Scope:** ~3,000 - 5,000 lines (mostly compatibility features and advanced features)
- **Status:** Production-ready for most BASIC programs

**Remaining items** (not blocking production use):
- qbs-compatible string system (only needed for binary compatibility)
- CMEM support (only needed for legacy programs)
- MKx$/CVx type conversion functions
- File locking (LOCK/UNLOCK)
- COM port support
- Some advanced graphics features
- Threading support
- HTTP client
- Compression (zlib)

---

## Partial Implementations Doc – Completed Items (moved 2026-01-31)

*Completed items moved from [PARTIAL_IMPLEMENTATIONS.md](../ThingsToDo/PARTIAL_IMPLEMENTATIONS.md) “Recent Changes” section.*

### 2026-01-31
- [x] **Documentation update** – Verified against current codebase. External runtime: I/O split into `io/` submodule (mod.rs, print.rs, input.rs, file.rs). New modules: bitops, buffer, cmem, completion, condvar, condvar_ffi, console_display_ffi, cp437, events, filepath, game_controller_ffi, http, http_ffi, logging, logging_ffi, mem_lock, mutex, mutex_ffi, qbs_compat, thread. Audio has midi.rs. Inline runtime: bitops.rs, logging.rs added. All references to `runtime/src/io.rs` updated to `runtime/src/io/` (file.rs, input.rs). HTTP client (libqb_http_*) and logging (qb_log*, libqb_log*) documented. Test count note updated to 1,700+.
- [x] **Implementation Order – Core I/O** – Strings, console I/O, file I/O, keyboard, math (done for bootstrap).
- [x] **Implementation Order – Graphics foundation** – SDL2 window, screen modes, CLS, COLOR, PSET, LINE, CIRCLE, VIEW, WINDOW, GET/PUT, _PUTIMAGE, alpha blending, PCOPY, screen pages (done in external).
- [x] **Implementation Order – Graphics extended** – Images, fonts, _LOADFONT, _PRINTSTRING, Unicode; _MAPTRIANGLE, _COPYPALETTE, _DISPLAYORDER.
- [x] **Implementation Order – Audio** – BEEP, SOUND, _SNDOPEN/_SNDPLAY family, PLAY, rodio backend (done in external).
- [x] **Implementation Order – Input** – Mouse, game controller/joystick (STICK, STRIG, _DEVICES, _AXIS, _BUTTON, ON STRIG) (done in external).
- [x] **Implementation Order – System** – Dialogs, clipboard; networking (`qb_net_*` in runtime and header); directory ops (`qb_chdir`, `qb_mkdir`, `qb_rmdir`, `qb_dir_exists` in header and io.rs) (done in external).
- [x] **Phase 1.1 Memory Management** – _MEMNEW, _MEMFREE, _MEMEXISTS, _MEMCOPY, _MEMGET, _MEMPUT, _MEMFILL, _MEM, _MEMELEMENT complete (inline + external). _MEMSOUND/_MEMIMAGE stub (returns empty).
- [x] **Phase 1.2 String System** – All functions complete: LEN, LEFT$, RIGHT$, MID$, INSTR, _INSTRREV, UCASE$, LCASE$, LTRIM$, RTRIM$, _TRIM$, SPACE$, STRING$, CHR$, ASC, STR$, VAL, HEX$, OCT$, _BIN$ (inline + external).
- [x] **Phase 1.3 Math Functions** – All complete: ABS, SGN, INT, FIX, CINT, CLNG, CSNG, CDBL, SQR, LOG, EXP, SIN, COS, TAN, ATN, _ASIN, _ACOS, _ATAN2, _SINH, _COSH, _TANH, _PI, RND, RANDOMIZE, _D2R, _R2D (inline + external).
- [x] **Phase 1.4 File I/O** – All complete: OPEN, CLOSE, PRINT #, INPUT #, LINE INPUT #, WRITE #, GET, PUT, SEEK, LOC, LOF, EOF, FREEFILE, KILL, NAME, CHDIR, MKDIR, RMDIR, _FILEEXISTS, _DIREXISTS (inline + external).
- [x] **Phase 3.1 Keyboard** – INKEY$, INPUT, LINE INPUT, _KEYHIT, _KEYDOWN, _KEYCLEAR complete (inline + external).
- [x] **Phase 3.2 Mouse** – _MOUSEINPUT, _MOUSEX/Y, _MOUSEBUTTON, _MOUSEWHEEL, _MOUSESHOW/HIDE, _MOUSEMOVE full in external; inline stubs.
- [x] **Phase 3.3 Game Controller** – STICK, STRIG, _DEVICES, _DEVICE$, _AXIS, _BUTTON full in external; inline stubs.
- [x] **Phase 4 Audio** – BEEP, SOUND, PLAY, _SNDOPEN/_SNDCLOSE, _SNDPLAY/PAUSE/STOP, _SNDLOOP/VOL/BAL, _SNDPLAYING/LEN/GETPOS/SETPOS full in external; inline stubs.
- [x] **Phase 5.1 Timing** – TIMER, _DELAY, SLEEP, _LIMIT, DATE$, TIME$ complete (inline + external).
- [x] **Phase 5.2 Environment and Shell** – ENVIRON$, ENVIRON, COMMAND$, _OS$, _SHELL, SHELL, SYSTEM, END complete (inline + external).
- [x] **Phase 5.3 Dialogs and Clipboard** – _MESSAGEBOX, _OPENFILEDIALOG$, _SAVEFILEDIALOG$, _SELECTFOLDERDIALOG$, _CLIPBOARD$ full in external; inline stubs.
- [x] **Phase 5.4 Error Handling** – ON ERROR GOTO, ON ERROR GOTO _NEWHANDLER, RESUME, ERR, ERL, _ERRORMESSAGE$, ERROR complete (inline + external).
- [x] **Phase 6 Networking** – _OPENHOST, _OPENCONNECTION, _OPENCLIENT, _CONNECTED, _CLOSEHOST (qb_net_*) implemented in external runtime and header; inline stubs. API differs from QB64pe (numeric port vs string).
- [x] **File I/O Features (FIELD/LSET/RSET)** – qb_field_start, qb_field_add, LSET/RSET fully implemented in runtime/src/io/file.rs.
- [x] **Debugger Infrastructure** – run/pause/step_over/step_into/step_out/stop, process_events/handle_event, variable evaluation, attach mode, expression evaluation (DAP) infrastructure complete; requires runtime integration hooks.

### 2026-01-28
- [x] **Documentation update** – Verified against current codebase. External runtime tree now includes `array_registry.rs`. Array metadata note corrected (inline: `src/codegen/c_backend/runtime/arrays.rs`; external: `runtime/src/array_registry.rs`).

### 2026-01-27
- [x] **FIELD statement** – Fully implemented (see File I/O Features in PARTIAL_IMPLEMENTATIONS.md).
- [x] **LSET/RSET** – Fully implemented (see File I/O Features in PARTIAL_IMPLEMENTATIONS.md).
- [x] **Graphics features** – Console scrolling, per-image palettes, STEP position tracking, and window functions verified/implemented (see Graphics Features in PARTIAL_IMPLEMENTATIONS.md).

### 2026-01-26
- [x] **Array metadata tracking** – Hash table registry; `qb_array_register` / `qb_array_register_md`, `qb_array_update`, `qb_array_erase`; LBOUND/UBOUND correct. Paths documented in PARTIAL_IMPLEMENTATIONS.md.
