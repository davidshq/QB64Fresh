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
- [x] Built-in platform constants: `WIN`, `WINDOWS`, `LINUX`, `MAC`, `32BIT`, `64BIT`
  - Evaluated at compile time for `$IF` conditional compilation
  - Uses BASIC convention: -1 for TRUE, 0 for FALSE
  - Supports boolean operators: `AND`, `OR`, `NOT`, `XOR`
  - Supports comparisons: `=`, `<>`, `<`, `>`, `<=`, `>=`

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

---

## Phase 6: Tooling & Ecosystem (Partial)

### Testing
- [x] Expand test suite for all built-ins (315 integration tests)
- [x] Achieve 80%+ line coverage (81.63% achieved!)
- [x] SHARED variable scope - SUBs/FUNCTIONs can access module-level variables via SHARED statement

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

- **Total source code:** ~29,130+ lines of Rust
- **Unit tests:** 205
- **Integration tests:** 315
- **Total tests:** 600+
- **Line coverage:** 81.63% ✅
- **Statement types:** 60+
- **Built-in functions:** 30+

---

*Last updated: 2026-01-19 (Session 026 - Phase 2 complete with platform constants)*
