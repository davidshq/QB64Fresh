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
- [ ] Implement `_KEYHIT` function (deferred - QB64-specific extension)
- [ ] Implement `_KEYDOWN` function (deferred - QB64-specific extension)

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
- [x] `_TRIM$` function
- [x] `STRING$()` function
- [x] `MKI$`, `MKL$`, `MKS$`, `MKD$` (pack numbers to strings)
- [x] `CVI`, `CVL`, `CVS`, `CVD` (unpack strings to numbers)

### Date/Time Enhancements
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

### Testing
- [ ] Expand test suite for all built-ins
- [ ] Integration tests for compiled programs
- [ ] Compatibility tests against QB64 programs
- [ ] Fuzzing for parser robustness

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
