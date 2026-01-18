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
- [ ] `SWAP` statement (already implemented in parser, needs verification)
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

## Phase 2: Core Extensions (Medium-Term)

### Conditional Compilation
- [ ] `$IF` / `$ELSE` / `$ELSEIF` / `$END IF` directives
- [ ] `$LET` directive for compile-time variables
- [ ] `$INCLUDE` - actually read and parse included files
- [ ] `$CHECKING` directive
- [ ] Built-in constants: `WIN`, `LINUX`, `MAC`, `32BIT`, `64BIT`

### Memory Operations
- [ ] `_MEM` type full support
- [ ] `_MEMNEW` function
- [ ] `_MEMFREE` statement
- [ ] `_MEMGET` / `_MEMPUT`
- [ ] `_MEMCOPY`
- [ ] `_MEMFILL`
- [ ] `_OFFSET` pointer arithmetic

### String Enhancements
- [ ] `_INSTRREV` function
- [ ] `_TRIM$` function
- [ ] `STRING$()` function
- [ ] `MKI$`, `MKL$`, `MKS$`, `MKD$` (pack numbers to strings)
- [ ] `CVI`, `CVL`, `CVS`, `CVD` (unpack strings to numbers)

### Date/Time Enhancements
- [ ] `_DATE$` (QB64 format)
- [ ] `_TIME$` (QB64 format)
- [ ] `_AUTODISPLAY` / `_DISPLAY` timing

---

## Phase 3: Graphics System (Medium-Term)

### Screen Setup
- [ ] `SCREEN` statement (text and graphics modes)
- [ ] `WIDTH` statement
- [ ] `CLS` statement (clear screen)
- [ ] `COLOR` statement
- [ ] `LOCATE` statement (cursor positioning)
- [ ] `VIEW` statement (viewport)
- [ ] `WINDOW` statement (coordinate mapping)

### Basic Drawing
- [ ] `PSET` / `PRESET` (plot point)
- [ ] `LINE` statement (lines and boxes)
- [ ] `CIRCLE` statement
- [ ] `PAINT` statement (flood fill)
- [ ] `DRAW` statement (turtle graphics)
- [ ] `POINT()` function (read pixel)

### QB64 Graphics Extensions
- [ ] `_NEWIMAGE` function
- [ ] `_LOADIMAGE` function
- [ ] `_FREEIMAGE` statement
- [ ] `_PUTIMAGE` statement
- [ ] `_SOURCE` / `_DEST` statements
- [ ] `_COPYIMAGE` function
- [ ] `_SCREENIMAGE` function
- [ ] `_WIDTH` / `_HEIGHT` functions
- [ ] `_PRINTSTRING` statement
- [ ] `_PRINTWIDTH` function
- [ ] `_RGB` / `_RGBA` functions
- [ ] `_RGB32` / `_RGBA32` functions
- [ ] Alpha blending support

### Graphics Backend Integration
- [ ] Integrate SDL2 or similar for window management
- [ ] Implement frame buffer
- [ ] Implement `_DISPLAY` / `_AUTODISPLAY`
- [ ] Hardware acceleration option

---

## Phase 4: Sound System (Medium-Term)

### Classic BASIC Sound
- [ ] `BEEP` statement
- [ ] `SOUND` statement (frequency, duration)
- [ ] `PLAY` statement (music macro language)

### QB64 Sound Extensions
- [ ] `_SNDOPEN` function
- [ ] `_SNDCLOSE` statement
- [ ] `_SNDPLAY` statement
- [ ] `_SNDSTOP` statement
- [ ] `_SNDPAUSE` / `_SNDRESUME`
- [ ] `_SNDLOOP` statement
- [ ] `_SNDVOL` statement
- [ ] `_SNDBAL` statement (balance)
- [ ] `_SNDLEN` function
- [ ] `_SNDGETPOS` / `_SNDSETPOS`
- [ ] `_SNDPLAYING` function
- [ ] `_SNDRAW` for direct audio synthesis

### Sound Backend Integration
- [ ] Integrate audio library (SDL2_mixer, miniaudio, or similar)
- [ ] Support common formats (WAV, MP3, OGG)

---

## Phase 5: Advanced Features (Long-Term)

### C Library Integration
- [ ] `DECLARE LIBRARY` statement
- [ ] `DECLARE DYNAMIC LIBRARY`
- [ ] Automatic header parsing
- [ ] Type marshalling for C interop
- [ ] Callback support

### Networking (QB64 Extensions)
- [ ] `_OPENHOST` function
- [ ] `_OPENCONNECTION` function
- [ ] `_OPENCLIENT` function
- [ ] `_CONNECTED` function
- [ ] Network stream I/O

### Input Devices
- [ ] Mouse support (`_MOUSEX`, `_MOUSEY`, `_MOUSEBUTTON`, etc.)
- [ ] Joystick/gamepad support
- [ ] Touch input support

### Clipboard
- [ ] `_CLIPBOARD$` function (get)
- [ ] `_CLIPBOARD$` statement (set)

### System Integration
- [ ] `SHELL` statement
- [ ] `_SHELLHIDE`
- [ ] `KILL` statement (delete file)
- [ ] `NAME` statement (rename file)
- [ ] `MKDIR` / `RMDIR` / `CHDIR`
- [ ] `_DIREXISTS` / `_FILEEXISTS`
- [ ] `_DIR$` function (directory listing)

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

**Design Decisions Needed:**
- Graphics backend: SDL2 vs native vs WebAssembly target
- Sound backend: SDL2_mixer vs miniaudio vs platform-native
- How to handle `PEEK`/`POKE` in a safe manner
- Memory model for `_MEM` operations
