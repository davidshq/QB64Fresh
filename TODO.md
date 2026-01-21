# QB64Fresh TODO

*Last updated: 2026-01-20 (Session 034)*

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

### Medium Priority - QB64 Extensions

#### String Functions
- [ ] `_CV(type, string$)` function - generic convert string to type
- [ ] `_MK$(type, value)` function - generic convert value to string

#### Utility Functions
- [ ] `_CAST(type, value)` function - explicit type cast
- [ ] `_DEFINE` statement - define default variable types by letter range

#### Networking (Extended)
- [x] `_DONTWAIT` keyword - non-blocking network operations ✅ (Session 034)

### Lower Priority - QB64 Advanced Extensions

#### Graphics (Extended)
- [x] `_CLIP` keyword - clipping mode for _PUTIMAGE ✅ (Session 034)
- [x] `_SQUAREPIXELS` keyword - square pixels mode ✅ (Session 034)
- [x] `_SEAMLESS` keyword - seamless image tiling ✅ (Session 034)
- [x] `_STRETCH` keyword - stretch mode for _PUTIMAGE ✅ (Session 034)
- [x] `_BEHIND` keyword - draw behind existing content ✅ (Session 034)

#### Sound (Extended)
- [x] `_WAVE` keyword - waveform type for sound synthesis ✅ (Session 034)

#### Console Mode
- [ ] `$CONSOLE` / `$CONSOLE:ONLY` metacommands

#### Resize Events
- [ ] `$RESIZE:ON` / `$RESIZE:OFF` metacommands
- [ ] `$RESIZE:STRETCH` / `$RESIZE:SMOOTH` metacommands

#### File I/O (Extended)
- [ ] `$EMBED:'filename'` metacommand - embed file in executable

#### Miscellaneous QB64 Features
- [ ] `OPTION _EXPLICIT` / `OPTION _EXPLICITARRAY` - require declarations
- [ ] `SELECT EVERYCASE` - check all cases

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

### Type Modifiers & Keywords (Partially Implemented)
- [ ] `_BIT` type - single bit type modifier
- [x] `_ALL` keyword - all items/modes modifier ✅ (Session 034)
- [x] `_BLINK` keyword - text blinking mode ✅ (Session 034)
- [x] `_OFF` keyword - off state for toggles ✅ (Session 034)
- [x] `_ONLY` keyword - exclusive mode modifier ✅ (Session 034)

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
