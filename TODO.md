# QB64Fresh TODO

*Last updated: 2026-01-21 (Session 043)*

A prioritized roadmap for QB64Fresh development. For completed features, see [TODO-completed.md](TODO-completed.md).

---
- All Phase 1 items have been completed - see TODO-completed.md.
- All Phase 2 items have been completed - see TODO-completed.md.
- All Phase 4 items have been implemented - see TODO-completed.md.
- Phase 5:
  - All C Library Integration items completed - see TODO-completed.md.
- Phase 7:
  - All metacommands have been implemented - see TODO-completed.md.
---

## Phase 3: Graphics System - Remaining Items

### Not Yet Implemented
- [ ] Alpha blending support *(Medium - 2-3 sessions)*
- [ ] Hardware acceleration option *(Large - 4-6 sessions, requires GPU backend work)*
- [ ] Multiple screen pages *(Medium - 2-3 sessions)*

---

## Phase 5: Advanced Features - Remaining Items

### Networking
- [ ] Network stream I/O (PUT/GET with network handles) *(Medium - 2-3 sessions)*

### Input Devices
- [ ] Joystick/gamepad support *(Medium - 2-3 sessions, SDL2 has gamepad API)*
- [ ] Touch input support *(Medium - 2-3 sessions)*

### Multi-threading (QB64 Extension)
- [ ] `_THREAD` support *(Large - 4-6 sessions, complex runtime changes)*
- [ ] Thread synchronization primitives *(Medium - 2-3 sessions, after _THREAD)*

---

## Phase 6: Tooling & Ecosystem

### Debugging
- [ ] Source-level debugging support *(X-Large - 10+ sessions, requires debug info generation)*
  - [ ] Breakpoints *(included above)*
  - [ ] Variable inspection *(included above)*
  - [ ] Step execution *(included above)*

### Optimization
- [ ] Dead code elimination *(Medium - 2-3 sessions)*
- [ ] Loop optimization *(Medium - 2-3 sessions)*
- [ ] Inline small functions *(Medium - 2-3 sessions)*

### Documentation
- [ ] Language reference documentation *(Large - 4-6 sessions)*
- [ ] Tutorial/getting started guide *(Medium - 2-3 sessions)*

### Testing (See TESTING_INFRASTRUCTURE_PLAN.md for details)
- [ ] Compatibility tests against QB64 programs *(Ongoing - add as discovered)*

---

## Phase 7: Missing Language Features

### OpenGL Commands (Intentionally Excluded)

QB64PE includes ~300+ `_GL*` commands (e.g., `_GLBEGIN`, `_GLEND`, `_GLVERTEX3F`, etc.)
for raw OpenGL access. These are **intentionally excluded** from QB64Fresh because:

1. We use SDL2/winit for graphics, not raw OpenGL
2. Raw GL commands expose implementation details that reduce portability
3. The `_MAPTRIANGLE` statement provides 3D capability without raw GL
4. Future WebGL/Vulkan backends would be incompatible with GL commands

If raw OpenGL is needed, users can use `DECLARE LIBRARY` to call OpenGL functions directly.

---
## Phase 8: Class VB RAD Visual Designer

---

## Known Issues / Technical Debt

### Low Priority

- [ ] Unicode support: Currently ASCII-focused *(Large - 4-6 sessions for full Unicode)*
- [ ] Windows-specific path handling in file I/O *(Small - 1 session)*

---

## Notes

**Dependencies Integrated:**
- SDL2 crate for graphics (feature: `graphics-sdl2`)
- rodio crate for audio (feature: `audio-rodio`)
- Runtime library provides file I/O, graphics, and audio

**Design Decisions Made:**
- Graphics backend: Trait-based abstraction with SDL2 as default, mock for testing
- Sound backend: Trait-based abstraction with rodio as default, mock for testing
- PEEK/POKE use sandboxed conventional memory (cmem) - a 1MB heap buffer emulating DOS memory model, matching QB64pe's approach. This allows legacy programs to do pointer arithmetic tricks safely without accessing real system memory.

**Design Decisions Needed:**
- Memory model for `_MEM` operations (integration with cmem for VARPTR compatibility)

---

## Code Quality Summary

**Overall Health:** Excellent
- **Test Coverage:** 81.63% (850+ tests, including 31 LSP tests)
- **Clippy Warnings:** 0
- **Security Issues:** 0

**File Size Concerns:** (monitor for growth)
| File | Lines | Status |
|------|-------|--------|
| runtime.rs | 4,141 | Large - C code generator, hard to split |
| stmt.rs (codegen) | 3,690 | Large - File I/O extracted |
| statements.rs (parser) | 3,865 | Large - Already split from main parser |

**Stub Functions:** Many graphics/audio/input functions are stubs returning safe defaults.
This is intentional for compatibility. See runtime.rs for implementation guidance.

It IS a genuine bug. Look at the results:

IDEErrorColor (correct) - declared at cfg_global.bas:30 and used in 8 places
IDEErrroColor (typo) - used in only 1 place, on the same line that uses the correct spelling twice!

TempDarkerErrorColor~& = _RGB32(_RED32(IDEErrorColor) * .5, _GREEN32(IDEErrroColor) * .5, _BLUE32(IDEErrorColor) * .5)
★ Insight ─────────────────────────────────────

This is a real bug in QB64PE. The typo IDEErrroColor is never declared, so BASIC treats it as an implicit variable initialized to 0. This means the green component of TempDarkerErrorColor is always 0 * 0.5 = 0 instead of using the actual green value from IDEErrorColor. The darker error color ends up more purple/magenta than intended.

QB64PE compiles it without error because BASIC allows undeclared variables - they're just implicitly created. Our compiler does the same, which is why this isn't causing a compilation error for us either.
