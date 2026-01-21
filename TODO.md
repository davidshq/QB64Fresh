# QB64Fresh TODO

*Last updated: 2026-01-20 (Session 038)*

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

### Language Server Protocol
- [ ] Implement full LSP server *(Large - 5-8 sessions for full implementation)*
  - [ ] Go-to-definition *(included above)*
  - [ ] Find references *(included above)*
  - [ ] Hover information *(included above)*
  - [ ] Code completion *(included above)*
  - [ ] Diagnostics (real-time error checking) *(included above)*
  - [ ] Signature help *(included above)*

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
- [x] Add unit tests for new parser modules (graphics, audio, system, file_io) *(Completed Session 040)*

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

## Known Issues / Technical Debt

### Low Priority

- [x] **STRING * n in UDTs** *(Completed)*
      Fixed: The lexer tokenizes `s.PERSON` as a single identifier (supporting classic
      BASIC naming like `player.move`). The semantic analyzer now detects dotted names
      where the first part is a UDT variable and handles them as field assignments.
      Code generation properly uses strncpy for fixed-length string field assignments.

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
- **Test Coverage:** 81.63% (820+ tests)
- **Clippy Warnings:** 0
- **Security Issues:** 0

**File Size Concerns:** (monitor for growth)
| File | Lines | Status |
|------|-------|--------|
| runtime.rs | 3,956 | Large - C code generator, hard to split |
| stmt.rs (codegen) | 3,593 | Large - File I/O extracted |
| statements.rs (parser) | 3,595 | Large - Already split from main parser |

**Stub Functions:** Many graphics/audio/input functions are stubs returning safe defaults.
This is intentional for compatibility. See runtime.rs for implementation guidance.
