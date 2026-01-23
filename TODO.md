# QB64Fresh TODO

*Last updated: 2026-01-23 (Session 044)*

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

### Debugging (`tools/debug`)
Debugger infrastructure has been scaffolded as a workspace member (44 tests passing):
- [x] Debug symbol extraction from AST (`symbols.rs`) - types, variables, scopes, procedures
- [x] Value representation types (`values.rs`) - scalars, arrays, UDTs, display formatting
- [x] Call stack frame structures (`frames.rs`) - stack frames, frame navigation, variable groups
- [x] Debug Adapter Protocol types (`dap.rs`) - full DAP message types for IDE integration
- [x] Multi-file source management (`sources.rs`) - $INCLUDE handling, line mapping
- [x] Watch expression parsing (`watch.rs`) - variables, array indices, UDT member access

**Still needs runtime integration:**
- [ ] Runtime state capture *(requires debug info in generated C)*
- [ ] Live breakpoint execution *(requires runtime hooks)*
- [ ] Variable value reading *(requires memory access protocol)*
- [ ] Step execution *(requires instruction-level control)*

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

## Phase 8: VSCode Extension Enhancements

These can be worked on independently of the compiler/runtime:

- [ ] Debugger support (DAP) *(Large - requires runtime integration, see Phase 6)*
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

---

## Phase 9: Future - Visual Designer

Potential future work for a VB-style RAD visual designer.

---

## Known Issues / Technical Debt

### Low Priority

- [ ] Unicode support: UCASE$/LCASE$ now UTF-8 safe, added char counting helpers. Full Unicode support still needed *(Large - 4-6 sessions)*
- [x] Windows-specific path handling in file I/O (fixed type mismatch in declarations)

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
