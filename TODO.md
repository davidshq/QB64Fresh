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
- [ ] GET/PUT full pixel copying implementation *(Small - 1 session, stubs exist)*

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
- [ ] Constant folding (expand current) *(Small - 1 session)*
- [ ] Loop optimization *(Medium - 2-3 sessions)*
- [ ] Inline small functions *(Medium - 2-3 sessions)*

### Documentation
- [ ] Language reference documentation *(Large - 4-6 sessions)*
- [x] Migration guide from QB64 *(Small - 1-2 sessions)* ✅ *COMPLETED - see docs/MIGRATION_GUIDE.md*
- [ ] Tutorial/getting started guide *(Medium - 2-3 sessions)*
- [ ] Example programs *(Small - 1-2 sessions)*

### Testing (See TESTING_INFRASTRUCTURE_PLAN.md for details)
- [ ] Compatibility tests against QB64 programs *(Ongoing - add as discovered)*
- [x] Add comprehensive recursion test suite *(Small - 1 session)* ✅ *COMPLETED - 32 tests in `recursion` module*
- [ ] Add unit tests for new parser modules (graphics, audio, system, file_io) *(Small - 1-2 sessions)*

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

### High Priority

- [ ] **Panic calls in semantic module** *(Small - 1 session)*
      6 panic calls in `src/semantic/mod.rs` (lines 2185-2230) for built-in constant handling.
      Should return proper semantic errors instead of panicking.

- [ ] **Stack overflow in QB45 tests** *(Medium - 2-3 sessions for proper fix)*
      Deeply nested expressions cause stack overflow in recursive descent parser.
      Workaround: Set `RUST_MIN_STACK=16777216` before running tests.
      Proper fix: Implement iterative expression parsing.

### Medium Priority

- [ ] **GOSUB uses GCC computed goto extension** *(Medium - 2-3 sessions for MSVC alternative)*
      The GOSUB/RETURN implementation uses GCC's computed goto extension (`&&label` for label
      addresses, `goto *ptr` for indirect jumps). This works with GCC and Clang but NOT MSVC.
      For MSVC support, would need a switch-based dispatch table alternative.
      Low priority since most users compile with GCC/MinGW.

- [x] **QB45 memory features: DEF SEG, VARSEG, VARPTR, BLOAD, BSAVE** ✅ *COMPLETED*
      All memory segment operations are fully implemented and working.
      Test suite shows 96.5% compatibility (136/141 files passing).
      thebob/ directory (heavy BLOAD/BSAVE usage) passes 100%.

- [ ] **Parser expect() calls should check token availability** *(Small - 1 session)*
      ~25 `.expect()` calls in `src/parser/graphics.rs` could panic on malformed input.
      Should add proper token availability checks.

- [x] Recursion: Should work but needs thorough testing *(Small - 1 session to add test coverage)* ✅ *COMPLETED - 32 tests added*

- [ ] **DECLARE FUNCTION doesn't consume return type** *(Small - 1 session)*
      `DECLARE FUNCTION Foo(x AS LONG) AS LONG` fails to parse - the trailing `AS LONG` return
      type is not consumed by `parse_declare_function()` in `src/parser/statements.rs:2705`.
      Workaround: Omit the return type: `DECLARE FUNCTION Foo(x AS LONG)`.
      Fix: Add optional `AS type` parsing after the parameter list, similar to how
      `parse_function_definition()` handles it.

### Low Priority

- [ ] **STRING * n in UDTs** *(Medium - 2 sessions)*
      Fixed-length strings cause type mismatch errors in user-defined types.
      Affects 6 QB45 semantic test failures.

- [ ] Large array handling: Verify stack vs heap allocation *(Small - 1 session)*
- [ ] Unicode support: Currently ASCII-focused *(Large - 4-6 sessions for full Unicode)*
- [ ] Windows-specific path handling in file I/O *(Small - 1 session)*

- [ ] **Add doc comments to 16 undocumented modules** *(Small - 1-2 sessions)*
      Several parser and codegen modules lack module-level documentation.

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
