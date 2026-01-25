# QB64Fresh TODO

*Last updated: 2026-01-25*

A prioritized roadmap for QB64Fresh development. For completed features, see [TODO-completed.md](docs/archive/TODO-completed.md).

---

## Priority 0: Bootstrap Completion (BLOCKING)

**Current Status:** QB64pe compiles, `-h` works, but compilation crashes.

- [ ] **Fix memory exhaustion during compilation** *(Critical - blocks bootstrap)*
  - Bootstrapped QB64pe uses 25GB+ memory when compiling programs
  - Crashes with segfault when memory limited to 16GB
  - See [BOOTSTRAP_PLAN.md](BOOTSTRAP_PLAN.md) for details

- [ ] **Diagnose root cause**
  - Add memory tracking to generated C code
  - Create minimal test case that reproduces the crash
  - Compare with original QB64pe behavior

**IMPORTANT:** Always use `ulimit -v 16777216` when running QB64pe. See [docs/MEMORY_LIMITS.md](docs/MEMORY_LIMITS.md).

---
- All Phase 1, 2, 4 items have been completed — see [TODO-completed.md](docs/archive/TODO-completed.md).
- Phase 5: All C Library Integration items completed — see [TODO-completed.md](docs/archive/TODO-completed.md).
- Phase 7: All metacommands have been implemented — see [TODO-completed.md](docs/archive/TODO-completed.md).
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

---

## Phase 6: Tooling & Ecosystem

### Debugging (`tools/debug`)
Debugger infrastructure has been scaffolded as a workspace member (44 tests passing). Core components (symbols, values, frames, dap, sources, watch) — see [TODO-completed.md](docs/archive/TODO-completed.md).

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
- Formatter, linter, format/lint on save, build errors, settings, code actions, symbols, rename, snippets — see [TODO-completed.md](docs/archive/TODO-completed.md)

---

## Phase 9: Future - Visual Designer

Potential future work for a VB-style RAD visual designer.

---

**Design Decisions Made:**
- Graphics backend: Trait-based abstraction with SDL2 as default, mock for testing
- Sound backend: Trait-based abstraction with rodio as default, mock for testing
- PEEK/POKE use sandboxed conventional memory (cmem) - a 1MB heap buffer emulating DOS memory model, matching QB64pe's approach. This allows legacy programs to do pointer arithmetic tricks safely without accessing real system memory.

**Design Decisions Needed:**
- Memory model for `_MEM` operations (integration with cmem for VARPTR compatibility)