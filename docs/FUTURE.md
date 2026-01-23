# QB64Fresh Future Development

*Last updated: 2026-01-22*

This document outlines features that are planned but not yet implemented, along with known limitations and design considerations for future work.

---

## Current Status

QB64Fresh is in **active development** with the core compiler pipeline complete:
- **Parser:** 99.1% QB4.5 compatibility (114/115 test files passing)
- **Semantic Analysis:** Full type checking and symbol resolution
- **Code Generation:** Complete C backend with constant folding
- **Runtime:** Graphics (SDL2), Audio (Rodio), File I/O, Networking (TCP)
- **LSP:** Full language server with go-to-definition, find references, hover, completion
- **Test Coverage:** 81.63% (1,100+ tests including unit, integration, and property-based)

---

## Remaining Features

### Graphics System Enhancements
- [ ] **Alpha blending support** *(Medium)*
      Full alpha channel blending for transparent sprites and overlays.

- [ ] **Hardware acceleration option** *(Large)*
      GPU-accelerated rendering path for demanding applications.

- [ ] **Multiple screen pages** *(Medium)*
      SCREEN page parameter for page flipping and double buffering.

### Networking
- [ ] **Network stream I/O** *(Medium)*
      PUT/GET with network handles for binary data transfer.
      Core TCP functions (_OPENHOST, _OPENCLIENT, _OPENCONNECTION, _CONNECTED) are implemented.

### Input Devices
- [ ] **Joystick/gamepad support** *(Medium)*
      Runtime functions (STICK, STRIG, _AXIS, _BUTTON, _DEVICES) are implemented.
      Needs SDL2 joystick enumeration and event loop integration.

- [ ] **Touch input support** *(Medium)*
      Mobile/touchscreen support for cross-platform deployment.

### Multi-threading (QB64 Extension)
- [ ] **`_THREAD` support** *(Large)*
      Complex runtime changes for thread management.

- [ ] **Thread synchronization primitives** *(Medium)*
      Mutexes, semaphores after _THREAD is implemented.

---

## Tooling & Ecosystem

### Debugging
- [ ] **Source-level debugging support** *(X-Large)*
      Requires debug info generation (DWARF/PDB).
      - Breakpoints
      - Variable inspection
      - Step execution

### Optimization
- [x] **Constant folding** - Implemented
- [ ] **Dead code elimination** *(Medium)*
- [ ] **Loop optimization** *(Medium)*
- [ ] **Inline small functions** *(Medium)*

### Documentation
- [ ] **Language reference documentation** *(Large)*
      Complete reference for all statements and functions.

- [ ] **Tutorial/getting started guide** *(Medium)*
      Beginner-friendly introduction to QB64Fresh.

---

## Known Limitations

### GOSUB/Computed Goto
- [ ] **GOSUB uses GCC computed goto extension** *(Medium)*

  The GOSUB/RETURN implementation uses GCC's computed goto extension (`&&label` for label
  addresses, `goto *ptr` for indirect jumps). This works with GCC and Clang but NOT MSVC.
  For MSVC support, would need a switch-based dispatch table alternative.
  Low priority since most users compile with GCC/MinGW.

### PEEK/POKE Memory Model
PEEK/POKE use sandboxed conventional memory (cmem) - a 1MB heap buffer emulating the DOS
memory model, matching QB64PE's approach. This allows legacy programs to do pointer
arithmetic tricks safely without accessing real system memory.

### Unicode Support
- [ ] **Unicode support** *(Large)*
      Currently ASCII-focused. Full Unicode would require significant changes to string handling.

### Platform-Specific
- [ ] **Windows-specific path handling** *(Small)*
      Some file I/O edge cases with Windows path separators.

---

## Intentionally Excluded Features

### OpenGL Commands

QB64PE includes ~300+ `_GL*` commands (e.g., `_GLBEGIN`, `_GLEND`, `_GLVERTEX3F`, etc.)
for raw OpenGL access. These are **intentionally excluded** from QB64Fresh because:

1. We use SDL2/winit for graphics, not raw OpenGL
2. Raw GL commands expose implementation details that reduce portability
3. The `_MAPTRIANGLE` statement provides 3D capability without raw GL
4. Future WebGL/Vulkan backends would be incompatible with GL commands

If raw OpenGL is needed, users can use `DECLARE LIBRARY` to call OpenGL functions directly.

---

## Design Decisions Pending

### Memory Model
- `_MEM` operations integration with conventional memory (cmem) for VARPTR compatibility

---

## Code Quality

| Metric | Value |
|--------|-------|
| Test Coverage | 81.63% |
| Clippy Warnings | 0 |
| Security Issues | 0 |
| QB4.5 Compatibility | 99.1% (114/115 tests) |

---

## Contributing

See [DEVELOPMENT.md](../DEVELOPMENT.md) for contribution guidelines. Feature requests
and bug reports welcome at the project repository.
