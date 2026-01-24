# QB64Fresh Future Development

*Last updated: 2026-01-23*

This document outlines features that are planned but not yet implemented, along with known limitations and design considerations for future work.

---

## Current Status

QB64Fresh is in **active development** with the core compiler pipeline complete:

| Component | Status | Details |
|-----------|--------|---------|
| **Parser** | ✅ Complete | 99.1% QB4.5 compatibility (114/115 test files) |
| **Semantic Analysis** | ✅ Complete | Full type checking and symbol resolution |
| **Code Generation** | ✅ Complete | C backend with constant folding |
| **Runtime** | ✅ Complete | Graphics (SDL2), Audio (Rodio), File I/O, Networking |
| **LSP** | ✅ Complete | Go-to-definition, find references, hover, completion |
| **Formatter** | ✅ Complete | Keyword casing, indentation, style presets |
| **Linter** | ✅ Complete | Static analysis with configurable rules |
| **Debugger** | ⚠️ Infrastructure | Runtime integration pending |

### Codebase Metrics

| Metric | Value |
|--------|-------|
| Source Files | 85 (48 compiler + 17 runtime + 20 tools) |
| Lines of Code | ~72,895 |
| Tests | 1,500+ (all passing) |
| Test Coverage | 81.63% line coverage |
| QB4.5 Compatibility | 99.1% (114/115) |

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

- [ ] **Touch input support** *(Medium)*
      Mobile/touchscreen support for cross-platform deployment.

- [ ] **Joystick event handlers** *(Medium)*
      ON STRIG / STRIG ON/OFF/STOP event handlers not yet implemented.

### Multi-threading (QB64 Extension)
- [ ] **`_THREAD` support** *(Large)*
      Complex runtime changes for thread management.

- [ ] **Thread synchronization primitives** *(Medium)*
      Mutexes, semaphores after _THREAD is implemented.

---

## Tooling & Ecosystem

### Debugger (`tools/debug/`) ⚠️ Infrastructure Complete

**Implemented (~1,500 lines, 44 tests passing):**
- [x] **Debug symbol extraction** (`symbols.rs`) - Types, variables, scopes, procedures from AST
- [x] **Value representation** (`values.rs`) - Scalars, arrays, UDTs with hex/binary/char formats
- [x] **Call stack structures** (`frames.rs`) - Stack frames, navigation, variable grouping
- [x] **DAP protocol types** (`dap.rs`) - Full Debug Adapter Protocol for IDE integration
- [x] **Multi-file source management** (`sources.rs`) - $INCLUDE handling, source line mapping
- [x] **Watch expressions** (`watch.rs`) - Parse variables, array indices, UDT members

**Needs Runtime Integration:**
- [ ] **Runtime state capture** - Requires debug info emission in generated C code
- [ ] **Live breakpoint execution** - Requires runtime hooks to pause execution
- [ ] **Variable value reading** - Requires memory access protocol between debugger and runtime
- [ ] **Step execution** - Requires instruction-level control (step into/over/out)

### Optimization
- [x] **Constant folding** - Implemented
- [ ] **Dead code elimination** *(Medium)*
- [ ] **Loop optimization** *(Medium)*
- [ ] **Inline small functions** *(Medium)*

### Documentation
- [x] **Architecture documentation** - ARCHITECTURE.md, ADRs
- [x] **Development guide** - DEVELOPMENT.md
- [x] **Migration guide** - For QB64 users
- [ ] **Complete language reference** *(Large)* - All statements and functions
- [ ] **Tutorial/getting started guide** *(Medium)* - Beginner-friendly introduction

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

### Audio Limitations
Some audio functions have partial implementations due to Rodio library limitations:
- `_SNDOPENRAW()` - Raw audio stream support limited
- `_SNDBAL()` - 3D balance not fully supported
- `_SNDGETPOS()` / `_SNDSETPOS()` - Position tracking/seeking limited in some formats
- `_SNDRAW()` / `_SNDRAWLEN()` - Raw sample writing limited

See [STUB_FUNCTIONS_REMAINING.md](STUB_FUNCTIONS_REMAINING.md) for the complete list.

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

### Legacy Hardware Functions

These are stub-only for compatibility, not truly functional:
- Port I/O (`INP`, `OUT`, `WAIT`) - Security restrictions on modern OSes
- System interrupts (`INTERRUPT`, `INTERRUPTX`) - Not supported on modern systems
- Light pen (`PEN`) - Hardware doesn't exist
- DOS device control (`IOCTL`, `ERDEV`) - DOS doesn't exist

---

## Design Decisions Pending

### Memory Model
- `_MEM` operations integration with conventional memory (cmem) for VARPTR compatibility

---

## Code Quality

| Metric | Value |
|--------|-------|
| Test Coverage | 81.63% |
| Tests Passing | 1,500+ (all passing) |
| Clippy Warnings | 0 |
| Security Issues | 0 |
| QB4.5 Compatibility | 99.1% (114/115 tests) |

---

## Priority Summary

### High Priority (Next Sessions)
1. **Debugger runtime integration** - Infrastructure ready, needs C codegen hooks
2. **Document security model** - SHELL and file operation security

### Medium Priority (Next Month)
3. Graphics enhancements (alpha blending, screen pages)
4. Network stream I/O
5. ON STRIG event handlers

### Low Priority (Future)
6. Multi-threading (_THREAD)
7. Touch input support
8. Unicode support
9. Optimization passes (dead code, loop optimization)

---

## Contributing

See [DEVELOPMENT.md](../DEVELOPMENT.md) for contribution guidelines. Feature requests
and bug reports welcome at the project repository.
