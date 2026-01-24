# QB64Fresh Future Development

*Last updated: 2026-01-24*

This document outlines features that are planned but not yet implemented, along with known limitations and design considerations for future work.

**QB64 parity:** We aim to match QB64, not exceed it. Features we exclude for parity (e.g. hardware-accel toggle, touch API, _THREAD, compiler optimizations) and which we may revisit as **modern functionality** are in [ADR-0014: Scope and Intentionally Excluded Features](../adrs/ADR-0014-scope-and-excluded-features.md) §5.

---

## Tooling & Ecosystem

### Debugger (`tools/debug/`) ⚠️ Infrastructure Complete

**Needs Runtime Integration:**
- [ ] **Runtime state capture** - Requires debug info emission in generated C code
- [ ] **Live breakpoint execution** - Requires runtime hooks to pause execution
- [ ] **Variable value reading** - Requires memory access protocol between debugger and runtime
- [ ] **Step execution** - Requires instruction-level control (step into/over/out)

---

## DECLARE LIBRARY

### Implemented Features

- **Header Parsing** *(opt-in: `--features header-parsing`)* – `DECLARE LIBRARY "file.h"` can parse the header to auto-generate function declarations. Supports `#define`, `#ifdef`, structs; not function-like macros, unions, or C++. Manual declarations take precedence over parsed ones.

- **`_MEM` Support** – `_MEM` type works in DECLARE LIBRARY parameters. `_MEM(variable)` works as a function to get a memory descriptor.

- **Platform Constants** – `_64BIT`, `_32BIT`, `_WIN`, `_MAC`, `_WINDOWS`, `_LINUX`, `_MACOSX` are available as builtin constants for `$IF` conditions.

### Remaining Limitations

1. **`_MEMGET`/`_MEMPUT` AS type** – Optional `AS type` clause on `_MEMGET`/`_MEMPUT` is not supported. Workaround: use typed helper functions or cast via `_MK*`/`_CV*`.

2. **Callback Functions** – Callback signatures other than qsort-style (`int (*)(const void*, const void*)` via `_PROCPTR`) are not supported. Workaround: implement callback wrapper in C and link.

3. **VARPTR/cmem Integration** – VARPTR and conventional memory segment access are not implemented. Workaround: use `_OFFSET` with C helper functions for C interop.

---

## Intentionally Excluded Features

For rationale and full list of exclusions and stub-only features, see [ADR-0014: Scope and Intentionally Excluded Features](../adrs/ADR-0014-scope-and-excluded-features.md). Features we exclude for QB64 parity (hardware-accel toggle, touch API, _THREAD, compiler optimizations) and may revisit as **modern functionality** are in ADR-0014 §5.

### OpenGL Commands

QB64PE includes ~300+ `_GL*` commands (e.g., `_GLBEGIN`, `_GLEND`, `_GLVERTEX3F`, etc.)
for raw OpenGL access. These are **intentionally excluded** from QB64Fresh because:

1. We use SDL2/winit for graphics, not raw OpenGL
2. Raw GL commands expose implementation details that reduce portability
3. The `_MAPTRIANGLE` statement provides 3D capability without raw GL
4. Future WebGL/Vulkan backends would be incompatible with GL commands

If raw OpenGL is needed, users can use `DECLARE LIBRARY` to call OpenGL functions directly.
