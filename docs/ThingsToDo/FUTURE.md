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

1. **`_MEMGET`/`_MEMPUT` AS type** ✓ – Optional `AS type` clause is fully supported:
   - `x = _MEMGET(mem, offset, AS INTEGER)` - reads typed value from memory
   - `_MEMPUT mem, offset, value AS DOUBLE` - writes typed value to memory
   - Generates efficient C pointer dereferences without runtime overhead

2. **`_MEM` type in parameters** ✓ – `_MEM` can be used as parameter type in DECLARE LIBRARY.

3. **`_MEM(variable)` function** ✓ – Returns a `_MEM` block for the specified variable's memory.

4. **Header Parsing** ✓ – `DECLARE LIBRARY "file.h"` parses C prototypes (requires `header-parsing` feature).

5. **Callback Functions** ✓ – `_PROCPTR(procedureName)` generates wrappers with correct signatures:
   - FUNCTION callbacks return the proper C type
   - SUB callbacks return void
   - BYVAL/BYREF parameters handled correctly
   - Works with any parameter types (INTEGER, LONG, DOUBLE, etc.)

6. **VARPTR/VARSEG/SADD** ✓ – Memory address functions fully implemented:
   - `VARPTR(variable)` - returns address of variable as LONG
   - `VARPTR$(variable)` - returns binary string of address
   - `VARSEG(variable)` - returns 0 (flat memory model)
   - `SADD(string$)` - returns address of string data

### Remaining Limitations

None - full QB64PE parity achieved for DECLARE LIBRARY features.

---

## Platform Constants

### Implemented Features

The following platform detection constants are available for `$IF` conditional compilation:

| Constant | Description |
|----------|-------------|
| `_WINDOWS` / `_WIN` | True (-1) on Windows |
| `_LINUX` | True (-1) on Linux |
| `_MACOSX` / `_MAC` | True (-1) on macOS |
| `_64BIT` | True (-1) on 64-bit platforms |
| `_32BIT` | True (-1) on 32-bit platforms |

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
