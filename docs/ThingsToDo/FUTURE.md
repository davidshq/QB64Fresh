# QB64Fresh Future Development

*Last updated: 2026-01-24*

This document outlines features that are planned but not yet implemented, along with known limitations and design considerations for future work.

**QB64 parity:** We aim to match QB64, not exceed it. Features we exclude for parity (e.g. hardware-accel toggle, touch API, _THREAD, compiler optimizations) and which we may revisit as **modern functionality** are in [ADR-0014: Scope and Intentionally Excluded Features](../adrs/ADR-0014-scope-and-excluded-features.md) §5.

---

## Pending Work

### Debugger (`tools/debug/`) ⚠️ Infrastructure Complete

**Needs Runtime Integration:**
- [ ] **Runtime state capture** - Requires debug info emission in generated C code
- [ ] **Live breakpoint execution** - Requires runtime hooks to pause execution
- [ ] **Variable value reading** - Requires memory access protocol between debugger and runtime
- [ ] **Step execution** - Requires instruction-level control (step into/over/out)

---

## Recently Completed Features

### Unicode Font Rendering ✓

FreeType-based TrueType/OpenType font support:
- `_LOADFONT(path$, size, "UNICODE")` - Load TrueType fonts
- `_FREEFONT(handle)` - Release font resources
- `_UPRINTSTRING(x, y, text$)` - Render Unicode text
- `_UPRINTWIDTH(text$)` - Get text width in pixels
- `_UFONTHEIGHT` - Get font height
- `_ULINESPACING` - Get line spacing
- `_UCHARPOS(text$, pos)` - Get character X position
- `_MAPUNICODE` - CP437 to Unicode mapping table (256 codepoints)

### Screen Pages & Double Buffering ✓

Full support for SCREEN page parameters:
- `SCREEN mode, , active_page, visual_page` - Set drawing and display pages
- `PCOPY source, dest` - Copy page contents
- 4 screen pages available for double/triple buffering
- Classic QB45 double-buffering patterns work correctly

### DECLARE LIBRARY ✓

Full QB64PE parity achieved:

1. **`_MEMGET`/`_MEMPUT` AS type** – Optional `AS type` clause:
   - `x = _MEMGET(mem, offset, AS INTEGER)`
   - `_MEMPUT mem, offset, value AS DOUBLE`

2. **`_MEM` type in parameters** – `_MEM` as parameter type in DECLARE LIBRARY

3. **`_MEM(variable)` function** – Returns `_MEM` block for variable's memory

4. **Header Parsing** – `DECLARE LIBRARY "file.h"` parses C prototypes:
   - `#define` constants
   - `#ifdef`/`#ifndef` conditionals
   - Struct definitions (requires `header-parsing` feature)

5. **Callback Functions** – `_PROCPTR(procedureName)` with correct signatures:
   - FUNCTION callbacks return proper C type
   - SUB callbacks return void
   - BYVAL/BYREF parameters handled correctly

6. **VARPTR/VARSEG/SADD** – Memory address functions:
   - `VARPTR(variable)` - address as LONG
   - `VARPTR$(variable)` - binary string of address
   - `VARSEG(variable)` - returns 0 (flat memory model)
   - `SADD(string$)` - address of string data

### Graphics Features ✓

- **`_MAPTRIANGLE`** - Software texture mapping rasterizer for 3D
- **`_COPYPALETTE`** - Copy palette between images
- **`_DISPLAYORDER`** - Control rendering layer order

### Platform Features ✓

**Conditional Compilation Constants:**

| Constant | Description |
|----------|-------------|
| `_WINDOWS` / `_WIN` | True (-1) on Windows |
| `_LINUX` | True (-1) on Linux |
| `_MACOSX` / `_MAC` | True (-1) on macOS |
| `_64BIT` | True (-1) on 64-bit platforms |
| `_32BIT` | True (-1) on 32-bit platforms |

**Windows-Only Functions:** Desktop manipulation functions (Windows builds only)

**Legacy DOS Emulation:**
- `INTERRUPT`/`INTERRUPTX` - INT 0x33 mouse emulation
- Runtime warnings for unsupported legacy DOS functions

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
