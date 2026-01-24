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

## DECLARE LIBRARY Limitations

*(Only unimplemented items are listed.)*

2. **Header Parsing** – `DECLARE LIBRARY "file.h"` does not parse the header; the string is only a library identifier. Manually declare each `FUNCTION` and `SUB`. The `header-parsing` API (supports `#define`, `#ifdef`, structs; not function-like macros, unions, or C++) is not integrated into DECLARE LIBRARY.

3. **`_MEM`** – Not supported in DECLARE LIBRARY parameters. Also missing: `_MEM(variable)` (parser rejects: `_MEM` is lexed as type-only), optional `AS type` on `_MEMGET`/`_MEMPUT`, and VARPTR/cmem integration. Use `_OFFSET` with C helpers when you need `_MEM`-like behavior from C.

4. **Callback Functions** – Callback signatures other than qsort-style (`int (*)(const void*, const void*)` via `_PROCPTR`) are not supported; implement in C and link.

5. **Platform-Specific** – `_64BIT`/`_32BIT` and `_WIN`/`_MAC` aliases are not in builtins for `$IF` conditions.

---

## Known Limitations

### Unicode Support
- [ ] **Unicode support** *(Large)*
      Currently ASCII-focused. Full Unicode would require significant changes to string handling.
      **Partial:** UTF-8 in source and string literals; **`_MAPUNICODE`** (statement and function) is fully implemented with CP437 default table and customizable ASCII→Unicode mapping; inline C runtime has `qb_utf8_char_count`/`qb_utf8_char_to_byte`/`qb_strlen_chars` (unused by BASIC built-ins); UCASE$/LCASE$ preserve multi-byte UTF-8.
      LEN, LEFT$, RIGHT$, MID$, INSTR, CHR$, ASC, and compares remain byte-based.
      **QB64pe parity gap:** QB64pe implements `_UPRINTSTRING`, `_UPRINTWIDTH`, `_UCHARPOS`, `_UFONTHEIGHT`, `_ULINESPACING` in its font layer (FreeType). QB64Fresh parses and emits `qb_*` calls for all five, but the inline runtime **does not define** those symbols—programs using them would fail at link. To achieve stub-level parity, add definitions for `qb_uprintstring`, `qb_uprintwidth`, `qb_ucharpos`, `qb_ufontheight`, `qb_ulinespacing` (e.g. no-op/safe defaults).

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
