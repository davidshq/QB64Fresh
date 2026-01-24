# QB64Fresh Future Development

*Last updated: 2026-01-24*

This document outlines features that are planned but not yet implemented, along with known limitations and design considerations for future work.

**QB64 parity:** We aim to match QB64, not exceed it. Features we exclude for parity (e.g. hardware-accel toggle, touch API, _THREAD, compiler optimizations) and which we may revisit as **modern functionality** are in [ADR-0014: Scope and Intentionally Excluded Features](../adrs/ADR-0014-scope-and-excluded-features.md) §5.

---

## Remaining Features

### Graphics System Enhancements

- [ ] **Multiple screen pages** *(Medium)*
      SCREEN page parameter for page flipping and double buffering.
      Syntax and `qb_gfx_screen(mode, color, active, visual)` exist; runtime does not implement page flipping (params ignored).
      *(QB64 supports `SCREEN mode [, , active_page, visual_page]`—parity item.)*

### Networking
- [ ] **Network stream I/O** *(Medium)*
      PUT/GET with network handles for binary data transfer.
      Core TCP functions (_OPENHOST, _OPENCLIENT, _OPENCONNECTION, _CONNECTED) are implemented.
      *(QB64 supports PUT/GET on TCP/Stream handles via `special_handle_type::Stream` in libqb—parity item.)*

### Input Devices

- [ ] **Joystick event handlers** *(Medium)*
      ON STRIG / STRIG ON/OFF/STOP event handlers not yet implemented.
      Parsed and emitted (`qb_on_strig`, `qb_strig_control`); runtime stubs do not invoke handlers.
      *(QB64 supports ON STRIG and STRIG(button%) On/Off/Stop—parity item.)*

---

## Tooling & Ecosystem

### Debugger (`tools/debug/`) ⚠️ Infrastructure Complete

**Needs Runtime Integration:**
- [ ] **Runtime state capture** - Requires debug info emission in generated C code
- [ ] **Live breakpoint execution** - Requires runtime hooks to pause execution
- [ ] **Variable value reading** - Requires memory access protocol between debugger and runtime
- [ ] **Step execution** - Requires instruction-level control (step into/over/out)

---

## DECLARE LIBRARY Support

QB64Fresh supports `DECLARE LIBRARY`, `DECLARE DYNAMIC LIBRARY`, and `DECLARE STATIC LIBRARY` for C interop. Since we generate C code, this integration is natural and efficient.

### DECLARE LIBRARY Limitations

*(Only unimplemented items are listed.)*

1. **QB64-Specific Bundled Libraries** – QB64pe bundles (InForm GUI, QB64 OpenGL bindings) rely on its internals; use native GUI or SDL2 via DECLARE LIBRARY instead.

2. **Header Parsing** – `DECLARE LIBRARY "file.h"` does not parse the header; the string is only a library identifier. Manually declare each `FUNCTION` and `SUB`. The `header-parsing` API (supports `#define`, `#ifdef`, structs; not function-like macros, unions, or C++) is not integrated into DECLARE LIBRARY.

3. **`_MEM`** – Not supported in DECLARE LIBRARY parameters. Also missing: `_MEM(variable)` (parser rejects: `_MEM` is lexed as type-only), optional `AS type` on `_MEMGET`/`_MEMPUT`, and VARPTR/cmem integration. Use `_OFFSET` with C helpers when you need `_MEM`-like behavior from C.

4. **Callback Functions** – Callback signatures other than qsort-style (`int (*)(const void*, const void*)` via `_PROCPTR`) are not supported; implement in C and link.

5. **Platform-Specific** – `_64BIT`/`_32BIT` and `_WIN`/`_MAC` aliases are not in builtins for `$IF` conditions.

### Safety Considerations

DECLARE LIBRARY enables unsafe operations: no runtime type checking, manual C memory management, temporary string lifetime issues, and platform-specific behavior. See [ADR-0008](../adrs/ADR-0008-c-interoperability.md) for details.

---

## Known Limitations

### Unicode Support
- [ ] **Unicode support** *(Large)*
      Currently ASCII-focused. Full Unicode would require significant changes to string handling.
      **Partial:** UTF-8 in source and string literals; inline C runtime has
      `qb_utf8_char_count`/`qb_utf8_char_to_byte`/`qb_strlen_chars` (unused by BASIC built-ins);
      UCASE$/LCASE$ preserve multi-byte UTF-8; `_MAPUNICODE` (stubs).
      LEN, LEFT$, RIGHT$, MID$, INSTR, CHR$, ASC, and compares remain byte-based.

### Platform-Specific
- [x] **Windows-specific path handling** *(Small)* *(done 2026-01-24)*
      All path-taking file I/O now normalizes `\`→`/` on non-Windows: OPEN, $INCLUDE, _FILEEXISTS, _DIREXISTS, KILL, NAME, MKDIR, RMDIR, CHDIR, legacy OPEN (qb_file_open_legacy), BLOAD, BSAVE, _READFILE$, _WRITEFILE.

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

---

## Priority Summary

### High Priority (Next Sessions)
1. **Debugger runtime integration** - Infrastructure ready, needs C codegen hooks

### Medium Priority (Next Month)
2. Graphics enhancements (screen pages)
3. Network stream I/O
4. ON STRIG event handlers

### Low Priority (Future)
5. Unicode support

---

## Version History

- 2026-01-24: GOSUB/computed goto moved to ADR-0002 (Implementation notes); removed from Known Limitations.
- 2026-01-24: Parity exclusions (hardware accel, touch, _THREAD, optimizations) moved to ADR-0014 §5 with “may revisit as modern functionality”; removed from FUTURE. Priority and parity note simplified.
- 2026-01-24: QB64 parity notes: which remaining tasks QB64 also doesn't support (e.g. _THREAD, touch, hardware-accel toggle, optimizer, GOSUB extension); parity clarifications for screen pages, ON STRIG, network PUT/GET.
- 2026-01-24: Condensed (DECLARE LIBRARY, callbacks, $IF, Priority Summary); [SECURITY_MODEL.md](../SECURITY_MODEL.md); merged FUTURE2, legacy stubs (ON COM/ON UEVENT/ON SIGNAL); _MAPTRIANGLE, _COPYPALETTE, _DISPLAYORDER, Windows screen; audio complete, window control, alpha blending, INT 0x33; Unicode partial note; deduped.
- 2026-01-24: Status audit: tutorial [x] (Handbook); notes for screen pages (params exist, runtime stub), STRIG (parsed/emitted, stubs), language reference (substantial).
- 2026-01-23: Debugger implementation status (infrastructure vs runtime)
- 2026-01-20: Initial creation; OpenGL limitations and feature roadmap

---

## Contributing

See [DEVELOPMENT.md](../DEVELOPMENT.md) for contribution guidelines. Feature requests
and bug reports welcome at the project repository.
