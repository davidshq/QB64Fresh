# QB64Fresh Future Development

*Last updated: 2026-01-24*

This document outlines features that are planned but not yet implemented, along with known limitations and design considerations for future work.

**QB64 parity:** We aim to match QB64, not exceed it. Features we exclude for parity (e.g. hardware-accel toggle, touch API, _THREAD, compiler optimizations) and which we may revisit as **modern functionality** are in [ADR-0014: Scope and Intentionally Excluded Features](../adrs/ADR-0014-scope-and-excluded-features.md) §5.

---

## Feature Documentation

Completed features are documented in their respective locations:

| Feature Area | Documentation |
|--------------|---------------|
| **Debugger** | [ADR-0013](../adrs/ADR-0013-debugger-architecture.md), [tools/README.md](../../tools/README.md) |
| **Graphics** | [GRAPHICS.md](../GRAPHICS.md) |
| **C Interop (DECLARE LIBRARY)** | [ADR-0008](../adrs/ADR-0008-c-interoperability.md) |
| **Language Reference** | [QB64Fresh_LANGUAGE_REFERENCE.md](../QB64Fresh_LANGUAGE_REFERENCE.md) |
| **Audio System** | [ADR-0007](../adrs/ADR-0007-audio-system.md) |

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
