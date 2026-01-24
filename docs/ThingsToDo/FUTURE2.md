# Future Work & Unsupported Features

This document tracks features that are not yet implemented or cannot be supported in QB64Fresh.

**Last Updated:** January 24, 2026

---

### DECLARE LIBRARY Limitations
`DECLARE LIBRARY`, `DECLARE DYNAMIC LIBRARY`, and `DECLARE STATIC LIBRARY` are partially supported for C interop since we generate C code. Native system libraries work, but some QB64-specific libraries may not.

---

## Implementation Status

### Graphics - Fully Implemented

All core graphics commands are now implemented, including:

| Feature | Status | Notes |
|---------|--------|-------|
| `_MAPTRIANGLE` | ✅ | Software rasterizer with barycentric texture mapping |
| `_COPYPALETTE` | ✅ | Copy palette between images (per-image palette support) |
| `_DISPLAYORDER` | ✅ | Layer ordering (stores order, full compositing in SDL2 runtime) |

---

## Remaining Work

### Legacy Stubs (Very Low Priority)
- `ERDEV`/`ERDEV$` - DOS device errors (stub only)
- `ON COM`/`ON UEVENT`/`ON SIGNAL` - Event handlers (not in QB64pe either)
- `PEN` - Light pen (obsolete hardware)

---

## Implementation Statistics

**Total Built-in Functions/Subs:** 420+

| Category | Count | Percentage |
|----------|-------|------------|
| ✅ Fully Implemented | ~415 | 99% |
| 🔨 Legacy Stubs | ~5 | 1% |

---

## Version History

- 2026-01-24: _MAPTRIANGLE implemented with software rasterizer (barycentric texture mapping)
- 2026-01-24: _COPYPALETTE and _DISPLAYORDER implemented
- 2026-01-24: Windows-only features implemented (_SCREENPRINT, _SCREENCLICK, _SCREENIMAGE, _WINDOWHANDLE)
- 2026-01-24: Major update - audio complete, window control, alpha blending, INT 0x33 mouse emulation
- 2026-01-23: Updated with implementation status (infrastructure vs runtime)
- 2026-01-20: Initial creation, documented OpenGL limitations and feature roadmap
