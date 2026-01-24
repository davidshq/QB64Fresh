# Future Work & Unsupported Features

This document tracks features that are not yet implemented or cannot be supported in QB64Fresh.

**Last Updated:** January 24, 2026

---

## Not Supported (Architectural Limitations)

These features cannot be supported due to fundamental differences in how QB64Fresh works:

### OpenGL Commands (`_GL`)
The `SUB _GL` callback and all `_gl*` prefixed commands require a native OpenGL context. Since we use SDL2 for rendering, raw OpenGL calls cannot be passed through.

**Affected commands include:**
- `SUB _GL` - The OpenGL rendering callback
- `_GLRENDER` - OpenGL render control
- All `_gl*` functions (~100+ OpenGL functions)

**Workaround:** Use `_MAPTRIANGLE` for 2D/3D textured rendering (planned via SDL_RenderGeometry).

### DECLARE LIBRARY Limitations
`DECLARE LIBRARY`, `DECLARE DYNAMIC LIBRARY`, and `DECLARE STATIC LIBRARY` are partially supported for C interop since we generate C code. Native system libraries work, but some QB64-specific libraries may not.

---

## Platform-Specific Features

### Windows-Only Features (Implemented)

These features are implemented but only work on Windows. On other platforms they return safe defaults (0, -1, or no-op).

| Command | Description | Windows Behavior | Other Platforms |
|---------|-------------|------------------|-----------------|
| `_SCREENPRINT` | Simulates typing into focused program | Uses `SendInput()` API | No-op |
| `_SCREENCLICK` | Simulates mouse click on desktop | Uses `SendInput()` API | No-op |
| `_SCREENIMAGE` | Captures screenshot of desktop | Uses GDI `BitBlt()` | Returns -1 |
| `_WINDOWHANDLE` | Returns native window handle | Returns HWND | Returns 0 |

---

## Implementation Status

### Graphics - Not Yet Implemented

| Feature | Status | Notes |
|---------|--------|-------|
| `_MAPTRIANGLE` | ❌ | Needs SDL_RenderGeometry or custom rasterizer |
| `_COPYPALETTE` | ❌ | Copy palette between images |
| `_DISPLAYORDER` | ❌ | Layer ordering for hardware/software |

---

## Remaining Work

### Graphics (Low Priority)
- `_MAPTRIANGLE` - 3D textured triangle rendering
- `_COPYPALETTE` - Copy palette between images
- `_DISPLAYORDER` - Layer ordering

### Legacy Stubs (Very Low Priority)
- `ERDEV`/`ERDEV$` - DOS device errors (stub only)
- `ON COM`/`ON UEVENT`/`ON SIGNAL` - Event handlers (not in QB64pe either)
- `PEN` - Light pen (obsolete hardware)

---

## Implementation Statistics

**Total Built-in Functions/Subs:** 419

| Category | Count | Percentage |
|----------|-------|------------|
| ✅ Fully Implemented | ~410 | 98% |
| 🔨 Graphics Stubs | 3 | <1% |
| 🔨 Legacy Stubs | ~5 | 1% |
| ❌ Obsolete/Disabled | ~1 | <1% |

---

## Version History

- 2026-01-24: Windows-only features implemented (_SCREENPRINT, _SCREENCLICK, _SCREENIMAGE, _WINDOWHANDLE)
- 2026-01-24: Major update - audio complete, window control, alpha blending, INT 0x33 mouse emulation
- 2026-01-23: Updated with implementation status (infrastructure vs runtime)
- 2026-01-20: Initial creation, documented OpenGL limitations and feature roadmap
