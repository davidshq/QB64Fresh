# Session 040: Graphics Commands Implementation

**Date:** 2026-01-24
**Focus:** Implementing Window Control and Alpha Blending graphics commands

## Summary

Implemented two categories of graphics commands that were identified as missing from the GraphicsBackend trait:

1. **Window Control** - `_FULLSCREEN`, `_SCREENMOVE`, `_SCREENSHOW`, `_SCREENHIDE`
2. **Alpha Blending** - `_BLEND`, `_DONTBLEND`, `_CLEARCOLOR`

## Implementation Details

### Part 1: Window Control Functions

Added SDL2 window management capabilities:

| Command | Description | SDL2 API |
|---------|-------------|----------|
| `_FULLSCREEN [mode]` | Set fullscreen mode (0=windowed, 1=fullscreen, 2=desktop) | `SDL_SetWindowFullscreen` |
| `_SCREENMOVE x, y` | Move window to position | `SDL_SetWindowPosition` |
| `_SCREENSHOW` | Show window | `SDL_ShowWindow` |
| `_SCREENHIDE` | Hide window | `SDL_HideWindow` |

### Part 2: Alpha Blending Functions

Added per-image alpha blending control:

| Command | Description |
|---------|-------------|
| `_BLEND [handle]` | Enable alpha blending for image |
| `_DONTBLEND [handle]` | Disable alpha blending (direct pixel copy) |
| `_CLEARCOLOR color, handle` | Set transparency key (pixels matching color are skipped during `_PUTIMAGE`) |

The `ImageBuffer` struct was extended with:
- `blend_enabled: bool` - Per-image alpha blending toggle
- `clear_color: Option<u32>` - Per-image transparency key

The `copy_pixels` method now respects these settings during `_PUTIMAGE` operations.

### Alpha Blending Algorithm

When `blend_enabled` is true, standard alpha compositing is used:
```
out = src * alpha + dst * (1 - alpha)
```

When `clear_color` is set, pixels matching the color (RGB comparison, ignoring alpha) are skipped entirely.

## Files Modified

| File | Lines Added | Changes |
|------|-------------|---------|
| `runtime/src/graphics/mod.rs` | +100 | 12 new trait methods with default implementations |
| `runtime/src/graphics/sdl2.rs` | +135 | ImageBuffer fields, blend_colors(), trait implementations |
| `runtime/src/graphics_ffi.rs` | +105 | 9 FFI functions for C interop |
| `runtime/include/qb64fresh_rt.h` | +12 | C function declarations |
| `src/codegen/c_backend/runtime/graphics.rs` | +60 | Inline stubs for standalone compilation |
| `src/semantic/builtins.rs` | +25 | Updated function signatures |
| `src/codegen/c_backend/stmt/mod.rs` | +3 | Function name mappings |

**Total:** ~440 lines added across 7 files

## Testing

- All 388 library tests pass
- All 194 runtime tests pass
- Generated C code verified to call correct functions

### Test Program
```basic
SCREEN 13
_FULLSCREEN 1
_SCREENMOVE 100, 100
_SCREENSHOW
_SCREENHIDE
img& = _NEWIMAGE(100, 100, 32)
_BLEND img&
_DONTBLEND img&
_CLEARCOLOR _RGB32(255, 0, 255), img&
END
```

Generates correct C function calls:
```c
qb_fullscreen(1LL);
qb_screenmove(100LL, 100LL);
qb_screenshow();
qb_screenhide();
qb_blend(img_lng);
qb_dontblend(img_lng);
qb_clearcolor(qb__rgb32(255LL, 0LL, 255LL), img_lng);
```

## Architecture Notes

The implementation follows the existing pattern:
1. **Trait definition** - Default no-op in `GraphicsBackend` trait
2. **SDL2 implementation** - Real functionality using SDL2 APIs
3. **FFI layer** - `#[no_mangle] extern "C"` functions for C code
4. **Inline stubs** - Compile-time fallbacks for `--runtime inline` mode
5. **Semantic registration** - Function signatures for type checking
6. **Codegen mapping** - Statement-to-function name mapping

## Decisions Made

1. **Alpha blending default**: Enabled by default for new images (matches QB64 behavior)
2. **Clear color comparison**: RGB-only comparison, ignoring alpha channel
3. **Fullscreen modes**: 0=windowed, 1=true fullscreen, 2=desktop fullscreen (stretched)
4. **Return values**: Window control functions return previous state where applicable

## Next Steps

From the original plan, these commands remain unimplemented:
- `_COPYPALETTE` - Copy palette between images
- `_DISPLAYORDER` - Layer ordering for hardware/software
- `_MAPTRIANGLE` - 3D triangle with texture mapping

These can be added in future sessions using the same pattern.
