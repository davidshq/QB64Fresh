# Session 044: Screen Page Support (Multiple Video Pages)

**Date:** 2026-01-24
**Focus:** Implement multiple screen pages for page flipping and double buffering

## Summary

Implemented full support for SCREEN page parameters, enabling classic double-buffering patterns. This achieves QB64PE parity for the `SCREEN mode, , active_page, visual_page` syntax.

## Changes Made

### SDL2 Backend (`runtime/src/graphics/sdl2.rs`)

**Struct Changes:**
- Replaced `pixel_buffer: Vec<u32>` with `page_buffers: Vec<Vec<u32>>` (4 pages)
- Added `active_page: usize` - page being drawn to
- Added `visual_page: usize` - page being displayed
- Added `max_pages: usize` - maximum pages (4 for most modes)

**New Methods:**
- `set_active_page(page)` - Set which page receives drawing operations
- `set_visual_page(page)` - Set which page is displayed on screen
- `get_pages()` - Returns (active_page, visual_page) tuple
- `pcopy(src, dst)` - Copy contents between pages (was stub, now functional)

**Updated Methods:**
- `display()` - When active != visual, renders visual page buffer via SDL texture
- `cls()` - Clears only the active page buffer
- `set_pixel_buffer()` / `get_pixel_buffer()` - Now use `page_buffers[active_page]`
- All drawing operations automatically go to active page

### Graphics Trait (`runtime/src/graphics/mod.rs`)

Added trait methods with default no-op implementations:
- `set_active_page(&mut self, page: i32) -> Result<(), GraphicsError>`
- `set_visual_page(&mut self, page: i32) -> Result<(), GraphicsError>`
- `get_pages(&self) -> (i32, i32)`

### FFI Layer (`runtime/src/graphics_ffi.rs`)

**Updated:**
- `qb_gfx_screen()` - Now sets active/visual pages when specified (>= 0)

**New Functions:**
- `qb_gfx_set_active_page(page)` - Direct page control
- `qb_gfx_set_visual_page(page)` - Direct page control
- `qb_gfx_get_pages(*active, *visual)` - Query current pages

### Inline Runtime (`src/codegen/c_backend/runtime/graphics.rs`)

Added stub declarations for new FFI functions.

## How It Works

### Double Buffering Pattern

```basic
' Classic double-buffering pattern
SCREEN 12, , 0, 1          ' Draw to page 0, show page 1
DO
    CLS                     ' Clear active page (0)
    ' ... draw frame ...
    PCOPY 0, 1              ' Copy page 0 to page 1 (now visible)
    ' Or use: SCREEN 12, , 1, 0  ' Swap which page is active/visual
LOOP
```

### Implementation Details

1. **Page Buffers:** 4 independent pixel buffers, each `width * height` pixels
2. **Drawing:** All drawing ops go to `page_buffers[active_page]`
3. **Display:** When `display()` called, if active != visual:
   - Creates SDL texture from visual page buffer
   - Copies texture to canvas
   - Presents canvas
4. **PCOPY:** Clones buffer data from source to destination page

## Files Modified

- `runtime/src/graphics/sdl2.rs` - Core page buffer implementation
- `runtime/src/graphics/mod.rs` - Trait method additions
- `runtime/src/graphics_ffi.rs` - FFI functions
- `src/codegen/c_backend/runtime/graphics.rs` - Inline runtime stubs

## Testing

- All 388 library tests pass
- Runtime builds successfully
- Main compiler builds successfully

## QB64PE Parity Status

This completes the "Multiple screen pages" item from the parity list:
- ✅ SCREEN page parameter for page flipping
- ✅ PCOPY statement functional
- ✅ Double buffering support
