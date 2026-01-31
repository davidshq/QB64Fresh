# Session 114 — font.h (FreeType) libqb API

**Date:** 2026-01-31

## Goal

Implement libqb font.h API per [LIBQB_FUNCTIONALITY.md § 37](docs/QB64pe/LIBQB_FUNCTIONALITY.md): `FontLoadFileToMemory`, `FontRenderTextUTF32`, `FontRenderTextASCII`, constants, and `codepage437_to_unicode16[]`.

## Done

1. **qb64fresh_rt.h**
   - Added `INVALID_FONT_HANDLE`, `FONT_LOAD_*`, `FONT_RENDER_*` and declarations for `FontLoadFileToMemory`, `FontRenderTextUTF32`, `FontRenderTextASCII`, and `extern uint16_t codepage437_to_unicode16[256]`.

2. **cp437.rs**
   - Added `CODEPAGE437_TO_UNICODE16: [u16; 256]` (same code points as CP437, as u16).
   - Exported C symbol `codepage437_to_unicode16` for libqb.

3. **font_manager.rs**
   - Added `FONT_RENDER_MONOCHROME`.
   - Added `render_text_to_buffer(handle, codepoints: &[char], options)` returning `Option<(Vec<u8>, i32, i32)>` (alpha buffer, width, height). Composites glyphs into a single row-major 8-bit alpha buffer.

4. **font_ffi.rs**
   - `FontLoadFileToMemory(path, out_bytes)`: tries path, then Linux font search paths (`$HOME/.fonts`, `$HOME/.local/share/fonts`, `/usr/share/fonts/...`). Returns `malloc`’d buffer; caller must `free()`.
   - `FontRenderTextUTF32(fh, codepoint, codepoints, options, out_data, out_x, out_y)`: renders UTF-32 to alpha buffer via `FONT_MANAGER.render_text_to_buffer`; allocates buffer with `malloc`; returns 1/0.
   - `FontRenderTextASCII`: converts bytes via CP437 to UTF-32, then calls `FontRenderTextUTF32`.

5. **Docs**
   - LIBQB_FUNCTIONALITY.md § 37 now points to completed doc.
   - LIBQB_FUNCTIONALITY_COMPLETED.md § 37 updated with constants, `codepage437_to_unicode16`, and the three font.h functions.

## Notes

- Runtime build with full default features still fails on pre-existing errors (completion.rs condvar, thread.rs Send). Font code builds with `--no-default-features --features "freetype lazy_static"`.
- Render buffer format: 8-bit alpha, row-major; `FontRenderTextUTF32`/`FontRenderTextASCII` match QB64pe semantics (out_data malloc’d, caller frees).
