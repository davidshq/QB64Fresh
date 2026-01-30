# Review: CP437 + empty-filename changes (bugs and errors)

**Date:** 2026-01-30  
**Scope:** Changes from session 092 (CP437 glyph mapping, empty-filename file open).

## Summary

- **No logic or API bugs found** in cp437, font_manager re-export, graphics mock, or SDL2/graphics_ffi docs.
- **One robustness fix applied:** empty-filename handling now trims the mode string before comparing to `"r+b"` / `"rb+"`, so `" r+b "` or `"r+b\n"` are accepted and do not set error 64.

## Findings

### cp437.rs

- Table length: 256 entries (0x00–0xFF); layout matches original font_manager (0x7F = U+2302, 0x80–0xFF accented/box-drawing/Greek).
- `cp437_to_unicode(byte)` and `cp437_string_to_unicode(bytes)` are correct; indices are in range.

### font_manager.rs

- Re-export `pub use crate::cp437::{...}` is correct; `font_manager` is `#[cfg(feature = "freetype")]`, `cp437` is always compiled, so no ordering issue.
- Tests use `cp437_to_unicode` / `cp437_string_to_unicode` via the re-export; they still resolve correctly.

### graphics/mod.rs (mock)

- `crate::cp437::cp437_string_to_unicode(text)` is correct; `cp437` is always in the crate.

### file.rs (empty filename)

- **Fixed:** Mode comparison was exact (`mode_str == "r+b"`). If the C side passed `" r+b "` or `"r+b\n"`, we would set error 64. Mode is now trimmed before comparison.
- **Intentional behavior:** For empty filename + `r+b`/`rb+` we return without opening and without closing any existing handle on that `fnum`. So `OPEN 1, ""` with r+b is a no-op for that handle (previous file on 1 stays open). This matches “optional config file” and is acceptable.

### Other

- `qb_file_open_str` calls `qb_file_open`; empty-filename handling is in one place and applies to both entry points.
- No linter issues reported on the touched files.

## Verification

- `cargo build -p qb64fresh-runtime` still succeeds after the trim fix.
