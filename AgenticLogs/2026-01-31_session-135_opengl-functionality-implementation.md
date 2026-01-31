# Session 135: OpenGL functionality implementation (2026-01-31)

## Goal
Implement everything in `docs/QB64pe/QB64PE_OPENGL_FUNCTIONALITY.md` that wasn’t already in QB64Fresh.

## Done

1. **QB64pe-specific (SUB _GL, _GLRENDER, _GLCOMPAT)**  
   Already present: SUB _GL scope, _GLRENDER sub, _GLCOMPAT function.

2. **590 GL_* constants**  
   - Added script `scripts/gl_parse_header.py` that parses QB64pe’s `gl.h` and emits:
     - `src/semantic/builtins_opengl_constants.rs` (const array)
     - `src/semantic/builtins_opengl_functions.rs` (Rust registration)
     - `runtime/c_src/gl_wrappers_generated.inc` (C wrappers)
   - `builtins_opengl.rs` now:
     - Includes the constants and registers all 590 as `GL_*` and `_GL_*`.
     - Calls `builtins_opengl_functions::register_gl_functions(self)` for all _GL* from gl.h.
     - Keeps hand-added `_GLUPERSPECTIVE` (GLU).

3. **335 _GL* functions (from gl.h)**  
   - All registered in the semantic layer with correct param types (Long/Single/Double/Offset).
   - All have C wrappers in `gl_wrappers_generated.inc` (GL_CHECK / GL_CHECK_RET, correct casts).
   - `gl_wrappers.c` includes the generated .inc and keeps `call_gluPerspective`.

4. **_GL_* aliases**  
   Every GL constant is also registered as `_GL_*` (e.g. `_GL_TRIANGLES`).

## Files touched
- `scripts/gl_parse_header.py` (new): parse gl.h → constants, Rust registrations, C wrappers.
- `src/semantic/builtins_opengl.rs`: use generated constants + function module; keep _GLUPERSPECTIVE.
- `src/semantic/builtins_opengl_constants.rs` (generated): 590 (name, value) pairs.
- `src/semantic/builtins_opengl_functions.rs` (generated): 335 _GL* sub/function registrations.
- `src/semantic/mod.rs`: `mod builtins_opengl_functions;`
- `runtime/c_src/gl_wrappers.c`: include generated .inc; only hand-written `call_gluPerspective`.
- `runtime/c_src/gl_wrappers_generated.inc` (generated): 335 call_gl* wrappers.

## Regeneration
From repo root:
```bash
python3 scripts/gl_parse_header.py path/to/QB64pe/internal/c/parts/core/gl_header_for_parsing/gl.h src/semantic runtime/c_src
```
Then fix `builtins_opengl_constants.rs` doc comment to `///` if the script emits `//!`.

## Tests
- `cargo build --lib` and `cargo test --lib`: 428 passed.
- `cargo build --features opengl` in `runtime`: builds with all C wrappers.
