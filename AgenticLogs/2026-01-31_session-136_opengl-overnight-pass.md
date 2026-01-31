# Session 136: OpenGL overnight pass (2026-01-31)

## Goal
Another full pass on OpenGL: fix _GLRENDER mode constants, integration tests, void-arg fix, example and docs.

## Done

1. **_GLRENDER mode constants (_BEHIND, _ONTOP, _ONLY, _ONLYBACKGROUND)**
   - **Inline runtime** (`src/codegen/c_backend/runtime/graphics.rs`): Added stubs `qb_behind()`, `qb_ontop()`, `qb_only()`, `qb_onlybackground()` returning 0, 1, 2, 3 so generated C that uses `_GLRENDER _BEHIND` etc. compiles.
   - **External runtime**: `runtime/include/qb64fresh_rt.h` — declared the four functions. `runtime/src/graphics_ffi.rs` — implemented them (return 0, 1, 2, 3).

2. **Integration test helper**
   - `tests/integration_tests.rs`: `compile_to_c()` now sets `uses_opengl` from `program_uses_opengl(&typed_program)` so OpenGL-using sources get `#define QB64FRESH_OPENGL 1` and correct `call_gl*` emission.

3. **OpenGL integration tests**
   - New mod `opengl_integration`: `sub_gl_defines_opengl_macro`, `gl_constant_triangles`, `gl_begin_call_in_sub_gl` (SUB _GL + _GLBEGIN/_GLEND).
   - Added `glrender_with_behind`, `glrender_with_ontop`, `glrender_with_only` in session034 ( _GLRENDER _BEHIND/_ONTOP/_ONLY ).

4. **void-arg fix in gl_parse_header.py**
   - Functions like `glEnd(void)` were incorrectly registered with one parameter. Script now treats `params_str == "void"` as zero parameters. Regenerated `builtins_opengl_functions.rs` and `gl_wrappers_generated.inc` so `_GLEND` (and other void-only GL calls) have 0 params.

5. **Minimal OpenGL example and docs**
   - `examples/graphics/opengl_minimal.bas`: SCREEN 12, _GLRENDER _ONTOP, SUB _GL with _GLCLEAR, _GLBEGIN/_GLEND, _GLVERTEX3F, _GLCOLOR3F. Compiles to C successfully.
   - `examples/README.md`: Listed `opengl_minimal.bas` under Graphics.

## Verification
- `cargo build --lib` and `cargo build -p qb64fresh-runtime`: OK.
- `cargo test --test integration_tests opengl_integration` and `glrender_with_*`: all pass.
- `cargo test golden`: 10 passed (golden files already updated in same run).
- Example: `cargo run --bin qb64fresh -- examples/graphics/opengl_minimal.bas --emit-c` generates `opengl_minimal.c` with QB64FRESH_OPENGL and call_gl*.

## Files touched
- `src/codegen/c_backend/runtime/graphics.rs` — qb_behind/ontop/only/onlybackground stubs.
- `runtime/include/qb64fresh_rt.h` — declarations for those four.
- `runtime/src/graphics_ffi.rs` — implementations.
- `tests/integration_tests.rs` — compile_to_c uses program_uses_opengl; new opengl_integration mod; glrender_with_* tests.
- `scripts/gl_parse_header.py` — void-only params → 0 params.
- `src/semantic/builtins_opengl_functions.rs`, `runtime/c_src/gl_wrappers_generated.inc` — regenerated.
- `examples/graphics/opengl_minimal.bas` (new), `examples/README.md` — OpenGL example and index.
