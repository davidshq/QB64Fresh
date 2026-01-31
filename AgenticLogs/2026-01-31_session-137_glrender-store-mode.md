# Session 137: _GLRENDER store-mode and full implementation

**Date:** 2026-01-31

## Summary

User asked whether we should implement the TODO in `qb_glrender` (store mode, create GL context, main loop calls SUB _GL). We did the minimal store-mode first, then the user asked to implement it fully.

## Full implementation (this session)

1. **Runtime – SUB _GL registration**
   - `gl_ffi.rs`: Added `qb_gl_register_sub_gl(void (*cb)(void))` to store the generated C’s `qb_sub__gl` pointer. Added `invoke_sub_gl()` which sets `sub_gl_called`, calls the callback, clears `sub_gl_called`.
   - Header: Declared `qb_gl_register_sub_gl` under `#ifdef QB64FRESH_OPENGL`.

2. **Runtime – SDL2 OpenGL path**
   - When `opengl` and `graphics-sdl2` are enabled: create window with `.opengl()` so it can have a GL context.
   - Added `gl_context: Option<GLContext>` to `SDL2Backend`; create context lazily in `display_gl()` on first `_GLRENDER` use.
   - In `display()`: when `gl_render_mode() >= 0`, call `display_gl(mode)` instead of 2D path: make GL context current, `invoke_sub_gl()`, `gl_swap_window()`. (2D+GL composition for _BEHIND/_ONTOP left for later; currently GL-only when _GLRENDER is active.)

3. **Codegen**
   - When `uses_opengl`, in `main()` after `qb_init_startdir()` emit `#ifdef QB64FRESH_OPENGL` / `qb_gl_register_sub_gl(qb_sub__gl);` / `#endif`.

4. **Store mode** (from earlier in session)
   - `qb_glrender(mode)` stores mode in `GL_RENDER_MODE`; `gl_render_mode()` getter for the backend.

## Files changed

- `runtime/src/gl_ffi.rs`: Callback registration, `invoke_sub_gl()`.
- `runtime/include/qb64fresh_rt.h`: `qb_gl_register_sub_gl` declaration.
- `runtime/src/graphics/sdl2.rs`: Window with `.opengl()` when opengl; `gl_context`; `display_gl()`; `display()` branches to GL path when `gl_render_mode() >= 0`.
- `runtime/src/graphics_ffi.rs`: (unchanged from store-mode) `GL_RENDER_MODE`, `gl_render_mode()`.
- `src/codegen/c_backend/mod.rs`: Emit `qb_gl_register_sub_gl(qb_sub__gl)` at startup when `uses_opengl`.

## Notes

- _BEHIND / _ONTOP (2D + GL composition) not implemented: when _GLRENDER is active we use the GL-only path. Full 2D-under-GL or GL-over-2D would need render-to-texture and composition.
- Build runtime with `--features "graphics-sdl2,opengl"` and link generated C with that runtime for OpenGL programs.
