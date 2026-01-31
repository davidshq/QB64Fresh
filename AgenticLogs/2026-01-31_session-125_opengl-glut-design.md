# Session 125: OpenGL/GLUT Design (Modular, Optional)

**Date:** 2026-01-31

## Summary

User requested adding the same OpenGL/GLUT functionality as QB64pe to QB64Fresh, in a well-architected, modular way, ideally only included when actually used.

## Decisions

1. **Design document created:** [docs/ThingsToDo/OPENGL_GLUT_DESIGN.md](../ThingsToDo/OPENGL_GLUT_DESIGN.md)
   - QB64pe model summarized: hundreds of `_GL*` built-ins from gl.h, `SUB _GL`, `_GLRENDER`, `_GLCOMPAT`, `call_gl*` wrappers with `sub_gl_called` check (error 270).
   - Architecture: optional compiler layer (builtins + codegen under `#ifdef QB64FRESH_OPENGL`) and optional runtime feature (`opengl`) so OpenGL is only compiled/linked when the program uses `_GL*`/`SUB _GL` or `--opengl`.
   - “Only when used”: use detection (scan for `_GL*` or `SUB _GL`) or explicit `--opengl`/`--no-opengl`; runtime feature `opengl` not in default set.
   - Phased implementation: detection + flags → small built-in set + codegen → runtime GL context + call_gl* → expand set → docs/ADR/CI.

2. **ADR-0014:** Added a “Future: Optional OpenGL” paragraph pointing to the design doc; when implemented, ADR would be updated to “optional” rather than “excluded.”

3. **LIBQB_FUNCTIONALITY.md:** Added a reference to the OpenGL design doc in the core/OpenGL row.

## Accomplished

- [docs/ThingsToDo/OPENGL_GLUT_DESIGN.md](../ThingsToDo/OPENGL_GLUT_DESIGN.md) — full design: QB64pe model, principles, compiler/runtime layout, file layout, implementation phases.
- ADR-0014 and LIBQB_FUNCTIONALITY.md updated with cross-references.

## References

- QB64pe: `source/subs_functions/extensions/opengl/`, `internal/c/parts/core/gl_header_for_parsing/`, `tests/qbasic_testcases/open_gl/simple_example.bas`
- QB64Fresh: ADR-0014 (scope/excluded), ADR-0006 (graphics), runtime graphics mod.rs, builtins.rs, codegen runtime/graphics.rs
