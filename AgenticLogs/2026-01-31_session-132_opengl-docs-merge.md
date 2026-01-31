# Session 132: OpenGL docs merge (OPENGL_SUPPORT → OPENGL_GLUT_DESIGN)

**Date:** 2026-01-31

## Summary

Merged **OPENGL_SUPPORT.md** into **OPENGL_GLUT_DESIGN.md** (authoritative) and deleted OPENGL_SUPPORT.md. All references now point to OPENGL_GLUT_DESIGN or docs/OPENGL.md.

## Changes

1. **OPENGL_GLUT_DESIGN.md** (authoritative)
   - Added **Status** line in Summary: link to docs/OPENGL.md and note that optional OpenGL is implemented.
   - Added **§1.4 Type mapping** — QB64pe OpenGL → QB64 type table (from OPENGL_SUPPORT).
   - Added **§9 Current implementation status** — table of compiler/runtime/CI status.
   - Added **§10 QB64pe header analysis and code reuse** — gl.h stats, reuse (header, type mapping, wrapper pattern, Rust `gl` crate).
   - Added **§11 Challenges and estimated effort** — brief.
   - Added **§12 Alternatives not chosen** — WebGL, Vulkan, DECLARE LIBRARY.
   - Renumbered **File layout** to §13 and **Summary** to §14.

2. **OPENGL_SUPPORT.md**
   - Deleted. Content folded into OPENGL_GLUT_DESIGN (design, status, type table, reuse, challenges, alternatives).

3. **References**
   - **DOCS-README.md:** Planning table and directory tree now list OPENGL_GLUT_DESIGN.md instead of OPENGL_SUPPORT.md; description points to OPENGL.md for user-facing usage.
   - **CONSOLIDATION_RECOMMENDATIONS.md:** OpenGL row and §2.4 updated to “Done (merged)”; archive table and Implementation Order item 4 updated to note OPENGL_SUPPORT merged and deleted.

## Canonical OpenGL docs

- **User-facing:** [docs/OPENGL.md](docs/OPENGL.md) — how to enable, rules, building.
- **Design/implementation:** [docs/ThingsToDo/OPENGL_GLUT_DESIGN.md](docs/ThingsToDo/OPENGL_GLUT_DESIGN.md) — architecture, phases, status, type mapping, reuse, file layout.
