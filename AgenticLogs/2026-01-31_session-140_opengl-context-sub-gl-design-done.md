# Session 140 – OpenGL context and SUB _GL design (item 3 from REVIEW_IN_MORNING)

**Date:** 2026-01-31  
**Purpose:** Implement/document the design for when to create GL context and how main loop invokes SUB _GL (session 134 item 3).

## Done

1. **Shutdown:** In `runtime/src/graphics/sdl2.rs`, `shutdown()` now drops the OpenGL context before the window (`gl_context = None` before `canvas = None`) so SDL2 teardown order is correct.

2. **Design doc §5.2:** Updated `docs/ThingsToDo/OPENGL_GLUT_DESIGN.md` §5.2 with:
   - When to create context: lazily on first `display()` after `gl_render_mode() >= 0`.
   - How main loop invokes SUB _GL: the “main loop” is the existing SCREEN pipeline; every `display()` is the frame; when GL active, `display_gl()` makes context current, calls `invoke_sub_gl()` (SUB _GL), then `gl_swap_window()`.
   - Ordering: _ONLY (2) implemented (GL-only); _BEHIND/_ONTOP 2D+GL composition deferred.
   - Shutdown: GL context dropped before window.

3. **Status table:** Design doc “Current implementation status” now states _GLRENDER invokes SUB _GL each frame when mode ≥ 0 (GL-only path; _BEHIND/_ONTOP deferred).

4. **REVIEW_IN_MORNING:** Item 3 marked **Done** with summary of current behavior and design doc reference.

## No code change to display path

The display path was already correct: `display()` checks `gl_render_mode()`, calls `display_gl()` when ≥ 0; `display_gl()` creates context lazily, makes current, `invoke_sub_gl()`, swap. Only shutdown and documentation were updated.
