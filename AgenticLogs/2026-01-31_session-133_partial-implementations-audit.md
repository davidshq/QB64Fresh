# Session 133: Partial Implementations Doc Audit (2026-01-31)

## Summary

Updated `docs/ThingsToDo/PARTIAL_IMPLEMENTATIONS.md` to match the current codebase. Many items previously marked partial (⚠️) are **complete in external runtime** (🟢).

## Changes

1. **Intro** – Clarified that Phase 2 (Graphics) items are implemented in external runtime (`graphics_ffi.rs`, `graphics/sdl2.rs`); inline mode remains stubs.

2. **Graphics Features table** – Font row: external runtime 🟢 with `graphics-sdl2-ttf` or `graphics-sdl2-freetype`; corrected feature names.

3. **File I/O Features** – New section stating File I/O is complete (OPEN, CLOSE, PRINT #, INPUT #, GET/PUT, SEEK, LOF/EOF/LOC, FREEFILE); reference to TODO-completed.

4. **Phase 2 (Graphics) tables** – All screen management (2.1), drawing primitives (2.2), color/image (2.3), and text/fonts (2.4) rows updated from ⚠️ to 🟢. Added status legend: 🟢 = external runtime; inline stubs. Font subsection: both SDL2_ttf and FreeType feature options noted.

5. **Debugger Infrastructure** – Added dedicated section so TOC link works; summary of tools/debug and pending runtime hooks.

6. **Recent Changes** – New section documenting this audit (2026-01-31).

## Verification

- Checked `runtime/src/graphics_ffi.rs` for FFI coverage (screen, draw, color, image, font, mouse, etc.).
- Checked `runtime/Cargo.toml` for feature names (`graphics-sdl2-ttf`, `graphics-sdl2-freetype`).
- Cross-referenced `docs/archive/TODO-completed.md` for Phase 2/3 Graphics and File I/O completion.

## Outcome

PARTIAL_IMPLEMENTATIONS.md now accurately reflects that **external runtime has full Phase 2 graphics implementation**; the doc no longer suggests those items are partial. Inline mode remains correctly described as stubs.

## Follow-up (same day)

- **Completed items moved to TODO-completed:** Checklist items that were completed (Implementation Order – Core I/O through System, documentation update, etc.) were moved from `docs/ThingsToDo/PARTIAL_IMPLEMENTATIONS.md` to `docs/archive/TODO-completed.md` under **Partial Implementations Doc – Completed Items (moved 2026-01-31)**. PARTIAL_IMPLEMENTATIONS.md now points to TODO-completed for the full checklist; the active doc keeps only the status tables and notes.
