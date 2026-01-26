# Session 061: TODO, NEXT_STEPS, and ThingsToDo docs update

**Date:** 2026-01-25

## Summary

Updated TODO.md, NEXT_STEPS.md, and all markdown files in docs/ThingsToDo/ to reflect the current codebase state.

## Changes

### TODO.md
- Moved **Alpha blending** and **Multiple screen pages** from "Not Yet Implemented" to "Completed" under Phase 3 (Graphics). Both are implemented in the external runtime (SDL2).
- Added explicit link to [docs/ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md](docs/ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md) for the Testing subsection.

### NEXT_STEPS.md
- Related Documents: noted QB64pe compat (122/141, 86.5%); added [docs/QB64pe/](docs/QB64pe/) (QB64pe architecture, behavioral diffs, migration).

### docs/ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md
- **Totals:** 1,376+ → 1,390+; runtime 195 → 208.
- **Golden tests:** 10 total; 2 pass, 8 fail (codegen output diffs, e.g. STRIG dispatch ordering). Golden status and Current Test Failures section updated.
- **Module line counts:** string.rs 758→1700+, io.rs 850+→2000+.
- Changelog: runtime 195→208; golden 2 pass/8 fail; io/string line counts.

### docs/ThingsToDo/RUNTIME_IMPLEMENTATION_PLAN.md
- **External runtime:** Note that `qb_glrender` and `qb_glcompat` (no-op stubs) are in `graphics_ffi.rs`, `qb64fresh_rt.h`, and inline `graphics.rs`.

### docs/ThingsToDo/OPENGL_SUPPORT.md
- **Current QB64Fresh Codebase Status:** _GLRENDER and _GLCOMPAT rows updated: runtimes now define no-op stubs (inline in `graphics.rs`; external in `graphics_ffi.rs` and `qb64fresh_rt.h`). Programs link successfully.
- **Gaps for Minimal "GL-related" Stubs:** Renamed to "Minimal … — Done"; bullets updated to state stubs are in place; "To allow" line shortened.
- **Implementation Requirements note:** "neither is defined" → "No-op stubs are now defined" for qb_glrender/qb_glcompat.

### docs/ThingsToDo/RUNTIME_ARCHITECTURE_PERSPECTIVES.md
- Replaced references to deleted `STUB_FUNCTIONS_REMAINING.md` with [STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md) in Actions and Related.

### docs/ThingsToDo/INSTALLER_PLAN.md
- Migration Guide link: `../QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md` → `../QB64pe/QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md` (docs moved under docs/QB64pe/).

### docs/ThingsToDo/QB64Fresh_AND_MODERN_LANGUAGES.md
- **3.4 Gaps:** Removed "alpha blending" from "Not yet"; noted it is done (_BLEND, _DONTBLEND, _CLEARCOLOR).
- **4.3 Sustainability:** 1,500+ → 1,390+ tests; added QB4.5 compat (122/141, 86.5%).
- **Related:** BEHAVIORAL_DIFFERENCES.md and MIGRATION_GUIDE.md → `../QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md` and `../QB64pe/QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md`.

### Not changed
- **docs/ThingsToDo/QB64PE_IDE_FUNCTIONALITY_CHECKLIST_TODO.md:** Already dated 2026-01-25; no content changes.
- **docs/ThingsToDo/INFORM/** (INFORM_EXPERT_DISCUSSION.md, INFORM_FUNCTIONALITY.md): InForm-specific; no codebase-state updates.

## Verified (from codebase and test runs)

- **Compiler unit:** 404 passed.
- **Integration:** 720 passed.
- **Runtime:** 208 passed.
- **QB45 compat:** 122/141 (86.5%); 10 parser + 9 semantic failures.
- **Golden:** 2 pass, 8 fail (codegen diffs).
- **runtime/src/memory.rs:** Present.
- **qb_glrender / qb_glcompat:** Stubs in `src/codegen/c_backend/runtime/graphics.rs`, `runtime/src/graphics_ffi.rs`, `runtime/include/qb64fresh_rt.h`.
- **docs/QB64pe/:** Contains QB64PE_ARCHITECTURE, QB64PE_DEBUGGING, QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES, QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.
- **STUB_FUNCTIONS_REMAINING.md:** Deleted; STUB_FUNCTIONS_FULL.md in docs/archive/ is the reference.
