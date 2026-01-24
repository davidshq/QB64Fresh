# Session 044: Complete Language Reference

**Date:** 2026-01-24  
**Goal:** Complete QB64Fresh_LANGUAGE_REFERENCE.md per FUTURE.md task.

## Accomplished

### 1. Scope and Implementation Status (new section)
- Added **Scope and Implementation Status** as a top-level section and TOC entry.
- Documented: QB4.5 + curated QB64; excluded `_GL*`; stub-only (INP, OUT, WAIT, INTERRUPT, PEN, IOCTL, ERDEV, ON COM, ON UEVENT, ON SIGNAL); pointer to `src/semantic/builtins.rs` and ADR-0014 / FUTURE.md.

### 2. New or updated entries
- **`?`** — Shorthand for `PRINT` (QB4.5) in I/O Statements.
- **`_WRITEFILE path$, content$`** — Statement to write a string to a file (File I/O).
- **`_SCREENIMAGE[(x1,y1,x2,y2)]`** — Capture screen/region to image (Graphics Functions).
- **`DECLARE STATIC LIBRARY "name" ... END DECLARE`** — Static library declaration (C Library Integration).
- **`_PROCPTR(procedureName)`** — Callbacks for C (e.g. qsort); supported signature and ADR-0008 link.

### 3. Constants section (expanded)
- **Boolean/handle:** `_TRUE`, `_FALSE`, `_NONE`.
- **Platform:** `_WINDOWS`, `_LINUX`, `_MACOSX` (with legacy `WIN`/`LINUX`/`MAC` note).
- **Error codes:** `_ERR_*` pattern and examples; pointer to builtins for full list.
- **Keyboard:** `_KEY_F1`–`_KEY_F12`, nav, arrows, modifiers, etc.
- **Character:** `_NUL`–`_US`, `_DEL`; common ones (`_TAB`, `_LF`, `_CR`, `_ESC`).

### 4. See Also and Notes
- **See Also:** Replaced `MIGRATION_GUIDE.md` with `QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md`; added ADR-0014.
- **Notes #6–7:** `_GL*` exclusion; stub-only/legacy (PEEK/POKE, INP/OUT/WAIT, ON COM/UEVENT/SIGNAL) and ADR-0014.

### 5. FUTURE.md
- Marked **Complete language reference** as `[x]` with a short completion note.

## Files touched
- `docs/QB64Fresh_LANGUAGE_REFERENCE.md` — Scope, TOC, ?/PRINT, _WRITEFILE, _SCREENIMAGE, DECLARE STATIC LIBRARY, _PROCPTR, Constants, See Also, Notes.
- `docs/ThingsToDo/FUTURE.md` — Language reference task completed.

## Decisions
- **Scope vs. full impl table:** One Scope subsection with exclusion/stub pointers rather than a large implementation table; ADR-0014 and builtins stay the source of truth.
- **`_WRITEFILE` placement:** Documented as a File I/O statement (it is a SUB), and removed the duplicate from File Functions.
- **`?`:** Added to the PRINT entry; no separate `?` section.

## References
- ADR-0014 (scope, exclusions, stubs)
- FUTURE.md (DECLARE LIBRARY, _PROCPTR, _WRITEFILE)
- `src/semantic/builtins.rs` (built-in set)
