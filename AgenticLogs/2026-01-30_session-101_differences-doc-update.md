# Session 101: QB64Fresh_VS_QB64pe_DIFFERENCES.md update

**Date:** 2026-01-30

## Goal

Update `docs/QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md` to reflect the current codebase: fix broken links, align with implemented behavior (ERR/ERL, REDIM, BYREF, File I/O), and add references to runtime comparison tests.

## Accomplished

1. **Broken link:** Replaced reference to non-existent `FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md` (Cross-Procedure GOTO section) with ADR-0016 (docs/adrs/ADR-0016-intentional-behavioral-differences.md).

2. **See also:** Added link to tests/runtime_comparison/README.md for runtime comparison tests.

3. **Language semantics:** Documented current support for:
   - **REDIM and dynamic arrays** (§5): REDIM PRESERVE (including `PRESERVE` without underscore), dynamic arrays (`DIM a() AS type`), REDIM without PRESERVE; referenced tests 28, 202, 203, 206.
   - **BYREF parameters** (§6): SUB/FUNCTION BYREF for scalars, strings, UDTs; referenced tests 190, 191.

4. **File I/O:** Added "Sequential and line input" subsection: LINE INPUT # and INPUT # (string) identical behavior; OPEN with empty filename handled (IDE/config).

5. **Error handling (runtime):** Clarified ERR/ERL support: ERR and ERL set in ON ERROR handler; ERROR statement triggers jump and sets both; ERL is 0 for ERROR statement, may be set for runtime errors. Referenced tests 228, 230, 247.

## Files touched

- `docs/QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md`

## Decisions

- No change to OPENGL link (path `../ThingsToDo/OPENGL_SUPPORT.md` was already correct).
- Did not add a specific test count (249) in the doc; README and ADDITIONAL_TESTS hold current counts.
