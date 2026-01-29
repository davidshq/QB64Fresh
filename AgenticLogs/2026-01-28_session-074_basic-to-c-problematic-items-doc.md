# Session 074: BASIC-to-C Problematic Language Items Document

**Date:** 2026-01-28  
**Brief:** Created `docs/BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md` cataloging BASIC language items that are problematic when compiling to C (and when implementing the compiler in Rust).

## Accomplished

- **New doc:** `docs/BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md`
  - **Purpose:** Single reference for items that have caused bugs and items that may still cause problems.
  - **Sources:** REGRESSION_TEST_COVERAGE.md, KEY_LEARNINGS.md, QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md, TODO_CONSOLIDATED.md, ADR-0008, semantic/codegen error types, AgenticLogs.
  - **Structure:**
    1. Strings (QbString vs qb_string, double-wrap, temp pool, fixed-length, C API)
    2. Types (operators, _IIF, fixed-length, typedefs, signatures)
    3. BYREF/BYVAL (writeback, double-wrap; **BYREF scalars still broken**)
    4. Arrays (array vs function call, rename, OPTION BASE, scope, shadowing)
    5. Control flow (labels, forwards, EXIT/CONTINUE/RETURN, FOR/NEXT; cross-proc GOTO not supported)
    6. C interop (STRING→const char*, BYREF STRING limitations)
    7. Lexer/Parser ($CONSOLE workaround, SUB args, DATA, etc.)
    8. Identifiers (mangling, reserved words)
    9. Constants (_CHR_/_STR_, CONST)
    10. Built-ins (_SHELLHIDE, RND, etc.)
  - **Summary table** at end with counts of “caught us” vs “may still be problematic” per category.

## Decisions

- Included both **resolved** items (so we don’t forget what bit us) and **potential** items (so future work and code review can watch for them).
- Called out **BYREF scalar parameters** as the main remaining semantic bug (local copy, no writeback).

## Follow-up: Fixes Needed Doc

- **New doc:** `docs/ThingsToDo/FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md`
  - Full codebase review for instances of problematic language items that need fixes.
  - **High:** DECLARE DYNAMIC LIBRARY (not implemented), OPEN access/lock modes (not passed to runtime), LBOUND/UBOUND stubs in external mode (return 0).
  - **Medium:** RUN stub, cross-function GOTO error message, temp pool overflow logging, external type registry.
  - **Low:** ON ERROR _NEWHANDLER test, BYREF scalar regression test, duplicate label test.
  - BYREF scalar parameters confirmed fixed (pointer alias + dereference); only optional test added.

## Related

- [REGRESSION_TEST_COVERAGE.md](../docs/REGRESSION_TEST_COVERAGE.md)
- [KEY_LEARNINGS.md](../docs/KEY_LEARNINGS.md)
- [TODO_CONSOLIDATED.md](../docs/ThingsToDo/TODO_CONSOLIDATED.md)
- [FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md](../docs/ThingsToDo/FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md)
