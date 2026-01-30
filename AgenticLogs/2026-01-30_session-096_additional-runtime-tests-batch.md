# Session 096: Additional runtime comparison tests batch

**Date:** 2026-01-30

## Goal

Create as many tests from `tests/runtime_comparison/ADDITIONAL_TESTS.md` as possible without requiring user permission. Skip tests that need permission; note them for later.

## Accomplished

- **Created 87 new `.bas` tests** in `tests/runtime_comparison/`:
  - Control flow: 147–153 (skipped 146 ELSEIF blocked)
  - Math/numeric: 154–165
  - String: 166–173
  - File I/O: 174–183
  - PRINT/console: 184–189
  - Procedures/type: 192–201 (skipped 190–191 BYREF)
  - Arrays/const: 204–211 (skipped 202–203 REDIM)
  - System: 212–214, 216–219 (skipped 215 SHELL)
  - DATA/gosub: 220–227
  - Error: 228–230
  - RND/timer: 232–234
  - Other: 237 (CSRLIN/POS)
  - Edge: 260–264

- **Skipped (no file created):** 146, 190, 191, 202, 203, 215, 231, 235, 236, 238, 239 (documented in ADDITIONAL_TESTS.md under "Tests skipped (need permission or deferred)").

- **Updated ADDITIONAL_TESTS.md:** Added section "Tests skipped (need permission or deferred)" and a note on six tests that were created but currently fail under QB64Fresh (147, 151, 179, 194, 197, 218).

## Run results (QB64Fresh-only)

- Ran `RUN_PE=0 ./run_comparison.sh`: **230 tests** total (143 existing + 87 new).
- **6 new tests** currently fail with QB64Fresh:
  - 147_on_error_resume_0: FAIL_LINK_FRESH (RESUME 0 runtime)
  - 151_exit_select: FAIL_COMPILE_FRESH (EXIT SELECT not supported)
  - 179_file_lock: FAIL_COMPILE_FRESH (LOCK/UNLOCK)
  - 194_array_param_2d: FAIL_COMPILE_FRESH (2D array param syntax)
  - 197_type_array_field: FAIL_COMPILE_FRESH (TYPE with array field)
  - 218_fre: FAIL_COMPILE_FRESH (FRE)

All other new tests compile and run (output comparison with QB64pe can be done later via `./diff_results.sh` when QB64pe is built).

## Decisions

- Used deterministic, console-only tests; no INPUT, SHELL, or dialogs.
- ENVIRON$, COMMAND$, CHDIR, MKDIR/RMDIR, _FILEEXISTS, _DIREXISTS: created tests that use normal filesystem (relative paths); output may differ by environment but no permission needed.
- Left the six failing tests in the suite so they act as placeholders once compiler/runtime support is added.
