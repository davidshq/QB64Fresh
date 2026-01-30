# Session 095: Additional Runtime Comparison Tests Doc (2026-01-30)

## Goal

Create a document that lists all additional tests we might want to add to the QB64Fresh/QB64pe runtime comparison, covering all functionality. If QB64pe wasn't built, run `setup_lnx.sh` and then run the tests.

## Done

1. **ADDITIONAL_TESTS.md**  
   Created `tests/runtime_comparison/ADDITIONAL_TESTS.md` with:
   - **Prerequisites:** Run `../QB64pe/setup_lnx.sh` if QB64pe not built; run `run_comparison.sh` and `diff_results.sh` from QB64Fresh repo root.
   - **Blocked tests (5):** 19 (ELSEIF), 28 (REDIM), 47 (BYREF), 59 (SEEK), 120 (REDIM no PRESERVE). Do not add more tests for these until QB64Fresh fixes are in.
   - **Additional tests by category:** Control flow (146–153), math/numeric (154–165), string (166–172), file I/O (173–182), PRINT/console (183–188), procedures (189–195), TYPE/UDT (196–200), arrays/REDIM (201–205), constants/literals (206–210), system/environment (211–215), DATA/READ/RESTORE (216–219), GOSUB/GOTO/labels (220–223), error handling (224–226), RND/TIMER (227–229), QB64pe compile_tests to adapt (230–239), qbasic_testcases (240–249), edge/stress (250–254).
   - **Numbering:** Use 146+ for new tests; optional blocks by category.
   - **How to add a test:** Create `NN_name.bas` with `$CONSOLE:ONLY`, run comparison, document differences in DIFFERENCES.md.

2. **QB64pe status**  
   Confirmed `qb64pe` binary exists at `../QB64pe/qb64pe`; no need to run `setup_lnx.sh` for this session.

3. **Current test count**  
   Confirmed 143 `.bas` tests in `runtime_comparison/`; DIFFERENCES.md and README already describe the suite.

## Notes

- Run comparison with `bash tests/runtime_comparison/run_comparison.sh` if the script isn’t executable. A syntax error was seen once near the end of a full run; doc suggests using `RUN_PE=0` for QB64Fresh-only if needed.
- ADDITIONAL_TESTS.md is the single backlog for “what to add next” and points to DIFFERENCES.md for current results and blocked items.
