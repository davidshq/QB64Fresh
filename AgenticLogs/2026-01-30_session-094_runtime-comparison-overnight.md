# Session 094: Runtime Comparison Tests Overnight

**Date:** 2026-01-30

## Summary

Creating a large suite of runtime comparison tests (QB64Fresh vs QB64pe) to exercise every feature and document all behavioral differences. Work proceeds without user intervention; items requiring user input are recorded in `tests/runtime_comparison/MORNING_REVIEW.md`.

## Goals

1. Create hundreds of minimal .bas tests covering: math, strings, arrays, control flow, types, file I/O, error handling, procedures, etc.
2. Add a runner script that compiles and runs each test with both compilers and captures output.
3. Document all differences in `tests/runtime_comparison/DIFFERENCES.md`.
4. Defer any task that requires user intervention to MORNING_REVIEW.md.

## Accomplished

1. **MORNING_REVIEW.md** — Tasks needing user intervention (run QB64pe comparison, review DIFFERENCES, fix failing tests).
2. **run_comparison.sh** — Runner: compiles and runs each .bas with QB64Fresh (and QB64pe if built). `RUN_PE=0` skips QB64pe. Results in `results/fresh/` and `results/qb64pe/`.
3. **123 runtime comparison tests** — New tests 06_*.bas through 125_*.bas (plus existing 01–05). Categories: math, string, control flow, SUB/FUNCTION, TYPE, file I/O, DATA/READ, REDIM, EXIT, SELECT CASE variants, edge cases.
4. **DIFFERENCES.md** — Documented: known output differences (03 ERR/ERL, 05 file I/O), QB64Fresh compile/link failures (19 ELSEIF, 28 REDIM, 47 BYREF, 59 SEEK link, 120 REDIM link), test coverage by category.
5. **Full QB64Fresh run** — `RUN_PE=0 ./run_comparison.sh` completed: 118 tests run successfully; 5 fail (19, 28, 47 compile; 59, 120 link).

## Files

- `tests/runtime_comparison/` — 123 .bas files, run_comparison.sh, MORNING_REVIEW.md, DIFFERENCES.md, README.md, results/fresh/*.txt
- `AgenticLogs/2026-01-30_session-094_runtime-comparison-overnight.md` — this log
