# Session 093: Runtime Comparison (QB64Fresh vs QB64pe)

**Date:** 2026-01-30

## Summary

Completed the five “Immediate Investigation” runtime comparison tasks from GENERATED_C_REVIEW.md: string comparison, LBOUND/UBOUND, ON ERROR RESUME NEXT, GOSUB/RETURN, and file I/O. Added test programs, ran both compilers, and documented results.

## Accomplished

1. **Test programs** — Created `tests/runtime_comparison/` with five minimal .bas files:
   - `01_string_ops.bas` — concat, LEN, MID$, LEFT$, RIGHT$, comparison
   - `02_lbound_ubound.bas` — DIM arr(1 TO 5), LBOUND/UBOUND, iterate
   - `03_on_error_resume_next.bas` — ON ERROR GOTO, ERROR 5, ERR/ERL, RESUME NEXT
   - `04_gosub_return.bas` — GOSUB/RETURN
   - `05_file_io.bas` — OPEN, PRINT #, CLOSE, LINE INPUT #, KILL

2. **QB64Fresh inline runtime fix** — Inline-generated C was calling `qb_gfx_poll_events()` and `qb_gfx_display()` from `qb_limit()` before the graphics stubs were defined. Added forward declarations in `src/codegen/c_backend/runtime/mod.rs` for `qb_gfx_poll_events` and `qb_gfx_display` so inline builds compile with gcc -lm.

3. **$CONSOLE:ONLY** — Added to all five tests so QB64pe runs in console mode and prints to stdout.

4. **Ran both compilers** — QB64Fresh: emit C (--runtime inline), gcc -lm, run. QB64pe: ./qb64pe -x file.bas -o exe, run (from QB64pe dir with 4GB ulimit).

5. **Documented results** — Added “Runtime Comparison Results (2026-01-30)” to GENERATED_C_REVIEW.md and README in tests/runtime_comparison/.

## Findings

| Area | QB64Fresh | QB64pe | Match? |
|------|-----------|--------|--------|
| String ops | Correct (concat, LEN, MID$, LEFT$, RIGHT$, comparison -1) | Same | Yes |
| LBOUND/UBOUND | lbound 1, ubound 5, values 10..50 | Same | Yes |
| ON ERROR | Handler runs, RESUME NEXT works; ERR/ERL not printed in handler | ERR=5, ERL=0 printed | No — ERR/ERL in handler |
| GOSUB/RETURN | main/sub/main/sub/main | Same | Yes |
| File I/O | “first: first: ” / “second: second: ” (wrong read/print) | “first: line1” / “second: line2” | No — LINE INPUT or PRINT bug |

## Follow-ups

- **ERR/ERL in ON ERROR handler:** Verify that QB64Fresh sets `_qb_err` / `_qb_erl` before jumping to the handler and that generated code reads them in the handler.
- **File I/O:** Investigate LINE INPUT # and/or PRINT of string after LINE INPUT (possible wrong variable or buffer).

## Files touched

- `tests/runtime_comparison/` — new: 01–05 .bas, README
- `docs/ThingsToDo/GENERATED_C_REVIEW.md` — Runtime Comparison Results section
- `src/codegen/c_backend/runtime/mod.rs` — forward declarations for qb_gfx_poll_events, qb_gfx_display
