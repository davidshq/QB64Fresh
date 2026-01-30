# Session 097: Implement unimplemented tests (240–259) from ADDITIONAL_TESTS.md

**Date:** 2026-01-30

## Goal

Implement the remaining tests listed in `tests/runtime_comparison/ADDITIONAL_TESTS.md` that were not yet created: section 16 (QB64pe compile_tests–style 240–249) and section 17 (qbasic_testcases minimal console 250–259).

## Accomplished

- **Created 20 new `.bas` tests** in `tests/runtime_comparison/`:

  **240–248 (compile_tests–style):**
  - **240_data** – DATA/READ loop (from QB64pe `console_only/data.bas`).
  - **241_const_simple** – CONST with basic math (no unsigned/EQV/IMP).
  - **242_types_simple** – TYPE with LONG, DOUBLE, STRING only.
  - **243_iif** – _IIF numeric and string (minimal).
  - **244_op_exp** – Operator precedence (^, *, /, \, NOT).
  - **245_str_variants** – STR$ (number to string); _CAST skipped (QB64-specific).
  - **246_environ_set** – ENVIRON set then ENVIRON$ read.
  - **247_on_error_simple** – ON ERROR GOTO, ERR/ERL, RESUME NEXT (no DECLARE LIBRARY).
  - **248_str** – STR$ leading space for positive numbers.

  **249** – Skipped (logging/_LOG_* is QB64-specific).

  **250–259 (minimal console, qbasic_testcases style):**
  - **250_rot13_mini** – ROT13 on "ab" (inspired by misc/rot13.bas).
  - **251_print_chain** – PRINT with commas and semicolons.
  - **252_for_read_data** – FOR with READ/DATA.
  - **253_select_simple** – SELECT CASE with numeric and IS.
  - **254_gosub_simple** – GOSUB/RETURN with label.
  - **255_type_print** – TYPE with PRINT fields.
  - **256_const_use** – CONST used in expressions.
  - **257_math_builtin** – SQR, INT, SGN, ABS, SIN, COS, ATN.
  - **258_string_builtin** – LEN, LEFT$, RIGHT$, MID$, INSTR.
  - **259_control_mix** – WHILE, DO LOOP, EXIT DO.

- **QB64Fresh:** All 20 new tests compile with `qb64fresh --emit-c` (verified in-session).

- **Updated ADDITIONAL_TESTS.md:** Documented 240–248 and 250–259 as created; updated test count to 250; aligned section 16 table with actual filenames (245_str_variants, 246_environ_set, 247_on_error_simple, 248_str, 249 skipped).

## Decisions

- **245:** Use STR$ variants instead of _CAST; _CAST and QB64pe cast_test use _OFFSET, _UNSIGNED, _FLOAT, etc., which are out of scope for a minimal runtime comparison.
- **249:** Omit 249_logging; _LOG_TRACE/_LOG_INFO etc. are QB64-specific.
- **250–259:** New minimal deterministic programs (no INPUT, no graphics/sound), inspired by qbasic_testcases style rather than copying full QB64pe programs that use INPUT or SOUND.

## Next steps

- Run full `./run_comparison.sh` and `./diff_results.sh` when QB64pe is built to record any output differences for 240–248 and 250–259 in DIFFERENCES.md.
