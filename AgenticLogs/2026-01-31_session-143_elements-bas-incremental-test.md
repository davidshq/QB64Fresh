# Session 143: utilities/elements.bas incremental test

**Date:** 2026-01-31

## Goal

Run the incremental test for `utilities/elements.bas` (marked "Not yet tested" in QB64PE_INCREMENTAL_TESTING.md).

## Actions

1. **Created** `tests/qb64pe_incremental/02_utilities_elements.bas` following the same pattern as `02_utilities_hash.bas` and `02_utilities_type.bas`:
   - Core includes: `global/version.bas`, `global/settings.bas`, `global/constants.bas` (elements.bas uses `sp`, `sp2`, `sp3` from constants.bas)
   - Single include: `utilities/elements.bas` (no .bi; implementation only)
   - Minimal test code (DIM, PRINT)

2. **Ran QB64Fresh** on `02_utilities_elements.bas` with `--emit-c`:
   - Result: **Success** (exit 0). All phases (preprocess, lex, parse, semantic, codegen) completed; C generated to `02_utilities_elements.c`.

3. **C compilation** of generated file with `gcc -c -I runtime/include`:
   - Result: **Fails** with const qualifier mismatches (e.g. `qb_environ`, `qb_base64encode`, `qb_base64decode`, `qb_deflate`, `qb_inflate`, `qb_adler32`, `qb_crc32`, `qb_md5`). Header declares `const QbString*`, generated code has `QbString*`. Same class of issue as noted in the doc for the full compiler (fixed in session 071 for other paths); inline runtime stubs for this build path still emit non-const.

## Outcome

- **elements.bas** is now **tested** in the incremental suite.
- **QB64Fresh (all phases):** ✅ PASS.
- **C compilation:** ❌ Fails (pre-existing codegen const issue in inline runtime).
- **Doc updated:** QB64PE_INCREMENTAL_TESTING.md — elements.bas marked as tested with status and note about C compile.
