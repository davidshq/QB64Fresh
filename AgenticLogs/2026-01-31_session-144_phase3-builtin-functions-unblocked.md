# Session 144: Phase 3 Built-in Functions Unblocked

**Date:** 2026-01-31  
**Goal:** Unblock Phase 3 (Built-in Functions) in QB64pe incremental testing.

## Summary

Phase 3 was marked BLOCKED ("Needs main compiler infrastructure"). It is now **PASSES** by adding minimal stub infrastructure so `03_builtin_functions.bas` compiles in isolation.

## Changes

1. **`tests/qb64pe_incremental/sections/phase3_stubs.bas`** (new)
   - `DIM SHARED subfunc AS STRING * 256` — so `id.insubfunc = subfunc` in regid type-checks.
   - `DIM SHARED subfuncn AS LONG` — (review fix) used by regid and by const_eval/type.bas; defined in qb64pe.bas.
   - `FUNCTION validname (a$)` — stub (returns 1); defined in qb64pe.bas.
   - `FUNCTION tryRemoveSymbol$ (varname$)` — stub (returns ""); defined in utilities/type.bas. Note: real function may modify varname$ byref; stub does not (compile-only use).
   - `FUNCTION AddQuotes$ (s$)` — stub (CHR$(34) + s$ + CHR$(34)); defined in utilities/strings.bas.

2. **`tests/qb64pe_incremental/03_builtin_functions.bas`**
   - Include `sections/phase3_stubs.bas` after `ids_init.bas` and before `clearid_sub.bas`.

3. **Docs**
   - `docs/archive/QB64PE_INCREMENTAL_TESTING.md`: Phase 3 section and table updated to PASSES; infrastructure described.
   - `tests/qb64pe_incremental/QUICK_START.md`, `README.md`, `INCREMENTAL_TESTING.md`, `QUICK_REFERENCE.md`: Phase 3 status set to PASSES.
   - `docs/archive/TESTING-COMPLETED.md`: Phase 3 note updated (no longer BLOCKED).

## Result

- `cargo run --bin qb64fresh -- tests/qb64pe_incremental/03_builtin_functions.bas --emit-c -o /tmp/03_out.c` exits 0.
- Generated C: ~14,640 lines.

## Rationale

The built-in functions module and regid use symbols from qb64pe.bas and utilities we do not include in Phase 3. Providing stubs for those symbols (validname, tryRemoveSymbol$, AddQuotes$, subfunc, subfuncn) lets the compiler resolve names and type-check; stub bodies are sufficient for emission. No changes to the main QB64Fresh compiler were required.

---

## Review (bugs and errors)

**Reviewed:** Session 144 deliverables, phase3_stubs.bas, 03_builtin_functions.bas, regid_sub.bas, and docs.

- **Stubs:** phase3_stubs.bas correctly declares `subfunc`, `subfuncn`, `validname`, `tryRemoveSymbol$`, `AddQuotes$`. regid uses both `subfunc` and `subfuncn`; without `DIM SHARED subfuncn`, the codegen would emit `subfuncn` as a local in each procedure (wrong semantics). With it, generated C has one file-scope `subfuncn` used by regid and const_eval — correct.
- **Include order:** 03_builtin_functions.bas order (core → utility headers → implementations → idstruct/ids → phase3_stubs → clearid/regid → subs_functions) is correct.
- **Docs:** QB64PE_INCREMENTAL_TESTING.md, QUICK_REFERENCE.md, INCREMENTAL_TESTING.md, README.md, TESTING-COMPLETED.md consistently describe Phase 3 as PASSES with stub infrastructure; stub list in docs updated to include subfuncn where applicable.
- **No bugs found** in the Phase 3 test setup; stubs match regid/subs_functions requirements.
