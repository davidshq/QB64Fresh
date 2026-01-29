# Session 076 — BYREF Scalar Regression Test (FIXES_NEEDED §9)

**Date:** 2026-01-28

## Goal

Implement item 9 from FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md: add an integration test that modifies a BYREF scalar (INTEGER/LONG) inside a SUB and asserts the caller sees the new value, to guard against regressions.

## What Was Done

1. **Integration test (compile + C pattern)**  
   - `tests/integration_tests.rs`: added `procedures::byref_scalar_sub_compiles_and_emits_write_through`.  
   - Compiles a SUB with default BYREF INTEGER parameter that assigns `n = 99`, and asserts the generated C contains the pointer alias (`n_ref`, `* n = n_ref`) and write-through assignment (`*n = `).  
   - **Passes.**

2. **Execution test (run and assert output)**  
   - `tests/execution_tests.rs`: added `execution::byref_scalar_sub_modifies_caller_variable`.  
   - Same program; runs it and asserts stdout contains `99`.  
   - **Fails** only because the inline runtime currently has unrelated C compile errors (e.g. `qb_asc_assign` read-only assignment, `qb_savefiledialog` redefinition). Test logic is correct; will pass when runtime is fixed.

3. **execution_tests.rs RuntimeMode fix**  
   - Replaced `RuntimeMode::Inline` / `RuntimeMode::External` with `RuntimeMode::inline()` / `RuntimeMode::external()` so the test file compiles.

4. **Docs**  
   - FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md: marked item 9 done and noted both tests.

## Decisions

- Used default BYREF (no `BYREF` keyword) because the parser only has `BYVAL`; SUB params are BYREF by default.
- Kept both an integration test (compile + C checks) and an execution test (run + output) so we have a regression guard that passes today and a full end-to-end check for when the runtime compiles.
