# Session 098: REDIM compile/link fixes (28_redim, 120_redim_no_preserve)

**Date:** 2026-01-30

## Goal

Fix the two QB64Fresh runtime-comparison failures from MORNING_REVIEW.md:
1. **28_redim** — FAIL_COMPILE_FRESH (REDIM PRESERVE with `1 TO 5` dimensions)
2. **120_redim_no_preserve** — FAIL_LINK_FRESH (REDIM without PRESERVE; C type errors)

## Root causes

### 28_redim

- **Symptom:** Parse error "expected ), found To" at line 9: `REDIM PRESERVE a(1 TO 5) AS LONG`.
- **Cause:** Lexer only recognizes `_PRESERVE` (with underscore). The test uses `PRESERVE` (no underscore), so the parser treated "PRESERVE" as the first variable name and then tried to parse `a(1 TO 5)` as a separate expression (function call), where `TO` is invalid after `1`.
- **Fix:** In `parse_redim` (data_dims.rs), accept identifier "PRESERVE" (case-insensitive) as well as `TokenKind::Preserve` (_PRESERVE), and consume it so the next token is the array name.

### 120_redim_no_preserve

- **Symptom:** C compile errors: `a` used as `int32_t` with `realloc`/`memset`/`qb_array_register` (pointer expected); `a_sz__` undeclared.
- **Cause:** `DIM a() AS LONG` was treated as a scalar (dimensions empty ⇒ scalar). So we emitted `int32_t a = 0` instead of `int32_t* a = NULL` and the size-tracking variable.
- **Fix:** Distinguish "no parentheses" (scalar) from "empty parentheses" (dynamic array). Added `is_dynamic_array: bool` to `DimVariable` (AST) and `TypedDimVariable` (typed IR). Parser sets it when we see `(` and `parse_array_dimensions` returns empty. Global (analysis.rs) and local (implicit_vars.rs) declaration logic: when `dimensions.is_empty() && is_dynamic_array`, declare via `declare_array_var(..., &[])` (pointer + size var).

## Changes

- **Parser (data_dims.rs):** REDIM accepts `PRESERVE` (identifier) as well as `_PRESERVE`; all DIM/REDIM/STATIC variable parsing records `(dimensions, is_dynamic_array)` with `is_dynamic_array = had_parens && dimensions.is_empty()`.
- **AST (ast/stmt.rs):** `DimVariable` gains `is_dynamic_array: bool`.
- **Typed IR (typed_ir.rs):** `TypedDimVariable` gains `is_dynamic_array: bool`.
- **Semantic (checker):** misc.rs and definitions.rs set `is_dynamic_array` from AST when building TypedDimVariable; all test/helper DimVariable constructions get `is_dynamic_array: false`.
- **Codegen:** implicit_vars.rs `collect_dims`: for DIM, if `dimensions.is_empty() && is_dynamic_array` then call `declare_array_var` (dynamic array); analysis.rs global DIM: same branch for module-level `DIM a() AS type`.

## Verification

- `28_redim.bas` and `120_redim_no_preserve.bas` compile with `qb64fresh --emit-c --runtime inline`, C builds with `gcc -lm`, and run with expected output.
- DIFFERENCES.md updated: no remaining compile/link failures; 28 and 120 listed under Resolved.
- Parser tests (200) pass.
