# Session 107 — rounding.h Type Conversion and Rounding

**Date:** 2026-01-31

## Summary

Implemented libqb **rounding.h** equivalents in QB64Fresh: `qbr`, CSNG (`func_csng_*`), CDBL (`func_cdbl_float`), and _ROUND (`func_round_*`).

## Changes

### Runtime (`runtime/src/math.rs`)

- **qb_qbr(n: f64) -> i64** — Round to int64; clamp NaN/Inf and values outside i64 range.
- **qb_csng_float(n: f64) -> f32** — CSNG from float; set error 6 (overflow) if \|n\| > single max.
- **qb_csng_double(n: f64) -> f32** — CSNG from double; same overflow check.
- **qb_cdbl_float(n: f64) -> f64** — CDBL from float (identity in C ABI).
- **qb_round_double(n: f64) -> i64**, **qb_round_float(n: f64) -> i64** — _ROUND via qb_qbr.

C ABI uses `double` for all numeric args; Rust signatures match the header.

### Header (`runtime/include/qb64fresh_rt.h`)

Declared the six functions in the math section.

### Codegen

- **Inline C** (`src/codegen/c_backend/runtime/math.rs`): Emitted `qb_qbr`, `qb_csng_float`, `qb_csng_double`, `qb_cdbl_float`, `qb_round_double`, `qb_round_float` with overflow checks calling `qb_set_error(6, 0)` for CSNG.
- **Expr** (`src/codegen/c_backend/expr.rs`):
  - **CSNG**: Special-case; emit `qb_csng_float(arg)` or `qb_csng_double(arg)` from arg type.
  - **CDBL**: Special-case; emit `qb_cdbl_float(arg)` for Single, `(double)(arg)` for Double.
  - **_ROUND**: Map to `qb_round_double` (was `round`; _ROUND returns LONG).
- **Const fold** (`src/codegen/c_backend/const_fold.rs`): _ROUND float arg now folds to `FoldedValue::Integer(v.round() as i64)`.

### Tests

- Updated `round_function_constant_folded` to expect `4LL` (integer) instead of `4.0`.
- Updated `round_function_with_variable` to expect `qb_round_double(` instead of `round(`.

## Doc

- **docs/QB64pe/LIBQB_FUNCTIONALITY.md** — Section 26 (rounding.h) marked 🟢 for all four rows.
