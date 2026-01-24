# Session 048: Parser Bootstrap Fixes

**Date:** 2026-01-24
**Focus:** Fixing parser regressions blocking QB64pe bootstrap

## Summary

Fixed three parser bugs that were preventing QB64pe source code from parsing successfully. After these fixes, the bootstrap test parses all 2172 statements from QB64pe.

## Parser Bugs Fixed

### 1. SUB Call with Parenthesized Arguments (34→7 errors)

**Problem:** SUB calls with parenthesized arguments like `SubName (arg1), (arg2)` were failing because the parser saw `(` and tried to parse it as a function-call-style invocation.

**Root Cause:** In QB64, parentheses around arguments force BYVAL passing. The parser had special handling for `name(args)` style that conflicted with `name (arg), (arg)` style.

**Fix:** Removed the LeftParen branch for SUB calls in `control_etc.rs`. Now all SUB arguments are parsed uniformly as expressions (which can include parenthesized expressions).

**File:** `src/parser/statements/control_etc.rs`

### 2. STATIC AS Type-First Syntax (7→3 errors)

**Problem:** `STATIC AS type var1, var2` syntax was not recognized.

**Root Cause:** DIM already supported this QB64 alternate syntax, but STATIC didn't have the same handling.

**Fix:** Added type-first syntax support to STATIC parsing, mirroring the DIM implementation.

**File:** `src/parser/statements/data_dims.rs`

### 3. Function/Array in Comparison Expression (3→0 errors)

**Problem:** Expressions like `x = arr(1) = 5` or `x = ASC("A") = 65` were failing with "expected (, found Equals".

**Root Cause:** The `is_array_assignment()` function was too greedy. For `x = arr(1) = 5`, it would scan forward, find `arr(1)` followed by `=`, and incorrectly conclude this was an array assignment statement rather than a comparison expression.

**Fix:** Added an early check in `is_array_assignment()` to verify that the token immediately after the identifier is `(`. If not, return false immediately. This prevents matching patterns like `x = arr(1) = 5` where the `(` appears later in the expression.

**File:** `src/parser/statements/assignments.rs`

## Key Insight

The third bug demonstrates an important parser design principle: lookahead functions need to be strict about positional matching. The original function used greedy scanning (`find pattern anywhere ahead`) when it should have used precise matching (`verify pattern at exact position`).

## Test Results

- **Library tests:** 390 passed
- **QB45 compatibility:** 5 passed
- **Bootstrap parse test:** Now passes with 2172 statements

## Remaining Work

The bootstrap test now has 90 semantic errors (not parser errors):
- `_PALETTECOLOR` function not implemented
- `qberror_test` procedure undefined
- Type mismatches in `_IIF` expressions
- String used as array index

These are semantic analyzer issues to address in future sessions.
