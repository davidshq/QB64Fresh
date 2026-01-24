# Session 049: Bootstrap Semantic Error Fixes

**Date:** 2026-01-24
**Focus:** Fixing all 24 semantic errors preventing QB64PE bootstrap compilation

## Summary

Fixed all remaining semantic errors that were blocking QB64PE source compilation. The bootstrap test now passes successfully, compiling the full 2.64 MB QB64PE codebase.

## Errors Fixed (24 → 0)

### 1. Missing `_CHR_*` String Constants (16 errors fixed)

**Problem:** `_CHR_CR`, `_CHR_QUOTE`, `_CHR_LF`, `_CHR_SUB` were used as string constants but only registered in the C runtime as macros, not in the semantic analyzer.

**Solution:** Added `register_string_character_constants()` to `src/semantic/builtins.rs` that registers ~60 `_CHR_*` constants as STRING type values (not INTEGER ASCII codes like `_CR`, `_LF`).

**Files modified:**
- `src/semantic/builtins.rs`

### 2. Missing `_STR_*` String Constants (4 errors fixed)

**Problem:** `_STR_EMPTY`, `_STR_CRLF`, `_STR_LF`, `_STR_CR` were used but not registered.

**Solution:** Added these to the new `register_string_character_constants()` function.

**Files modified:**
- `src/semantic/builtins.rs`

### 3. `_SHELLHIDE` Not Registered as Function (3 errors fixed)

**Problem:** `_SHELLHIDE(command$)` was only registered as a statement, but QB64PE uses it as a function that returns the exit code (LONG).

**Solution:** Registered `_SHELLHIDE` as a built-in function returning `BasicType::Long`.

**Files modified:**
- `src/semantic/builtins.rs`

### 4. `_NEWHANDLER` Parsing Issue (3 errors fixed)

**Problem:** `ON ERROR GOTO _NEWHANDLER qberror_test` was being parsed as two statements:
1. `ON ERROR GOTO _NEWHANDLER`
2. `CALL qberror_test` (undefined procedure)

**Solution:** Modified `parse_label_target()` to recognize `_NEWHANDLER` as a modifier keyword and combine it with the following label name into `"_NEWHANDLER qberror_test"`.

**Files modified:**
- `src/parser/statements/control_etc.rs`
- `src/codegen/c_backend/stmt/error_jump.rs`

## Technical Details

### String Character Constants

QB64 has two sets of character-related constants:
- `_CR`, `_LF`, etc. - INTEGER ASCII codes (e.g., `_CR = 13`)
- `_CHR_CR`, `_CHR_LF`, etc. - STRING values (e.g., `_CHR_CR = "\r"`)

The semantic analyzer now registers both, with proper types.

### Error Handler Modifier Syntax

QB64's `_NEWHANDLER` is a modifier that pushes a new error handler scope:
```basic
ON ERROR GOTO _NEWHANDLER myHandler  ' Push new handler
' ... code ...
ON ERROR GOTO _LASTHANDLER           ' Pop to previous
```

The target string is now stored as `"_NEWHANDLER myHandler"` and parsed in codegen.

## Test Results

- **Library tests:** 390 passed
- **Bootstrap test:** PASSING (was failing with 24 errors)
- **QB45 compatibility:** 5 passed
- **Integration tests:** 651 passed (67 pre-existing failures unrelated to this work)

### Bootstrap Compilation Stats

```
Source size:     2.64 MB (2,636,233 bytes)
Tokens:          400,523
Statements:      2,172
C output:        93,761 lines (4.93 MB)
Total time:      ~1 second
```

## Key Insight

Compiler front-ends need complete knowledge of all built-ins at the semantic analysis phase, not just at code generation. Even though the C runtime had macros for `_CHR_CR` etc., the semantic analyzer needed to know their types to properly type-check expressions.
