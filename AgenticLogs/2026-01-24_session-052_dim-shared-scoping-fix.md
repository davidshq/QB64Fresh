# Session 052: DIM SHARED Variable Scoping Fix

**Date:** 2026-01-24
**Focus:** Fixing DIM SHARED variable accessibility in generated C code

## Summary

Fixed a critical bug where `DIM SHARED` variables at module level were being shadowed by implicit local declarations inside SUB/FUNCTION definitions. This was causing the QB64PE bootstrap to malfunction because command-line flags like `NoIDEMode`, `ConsoleMode`, etc. were not being properly shared between functions.

## Problem

In QB64 BASIC, `DIM SHARED` at module level makes variables globally accessible from all SUBs and FUNCTIONs without needing a local `SHARED` statement:

```basic
DIM SHARED AS _BYTE NoIDEMode, ConsoleMode, FormatMode
```

Our code generator was treating all variable assignments in functions as implicit local declarations:

```c
qb_string* qb_parsecmdlineargs_str(void) {
    int8_t NoIDEMode = 0;  // WRONG: shadows the global
    // ...
    NoIDEMode = 1;  // Sets local, not global!
}
```

This caused the command-line parser to set local variables, while the main program checked uninitialized globals.

## Solution

Added `shared_globals` parameter to `collect_implicit_locals()` that tracks all variables declared with `DIM SHARED`. These variables are excluded from implicit local declaration.

### Files Modified

1. **`src/codegen/c_backend/implicit_vars.rs`**
   - Added `shared_globals: &HashSet<String>` parameter
   - Include shared globals in the `dim_declared` set so they're not re-declared

2. **`src/codegen/c_backend/mod.rs`**
   - Collect DIM SHARED variable names into `shared_global_names`
   - Pass to emitter and `collect_implicit_locals` calls
   - Added import for `c_identifier`

3. **`src/codegen/c_backend/stmt/definitions.rs`**
   - Updated SUB and FUNCTION definition calls to pass `shared_global_names`

4. **`src/codegen/c_backend/stmt/mod.rs`**
   - Already had `shared_global_names` field on StmtEmitter (from previous session)

## Results

- All 390 unit tests pass
- QB64PE bootstrap compiles successfully (99K lines of C)
- Bootstrap runs and displays help text correctly
- INI file loading no longer hangs (the root cause was DIM SHARED shadowing)
- Command-line parsing now works correctly

## Before/After

**Before:**
```c
// In qb_parsecmdlineargs_str function
int8_t NoIDEMode = 0;  // Local shadows global at line 4252
```

**After:**
```c
// No local NoIDEMode - function uses global correctly
```

## Next Steps

- Task #2: Fix BINARY vs RANDOM mode file positioning (remaining file I/O issue)
