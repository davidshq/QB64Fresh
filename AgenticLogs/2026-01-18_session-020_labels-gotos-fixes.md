# Session 020: Labels/GOTOs Bug Fixes

**Date:** 2026-01-18
**Focus:** Verifying and fixing labels/gotos functionality

## Summary

Continued from interrupted session to verify labels and GOTOs work correctly. Found and fixed multiple bugs in the code generation.

## Issues Found & Fixed

### 1. Missing `refcount` field in `qb_string` struct
**File:** `src/codegen/c_backend/runtime.rs`

The `qb_string` struct was defined without a `refcount` field, but several functions (`qb_trim`, `qb_mki`, `qb_mkl`, `qb_mks`, `qb_mkd`) referenced `result->refcount = 1`.

**Fix:** Added `int refcount;` field to the struct definition at line 100.

### 2. Duplicate `_qb_argc/_qb_argv` definitions
**File:** `src/codegen/c_backend/runtime.rs`

These global variables were defined twice:
- Lines 1806-1807 (for COMMAND$)
- Lines 2777-2778 (for _COMMANDCOUNT)

**Fix:** Removed duplicate definitions at lines 2777-2778, keeping only the first set which is used by `qb_command()`.

### 3. ON...GOTO/ON...GOSUB label prefix mismatch
**File:** `src/codegen/c_backend/stmt.rs`

The code generators used inconsistent label prefixes:
- Regular GOTO: `goto label;`
- Regular Label: `label:`
- ON...GOTO: `goto _label_{}; ` (incorrectly used `_label_` prefix)
- ON...GOSUB: `goto _label_{};` (same issue)
- ON ERROR GOTO: `&&_label_{};` (same issue)
- RESUME: `goto _label_{};` (same issue)

**Fix:** Removed the `_label_` prefix from all four locations:
- Line 2322: ON...GOTO
- Line 2349: ON...GOSUB
- Line 2245: ON ERROR GOTO
- Line 2287: RESUME

## Verification

Created and ran test programs:

**test_goto.bas** - Basic GOTO/labels:
```basic
PRINT "Starting test"
GOTO skip_this
PRINT "This should NOT print"
skip_this:
PRINT "Jumped to skip_this!"
GOTO done
PRINT "This also should NOT print"
done:
PRINT "Done!"
```
Output: Works correctly - skips the right statements.

**test_on_goto.bas** - ON...GOTO:
```basic
FOR choice = 0 TO 4
    ON choice GOTO one, two, three
    ' ... (tests 1-based indexing)
NEXT choice
```
Output: Correctly branches to labels 1-3, falls through for 0 and 4+.

### 4. GOSUB/RETURN Implementation
**Files:** `src/codegen/c_backend/runtime.rs`, `src/codegen/c_backend/stmt.rs`

GOSUB was incorrectly implemented as a function call. In BASIC, GOSUB is a "jump with return address" mechanism, not a function call.

**Previous (broken) code:**
```c
// GOSUB label - treated as function call (WRONG!)
label();
// RETURN
return;
```

**Fix - Added GOSUB return stack:**
1. Added `_gosub_stack` and `_gosub_sp` to runtime (256-entry stack for nested GOSUBs)
2. GOSUB now pushes return address and jumps:
   ```c
   _gosub_stack[_gosub_sp++] = &&_qb_gosub_ret_N;
   goto label;
   _qb_gosub_ret_N:;
   ```
3. RETURN now pops from stack:
   ```c
   if (_gosub_sp > 0) goto *_gosub_stack[--_gosub_sp];
   ```

Uses GCC's computed goto extension (`&&label` gives label address, `goto *ptr` jumps to address).

## Verification

**test_gosub.bas** - Basic GOSUB/RETURN with nesting:
```basic
PRINT "Main program start"
GOSUB greet
PRINT "After first GOSUB"
GOSUB nested_test
PRINT "Main program end"
END

greet:
    PRINT "  Hello from greet!"
    RETURN

nested_test:
    PRINT "  Nested test - calling greet"
    GOSUB greet
    PRINT "  Back in nested_test"
    RETURN
```
Output: All subroutines execute correctly with proper nesting and returns.

**test_on_gosub.bas** - ON...GOSUB:
```basic
FOR choice = 0 TO 4
    ON choice GOSUB one, two, three
    PRINT "  Returned from ON GOSUB"
NEXT choice
```
Output: Correctly calls subroutines 1-3, falls through for out-of-range values.

### 5. Line Number Support (Classic BASIC)

Added support for numeric line numbers like in QB45/GW-BASIC:

```basic
10 PRINT "Line 10"
20 GOTO 50
50 PRINT "Jumped here"
```

**Implementation:**
- Line numbers at start of statements become labels (`_line_10:`, `_line_50:`)
- GOTO/GOSUB accept both identifiers and integers
- ON...GOTO/GOSUB accepts mixed lists (`ON x GOTO 100, myLabel, 300`)
- ON ERROR GOTO handles line numbers (special case: `0` disables handler)

**Files modified:**
- `src/parser/statements.rs`:
  - Added `parse_line_number_statement()` for `100 PRINT` syntax
  - Added `parse_label_target()` helper for identifier-or-integer
  - Updated `parse_goto()`, `parse_gosub()`, `parse_label_list()`, `parse_on_error()`

## Known Limitations (Documented in TODO.md)

1. **Computed goto is GCC extension** - GOSUB uses `&&label` and `goto *ptr` which work with GCC/Clang but NOT MSVC. For MSVC support, would need switch-based dispatch. Low priority since most users use GCC/MinGW.

## Files Modified

- `src/codegen/c_backend/runtime.rs` - struct fixes, GOSUB stack
- `src/codegen/c_backend/stmt.rs` - label prefix fixes, GOSUB/RETURN implementation
- `src/parser/statements.rs` - line number support
- `TODO.md` - documented GCC limitation, marked line numbers as complete
