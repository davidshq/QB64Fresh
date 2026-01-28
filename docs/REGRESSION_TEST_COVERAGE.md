# Regression Test Coverage Analysis

**Last Updated:** 2026-01-28  
**Purpose:** Document all major bugs fixed throughout the project's history and ensure regression tests prevent reintroduction.

## Overview

This document catalogs major problems encountered and fixed in QB64Fresh, analyzes existing test coverage, and identifies gaps where regression tests are needed. Focus is on recent history (2026-01-20+) as complexity increased significantly during bootstrap development.

**Statistics:**
- 44+ fix commits since 2026-01-20
- 6 major problem categories identified
- Multiple critical memory management issues resolved
- Several test gaps identified requiring attention

---

## Problem Catalog

### 1. Memory Management Issues (Critical Priority)

#### 1.1 String Temp Pool Memory Exhaustion
- **Commit:** `5c4d469` (2026-01-25)
- **Problem:** QB64pe bootstrap compilation consumed 39.8GB memory, causing system crashes
- **Root Cause:** String temp pool overflowed without proper tracking when main pool was full
- **Fix Applied:**
  - Added overflow tracking for string temp pool
  - Changed `qbs_tmp_base_get()` to return `uint64_t` with packed bases
  - Updated `qbs_cleanup()` to clean up both main pool and overflow strings
  - Added string writeback for BYREF parameters in SUB/FUNCTION
  - Fixed REDIM _PRESERVE size tracking variable scope
- **Impact:** Memory usage reduced from 39.8GB → 94MB
- **Test Status:** ✅ **Covered** - Regression test verifies overflow tracking mechanism exists in generated code
- **Test Location:** `tests/bootstrap_tests.rs:regression_tests::string_temp_pool_overflow_tracking`

#### 1.2 Reference-Counted String Memory Leaks
- **Commit:** `4dd4e77` (2026-01-25)
- **Problem:** 42+ GB memory explosion and segfaults during QB64pe bootstrap compilation
- **Root Cause:** Missing temp string pool with scoped cleanup, no retain/release pattern for string assignments
- **Fix Applied:**
  - Added temp string pool with `qbs_tmp_register`/`qbs_cleanup` functions
  - Implemented scoped cleanup in FOR/WHILE/DO loops (save/restore base)
  - Added per-statement cleanup in main program and procedure bodies
  - Used retain/release pattern for all string assignments (simple, array, UDT)
  - Removed explicit `qb_string_free` from fixed-length string assignments
  - Used `calloc` instead of `malloc` for string arrays (NULL initialization)
  - Added static empty string optimization (`_qbs_empty`)
- **Impact:** Memory properly bounded (tested with 100K iterations using ~2 MB)
- **Test Status:** ⚠️ **Partial** - Bootstrap test indirectly covers, but no explicit leak detection test
- **Test Location:** N/A (gap identified)

#### 1.3 String Double-Wrapping
- **Commit:** `115ae41` (2026-01-27)
- **Problem:** 807 C compilation errors due to double-wrapping `qb_str_from_c()` calls
- **Root Cause:** BYREF parameters, BYVAL parameters, and built-in function arguments were wrapping already-wrapped strings
- **Fix Applied:**
  - Added `unwrap_qb_str_from_c()` helper to extract inner expressions from `qb_str_from_c()` wrappers
  - Fixed double-wrapping in BYREF parameters, BYVAL parameters, and built-in function arguments
  - Fixed MID$ assignment to unwrap fixed-length strings for `strlen`/`strncpy`
  - Fixed `qb_gfx_printstring` and `qb_shell` to convert `qb_string*` to `const char*` properly
- **Impact:** Reduced C compilation errors from 807 → 264 (67% reduction)
- **Test Status:** ❌ **Gap** - No explicit tests for double-wrapping patterns
- **Test Location:** N/A (gap identified)

#### 1.4 Missing String Cleanup in Loops
- **Related to:** Commit `4dd4e77` (2026-01-25)
- **Problem:** String temp pool not cleaned up in FOR/WHILE/DO loops
- **Root Cause:** No scoped cleanup mechanism for loop bodies
- **Fix Applied:** Implemented save/restore base pattern for loop scopes
- **Test Status:** ⚠️ **Partial** - Covered by bootstrap test, but no explicit loop cleanup test
- **Test Location:** N/A (gap identified)

---

### 2. Type System & Codegen Bugs (High Priority)

#### 2.1 QbString vs qb_string Type Mismatch
- **Commit:** `3fe464b` (2026-01-27)
- **Problem:** 807 C compilation errors due to type name inconsistency (`QbString*` vs `qb_string*`)
- **Root Cause:** Runtime header uses `QbString*` but codegen emitted `qb_string*`
- **Fix Applied:** Changed `qb_string*` → `QbString*` throughout codegen to match runtime header
- **Impact:** Reduced C compilation errors from 807 → 69 (91% reduction)
- **Test Status:** ✅ **Covered** - Bootstrap test catches type mismatches
- **Test Location:** `tests/bootstrap_tests.rs:qb64pe_compiles_successfully`

#### 2.2 Array Variable Rename Bugs
- **Commit:** `3fe464b` (2026-01-27)
- **Problem:** Array access variables not using renamed names, causing variable shadowing
- **Root Cause:** `emit_array_access` and `emit_array_field_assignment` didn't apply `variable_renames`
- **Fix Applied:** Fixed array access variable rename bug by applying `variable_renames` in both functions
- **Impact:** Fixed variable shadowing in array access expressions
- **Test Status:** ❌ **Gap** - No explicit test for array variable rename scenarios
- **Test Location:** N/A (gap identified)

#### 2.3 Function Signature Mismatches
- **Commit:** `a436271` (2026-01-28)
- **Problem:** Function signature mismatches between semantic analyzer and codegen
- **Root Cause:** Type registry inconsistencies
- **Fix Applied:** Implemented TypeRegistry and standardized error handling
- **Test Status:** ✅ **Covered** - Bootstrap test catches signature mismatches
- **Test Location:** `tests/bootstrap_tests.rs:qb64pe_compiles_successfully`

#### 2.4 SELECT CASE String Comparisons
- **Commits:** `18560d1` (2026-01-23), `11389a2` (2026-01-27)
- **Problem:** SELECT CASE string comparisons not working correctly, especially for fixed-length strings
- **Root Cause:** Fixed-length strings not properly wrapped when comparing with `qb_string_compare()`
- **Fix Applied:**
  - SELECT CASE now correctly handles string comparisons using `qb_string_compare` for equality, ranges, and relational operators
  - Pass test expression type through to case match emission
  - Update runtime API usage to use `qb_string_data()` and `qb_string_len()` accessors
- **Impact:** Fixed string comparison logic in SELECT CASE statements
- **Test Status:** ⚠️ **Partial** - SELECT CASE tests exist (`tests/integration_tests.rs:580-626`) but only for numeric types
- **Test Location:** `tests/integration_tests.rs:580-626` (numeric only, gap for strings)

#### 2.5 qbt_ParseNum* to qb_string* Type Compatibility
- **Commit:** `1183a3a` (2026-01-27)
- **Problem:** Type incompatibility between `qbt_ParseNum*` and `qb_string*` types
- **Root Cause:** Runtime API type mismatch
- **Fix Applied:** Fixed type compatibility in runtime API usage
- **Test Status:** ✅ **Covered** - Bootstrap test catches type errors
- **Test Location:** `tests/bootstrap_tests.rs:qb64pe_compiles_successfully`

---

### 3. Parser Bugs (Medium Priority)

#### 3.1 SUB Calls with Parenthesized Arguments
- **Session:** 048 (2026-01-24)
- **Problem:** SUB calls with parenthesized arguments like `SubName (arg1), (arg2)` failed parsing
- **Root Cause:** Parser saw `(` and tried to parse as function-call-style invocation
- **Fix Applied:** Removed LeftParen branch for SUB calls, now all SUB arguments parsed uniformly as expressions
- **Impact:** Reduced errors from 34 → 7
- **Test Status:** ✅ **Covered** - QB45 compatibility tests and bootstrap parse test
- **Test Location:** `tests/qb45_compat.rs`, `tests/bootstrap_tests.rs:qb64pe_parses_successfully`

#### 3.2 STATIC AS Type-First Syntax
- **Session:** 048 (2026-01-24)
- **Problem:** `STATIC AS type var1, var2` syntax not recognized
- **Root Cause:** DIM supported this QB64 alternate syntax, but STATIC didn't
- **Fix Applied:** Added type-first syntax support to STATIC parsing, mirroring DIM implementation
- **Impact:** Reduced errors from 7 → 3
- **Test Status:** ✅ **Covered** - QB45 compatibility tests
- **Test Location:** `tests/qb45_compat.rs`

#### 3.3 Function/Array in Comparison Expression
- **Session:** 048 (2026-01-24)
- **Problem:** Expressions like `x = arr(1) = 5` or `x = ASC("A") = 65` failed with "expected (, found Equals"
- **Root Cause:** `is_array_assignment()` was too greedy, matching patterns where `(` appears later in expression
- **Fix Applied:** Added early check to verify token immediately after identifier is `(`
- **Impact:** Reduced errors from 3 → 0
- **Test Status:** ✅ **Covered** - Bootstrap parse test
- **Test Location:** `tests/bootstrap_tests.rs:qb64pe_parses_successfully`

#### 3.4 Single-Line IF-THEN-ELSE with Colons
- **Commit:** `9256d52` (2026-01-20)
- **Problem:** Single-line IF-THEN-ELSE with colon-separated statements not parsing correctly
- **Root Cause:** Parser handling of ELSE terminator and colon separators
- **Fix Applied:** Improved parser handling of ELSE terminator and colon separators
- **Test Status:** ✅ **Covered** - QB45 compatibility tests
- **Test Location:** `tests/qb45_compat.rs`

#### 3.5 DATA Statement Parsing
- **Commits:** `f605f17`, `c894567` (2026-01-20)
- **Problem:** DATA statement parsing issues for QB4.5 compatibility
- **Root Cause:** DATA statement not accepting operator tokens as values
- **Fix Applied:** DATA statement parsing improvements, now accepts operator tokens as values
- **Test Status:** ✅ **Covered** - QB45 compatibility tests
- **Test Location:** `tests/qb45_compat.rs`

#### 3.6 Empty Array Dimension Syntax
- **Commit:** `1c2fdad` (2026-01-20)
- **Problem:** Empty array dimension syntax in DIM/STATIC not supported
- **Root Cause:** Parser didn't handle empty dimensions
- **Fix Applied:** Added support for empty array dimension syntax
- **Test Status:** ✅ **Covered** - QB45 compatibility tests
- **Test Location:** `tests/qb45_compat.rs`

---

### 4. Runtime & FFI Issues (High Priority)

#### 4.1 Missing qb_dir() Declaration
- **Commit:** `988e3e5` (2026-01-26)
- **Problem:** SIGSEGV crash caused by implicit `int` return type assumption (64-bit pointer truncated to 32-bit)
- **Root Cause:** `qb_dir()` function not declared in `runtime/include/qb64fresh_rt.h`
- **Fix Applied:**
  - Added `qb_dir()` function declaration to runtime header
  - Added error checking for runtime initialization and shutdown
  - Improved codegen error handling and comments
- **Impact:** Fixed pointer truncation crash
- **Test Status:** ❌ **Gap** - No explicit test for FFI function declaration completeness
- **Test Location:** N/A (gap identified)
- **Related:** Session 065 documented GDB debugging session

#### 4.2 MID$ Assignment for Fixed-Length Strings
- **Commit:** `219a5ae` (2026-01-26)
- **Problem:** Stack corruption from incompatible pointer types in MID$ assignment
- **Root Cause:** MID$ assignment didn't handle fixed-length string arrays correctly, used wrong function signature
- **Fix Applied:**
  - Detect FixedString type and use manual character copying instead of `qb_mid_assign`
  - Prevents stack corruption from incompatible pointer types
- **Impact:** Fixed segmentation faults in MID$ operations
- **Test Status:** ⚠️ **Partial** - MID$ tests exist (`tests/integration_tests.rs:5073-5109`) but only for dynamic strings
- **Test Location:** `tests/integration_tests.rs:5073-5109` (dynamic strings only, gap for fixed-length)

#### 4.3 Runtime Initialization Order
- **Commit:** `219a5ae` (2026-01-26)
- **Problem:** Runtime not properly initialized before use
- **Root Cause:** Missing `qb_runtime_init()` and `qb_runtime_shutdown()` calls for external runtime mode
- **Fix Applied:** Added runtime initialization and shutdown calls
- **Test Status:** ❌ **Gap** - No explicit test for runtime initialization order
- **Test Location:** N/A (gap identified)

#### 4.4 FFI Error Reporting
- **Session:** 067 (2026-01-27)
- **Problem:** FFI functions returned simple `0`/`1` codes, losing detailed error information
- **Root Cause:** Errors silently ignored in FFI layer
- **Fix Applied:**
  - Created `log_ffi_error!` macro for consistent error logging
  - Updated all 36+ instances of `Err(_)` patterns in `graphics_ffi.rs`
  - All errors now logged to stderr with context
- **Impact:** Error visibility greatly improved for debugging
- **Test Status:** ❌ **Gap** - No explicit test for FFI error reporting
- **Test Location:** N/A (gap identified)

---

### 5. Semantic Analysis Bugs (Medium Priority)

#### 5.1 Missing _CHR_* and _STR_* Constants
- **Session:** 049 (2026-01-24)
- **Problem:** `_CHR_CR`, `_CHR_QUOTE`, `_CHR_LF`, `_CHR_SUB`, `_STR_EMPTY`, `_STR_CRLF`, etc. used but not registered
- **Root Cause:** Only registered in C runtime as macros, not in semantic analyzer
- **Fix Applied:** Added `register_string_character_constants()` to register ~60 `_CHR_*` and `_STR_*` constants as STRING type values
- **Impact:** Fixed 16+4 semantic errors
- **Test Status:** ❌ **Gap** - No explicit test for built-in constant registration
- **Test Location:** N/A (gap identified)

#### 5.2 Missing _SHELLHIDE Function Registration
- **Session:** 049 (2026-01-24)
- **Problem:** `_SHELLHIDE(command$)` only registered as statement, but QB64PE uses it as function returning LONG
- **Root Cause:** Incomplete built-in registration
- **Fix Applied:** Registered `_SHELLHIDE` as built-in function returning `BasicType::Long`
- **Impact:** Fixed 3 semantic errors
- **Test Status:** ✅ **Covered** - Bootstrap test catches semantic errors
- **Test Location:** `tests/bootstrap_tests.rs:qb64pe_compiles_successfully`

#### 5.3 ON ERROR GOTO _NEWHANDLER Parsing
- **Session:** 049 (2026-01-24)
- **Problem:** `ON ERROR GOTO _NEWHANDLER qberror_test` parsed as two statements instead of one
- **Root Cause:** `_NEWHANDLER` not recognized as modifier keyword
- **Fix Applied:** Modified `parse_label_target()` to recognize `_NEWHANDLER` as modifier and combine with following label name
- **Impact:** Fixed 3 semantic errors
- **Test Status:** ❌ **Gap** - No explicit test for error handler syntax
- **Test Location:** N/A (gap identified)

#### 5.4 Variable Scoping Issues
- **Commit:** `2cff924` (2026-01-24)
- **Problem:** Local variable scoping issues blocking bootstrap
- **Root Cause:** Incorrect scoping rules for local vs global variables
- **Fix Applied:** Fixed local variable scoping and keyboard/graphics stubs
- **Test Status:** ✅ **Covered** - Bootstrap test
- **Test Location:** `tests/bootstrap_tests.rs:qb64pe_compiles_successfully`

#### 5.5 Variable Name Suffix Mismatch
- **Commit:** `80f0a69` (2026-01-22)
- **Problem:** Variable name suffix mismatch in symbol table lookups
- **Root Cause:** Suffix fallback not implemented for constant lookups
- **Fix Applied:** Added suffix fallback for constant lookups in symbol table
- **Test Status:** ✅ **Covered** - Bootstrap test
- **Test Location:** `tests/bootstrap_tests.rs:qb64pe_compiles_successfully`

---

### 6. Codegen Structure Issues (Medium Priority)

#### 6.1 Duplicate Label Emission
- **Session:** 050 (2026-01-24)
- **Problem:** Labels like `Help_CheckFinishLine:` appeared 5 times due to ambiguous parsing
- **Root Cause:** No tracking of emitted labels
- **Fix Applied:** Added `emitted_labels` HashSet to StmtEmitter to track and skip duplicate labels
- **Test Status:** ❌ **Gap** - No explicit test for label uniqueness
- **Test Location:** N/A (gap identified)

#### 6.2 Missing Forward Declarations
- **Session:** 050 (2026-01-24)
- **Problem:** Cross-module dependencies not forward-declared, causing compilation errors
- **Root Cause:** `qb_gfx_screen` referenced variables defined later in different modules
- **Fix Applied:** Added forward declarations in `emit_forward_declarations()` in mod.rs
- **Test Status:** ❌ **Gap** - No explicit test for forward declaration requirements
- **Test Location:** N/A (gap identified)

#### 6.3 Array Scoping
- **Commit:** `a438673` (2026-01-23)
- **Problem:** Array scoping incorrect - main should use globals, procedures should use locals
- **Root Cause:** Incorrect scoping rules in codegen
- **Fix Applied:** Fixed array scoping so main uses globals, procedures use locals
- **Test Status:** ✅ **Covered** - Bootstrap test
- **Test Location:** `tests/bootstrap_tests.rs:qb64pe_compiles_successfully`

#### 6.4 REDIM SHARED
- **Commit:** `d8c5f3e` (2026-01-23)
- **Problem:** REDIM SHARED not handled correctly
- **Root Cause:** Missing SHARED handling in REDIM codegen
- **Fix Applied:** Fixed REDIM SHARED codegen
- **Test Status:** ✅ **Covered** - Bootstrap test
- **Test Location:** `tests/bootstrap_tests.rs:qb64pe_compiles_successfully`

#### 6.5 Function Call Names
- **Commit:** `503155b` (2026-01-22)
- **Problem:** Function calls not using canonical names with type suffix
- **Root Cause:** Function call name generation didn't apply canonical naming
- **Fix Applied:** Fixed function call names to use canonical names
- **Test Status:** ✅ **Covered** - Bootstrap regression test
- **Test Location:** `tests/bootstrap_tests.rs:505-534` (function_call_uses_canonical_name)

#### 6.6 Duplicate Labels and Static Strings
- **Commit:** `d8f974b` (2026-01-22)
- **Problem:** Duplicate labels and static string issues
- **Root Cause:** Label and string management in codegen
- **Fix Applied:** Fixed duplicate labels and static strings
- **Test Status:** ✅ **Covered** - Bootstrap test
- **Test Location:** `tests/bootstrap_tests.rs:qb64pe_compiles_successfully`

---

## Test Coverage Summary

### Coverage by Category

| Category | Total Issues | ✅ Covered | ⚠️ Partial | ❌ Gap |
|----------|--------------|------------|------------|--------|
| Memory Management | 4 | 0 | 4 | 0 |
| Type System & Codegen | 5 | 3 | 1 | 1 |
| Parser | 6 | 6 | 0 | 0 |
| Runtime & FFI | 4 | 0 | 1 | 3 |
| Semantic Analysis | 5 | 3 | 0 | 2 |
| Codegen Structure | 6 | 5 | 0 | 1 |
| **Total** | **30** | **17** | **6** | **7** |

### Test Status Legend

- ✅ **Covered** - Explicit test exists that would catch regression
- ⚠️ **Partial** - Test exists but doesn't cover all scenarios (e.g., numeric but not string)
- ❌ **Gap** - No explicit test exists, relies on indirect coverage (e.g., bootstrap test)

---

## Test Gap Analysis

### Critical Priority Gaps

These gaps could lead to reintroduction of severe bugs (memory exhaustion, crashes):

1. ~~**String temp pool overflow test** (1.1)~~ ✅ **COMPLETED**
   - **Impact:** Memory exhaustion (39.8GB → 94MB fix)
   - **Test Added:** `tests/bootstrap_tests.rs:regression_tests::string_temp_pool_overflow_tracking`
   - **Status:** Verifies overflow tracking mechanism exists in generated code

2. **String memory leak detection test** (1.2)
   - **Impact:** 42+ GB memory explosion fix
   - **Test Needed:** Test that verifies string cleanup in loops and procedures

3. **String double-wrapping test** (1.3)
   - **Impact:** 807 → 264 C errors (67% reduction)
   - **Test Needed:** Test BYREF/BYVAL parameters and built-in function arguments with strings

4. **SELECT CASE string comparison test** (2.4)
   - **Impact:** String comparisons in SELECT CASE broken
   - **Test Needed:** Test SELECT CASE with string types, especially fixed-length strings

### High Priority Gaps

These gaps could lead to compilation errors or runtime crashes:

5. **MID$ with fixed-length strings test** (4.2)
   - **Impact:** Stack corruption fix
   - **Test Needed:** Test MID$ assignment with fixed-length string arrays

6. **Array variable rename test** (2.2)
   - **Impact:** Variable shadowing fix
   - **Test Needed:** Test array access with renamed variables

7. **Runtime initialization order test** (4.3)
   - **Impact:** Runtime initialization fix
   - **Test Needed:** Test that runtime is initialized before use

8. **FFI declaration completeness check** (4.1)
   - **Impact:** Pointer truncation crash fix
   - **Test Needed:** Automated check that all FFI functions are declared in header

### Medium Priority Gaps

These gaps are less critical but should be addressed:

9. **Parser edge case tests** (3.x)
   - **Status:** Most parser bugs are covered by QB45 compatibility tests
   - **Note:** Could add explicit regression tests for specific edge cases

10. **Built-in constant registration test** (5.1)
    - **Impact:** 16+4 semantic errors fix
    - **Test Needed:** Test that all _CHR_* and _STR_* constants are registered

11. **Error handler syntax test** (5.3)
    - **Impact:** 3 semantic errors fix
    - **Test Needed:** Test ON ERROR GOTO _NEWHANDLER syntax

12. **Label uniqueness test** (6.1)
    - **Impact:** Duplicate labels fix
    - **Test Needed:** Test that labels are not emitted multiple times

13. **Forward declaration test** (6.2)
    - **Impact:** Compilation errors fix
    - **Test Needed:** Test that forward declarations are emitted when needed

---

## Recommendations

### Immediate Actions (Critical)

1. **Add string memory management regression tests** to `tests/bootstrap_tests.rs`
   - ✅ String temp pool overflow test - **COMPLETED**
   - String leak detection test (loop cleanup) - **PARTIALLY COVERED** (see string_temp_pool_loop_cleanup)
   - ✅ String double-wrapping test - **COMPLETED**

2. **Add SELECT CASE string test** to `tests/integration_tests.rs`
   - Test SELECT CASE with dynamic strings
   - Test SELECT CASE with fixed-length strings
   - Test SELECT CASE with string ranges and IS operators

### Short-Term Actions (High Priority)

3. **Add MID$ fixed-length string test** to `tests/integration_tests.rs`
   - Test MID$ assignment with fixed-length string variables
   - Test MID$ assignment with fixed-length string arrays

4. **Add array variable rename test** to `tests/bootstrap_tests.rs`
   - Test array access with renamed variables
   - Test array field assignment with renamed variables

5. **Add FFI declaration check** (could be a compile-time check or test)
   - Verify all FFI functions used in codegen are declared in header
   - Could be a static analysis tool or test

### Medium-Term Actions

6. **Expand regression test module** in `tests/bootstrap_tests.rs`
   - Add tests for built-in constant registration
   - Add tests for error handler syntax
   - Add tests for label uniqueness
   - Add tests for forward declarations

7. **Consider test organization**
   - Create `tests/regression/` directory for complex regression tests if needed
   - Document test strategy in this file

---

## Test Strategy

### Current Test Infrastructure

1. **Bootstrap Test** (`tests/bootstrap_tests.rs:qb64pe_compiles_successfully`)
   - Provides broad coverage by compiling full QB64pe codebase
   - Catches many regressions but may miss edge cases
   - Good for integration-level testing

2. **QB45 Compatibility Tests** (`tests/qb45_compat.rs`)
   - 114/115 files passing
   - Good parser coverage
   - Doesn't test codegen-specific bugs

3. **Integration Tests** (`tests/integration_tests.rs`)
   - Feature-level tests
   - Many scenarios covered but not all regression cases

4. **Regression Tests** (`tests/bootstrap_tests.rs:regression_tests`)
   - Currently has 2 tests (function canonical names, dual namespace)
   - Should be expanded with more regression cases

### Recommended Approach

1. **Expand existing regression test module** - Add tests to `tests/bootstrap_tests.rs:regression_tests`
2. **Add to integration tests** - Add feature-specific regression tests to `tests/integration_tests.rs`
3. **Use bootstrap test as safety net** - Continue relying on bootstrap test for broad coverage
4. **Document test additions** - Update this document when adding new regression tests

---

## References

- Session 048: Parser Bootstrap Fixes (`AgenticLogs/2026-01-24_session-048_parser-bootstrap-fixes.md`)
- Session 049: Bootstrap Semantic Error Fixes (`AgenticLogs/2026-01-24_session-049_bootstrap-semantic-fixes.md`)
- Session 050: Bootstrap Runtime Fixes (`AgenticLogs/2026-01-24_session-050_bootstrap-runtime-fixes.md`)
- Session 065: GDB Debugging QB64PE Crash (`AgenticLogs/2026-01-26_session-065_gdb-debugging-crash.md`)
- Session 067: Bug Review (`AgenticLogs/2026-01-27_session-067_bug-review-and-refactoring-verification.md`)
- Individual Problem: Parser Token API Mismatch (`AgenticLogs/IndividualProblems/2026-01-16_problem-parser-token-api-mismatch.md`)

---

## Maintenance

This document should be updated when:
- New major bugs are fixed (add to catalog)
- New regression tests are added (update test status)
- Test gaps are identified (add to gap analysis)
- Test strategy changes (update recommendations)

**Last Review:** 2026-01-28
