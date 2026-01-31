# Testing Infrastructure - Completed Items

**Created:** 2026-01-20
**Updated:** 2026-01-28
**Purpose:** Archive of completed testing infrastructure milestones for QB64Fresh

---

## Resolved 2026-01-30 (Recent Completions)

### ✅ QB64pe Incremental Testing Strategy Complete (2026-01-28)

**Source:** [QB64PE_INCREMENTAL_TESTING.md](QB64PE_INCREMENTAL_TESTING.md) (this archive).

**Status:** Full QB64pe compilation succeeds through all QB64Fresh phases (preprocess, lex, parse, semantic, codegen); generated C compiles with 0 errors.

**Completed phases:**
- **Phase 1:** Core infrastructure (version, settings, constants) — PASSES
- **Phase 2:** Utilities (hash, type; const_eval partial) — PASSES where tested
- **Phase 4:** Core compiler (without IDE) — COMPILES SUCCESSFULLY
- **Phase 5:** Full compiler — COMPILES SUCCESSFULLY

Phase 3 (Built-in Functions in isolation) now PASSES using stub infrastructure (sections/phase3_stubs.bas: validname, tryRemoveSymbol$, AddQuotes$, subfunc, subfuncn). Full doc: [QB64PE_INCREMENTAL_TESTING.md](QB64PE_INCREMENTAL_TESTING.md).

---

## Resolved 2026-01-28 (Recent Completions)

### ✅ Coverage Reporting Restored (2026-01-25)
- **Coverage tool:** `cargo llvm-cov --workspace` verified working
- **CI integration:** Coverage job configured in `.github/workflows/ci.yml`
- **Coverage target:** **81.63%** achieved (exceeds 80% target)
- **Status:** Complete and tracked in CI

### ✅ STRING * n Implicit Conversion (2026-01-27)
- **Implementation:** Implicit conversion between STRING and FixedString (STRING * n) types
- **Scope:** Assignments now automatically convert between STRING ↔ STRING * n
- **Impact:** Resolves type compatibility issues in assignments
- **Status:** Complete and tested

## Resolved 2026-01-25 (Blocking Failures → All Passing)

All previously tracked test failures were fixed:

| Item | Previous State | Resolution |
|------|----------------|------------|
| Golden tests (8/10 failing) | Codegen drift vs. `.golden` files | Golden files updated or codegen aligned; all 10 passing |
| `test_invalid_binary_op` | Assertion expecting errors failed | Semantic checker or test updated; passing |
| EXIT statement | integration_tests failure (codegen incomplete) | EXIT statement codegen implemented |
| _STATUSCODE function | integration_tests failure | _STATUSCODE implementation completed |
| _MAPUNICODE statement | integration_tests failure | _MAPUNICODE statement implementation completed |

**Test counts after resolution:** 404 unit, 720 integration, 10 golden, 19 proptest, 3 compatibility, 27 execution, 195 runtime — all passing.

### Resolved in Earlier Updates (moved from plan 2026-01-25)

- **ControlChr** — Fully implemented (parser, semantic, codegen).
- **rodio API mismatch** — Updated to rodio 0.21, API compatible.

---

## Executive Summary

The QB64Fresh testing infrastructure has been substantially implemented:

- **217 unit tests** in source modules
- **539 integration tests** (0 ignored)
- **10 golden tests** for C code generation snapshots
- **16 compatibility test fixtures** (12 success + 4 error, auto-discovered)
- **19 property-based tests** using proptest (thousands of iterations)
- **30 benchmarks** measuring compiler performance
- **163 runtime tests** (all passing)

**Total: ~900 tests** across the main compiler (11 doc-test ignored for setup requirements).
**QB64PE Compatibility:** 99.1% (114/115 files compile successfully, excluding open_gl)
**Fuzz testing:** 3 fuzz targets verified (~4.6M inputs, 0 crashes)

---

## Completed Milestones

### ✅ Runtime Test Coverage Gaps Resolved (2026-01-21 Session 041)

The following high-priority gaps were addressed:

| Area | Previous State | New State |
|------|----------------|-----------|
| io.rs unit tests | 1 test | **60+ tests** ✅ |
| File I/O runtime | No tests | **Tested via io.rs** ✅ |
| String edge cases | Basic coverage | **80+ edge case tests** ✅ |
| End-to-end execution | None | **27 execution tests** ✅ |

**io.rs comprehensive tests** (~60 tests):
- Print functions (`qb_print_int`, `qb_print_float`, `qb_print_string`, etc.)
- Console functions (`qb_cls`, `qb_locate`, `qb_color`)
- File system ops (`qb_file_kill`, `qb_file_rename`, `qb_mkdir`, `qb_rmdir`, etc.)
- Network functions (`qb_net_openhost`, `qb_net_openclient`, etc.)
- Shell functions (`qb_shell`, `qb_shell_hide`)

**string.rs edge case tests** (~80 tests):
- Null pointer handling (all functions tested with null inputs)
- Reference counting (retain/release patterns, operations preserving originals)
- Edge case values (negative indices, overflow, empty strings)
- Large strings (1MB creation, 200KB concat, memory stress)
- Binary data (embedded nulls, high bytes)
- String conversion (STR$, VAL, CHR$, ASC)

**End-to-end execution tests** (tests/execution_tests.rs):
- Test framework that compiles BASIC → C → executable → verifies output
- **27 tests all passing** after bug fixes in Session 042

### ✅ Inline Runtime Bug Fixes (2026-01-21 Session 042)

Fixed three bugs in the inline C runtime that were blocking execution tests:

| Bug | Issue | Fix |
|-----|-------|-----|
| `qb_lset`/`qb_rset` struct access | Used `->length` instead of `->len` | Fixed field name in [runtime.rs:1588-1606](src/codegen/c_backend/runtime.rs#L1588-L1606) |
| `qb_string_release` missing | Not defined in inline runtime | Added with proper refcount decrement in [runtime.rs:154-164](src/codegen/c_backend/runtime.rs#L154-L164) |
| `qb_string` refcount init | Strings created without refcount=1 | Added `str->refcount = 1;` to all string creation functions |

All 27 execution tests now pass.

### ✅ Runtime Test Coverage Complete (2026-01-21)

Added comprehensive runtime library testing:

**io.rs tests (~60 tests)**:
- Print functions (qb_print_int, qb_print_float, qb_print_string, etc.)
- Console functions (qb_cls, qb_locate, qb_color)
- File system ops (qb_file_kill, qb_file_rename, qb_mkdir, qb_rmdir, etc.)
- Network functions (qb_net_openhost, qb_net_openclient, etc.)
- Shell functions (qb_shell, qb_shell_hide)

**string.rs edge case tests (~80 tests)**:
- Null pointer handling (all functions tested with null inputs)
- Reference counting (retain/release patterns, operations preserving originals)
- Edge case values (negative indices, overflow, empty strings)
- Large strings (1MB creation, 200KB concat, memory stress)
- Binary data (embedded nulls, high bytes)
- String conversion (STR$, VAL, CHR$, ASC)

**End-to-end execution tests** (tests/execution_tests.rs):
- Test framework that compiles BASIC → C → executable → verifies output
- 27 test cases covering: print, variables, loops, conditionals, functions, arrays
- Tests use BYVAL for function parameters (workaround for by-ref codegen limitation)

Total runtime tests: **163** (up from 44)

### ✅ Parser Module Tests Complete (2026-01-21)

Added 21 edge case tests to the parser module covering:
- Graphics statement parsing (GET, PUT, VIEW PRINT, etc.)
- Audio statement parsing (BEEP, SOUND, PLAY)
- System statement parsing (SHELL, RUN, CHAIN)
- File I/O parsing (OPEN, CLOSE, INPUT#, PRINT#)
- Edge cases like malformed statements, empty inputs

### ✅ Codegen Tests for Stubs Complete (2026-01-21)

Added integration tests verifying codegen for previously stub-only areas:
- **File I/O codegen tests** (22 tests) - OPEN, CLOSE, INPUT#, PRINT#, GET, PUT, LOC, LOF, EOF
- **Graphics codegen tests** (43 tests) - PSET, LINE, CIRCLE, PAINT, GET, PUT, SCREEN, etc.
- **Sound codegen tests** (29 tests) - BEEP, SOUND, PLAY, _SNDOPEN, _SNDPLAY, etc.

These verify the compiler generates valid C code for these statements. Note: These are **compiler tests**, not runtime behavioral tests.

### ✅ Runtime Compilation Fixed (2026-01-20)

The runtime library `*_step` methods have been added to the `GraphicsBackend` trait:
- `pset_step()` - Plot pixel with STEP support
- `line_step()` - Draw line with STEP for both endpoints
- `circle_step()` - Draw circle with STEP center
- `paint_step()` - Flood fill with STEP starting point

All 44 runtime tests now pass.

---

## Testing Architecture (Implemented)

### Tier 1: Unit Tests ✅ COMPLETE
**Location:** `src/**/*.rs` (inline `#[cfg(test)]` modules)
**Purpose:** Test individual functions and methods in isolation
**Status:** 217 tests passing

```
src/
├── lexer/
│   └── token.rs          ✅ Good coverage
├── parser/
│   ├── expressions.rs    ✅ Good coverage
│   ├── statements.rs     ✅ Good coverage
│   ├── control_flow.rs   ✅ Good coverage
│   ├── graphics.rs       ✅ New - modularized
│   ├── audio.rs          ✅ New - modularized
│   ├── system.rs         ✅ New - modularized
│   └── file_io.rs        ✅ New - modularized
├── semantic/
│   ├── checker/          ✅ Good coverage (via integration)
│   └── types.rs          ✅ Good coverage
└── codegen/
    └── c_backend/        ✅ Good coverage (via integration)
```

### Tier 2: Integration Tests ✅ COMPLETE
**Location:** `tests/integration_tests.rs`
**Purpose:** Test complete compiler pipeline end-to-end
**Status:** 539 tests passing, 0 ignored

Tests cover:
- Basic programs (hello world, comments, END)
- Variables (DIM, type suffixes, all numeric types)
- Expressions (arithmetic, logical, comparison, string)
- Control flow (IF/ELSE, FOR/NEXT, WHILE/WEND, DO/LOOP, SELECT CASE)
- Procedures (SUB, FUNCTION, parameters, recursion)
- Arrays (1D, 2D, indexing, REDIM, ERASE)
- DATA/READ statements
- Type conversions (CINT, CLNG, CSNG, CDBL)
- Built-in functions (math, type conversion, string)
- String functions (LEN, LEFT$, RIGHT$, MID$, UCASE$, LCASE$, CHR$, ASC, INSTR, SPACE$, STRING$)
- RND/RANDOMIZE (random number generation)
- Error detection (type mismatch, undefined procedures)
- File compilation (example .bas files)
- Graphics (GET/PUT arrays, VIEW PRINT, _PRINTWIDTH)
- Sound (BEEP, SOUND, PLAY)
- Window control (_SCREENMOVE, _FULLSCREEN, etc.)
- Conditional compilation ($IF, $ELSE, $END IF)

### Tier 3: Golden Tests ✅ COMPLETE
**Location:** `tests/golden_tests.rs` + `tests/golden/*.golden`
**Purpose:** Snapshot testing for generated C code
**Status:** 10 tests passing

```
tests/
├── golden_tests.rs            # Test harness
└── golden/                    # Golden files (expected C output)
    ├── hello_world.golden
    ├── arithmetic.golden
    ├── variables.golden
    ├── for_loop.golden
    ├── control_flow.golden
    ├── function.golden
    ├── array.golden
    ├── data_read.golden
    ├── error_type_mismatch.err.golden
    └── error_undefined_variable.err.golden
```

Update golden files: `UPDATE_GOLDEN=1 cargo test --test golden_tests`

### Tier 4: Compatibility Tests ✅ COMPLETE

QB64Fresh has **two complementary compatibility test systems**:

#### 4a. Local Fixtures (`tests/compatibility.rs`)
**Location:** `tests/compatibility.rs` + `tests/fixtures/`
**Purpose:** Curated tests for features we've implemented
**Status:** 16 fixture files auto-discovered (12 success + 4 error)

```
tests/
├── compatibility.rs           # Test harness for local fixtures
└── fixtures/
    ├── success/               # Tests that should compile successfully
    │   ├── print_hello.bas    # Basic output
    │   ├── for_loop.bas       # FOR/NEXT loops
    │   ├── for_step.bas       # FOR with STEP
    │   ├── select_case.bas    # SELECT CASE
    │   ├── while_wend.bas     # WHILE/WEND loops
    │   ├── do_loop.bas        # DO/LOOP variants
    │   ├── sub_function.bas   # SUB and FUNCTION
    │   ├── data_read.bas      # DATA/READ statements
    │   ├── array_ops.bas      # Array operations
    │   ├── math_expr.bas      # Mathematical expressions
    │   ├── string_concat.bas  # String concatenation
    │   └── basic_math.bas     # Basic arithmetic
    └── error/                 # Tests that should produce errors
        ├── type_mismatch.bas + .err
        ├── undefined_var.bas + .err
        ├── unclosed_if.bas + .err
        └── duplicate_definition.bas + .err
```

Run: `cargo test --test compatibility`

#### 4b. QB64pe Test Suite (`tests/qb45_compat.rs`)
**Location:** `tests/qb45_compat.rs` (reads from `../QB64pe/tests/`)
**Purpose:** Track compatibility with real-world QB64 programs
**Status:** 141 files tested, **114 passing (99.1%, excluding open_gl)**

This test runner executes tests directly from the QB64pe repository without
copying them. It provides compatibility tracking against real QB4.5 and QB64 programs.

**Final Results (2026-01-20):**
| Category | Files | Passing | Rate |
|----------|-------|---------|------|
| pete | 42 | 42 | **100%** |
| misc | 46 | 45 | **98%** |
| thebob | 19 | 19 | **100%** |
| qb45com | 5 | 5 | **100%** |
| n54 | 3 | 3 | **100%** |
| **Total (excl. open_gl)** | **115** | **114** | **99.1%** |

**Note:** 26 open_gl tests are intentionally excluded (QB64Fresh uses SDL2, not raw OpenGL).

Run: `cargo test --test qb45_compat -- --nocapture`

### Tier 5: Benchmarks ✅ COMPLETE
**Location:** `benches/compiler_benchmarks.rs`
**Purpose:** Track compiler performance over time
**Status:** 30 benchmarks passing

Benchmark groups:
- `lexer/lex/*` - Tokenization speed
- `parser/parse/*` - Parsing speed
- `semantic/analyze/*` - Type checking speed
- `codegen/generate/*` - Code generation speed
- `full_compilation/compile/*` - Complete pipeline

Run: `cargo bench`

### Tier 6: Property-Based Testing ✅ COMPLETE
**Location:** `tests/proptest_tests.rs`
**Purpose:** Find edge cases through randomized testing
**Status:** 19 tests passing

Property tests verify the compiler never panics on arbitrary input:
- `lexer_never_panics` - Random binary input
- `parser_handles_arbitrary_input` - Handles all ASCII
- `parser_never_panics_on_basic_input` - BASIC-like random code
- `full_pipeline_never_panics` - Complete compilation pipeline
- `handles_long_programs` - Stress testing with large inputs
- `handles_nested_expressions` - Deep nesting handling
- Plus deterministic edge case tests (unicode, null bytes, unterminated strings)

Run: `cargo test --test proptest_tests`

### Tier 7: Fuzz Testing ✅ COMPLETE
**Location:** `fuzz/`
**Purpose:** Continuous fuzzing to find edge cases
**Status:** All 3 targets verified with ~4.6M total inputs, 0 crashes found

Fuzz targets:
- `fuzz_lexer` - Fuzz arbitrary input to the lexer
- `fuzz_parser` - Fuzz arbitrary token sequences to the parser
- `fuzz_full_pipeline` - Fuzz the complete compilation pipeline

---

## Completed Implementation Phases

### Phase 1: Foundation ✅ COMPLETE

#### 1.1 Integration Test Framework ✅
Implemented in `tests/integration_tests.rs` with 539 tests covering:
- Full compilation pipeline (lex → parse → analyze → codegen)
- Helper functions: `compile_to_c()`, `assert_compiles()`, `assert_compile_error()`
- Organized into 59 modules by feature area

#### 1.2 Golden/Snapshot Testing ✅
Implemented in `tests/golden_tests.rs` (manual approach instead of `insta`):
- Compares generated C code against `.golden` files
- Environment variable `UPDATE_GOLDEN=1` to regenerate expected output
- 10 golden tests covering various language features

#### 1.3 Codegen Tests ✅
Covered via integration tests that verify:
- C code generation succeeds for valid programs
- Generated code contains expected constructs
- Error cases produce appropriate errors

### Phase 2: Coverage Expansion ✅ COMPLETE

#### 2.1 Statement-by-Statement Tests ✅
Integration tests cover each statement type:
- All control flow (IF, FOR, WHILE, DO, SELECT CASE)
- All data operations (DIM, DATA, READ, RESTORE)
- All procedures (SUB, FUNCTION, DECLARE)
- Graphics primitives (GET, PUT, VIEW PRINT)

#### 2.2 Semantic Checker Tests ✅
Covered via integration tests and error detection tests.

### Phase 3: Compatibility Testing ✅ COMPLETE

#### 3.1 Local Fixtures ✅
Implemented in `tests/compatibility.rs`:
- Auto-discovers `.bas` files in `tests/fixtures/`
- Matches `.output` files for success tests
- Matches `.err` files for error tests
- 16 fixture files

#### 3.2 QB64pe Test Runner ✅
Implemented in `tests/qb45_compat.rs`:
- Runs tests directly from `../QB64pe/tests/qbasic_testcases/`
- No need to copy/port files - reads them in place
- 99.1% compatibility achieved (114/115, excluding open_gl)
- Provides failure diagnostics by stage (lexer/parser/semantic/codegen)

### Phase 4: Benchmarking ✅ COMPLETE

#### 4.1 Criterion Benchmarks ✅
Implemented in `benches/compiler_benchmarks.rs`:
- 6 test programs of varying complexity
- 5 benchmark groups (lexer, parser, semantic, codegen, full)
- 30 total benchmarks
- HTML reports generated in `target/criterion/`

### Phase 5: Advanced Testing ✅ COMPLETE

#### 5.1 Property-Based Testing ✅
Implemented in `tests/proptest_tests.rs` with 19 tests using `proptest` crate.

#### 5.2 Fuzzing with `cargo-fuzz` ✅
Implemented in `fuzz/` directory with 3 fuzz targets.
Verified with ~4.6M total inputs, 0 crashes found.

---

## Completed Checklist Items

### Immediate ✅ COMPLETE
- [x] **Fix runtime compilation errors** - Added `*_step` methods to GraphicsBackend trait (2026-01-20)

### Short Term ✅ COMPLETE
- [x] QB64pe test runner implemented - no need to port, tests run from source
- [x] Implicit numeric type coercion (INTEGER↔LONG, DOUBLE↔INTEGER, etc.)
- [x] Console metacommands (`$CONSOLE`, `$CONSOLE:ONLY`, `$SCREENHIDE`, `$SCREENSHOW`)
- [x] REDIM SHARED syntax
- [x] Graphics coordinate syntax for GET/PUT

### Long Term ✅ COMPLETE
- [x] Set up continuous fuzzing with `cargo-fuzz`
- [x] 50%+ QB4.5 compatibility tests passing (**99.1%** achieved, excluding open_gl!)

### Phase 3 Goals ✅ COMPLETE
- [x] QB64pe test runner implemented (reads tests in place, no porting needed)
- [x] 50+ QB4.5 compatibility tests passing (**114/115 = 99.1%** excluding open_gl - target exceeded!)
- [x] 80%+ QB4.5 compatibility (**99.1%** achieved, excluding open_gl!)

---

## Test Tools Installed

| Tool | Purpose | Status |
|------|---------|--------|
| `cargo test` | Built-in test runner | ✅ Using |
| `pretty_assertions` | Better diff output | ✅ Installed |
| `criterion` | Benchmarking | ✅ Installed |
| `proptest` | Property-based testing | ✅ Installed (19 tests) |
| `cargo-llvm-cov` | Coverage reporting | ✅ Installed |

---

## Test Modules by Category (59 Total)

| Category | Modules | Notes |
|----------|---------|-------|
| **Core Language** | basic_programs, variables, expressions, control_flow, procedures, arrays, constants, literals | Foundation tests |
| **Control Flow** | control_flow (IF/FOR/WHILE/DO/SELECT) | All variants covered |
| **Data** | data_statements, shared_variables, field_statement | DATA/READ, SHARED scope |
| **Functions** | builtin_functions, math_functions, type_conversions, extended_math | 50+ built-in functions |
| **Trigonometry** | hyperbolic_functions, reciprocal_trig_functions, angle_conversions, gradian_conversions | Sin/cos/tan + extensions |
| **Strings** | print_formatting, print_using, print_shorthand, string_comparison | LEN, LEFT$, MID$, etc. |
| **I/O** | file_io, console_input, keyboard_input, lprint_statement | File + console I/O |
| **Graphics** | graphics_stubs, extended_graphics, font_stubs, font_functions | GET/PUT, VIEW PRINT, _PRINTWIDTH |
| **Sound** | sound_statements | BEEP, SOUND, PLAY |
| **Window/Display** | window_control_functions, desktop_functions, dialog_functions, screenicon_statement | Screen control |
| **System** | timing_statements, utility_functions, run_statement, chain_statement | SLEEP, _DELAY, RUN, CHAIN |
| **Advanced** | bitwise_operations, inline_conditional_functions, number_conversion_functions | _SHL, _IIF, _BIN$ |
| **Errors** | error_detection, error_extensions | Type mismatch, undefined vars |
| **Meta** | conditional_compilation, trace_statements, assert_statement | $IF/$ELSE, TRON/TROFF |

---

## Session History

The testing infrastructure was built incrementally across many sessions:

- **Session 019:** SYSTEM, labeled DATA, File I/O tests, console INPUT tests, built-in functions
- **Session 020:** SLEEP, _DELAY, _LIMIT, ERASE, TAB, SPC, POS, CSRLIN, _KEYHIT, _KEYDOWN, _KEYCLEAR
- **Session 021:** PRINT USING, ? as PRINT alias, bitwise ops, keyboard functions, graphics stubs
- **Session 022:** Hyperbolic trig, angle conversion, string compare, error extensions
- **Session 023:** Reciprocal trig, gradian conversions, _TOSTR$, _BIN$, _IIF/_IIF$, window control
- **Session 024:** ENDIF keyword, _READFILE$/_WRITEFILE file helpers
- **Session 025+:** Coverage improved to 81.63% (target achieved!)
- **Session 027:** Phase 3 graphics completion - GET/PUT arrays, VIEW PRINT
- **Session 028:** Comprehensive review; QB64pe compatibility 78.7%
- **Session 029:** Fixed runtime `*_step` methods; all 44 runtime tests passing
- **Session 030:** QB4.5 compatibility improved to 83.0%; added _SND* functions
- **Session 031:** QB4.5 compatibility improved to **99.1%** (114/115, excluding open_gl)
- **Session 032:** Added 32 QB64 extension functions; 486 integration tests
- **Session 033:** Added 20 QB64 extension functions; 506 integration tests
- **Session 034:** Added 28 QB64 extension keywords/functions; 526 integration tests
- **Session 035:** Added date/time functions, console functions, memory SUBs; 539 integration tests
- **Session 036:** Synchronized test counts (730+ tests total)
- **Session 041-042:** Runtime test coverage gaps resolved (io.rs 60+ tests, string.rs 80+ tests, execution tests 27)
- **2026-01-25:** Coverage reporting restored, 81.63% coverage achieved (CI configured)
- **2026-01-27:** STRING * n implicit conversion implemented

---

*Document created: 2026-01-20*
*Source: Extracted from TESTING_INFRASTRUCTURE_PLAN.md*
