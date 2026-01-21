# Testing Infrastructure Plan

**Created:** 2026-01-18
**Updated:** 2026-01-20 (Session 031)
**Purpose:** Comprehensive plan for building out QB64Fresh testing infrastructure
**Based On:** QB64PE testing framework analysis + codebase review findings

---

## Executive Summary

**UPDATE:** As of 2026-01-20 (Session 032), the testing infrastructure has been substantially implemented:
- **217 unit tests** in source modules
- **486 integration tests** (0 ignored)
- **10 golden tests** for C code generation snapshots
- **16 compatibility test fixtures** (12 success + 4 error, auto-discovered)
- **19 property-based tests** using proptest (thousands of iterations)
- **30 benchmarks** measuring compiler performance
- **44 runtime tests** (all passing)

Total: **680+ tests** across the main compiler (11 doc-test ignored for setup requirements).
**QB64PE Compatibility:** 99.1% (114/115 files compile successfully, excluding open_gl)
**Fuzz testing:** 3 fuzz targets verified (~4.6M inputs, 0 crashes)

### ✅ Runtime Compilation Fixed (2026-01-20)

The runtime library `*_step` methods have been added to the `GraphicsBackend` trait:
- `pset_step()` - Plot pixel with STEP support
- `line_step()` - Draw line with STEP for both endpoints
- `circle_step()` - Draw circle with STEP center
- `paint_step()` - Flood fill with STEP starting point

All 44 runtime tests now pass.

---

## Current State Analysis

### What We Have (Updated 2026-01-20)
- Unit tests integrated into source files using `#[cfg(test)]` modules
- **217 passing unit tests** across compiler modules
- **486 integration tests** covering full compilation pipeline
- **10 golden tests** for codegen snapshot verification
- **16 compatibility test fixtures** in QB64pe-style format
- **30 criterion benchmarks** for performance tracking
- Good lexer, parser, and semantic test coverage
- Comprehensive codegen testing via integration tests
- **58 test modules** covering different language features

### Test Modules by Category

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

### Remaining Gaps

| Area | Current State | Risk Level | Recommendation |
|------|---------------|------------|----------------|
| Runtime tests | ✅ **FIXED** | LOW | All 44 tests passing |
| STEP graphics variants | ✅ **FIXED** | LOW | Trait methods added |
| STRING * n assignment | Type mismatch error | Medium | Add implicit padding/conversion |
| Coverage reporting | Ready to run | Low | Run `cargo llvm-cov --workspace` |
| File I/O runtime | Stubs only | Low | Covered by codegen tests |

---

## Testing Architecture (Implemented)

### Tier 1: Unit Tests ✅ IMPLEMENTED
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

### Tier 2: Integration Tests ✅ IMPLEMENTED
**Location:** `tests/integration_tests.rs`
**Purpose:** Test complete compiler pipeline end-to-end
**Status:** 486 tests passing, 0 ignored

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

### Tier 3: Golden Tests ✅ IMPLEMENTED
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

### Tier 4: Compatibility Tests ✅ IMPLEMENTED

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
**Status:** 141 files tested, **111 passing (78.7%)**

This test runner executes tests directly from the QB64pe repository without
copying them. It provides compatibility tracking against real QB4.5 and QB64 programs.

**Current Results (2026-01-20 Session 031):**
| Category | Files | Passing | Rate |
|----------|-------|---------|------|
| pete | 42 | 42 | **100%** |
| misc | 46 | 45 | **98%** |
| thebob | 19 | 19 | **100%** |
| qb45com | 5 | 5 | **100%** |
| n54 | 3 | 3 | **100%** |
| **Total (excl. open_gl)** | **115** | **114** | **99.1%** |

**Note:** 26 open_gl tests are intentionally excluded (QB64Fresh uses SDL2, not raw OpenGL).

**Remaining failure (1 file):**
- frog.bas: Bug in original code (`SCORE > HISCORE` where HISCORE is a UDT array)

**Recent improvements:**
- `REDIM SHARED` / `REDIM _PRESERVE SHARED` syntax
- `CIRCLE STEP` / `PAINT STEP` relative coordinates
- DATA statement hex-like values (`DATA 8B,E5`)
- Graphics GET/PUT with coordinate syntax

Run: `cargo test --test qb45_compat -- --nocapture`

### Tier 5: Benchmarks ✅ IMPLEMENTED
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

### Tier 6: Property-Based Testing ✅ IMPLEMENTED
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

### Tier 7: Fuzz Testing ✅ VERIFIED
**Location:** `fuzz/`
**Purpose:** Continuous fuzzing to find edge cases
**Status:** All 3 targets verified with ~4.6M total inputs, 0 crashes found

Fuzz targets:
- `fuzz_lexer` - Fuzz arbitrary input to the lexer
- `fuzz_parser` - Fuzz arbitrary token sequences to the parser
- `fuzz_full_pipeline` - Fuzz the complete compilation pipeline

**Setup:**
```bash
# Install cargo-fuzz (requires nightly Rust)
rustup install nightly
cargo +nightly install cargo-fuzz

# Run a fuzz target
cd fuzz
cargo +nightly fuzz run fuzz_lexer

# Run with limited time
cargo +nightly fuzz run fuzz_lexer -- -max_total_time=60
```

---

## Implementation Phases

### Phase 1: Foundation ✅ COMPLETE

#### 1.1 Integration Test Framework ✅
Implemented in `tests/integration_tests.rs` with 486 tests covering:
- Full compilation pipeline (lex → parse → analyze → codegen)
- Helper functions: `compile_to_c()`, `assert_compiles()`, `assert_compile_error()`
- Organized into 58 modules by feature area

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

### Phase 2: Coverage Expansion ✅ MOSTLY COMPLETE

#### 2.1 Statement-by-Statement Tests ✅
Integration tests cover each statement type:
- [x] All control flow (IF, FOR, WHILE, DO, SELECT CASE)
- [x] All data operations (DIM, DATA, READ, RESTORE)
- [x] All procedures (SUB, FUNCTION, DECLARE)
- [x] Graphics primitives (GET, PUT, VIEW PRINT)
- [ ] File I/O at runtime level (stubs only - codegen tested)
- [ ] Graphics runtime (stubs only - codegen tested)
- [ ] Sound runtime (stubs only - codegen tested)

#### 2.2 Semantic Checker Tests ✅
Covered via integration tests and error detection tests.

### Phase 3: Compatibility Testing ✅ IMPLEMENTED

#### 3.1 Local Fixtures ✅
Implemented in `tests/compatibility.rs`:
- Auto-discovers `.bas` files in `tests/fixtures/`
- Matches `.output` files for success tests
- Matches `.err` files for error tests
- 16 fixture files currently

#### 3.2 QB64pe Test Runner ✅
Implemented in `tests/qb45_compat.rs`:
- Runs tests directly from `../QB64pe/tests/qbasic_testcases/`
- No need to copy/port files - reads them in place
- Currently tests 115 files (excluding open_gl), 114 passing (99.1%)
- Provides failure diagnostics by stage (lexer/parser/semantic/codegen)

#### 3.3 Future: Increase Compatibility
- Most remaining failures are parser/semantic errors from missing features
- Key missing features: STRING * n implicit conversion, @ lexer token
- As features are implemented, more tests will automatically pass

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

## Test Tools

### Currently Installed
| Tool | Purpose | Status |
|------|---------|--------|
| `cargo test` | Built-in test runner | ✅ Using |
| `pretty_assertions` | Better diff output | ✅ Installed |
| `criterion` | Benchmarking | ✅ Installed |
| `proptest` | Property-based testing | ✅ Installed (19 tests) |
| `cargo-llvm-cov` | Coverage reporting | ✅ Installed (blocked by runtime errors) |

### Future Additions
| Crate | Purpose | Status |
|-------|---------|--------|
| `cargo-mutants` | Mutation testing | Not yet installed |

---

## Success Metrics

### Phase 3 Goals ✅ COMPLETE
- [x] QB64pe test runner implemented (reads tests in place, no porting needed)
- [x] 50+ QB4.5 compatibility tests passing (**114/115 = 99.1%** excluding open_gl - target exceeded!)
- [x] 80%+ QB4.5 compatibility (**99.1%** achieved, excluding open_gl!)
- [ ] Automated comparison with QB64PE output (compile_tests now supported)

### Phase 4 Goals ✅ UNBLOCKED
- [ ] 80%+ line coverage (was 81.63%, runtime now compiles - ready to measure)

### New Tests Needed

| Area | Priority | Rationale |
|------|----------|-----------|
| ~~Fix runtime `*_step` methods~~ | ~~HIGH~~ | ✅ FIXED (2026-01-20) |
| STRING * n conversion tests | Medium | Would fix 6+ QB64pe failures |
| @ and \| lexer tokens | Low | Would fix 1 QB64pe file |
| Extended ASCII handling | Low | Would fix 1 QB64pe file |

---

## Implementation Checklist

### Immediate ✅ COMPLETE
- [x] **Fix runtime compilation errors** - Added `*_step` methods to GraphicsBackend trait (2026-01-20)

### Short Term ✅ COMPLETE
- [x] QB64pe test runner implemented - no need to port, tests run from source
- [x] Implicit numeric type coercion (INTEGER↔LONG, DOUBLE↔INTEGER, etc.)
- [x] Console metacommands (`$CONSOLE`, `$CONSOLE:ONLY`, `$SCREENHIDE`, `$SCREENSHOW`)
- [x] REDIM SHARED syntax
- [x] Graphics coordinate syntax for GET/PUT

### Medium Term ✅ MOSTLY COMPLETE
- [x] Get 80%+ QB64pe tests passing (**99.1%** achieved, excluding open_gl!)
- [ ] Restore coverage reporting (fix runtime first)
- [ ] STRING * n implicit conversion

### Long Term
- [x] Set up continuous fuzzing with `cargo-fuzz` ✅
- [ ] Restore 80%+ coverage measurement
- [x] 50%+ QB4.5 compatibility tests passing (**99.1%** achieved, excluding open_gl!)

---

## Quick Reference: Running Tests

```bash
# Run all tests (compiler + runtime)
cargo test

# Run specific test suites
cargo test -p qb64fresh --test integration_tests    # 486 integration tests
cargo test -p qb64fresh --test golden_tests         # 10 golden tests
cargo test -p qb64fresh --test compatibility        # 16 local fixture tests
cargo test -p qb64fresh --test proptest_tests       # 19 property-based tests
cargo test -p qb64fresh --test qb45_compat          # QB64pe compatibility (141 files)

# Run unit tests only
cargo test -p qb64fresh --lib                       # 217 unit tests

# QB64pe compatibility tests (with output)
cargo test --test qb45_compat -- --nocapture
cargo test --test qb45_compat all_testcases_summary -- --nocapture  # Full summary
cargo test --test qb45_compat diagnose_failures -- --nocapture      # Debug failures
VERBOSE=1 cargo test --test qb45_compat -- --nocapture              # Show passing files

# Run benchmarks
cargo bench                            # Full benchmark suite
cargo bench -- "lexer"                 # Specific benchmark group

# Update golden files (after intentional changes)
UPDATE_GOLDEN=1 cargo test --test golden_tests

# Run with verbose output
cargo test -- --nocapture

# Coverage reporting (runtime fixed - now available)
cargo llvm-cov --workspace             # Console summary
cargo llvm-cov --workspace --html      # HTML report in target/llvm-cov/html
```

---

## References

- QB64PE Testing Framework: `QB64pe/docs/testing.md`
- QB64PE Test Cases: `QB64pe/tests/compile_tests/`
- QB45 Compatibility Report: `docs/QB45_COMPATIBILITY_REPORT.md`
- Rust Testing Book: https://doc.rust-lang.org/book/ch11-00-testing.html
- Criterion Documentation: https://bheisler.github.io/criterion.rs/book/

---

*Document created as part of QB64Fresh codebase review - 2026-01-18*
*Updated: 2026-01-18 - Marked completed items after testing infrastructure implementation*
*Updated: 2026-01-18 - Added coverage reporting (56.53%), property-based testing (19 tests), CI coverage job*
*Updated: 2026-01-18 - String functions and RND/RANDOMIZE implemented, 5 tests enabled (110 passing, 2 ignored)*
*Updated: 2026-01-18 - Session 019: SYSTEM, labeled DATA, File I/O tests, console INPUT tests, built-in functions*
*Updated: 2026-01-18 - Session 020: SLEEP, _DELAY, _LIMIT, ERASE, TAB, SPC, POS, CSRLIN, _KEYHIT, _KEYDOWN, _KEYCLEAR*
*Updated: 2026-01-18 - Session 021: PRINT USING, ? as PRINT alias, bitwise ops, keyboard functions, graphics stubs*
*Updated: 2026-01-18 - Session 022: Hyperbolic trig, angle conversion, string compare, error extensions*
*Updated: 2026-01-18 - Session 023: Reciprocal trig, gradian conversions, _TOSTR$, _BIN$, _IIF/_IIF$, window control*
*Updated: 2026-01-19 - Consolidated test counts; coverage improved to 72.67%; updated golden files*
*Updated: 2026-01-19 - Session 024: ENDIF keyword, _READFILE$/_WRITEFILE file helpers*
*Updated: 2026-01-19 - Session 025+: Updated test counts; coverage improved to 81.63% (target achieved!)*
*Updated: 2026-01-19 - Session 027: Phase 3 graphics completion - GET/PUT arrays, VIEW PRINT; 340 integration tests*
*Updated: 2026-01-20 - Session 028: Comprehensive review; QB64pe compatibility now 78.7% (111/141); identified runtime compilation blocker*
*Updated: 2026-01-20 - Session 029: Fixed runtime `*_step` methods - added pset_step, line_step, circle_step, paint_step to GraphicsBackend trait; all 44 runtime tests passing*
*Updated: 2026-01-20 - Session 030: QB4.5 compatibility improved to 83.0% (117/141); added _SND* functions (_SNDPLAYFILE, _SNDPLAYCOPY, _SNDSETPOS, _SNDCOPY, _SNDPLAYING, _SNDGETPOS, _SNDLEN, _SNDPAUSED); parser fixes (LINE style pattern, SUB calls with parenthesized args, empty array dimensions, single-line IF-THEN-ELSE with colons)*
*Updated: 2026-01-20 - Session 031: QB4.5 compatibility improved to 99.1% (114/115, excluding open_gl); platform constants renamed to _WIN, _LINUX, _MAC etc.; SHARED implicit declaration; label parsing at line start only; UnterminatedString in DATA; LEN() accepts UDTs; SCREEN function added*
