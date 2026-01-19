# Testing Infrastructure Plan

**Created:** 2026-01-18
**Updated:** 2026-01-19 (Session 025+)
**Purpose:** Comprehensive plan for building out QB64Fresh testing infrastructure
**Based On:** QB64PE testing framework analysis + codebase review findings

---

## Executive Summary

**UPDATE:** As of 2026-01-19, the testing infrastructure has been substantially implemented:
- **205 unit tests** in source modules
- **315 integration tests** (0 ignored)
- **10 golden tests** for C code generation snapshots
- **16 compatibility test fixtures** (12 success + 4 error, auto-discovered)
- **19 property-based tests** using proptest (thousands of iterations)
- **30 benchmarks** measuring compiler performance
- **44 runtime tests**

Total: **600+ tests** across the workspace (11 ignored for platform-specific features).
**Line coverage:** 81.63% (measured via cargo-llvm-cov) ✅ Target achieved!
**Fuzz testing:** 3 fuzz targets verified (~4.6M inputs, 0 crashes)

---

## Current State Analysis

### What We Have (Updated)
- Unit tests integrated into source files using `#[cfg(test)]` modules
- **205 passing unit tests** across compiler modules
- **315 integration tests** covering full compilation pipeline
- **10 golden tests** for codegen snapshot verification
- **16 compatibility test fixtures** in QB64pe-style format
- **30 criterion benchmarks** for performance tracking
- **44 runtime library tests**
- Good lexer, parser, and semantic test coverage
- Comprehensive codegen testing via integration tests

### Remaining Gaps
| Module | Lines | Tests | Risk Level |
|--------|-------|-------|------------|
| `src/codegen/c_backend/stmt.rs` | 2,533 | Via integration | Medium |
| `src/parser/statements.rs` | 1,200+ | Good | Low |
| `src/semantic/checker/statements.rs` | 1,000+ | Good | Low |
| `runtime/src/` | ~500 | 44 tests | Low |

---

## Testing Architecture (Implemented)

### Tier 1: Unit Tests ✅ IMPLEMENTED
**Location:** `src/**/*.rs` (inline `#[cfg(test)]` modules)
**Purpose:** Test individual functions and methods in isolation
**Status:** 205 tests passing

```
src/
├── lexer/
│   └── token.rs          ✅ Good coverage
├── parser/
│   ├── expressions.rs    ✅ Good coverage
│   ├── statements.rs     ✅ Good coverage
│   └── control_flow.rs   ✅ Good coverage
├── semantic/
│   ├── checker/          ✅ Good coverage (via integration)
│   └── types.rs          ✅ Good coverage
└── codegen/
    └── c_backend/        ✅ Good coverage (via integration)
```

### Tier 2: Integration Tests ✅ IMPLEMENTED
**Location:** `tests/integration_tests.rs`
**Purpose:** Test complete compiler pipeline end-to-end
**Status:** 315 tests passing, 0 ignored

Tests cover:
- Basic programs (hello world, comments, END)
- Variables (DIM, type suffixes, all numeric types)
- Expressions (arithmetic, logical, comparison, string)
- Control flow (IF/ELSE, FOR/NEXT, WHILE/WEND, DO/LOOP, SELECT CASE)
- Procedures (SUB, FUNCTION, parameters, recursion)
- Arrays (1D, 2D, indexing)
- DATA/READ statements
- Type conversions (CINT, CLNG, CSNG, CDBL)
- Built-in functions (math, type conversion)
- String functions (LEN, LEFT$, RIGHT$, MID$, UCASE$, LCASE$, CHR$, ASC, INSTR, SPACE$, STRING$)
- RND/RANDOMIZE (random number generation)
- Error detection (type mismatch, undefined procedures)
- File compilation (example .bas files)

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
    ├── error_type_mismatch.golden
    └── error_undefined_variable.golden
```

Update golden files: `UPDATE_GOLDEN=1 cargo test --test golden_tests`

### Tier 4: Compatibility Tests ✅ IMPLEMENTED

QB64Fresh has **two complementary compatibility test systems**:

#### 4a. Local Fixtures (`tests/compatibility.rs`)
**Location:** `tests/compatibility.rs` + `tests/fixtures/`
**Purpose:** Curated tests for features we've implemented
**Status:** 16 fixture files auto-discovered

These are tests written specifically for QB64Fresh that verify compilation succeeds
and (optionally) that output matches expected values.

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
**Status:** 141 files tested, **39 passing (27.7%)**

This test runner executes tests directly from the QB64pe repository without
copying them. It provides compatibility tracking against real QB4.5 and QB64 programs.

**Current Results (2026-01-19):**
| Category | Files | Passing | Rate |
|----------|-------|---------|------|
| qb45com | 5 | 0 | 0% |
| misc | 46 | 11 | 23.9% |
| n54 | 3 | 0 | 0% |
| pete | 68 | 28 | 41.2% |
| thebob | 19 | 0 | 0% |
| **Total** | **141** | **39** | **27.7%** |

**Failure breakdown:**
- Semantic errors: 52 (missing features like SHARED, DEF SEG, etc.)
- Parser errors: 25 (unimplemented syntax)
- I/O errors: 24 (missing files, encoding issues)
- Lexer errors: 1

**Recent improvements (Session 025):**
- Implicit numeric type coercion (INTEGER↔LONG, etc.) - unlocked 23 more tests
- `$CONSOLE`, `$CONSOLE:ONLY`, `$SCREENHIDE`, `$SCREENSHOW` metacommands

Run: `cargo test --test qb45_compat -- --nocapture`

**Note:** The QB64pe repository also has `tests/compile_tests/` with 100+ structured
tests (`.bas` + `.output` pairs). These now parse correctly with the new metacommand
support.

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
Implemented in `tests/integration_tests.rs` with 315 tests covering:
- Full compilation pipeline (lex → parse → analyze → codegen)
- Helper functions: `compile_to_c()`, `assert_compiles()`, `assert_compile_error()`
- Organized into modules by feature area

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
- [ ] `OPEN/CLOSE/PRINT#/INPUT#/GET/PUT` (file I/O not yet implemented)
- [ ] Graphics statements (runtime stubs only)
- [ ] Sound statements (runtime stubs only)

#### 2.2 Semantic Checker Tests ✅
Covered via integration tests and error detection tests:
- [ ] Array bounds checking (runtime feature)

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
- Currently tests 141 files, 39 passing (27.7%)
- Provides failure diagnostics by stage (lexer/parser/semantic/codegen)

#### 3.3 Future: Increase Compatibility
- Most failures are semantic errors (52) from missing features
- Key missing features: SHARED scope, DEF SEG, graphics primitives
- As features are implemented, more tests will automatically pass

### Phase 4: Benchmarking ✅ COMPLETE

#### 4.1 Criterion Benchmarks ✅
Implemented in `benches/compiler_benchmarks.rs`:
- 6 test programs of varying complexity
- 5 benchmark groups (lexer, parser, semantic, codegen, full)
- 30 total benchmarks
- HTML reports generated in `target/criterion/`

Sample results:
| Program | Full Compilation | Throughput |
|---------|-----------------|------------|
| hello_world | ~19 µs | ~1 MiB/s |
| complex (500 bytes) | ~48 µs | ~10 MiB/s |

### Phase 5: Advanced Testing ✅ COMPLETE

#### 5.1 Property-Based Testing ✅
Implemented in `tests/proptest_tests.rs` with 19 tests using `proptest` crate.

#### 5.2 Fuzzing with `cargo-fuzz` ✅
Implemented in `fuzz/` directory with 3 fuzz targets:
- `fuzz_lexer` - Fuzz arbitrary input to the lexer
- `fuzz_parser` - Fuzz arbitrary token sequences to the parser
- `fuzz_full_pipeline` - Fuzz the complete compilation pipeline

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
| `cargo-llvm-cov` | Coverage reporting | ✅ Installed (CI integrated) |

### Future Additions
| Crate | Purpose | Status |
|-------|---------|--------|
| `cargo-mutants` | Mutation testing | Not yet installed |

---

## Success Metrics

### Phase 3 Goals 🔄 IN PROGRESS
- [x] QB64pe test runner implemented (reads tests in place, no porting needed)
- [ ] 50+ QB4.5 compatibility tests passing (currently 41/141 = 29.1%)
- [ ] Automated comparison with QB64PE output (compile_tests now supported)

### Phase 4 Goals ✅ COMPLETE
- [x] 80%+ line coverage (**81.63%** achieved! - up from 72.67%)

---

## Implementation Checklist

### Short Term ✅ COMPLETE
- [x] QB64pe test runner implemented - no need to port, tests run from source
- [x] Implicit numeric type coercion (INTEGER↔LONG, DOUBLE↔INTEGER, etc.)
- [x] Console metacommands (`$CONSOLE`, `$CONSOLE:ONLY`, `$SCREENHIDE`, `$SCREENSHOW`)

### Medium Term 🔄 IN PROGRESS
- [ ] Get 50+ QB64pe tests passing (currently 41 after SHARED implementation)
- [ ] Integrate `compile_tests` runner for structured output comparison

### Long Term
- [x] Set up continuous fuzzing with `cargo-fuzz` ✅
- [x] Achieve 80%+ coverage (**81.63%** achieved!)
- [ ] 50%+ QB4.5 compatibility tests passing

---

## Quick Reference: Running Tests

```bash
# Run all tests
cargo test --workspace

# Run specific test suites
cargo test --test integration_tests    # 277 integration tests
cargo test --test golden_tests         # 10 golden tests
cargo test --test compatibility        # 16 local fixture tests
cargo test --test proptest_tests       # 19 property-based tests
cargo test --test qb45_compat          # QB64pe compatibility (141 files)

# Run unit tests only
cargo test --lib

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

# Coverage reporting
cargo llvm-cov --workspace             # Console summary
cargo llvm-cov --workspace --html      # HTML report in target/llvm-cov/html
cargo llvm-cov --workspace --lcov      # LCOV format for CI
```

---

## References

- QB64PE Testing Framework: `QB64pe/docs/testing.md`
- QB64PE Test Cases: `QB64pe/tests/compile_tests/`
- Rust Testing Book: https://doc.rust-lang.org/book/ch11-00-testing.html
- Criterion Documentation: https://bheisler.github.io/criterion.rs/book/

---

*Document created as part of QB64Fresh codebase review - 2026-01-18*
*Updated: 2026-01-18 - Marked completed items after testing infrastructure implementation*
*Updated: 2026-01-18 - Added coverage reporting (56.53%), property-based testing (19 tests), CI coverage job*
*Updated: 2026-01-18 - String functions and RND/RANDOMIZE implemented, 5 tests enabled (110 passing, 2 ignored)*
*Updated: 2026-01-18 - Session 019: SYSTEM, labeled DATA, File I/O tests, console INPUT tests, built-in functions (TIMER, DATE$, TIME$, TRIM$), fuzz infrastructure (128 tests, 0 ignored, 59.92% coverage)*
*Updated: 2026-01-18 - Session 020: SLEEP, _DELAY, _LIMIT, ERASE, TAB, SPC, POS, CSRLIN, _KEYHIT, _KEYDOWN, _KEYCLEAR; fuzz testing verified (~4.6M inputs, 0 crashes); 148 tests*
*Updated: 2026-01-18 - Session 021: PRINT USING, ? as PRINT alias, bitwise ops (_SHL/_SHR/_ROL/_ROR/_READBIT/_SETBIT/_RESETBIT/_TOGGLEBIT), keyboard (_CINP, lock keys), graphics stubs, _CLAMP/_HYPOT tests; 176 tests*
*Updated: 2026-01-18 - Session 022: Hyperbolic trig (_SINH/_COSH/_TANH/_ASINH/_ACOSH/_ATANH), angle conversion (_D2R/_R2D), _NEGATE, string compare (_STRCMP/_STRICMP), error extensions (_ERRORLINE/_ERRORMESSAGE$), utility funcs (_COMMANDCOUNT/_ENVIRONCOUNT), font stubs, desktop/window funcs, dialog boxes, RodioBackend for audio; 207 tests*
*Updated: 2026-01-18 - Session 023: Reciprocal trig (_SEC/_CSC/_COT/_SECH/_CSCH/_COTH/_ARCSEC/_ARCCSC/_ARCCOT/_ARCSECH/_ARCCSCH/_ARCCOTH), gradian conversions (_D2G/_G2D/_G2R/_R2G), _TOSTR$, _BIN$, _IIF/_IIF$, window control (_SCREENMOVE/_SCREENHIDE/_SCREENSHOW/_FULLSCREEN/_SCREENCLICK), sound codegen (BEEP/SOUND/PLAY runtime), _FONT/_FREEFONT; 239 tests*
*Updated: 2026-01-19 - Consolidated test counts: 534+ tests total (170 unit, 272 integration, 10 golden, 19 proptest, 44 runtime, 19 misc); coverage improved to 72.67%; updated golden files for current codegen output*
*Updated: 2026-01-19 - Session 024: ENDIF keyword, _READFILE$/_WRITEFILE file helpers, verified implicit SUB calls already working, register_builtin_sub() for built-in SUBs; 277 integration tests*
*Updated: 2026-01-19 - Session 025+: Updated test counts (205 unit, 307 integration, 600+ total); coverage improved to 81.63% (target achieved!); updated golden files for current codegen output*
