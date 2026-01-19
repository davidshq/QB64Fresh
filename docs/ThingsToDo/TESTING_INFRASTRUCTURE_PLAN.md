# Testing Infrastructure Plan

**Created:** 2026-01-18
**Updated:** 2026-01-18
**Purpose:** Comprehensive plan for building out QB64Fresh testing infrastructure
**Based On:** QB64PE testing framework analysis + codebase review findings

---

## Executive Summary

**UPDATE:** As of 2026-01-18, the testing infrastructure has been substantially implemented:
- **163 unit tests** in source modules
- **239 integration tests** (0 ignored)
- **10 golden tests** for C code generation snapshots
- **16 compatibility test fixtures** (auto-discovered)
- **19 property-based tests** using proptest (thousands of iterations)
- **30 benchmarks** measuring compiler performance
- **37 runtime tests**

Total: **461+ tests** across the workspace.
**Line coverage:** 59.92% (measured via cargo-llvm-cov)
**Fuzz testing:** 3 fuzz targets verified (~4.6M inputs, 0 crashes)

---

## Current State Analysis

### What We Have (Updated)
- Unit tests integrated into source files using `#[cfg(test)]` modules
- **163 passing unit tests** across compiler modules
- **148 integration tests** covering full compilation pipeline
- **10 golden tests** for codegen snapshot verification
- **16 compatibility test fixtures** in QB64pe-style format
- **30 criterion benchmarks** for performance tracking
- **37 runtime library tests**
- Good lexer, parser, and semantic test coverage
- Comprehensive codegen testing via integration tests

### Remaining Gaps
| Module | Lines | Tests | Risk Level |
|--------|-------|-------|------------|
| `src/codegen/c_backend/stmt.rs` | 2,533 | Via integration | Medium |
| `src/parser/statements.rs` | 1,200+ | Good | Low |
| `src/semantic/checker/statements.rs` | 1,000+ | Good | Low |
| `runtime/src/` | ~500 | 37 tests | Low |

---

## Testing Architecture (Implemented)

### Tier 1: Unit Tests ✅ IMPLEMENTED
**Location:** `src/**/*.rs` (inline `#[cfg(test)]` modules)
**Purpose:** Test individual functions and methods in isolation
**Status:** 163 tests passing

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
**Status:** 239 tests passing, 0 ignored

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
**Location:** `tests/compatibility.rs` + `tests/fixtures/`
**Purpose:** Verify compatibility with existing BASIC programs
**Status:** 16 fixture files auto-discovered

```
tests/
├── compatibility.rs           # QB64pe-style test harness
├── common/mod.rs              # Shared test utilities
└── fixtures/
    ├── success/               # Tests that should compile successfully
    │   ├── hello.bas + .output
    │   ├── for_step.bas + .output
    │   ├── select_case.bas + .output
    │   ├── while_wend.bas + .output
    │   ├── do_loop.bas + .output
    │   ├── sub_function.bas + .output
    │   ├── data_read.bas + .output
    │   ├── array_ops.bas + .output
    │   └── math_expr.bas + .output
    └── error/                 # Tests that should produce errors
        ├── type_mismatch.bas + .err
        ├── undefined_var.bas + .err
        └── duplicate_definition.bas + .err
```

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
Implemented in `tests/integration_tests.rs` with 148 tests covering:
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

### Phase 3: Compatibility Testing ✅ STARTED

#### 3.1 QB64pe-Style Test Format ✅
Implemented in `tests/compatibility.rs`:
- Auto-discovers `.bas` files in `tests/fixtures/`
- Matches `.output` files for success tests
- Matches `.err` files for error tests
- 16 fixture files currently

#### 3.2 Future: Port QB4.5 Test Cases
From `QB64pe/tests/qbasic_testcases/qb45com/`:
- Many programs require unimplemented features (string functions, INKEY$, etc.)
- Will port as features are implemented

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

### Phase 5: Advanced Testing (Future)

#### 5.1 Property-Based Testing
Using `proptest` crate (not yet implemented):
```rust
proptest! {
    #[test]
    fn lexer_never_panics(s: String) {
        let _ = qb64fresh::lexer::lex(&s);
    }
}
```

#### 5.2 Fuzzing with `cargo-fuzz`
Not yet implemented. Fuzz targets would be:
- Lexer (random strings → never panic)
- Parser (random token sequences → never panic)
- Full pipeline (random source → meaningful errors or success)

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
| Crate | Purpose | Add to Cargo.toml |
|-------|---------|-------------------|
| `cargo-fuzz` | Fuzz testing | `cargo install cargo-fuzz` |

---

## Success Metrics

### Phase 3 Goals 🔄 IN PROGRESS
- [ ] 50+ QB4.5 compatibility tests passing (blocked by unimplemented features)
- [ ] Automated comparison with QB64PE output

### Phase 4 Goals ✅ BENCHMARKS + PROPTEST + FUZZING COMPLETE
- [ ] 80%+ line coverage (currently 59.92%)

---

## Implementation Checklist

### Short Term (Next 2 Weeks) ✅ MOSTLY COMPLETE
- [ ] Port more QB4.5 test cases (as features are implemented)

### Medium Term (Next Month) 🔄 IN PROGRESS
- [ ] Port 50+ compatibility tests

### Long Term (Next Quarter)
- [ ] Set up continuous fuzzing with `cargo-fuzz`
- [ ] Achieve 80%+ coverage
- [ ] All QB4.5 compatibility tests passing

---

## Quick Reference: Running Tests

```bash
# Run all tests
cargo test --workspace

# Run specific test suites
cargo test --test integration_tests    # 239 integration tests
cargo test --test golden_tests         # 10 golden tests
cargo test --test compatibility        # 16 fixture tests
cargo test --test proptest_tests       # 19 property-based tests

# Run unit tests only
cargo test --lib

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
