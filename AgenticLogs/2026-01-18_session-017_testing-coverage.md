# Session 017: Testing Coverage and Property-Based Testing

**Date:** 2026-01-18
**Focus:** Continuing testing infrastructure - coverage reporting and property-based testing

## Summary

Implemented remaining testing infrastructure items per TESTING_INFRASTRUCTURE_PLAN.md:
- Set up CI coverage reporting with cargo-llvm-cov
- Measured and documented current coverage percentage (56.53%)
- Added property-based testing with proptest (19 tests)

## Test Status After This Session

- **Integration tests:** 110 tests (7 ignored)
- **Golden tests:** 10 tests
- **Compatibility tests:** 3 test functions discovering 16 fixtures
- **Property-based tests:** 19 tests (new)
- **Runtime tests:** 37 tests
- **Benchmarks:** 30 benchmarks
- **Line coverage:** 56.53%

**Total:** 350+ tests across the workspace

## Accomplishments

### 1. CI Coverage Reporting

Added a new `coverage` job to `.github/workflows/ci.yml`:
- Installs `llvm-tools-preview` component
- Uses `taiki-e/install-action@cargo-llvm-cov` for fast installation
- Generates LCOV format coverage report
- Uploads to Codecov (if configured)
- Saves coverage artifact for download

### 2. Coverage Measurement

Measured current line coverage at **56.53%**:

| Module Category | Coverage |
|-----------------|----------|
| Lexer | 90%+ (excellent) |
| AST | 100% |
| Parser (core) | 65-95% |
| Parser (directives) | 18% (low) |
| Semantic (core) | 85%+ |
| Semantic (statements) | 20% (low) |
| Codegen (runtime) | 99% |
| Codegen (statements) | 36% |
| Runtime (string) | 86% |
| Runtime (FFI) | 6-10% (expected) |

**High coverage modules:** lexer, ast, codegen analysis, c_backend runtime
**Low coverage modules:** statements.rs, directives.rs, FFI modules

### 3. Property-Based Testing with Proptest

Created `tests/proptest_tests.rs` with 19 tests:

**Randomized property tests:**
- `lexer_never_panics` - Arbitrary binary input
- `lexer_handles_basic_like_input` - BASIC-like random code
- `lexer_handles_printable_ascii` - Printable ASCII strings
- `parser_never_panics_on_basic_input` - Random BASIC code parsing
- `parser_handles_arbitrary_input` - Any input parsing
- `full_pipeline_never_panics` - Complete compilation
- `handles_long_programs` - Up to 100 statements
- `handles_nested_expressions` - Deep nesting (up to 20 levels)
- `handles_string_edge_cases` - Various string contents
- `handles_long_identifiers` - Up to 256 character identifiers
- `handles_boundary_numbers` - i64 boundary values

**Deterministic edge case tests:**
- `empty_input_doesnt_panic`
- `null_byte_in_string_doesnt_panic`
- `unicode_input_doesnt_panic`
- `very_long_line_doesnt_panic`
- `many_newlines_doesnt_panic`
- `unbalanced_parens_doesnt_panic`
- `unterminated_string_doesnt_panic`
- `repeated_keywords_doesnt_panic`

### 4. Updated Testing Infrastructure Plan

Updated TESTING_INFRASTRUCTURE_PLAN.md with:
- New test counts (350+ total)
- Coverage metrics (56.53%)
- Property testing as Tier 6
- Updated checklists and status indicators
- Added coverage commands to quick reference

## Technical Insights

### Property-Based Testing Benefits
- Tests verify safety against **any** input, not just chosen examples
- proptest generates thousands of random inputs per test
- When failures are found, proptest **shrinks** to find minimal failing case
- Catches edge cases developers rarely think of (null bytes, unicode, deep nesting)

### Coverage Analysis
- FFI modules have low coverage because they're primarily tested via integration
- Statement handlers have lower coverage because many branches handle edge cases
- The `c_backend/runtime.rs` has 99% coverage because it's mostly string literals
- Parser directives have low coverage because $IF, $LET, etc. aren't heavily tested

## Files Modified

- `.github/workflows/ci.yml` - Added coverage job
- `Cargo.toml` - Added proptest dependency
- `docs/ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md` - Updated progress

## Files Created

- `tests/proptest_tests.rs` - 19 property-based tests

## Commands Reference

```bash
# Run property tests
cargo test --test proptest_tests

# Run with more iterations
PROPTEST_CASES=10000 cargo test --test proptest_tests

# Coverage commands
cargo llvm-cov --workspace             # Console summary
cargo llvm-cov --workspace --html      # HTML report
cargo llvm-cov --workspace --lcov      # LCOV format

# View HTML coverage report
open target/llvm-cov/html/index.html
```
