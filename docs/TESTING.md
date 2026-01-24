# Testing Guide for QB64Fresh

**Last Updated:** 2026-01-23

This document provides a comprehensive guide to the testing infrastructure for QB64Fresh, a modern BASIC compiler written in Rust.

---

## Table of Contents

- [Overview](#overview)
- [Test Types](#test-types)
- [Running Tests](#running-tests)
- [Test Organization](#test-organization)
- [Writing Tests](#writing-tests)
- [Test Utilities](#test-utilities)
- [Coverage and Metrics](#coverage-and-metrics)
- [Troubleshooting](#troubleshooting)
- [Best Practices](#best-practices)

---

## Overview

QB64Fresh uses a **multi-layered testing strategy** to ensure correctness and compatibility:

- **Unit tests** - Test individual components in isolation
- **Integration tests** - Test the full compilation pipeline
- **Golden tests** - Compare generated code against known-good outputs
- **Execution tests** - Compile and run programs to verify runtime behavior
- **Property-based tests** - Fuzz inputs to find edge cases
- **Compatibility tests** - Verify QB64/QBasic compatibility

### Current Test Statistics

| Test Suite | Count | Status | Purpose |
|------------|-------|--------|---------|
| **Compiler unit tests** | 388 | ✅ passing | Test individual modules |
| **Integration tests** | 718 | ✅ passing | End-to-end compilation |
| **Golden tests** | 10 | ✅ passing | Codegen regression detection |
| **Property-based** | 19 | ✅ passing | Input fuzzing |
| **Compatibility** | 3 | ✅ passing | Local fixture tests |
| **Runtime tests** | 194 | ✅ passing | Runtime library testing |
| **Execution tests** | 27 | ✅ passing | Full compile-and-run tests |
| **QB64pe compatibility** | 141 files | 97.9% pass | External test suite |

*Use `UPDATE_GOLDEN=1 cargo test golden` to regenerate golden files after intentional codegen changes.*

**Total: 1,500+ tests** across the compiler and runtime library.

---

## Test Types

### 1. Unit Tests

**Location:** `src/**/*.rs` (in `#[cfg(test)]` modules)

**Purpose:** Test individual functions, methods, and modules in isolation.

**Example:**
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_basic() {
        let tokens = lex("PRINT \"Hello\"");
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].kind, TokenKind::Print);
    }
}
```

**Running:**
```bash
# All unit tests
cargo test -p qb64fresh --lib

# Specific module
cargo test --lib lexer::tests

# Specific test
cargo test --lib test_lexer_basic
```

### 2. Integration Tests

**Location:** `tests/integration_tests.rs`

**Purpose:** Test the full compilation pipeline from source code to generated C output.

**Structure:**
- Tests are organized by feature area (expressions, statements, control flow, etc.)
- Each test compiles BASIC source and verifies the generated C code
- Tests both success and error cases

**Example:**
```rust
#[test]
fn hello_world() {
    let source = r#"PRINT "Hello, World!""#;
    assert_compiles(source);
}

#[test]
fn undefined_variable() {
    let source = "PRINT x";
    assert_compile_error(source, "undefined");
}
```

**Running:**
```bash
# All integration tests
cargo test --test integration_tests

# Specific test
cargo test --test integration_tests hello_world
```

### 3. Golden Tests

**Location:** `tests/golden_tests.rs` and `tests/golden/`

**Purpose:** Detect unintended changes to code generation by comparing against "golden" (expected) output files.

**How it works:**
1. Each test has a `.bas` source file and a `.golden` expected output
2. The test compiles the source and compares against the golden file
3. If they differ, the test fails showing a diff

**Updating golden files:**
When making intentional changes to code generation:
```bash
UPDATE_GOLDEN=1 cargo test --test golden_tests
```

This updates all `.golden` files to match current output.

**Running:**
```bash
# Run golden tests
cargo test --test golden_tests

# Update golden files after intentional changes
UPDATE_GOLDEN=1 cargo test --test golden_tests
```

### 4. Execution Tests

**Location:** `tests/execution_tests.rs`

**Purpose:** Verify that programs not only compile correctly, but also execute and produce expected output.

**Process:**
1. Compile BASIC source to C code
2. Compile the C code to an executable (linking with runtime)
3. Run the executable
4. Verify the output matches expectations

**Example:**
```rust
#[test]
fn print_hello() {
    let source = r#"PRINT "Hello""#;
    let output = compile_and_run(source).unwrap();
    assert_eq!(output.trim(), "Hello");
}
```

**Running:**
```bash
cargo test --test execution_tests
```

**Note:** These tests require a C compiler (gcc/clang) and may be slower than other tests.

### 5. Property-Based Tests

**Location:** `tests/proptest_tests.rs`

**Purpose:** Use fuzzing to find edge cases and verify properties hold across many inputs.

**Framework:** Uses the `proptest` crate for property-based testing.

**Example:**
```rust
proptest! {
    #[test]
    fn test_expression_parsing(s in r"[a-zA-Z0-9+\-*/() ]+") {
        // Test that various expression strings parse without panicking
        let tokens = lex(&s);
        let mut parser = Parser::new(&tokens);
        let _ = parser.parse_expression(Precedence::Lowest);
    }
}
```

**Running:**
```bash
cargo test --test proptest_tests
```

### 6. Compatibility Tests

**Location:** `tests/compatibility.rs` and `tests/qb45_compat.rs`

**Purpose:** Verify compatibility with QB64/QBasic programs.

**Types:**
- **Local fixtures** (`tests/compatibility.rs`) - Small test cases in the repo
- **QB64pe test suite** (`tests/qb45_compat.rs`) - Runs against 141 programs from QB64pe

**Running:**
```bash
# Local compatibility tests
cargo test --test compatibility

# QB64pe compatibility suite (requires large stack)
RUST_MIN_STACK=8388608 cargo test --test qb45_compat

# With verbose output
cargo test --test qb45_compat -- --nocapture
```

### 7. Runtime Tests

**Location:** `runtime/src/**/*.rs` (in `#[cfg(test)]` modules)

**Purpose:** Test the runtime library (string handling, I/O, graphics, audio, etc.)

**Running:**
```bash
# All runtime tests
cargo test -p qb64fresh-runtime --lib

# With mock backends only (for CI)
cargo test -p qb64fresh-runtime --lib --no-default-features --features "graphics-mock audio-mock"

# With real backends
cargo test -p qb64fresh-runtime --lib --features "graphics-sdl2 audio-rodio"
```

---

## Running Tests

### Quick Reference

```bash
# Run all tests
cargo test

# Run only compiler tests
cargo test -p qb64fresh

# Run only runtime tests
cargo test -p qb64fresh-runtime

# Run specific test suite
cargo test --test integration_tests
cargo test --test golden_tests
cargo test --test execution_tests
cargo test --test proptest_tests
cargo test --test compatibility

# Run specific test
cargo test test_name

# Run tests with output
cargo test -- --nocapture

# Run tests in parallel (default) or sequentially
cargo test -- --test-threads=1

# Run only failing tests (after a failure)
cargo test -- --failed
```

### Test Filtering

```bash
# Run tests matching a pattern
cargo test parser          # All tests with "parser" in name
cargo test --lib parser::  # All parser module tests

# Run tests in a specific module
cargo test --lib lexer::tests
cargo test --lib semantic::checker::tests
```

### Verbose Output

```bash
# Show test output even for passing tests
cargo test -- --nocapture

# Show more detailed output
RUST_LOG=debug cargo test -- --nocapture

# Show test timing
cargo test -- --nocapture --show-output
```

### Benchmarks

```bash
# Run all benchmarks
cargo bench

# Run specific benchmark
cargo bench -- "lexer"

# Generate HTML report
cargo bench -- --html
```

---

## Test Organization

### Directory Structure

```
QB64Fresh/
├── src/                          # Source code
│   ├── lexer/
│   │   └── mod.rs                # Contains #[cfg(test)] mod tests
│   ├── parser/
│   │   └── mod.rs                # Contains #[cfg(test)] mod tests
│   └── ...
├── tests/                        # Integration tests
│   ├── common/
│   │   └── mod.rs                # Shared test utilities
│   ├── fixtures/                 # Test fixtures
│   │   ├── success/              # Valid programs with expected output
│   │   │   ├── *.bas             # BASIC source files
│   │   │   └── *.output          # Expected stdout
│   │   └── error/                # Error cases with expected messages
│   │       ├── *.bas             # Invalid programs
│   │       └── *.err             # Expected error output
│   ├── golden/                   # Golden test files
│   │   ├── *.bas                 # Source files
│   │   └── *.golden              # Expected compiler output
│   ├── compatibility.rs          # Local compatibility tests
│   ├── execution_tests.rs        # Full compile-and-run tests
│   ├── golden_tests.rs           # Golden file testing
│   ├── integration_tests.rs      # Integration tests (720 tests)
│   ├── proptest_tests.rs         # Property-based tests
│   └── qb45_compat.rs            # QB64pe compatibility suite
└── runtime/                      # Runtime library
    └── src/
        └── **/*.rs               # Contains #[cfg(test)] mod tests
```

### Test Module Organization

**Unit tests** are co-located with the code they test:
```rust
// src/lexer/mod.rs
pub fn lex(source: &str) -> Vec<Token> {
    // ... implementation
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        // ... test code
    }
}
```

**Integration tests** are organized by feature area:
```rust
// tests/integration_tests.rs
mod expressions {
    // Expression-related tests
}

mod statements {
    // Statement-related tests
}

mod control_flow {
    // Control flow tests
}
```

---

## Writing Tests

### Writing Unit Tests

1. **Create a `#[cfg(test)]` module** in the same file as your code:
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_my_function() {
        let result = my_function();
        assert_eq!(result, expected_value);
    }
}
```

2. **Test both success and error cases:**
```rust
#[test]
fn test_success_case() {
    assert_eq!(parse("123"), Ok(123));
}

#[test]
fn test_error_case() {
    assert!(parse("invalid").is_err());
}
```

3. **Use descriptive test names:**
```rust
// Good
#[test]
fn test_parse_integer_with_leading_zeros() { }

// Bad
#[test]
fn test1() { }
```

### Writing Integration Tests

1. **Use the helper functions** from `tests/common/mod.rs`:
```rust
use tests::common::*;

#[test]
fn my_integration_test() {
    assert_compiles("PRINT \"Hello\"");
    assert_compile_error("PRINT x", "undefined");
}
```

2. **Test the full pipeline:**
```rust
#[test]
fn test_feature() {
    let source = r#"
        DIM x AS INTEGER
        x = 42
        PRINT x
    "#;
    
    // Should compile successfully
    assert_compiles(source);
    
    // Or verify specific generated code
    let c_code = compile_to_c(source).unwrap();
    assert!(c_code.contains("int32_t x"));
}
```

3. **Group related tests:**
```rust
mod my_feature {
    use super::*;

    #[test]
    fn test_basic() { }

    #[test]
    fn test_edge_case() { }
}
```

### Writing Golden Tests

1. **Create source and golden files:**
```bash
# Create tests/golden/my_test.bas
PRINT "Hello"

# Create tests/golden/my_test.golden (initial content from first run)
```

2. **Add the test:**
```rust
#[test]
fn my_test() {
    run_golden_test("my_test");
}
```

3. **Update golden files after intentional changes:**
```bash
UPDATE_GOLDEN=1 cargo test --test golden_tests my_test
```

### Writing Execution Tests

1. **Use the `compile_and_run` helper:**
```rust
#[test]
fn test_program_output() {
    let source = r#"PRINT "Hello""#;
    let output = compile_and_run(source).unwrap();
    assert_eq!(output.trim(), "Hello");
}
```

2. **Handle errors appropriately:**
```rust
#[test]
fn test_program_fails() {
    let source = "PRINT undefined_var";
    let result = compile_and_run(source);
    assert!(result.is_err());
}
```

---

## Test Utilities

### Common Test Helpers

Located in `tests/common/mod.rs`:

- **`compile_to_c(source: &str) -> Result<String, String>`** - Compile BASIC to C
- **`compile_with_mode(source: &str, mode: RuntimeMode) -> Result<String, String>`** - Compile with specific runtime mode
- **`assert_compiles(source: &str)`** - Assert compilation succeeds
- **`assert_compile_error(source: &str, expected: &str)`** - Assert compilation fails with expected error
- **`token_count(source: &str) -> usize`** - Count tokens in source
- **`statement_count(source: &str) -> Result<usize, String>`** - Count statements

### Example Usage

```rust
use tests::common::*;

#[test]
fn test_compilation() {
    // Test successful compilation
    assert_compiles("PRINT \"Hello\"");

    // Test compilation error
    assert_compile_error("PRINT x", "undefined");

    // Get generated C code
    let c_code = compile_to_c("DIM x AS INTEGER").unwrap();
    assert!(c_code.contains("int32_t"));
}
```

---

## Coverage and Metrics

### Code Coverage

**Tool:** `cargo-llvm-cov` (or `cargo-tarpaulin`)

**Running coverage:**
```bash
# Install tool
cargo install cargo-llvm-cov

# Generate coverage report
cargo llvm-cov --workspace

# Generate HTML report
cargo llvm-cov --workspace --html
# Open target/llvm-cov/html/index.html
```

**Target:** Aim for 80%+ coverage on critical paths.

### Test Metrics

Current test statistics (as of 2026-01-23):

- **Total tests:** 1,500+
- **Compiler unit tests:** 388
- **Integration tests:** 718
- **Runtime tests:** 194
- **Execution tests:** 27
- **Property-based tests:** 19
- **Golden tests:** 10
- **Compatibility:** 141 files (97.9% pass)

### Benchmarking

**Tool:** `criterion` (configured in `Cargo.toml`)

**Running benchmarks:**
```bash
cargo bench                    # All benchmarks
cargo bench -- "lexer"        # Specific benchmark group
```

**Benchmark files:** `benches/compiler_benchmarks.rs`

---

## Troubleshooting

### Common Issues

#### 1. Tests Fail Due to Golden File Mismatch

**Symptom:** Golden tests fail with diff output.

**Solution:** If the change is intentional:
```bash
UPDATE_GOLDEN=1 cargo test --test golden_tests
```

If the change is unintentional, investigate the codegen changes.

#### 2. Execution Tests Fail with "Command not found"

**Symptom:** `gcc` or `clang` not found.

**Solution:** Install a C compiler:
```bash
# Ubuntu/Debian
sudo apt-get install build-essential

# macOS
xcode-select --install

# Or use clang
sudo apt-get install clang
```

#### 3. QB64pe Compatibility Tests Require Large Stack

**Symptom:** Stack overflow when running `qb45_compat` tests.

**Solution:** Increase stack size:
```bash
RUST_MIN_STACK=8388608 cargo test --test qb45_compat
```

#### 4. Tests Pass Locally but Fail in CI

**Common causes:**
- Missing system dependencies (C compiler, libraries)
- Different feature flags
- Platform-specific behavior
- Timing issues

**Solution:** Check `.github/workflows/ci.yml` for CI configuration.

#### 5. Property-Based Tests Take Too Long

**Symptom:** `proptest` tests run for a very long time.

**Solution:** Reduce the number of test cases:
```rust
proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]  // Default is 256
    #[test]
    fn my_test(...) { }
}
```

### Debugging Test Failures

1. **Run with verbose output:**
```bash
cargo test -- --nocapture
```

2. **Run a specific test:**
```bash
cargo test test_name -- --nocapture
```

3. **Use a debugger:**
```bash
# Set breakpoint in test
rust-gdb --args cargo test test_name
```

4. **Check test output files:**
Some tests write temporary files to `/tmp` or `target/` - check these for clues.

---

## Best Practices

### 1. Test Organization

- **Co-locate unit tests** with the code they test
- **Group integration tests** by feature area
- **Use descriptive test names** that explain what is being tested
- **Keep tests focused** - one assertion per test when possible

### 2. Test Data

- **Use fixtures** for complex test data (see `tests/fixtures/`)
- **Prefer inline strings** for simple test cases
- **Use golden files** for large expected outputs

### 3. Assertions

- **Use specific assertions:**
```rust
// Good
assert_eq!(result, expected_value);
assert!(result.contains("expected"));

// Less good
assert!(result == expected_value);
```

- **Provide helpful error messages:**
```rust
assert_eq!(
    result, expected,
    "Failed to parse expression: {}",
    source
);
```

### 4. Test Independence

- **Tests should not depend on each other**
- **Tests should be runnable in any order**
- **Tests should not rely on external state**

### 5. Performance

- **Keep unit tests fast** (< 1ms each)
- **Integration tests can be slower** but should complete in seconds
- **Use `#[ignore]` for slow tests** that aren't run in normal development:
```rust
#[test]
#[ignore]  // Only run with --ignored
fn slow_compatibility_test() { }
```

### 6. Error Testing

- **Test both success and error paths**
- **Verify error messages are helpful**
- **Test edge cases and boundary conditions**

### 7. Documentation

- **Document complex test logic** with comments
- **Explain why a test exists** if it's not obvious
- **Reference related issues** or requirements

---

## References

- **Testing Infrastructure Plan:** `docs/ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md`
- **ADR-0005:** `docs/adrs/ADR-0005-testing-framework.md`
- **Rust Testing Book:** https://doc.rust-lang.org/book/ch11-00-testing.html
- **Criterion Documentation:** https://bheisler.github.io/criterion.rs/book/
- **Proptest Documentation:** https://altsysrq.github.io/proptest-book/

---

*For questions or issues with testing, see the [Development Guide](DEVELOPMENT.md) or open an issue on GitHub.*
