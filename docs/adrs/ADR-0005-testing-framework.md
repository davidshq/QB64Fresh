# ADR-0005: Testing Framework

## Status

**Accepted** - January 16, 2026

## Context

A compiler must be thoroughly tested to ensure correctness and compatibility. QB64pe has an extensive test suite that we should leverage while also building unit tests for our architecture.

Key considerations:
- Need unit tests for individual components
- Need integration tests for end-to-end compilation
- Must ensure QB64pe compatibility
- Should support test-driven development
- Need performance benchmarking capability

## Decision

**We chose Rust's built-in testing framework combined with QB64pe's test suite**.

### Testing Strategy

1. **Unit tests**: Each compiler phase tested in isolation
   - Use Rust's `#[test]` attribute
   - `#[cfg(test)]` modules keep tests close to code
   - Test both success and error cases

2. **Integration tests**: End-to-end compilation
   - Tests in `tests/` directory
   - Compile sample programs start to finish
   - Verify generated code correctness

3. **Compatibility tests**: Run QB64pe test suite
   - Port relevant tests from QB64pe
   - Verify behavior matches expected output
   - Track compatibility progress

4. **Golden tests**: Compare output against expected results
   - Store expected outputs for programs
   - Detect regressions automatically
   - Version control expected results

### Key Test Categories (from QB64pe to port)

- `tests/compile_tests/` - Feature-specific compilation tests
- `tests/qbasic_testcases/` - Classic QBasic program compatibility

### Rationale

1. **Built-in testing is sufficient**: No need for external framework
2. **Tests near code**: `#[cfg(test)]` modules improve maintainability
3. **Cargo integration**: `cargo test` runs all tests automatically
4. **QB64pe suite ensures compatibility**: Proven test coverage
5. **Benchmarking available**: `cargo bench` for performance testing

### Alternatives Considered

- **External test framework**: Unnecessary complexity
- **Only unit tests**: Insufficient coverage
- **Only integration tests**: Harder to isolate failures
- **Custom test harness**: Reinventing the wheel

## Consequences

### Positive

- No external dependencies for testing
- Tests run in parallel automatically
- Easy to write and run tests
- QB64pe suite provides comprehensive compatibility coverage
- Doc tests validate examples in documentation
- Benchmarks track performance regressions
- Can filter tests by name or module
- CI integration is straightforward

### Negative

- QB64pe test suite requires porting effort
- Some QB64pe tests may need adaptation
- Rust's test output can be verbose
- No built-in test coverage tool (need external tool like `tarpaulin`)

### Current Status

**Implemented** - January 18, 2026

| Test Suite | File | Tests | Status |
|------------|------|-------|--------|
| **Unit tests** | `src/*/mod.rs` | 163 | All passing |
| **Integration tests** | `tests/integration_tests.rs` | 110 | 57 pass, 53 pending |
| **Golden tests** | `tests/golden_tests.rs` | 10 | All passing |
| **Compatibility tests** | `tests/compatibility.rs` | 3 | All passing |

**Total**: 233 passing tests (+ 53 pending for unimplemented features)

Coverage includes:
- Unit tests for each compiler module (lexer, parser, semantic, codegen)
- Integration tests for end-to-end compilation
- Golden tests for C code generation regression detection
- Compatibility test framework ready for QB64pe test porting
- Both inline and external runtime modes

### Test Organization

```
QB64Fresh/
├── src/
│   ├── lexer/
│   │   └── mod.rs              # Contains #[cfg(test)] mod tests
│   ├── parser/
│   │   └── mod.rs              # Contains #[cfg(test)] mod tests
│   ├── semantic/
│   │   └── checker/*.rs        # Contains #[cfg(test)] mod tests
│   ├── codegen/
│   │   └── c_backend/*.rs      # Contains #[cfg(test)] mod tests
│   └── ...
├── tests/
│   ├── integration_tests.rs    # 110 end-to-end compilation tests
│   ├── golden_tests.rs         # Golden/snapshot test framework
│   ├── compatibility.rs        # QB64pe-format test framework
│   ├── common/
│   │   └── mod.rs              # Shared test utilities
│   ├── fixtures/
│   │   ├── success/            # Tests expected to compile
│   │   │   └── *.bas, *.output
│   │   └── error/              # Tests expected to fail
│   │       └── *.bas, *.err
│   └── golden/
│       └── *.bas, *.golden     # Golden test fixtures
└── runtime/
    └── src/
        └── lib.rs              # Contains #[cfg(test)] mod tests
```

### Running Tests

```bash
# Run all tests
cargo test

# Run specific test suites
cargo test --lib                    # Unit tests
cargo test --test integration_tests # Integration tests
cargo test --test golden_tests      # Golden tests
cargo test --test compatibility     # Compatibility tests

# Update golden files after intentional code generation changes
UPDATE_GOLDEN=1 cargo test --test golden_tests
```
