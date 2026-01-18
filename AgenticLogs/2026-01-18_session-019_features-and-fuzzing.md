# Session 019: Language Features and Fuzz Testing Infrastructure

**Date:** 2026-01-18
**Focus:** Implementing remaining language features (A-E) and testing infrastructure (F-G)

## Summary

Implemented multiple language features and set up fuzz testing infrastructure:
- SYSTEM statement
- Labeled DATA syntax for RESTORE
- File I/O test coverage
- Built-in functions (TIMER, DATE$, TIME$, TRIM$)
- Console INPUT tests
- Fuzz testing infrastructure with cargo-fuzz

## Test Status After This Session

- **Integration tests:** 128 tests (0 ignored)
- **Golden tests:** 10 tests (updated)
- **Property tests:** 19 tests
- **Runtime tests:** 37 tests
- **Benchmarks:** 30 benchmarks
- **Line coverage:** 59.92% (up from 56.53%)
- **Fuzz targets:** 3 (lexer, parser, full pipeline)

## Accomplishments

### 1. SYSTEM Statement

Added full support for SYSTEM (exit program immediately):
- **Lexer:** Added `TokenKind::System` token
- **AST:** Added `StatementKind::System`
- **Parser:** Added `parse_system()`
- **Semantic:** Added type checking
- **Codegen:** Emits `exit(0);`
- Enabled previously ignored test

### 2. Labeled DATA Syntax

Fixed label parsing for DATA statements:
- Labels are now parsed when an identifier is followed by a colon
- Added lookahead check in `parse_identifier_statement()`
- Pattern: `mydata:` becomes `StatementKind::Label { name: "mydata" }`
- RESTORE can now target labeled DATA statements
- Enabled previously ignored test

### 3. File I/O Test Coverage

Added 8 integration tests for file I/O:
- `open_for_output`, `open_for_input`, `open_for_append`
- `print_to_file`, `input_from_file`, `line_input_from_file`
- `close_all_files`, `write_to_file`

File I/O was already implemented; tests confirm it works.

### 4. Built-in Functions

Added/completed runtime implementations for:
- **TIMER** - Returns seconds since midnight (float, with millisecond precision)
  - Windows: Uses `GetLocalTime` via windows.h
  - Unix: Uses `gettimeofday` via sys/time.h
- **DATE$** - Returns date in MM-DD-YYYY format (classic QBasic)
- **TIME$** - Returns time in HH:MM:SS format
- **TRIM$** - Registered in semantic analyzer (runtime already existed as qb_trim)

Added 4 new tests for these functions.

### 5. Console INPUT Tests

Added 6 integration tests for console INPUT:
- `input_single_variable`, `input_with_prompt`
- `input_multiple_variables`, `input_without_question_mark`
- `line_input_statement`, `line_input_with_prompt`

Console INPUT was already implemented; tests confirm it works.

### 6. Fuzz Testing Infrastructure

Created `fuzz/` directory with:
- `Cargo.toml` - Fuzz target configuration
- `fuzz_targets/fuzz_lexer.rs` - Lexer fuzzing
- `fuzz_targets/fuzz_parser.rs` - Parser fuzzing
- `fuzz_targets/fuzz_full_pipeline.rs` - Full compilation pipeline fuzzing
- `.gitignore` - Excludes artifacts

**Usage:**
```bash
# Install cargo-fuzz (requires nightly Rust)
rustup install nightly
cargo +nightly install cargo-fuzz

# Run a fuzz target
cd fuzz
cargo +nightly fuzz run fuzz_lexer
```

## Technical Insights

### TIMER Implementation

Cross-platform time measurement:
```c
#ifdef _WIN32
// Windows - GetLocalTime provides millisecond resolution
SYSTEMTIME st;
GetLocalTime(&st);
return st.wHour * 3600.0f + st.wMinute * 60.0f + st.wSecond + st.wMilliseconds / 1000.0f;
#else
// Unix - gettimeofday provides microsecond resolution
struct timeval tv;
gettimeofday(&tv, NULL);
struct tm* tm = localtime(&tv.tv_sec);
return tm->tm_hour * 3600.0f + tm->tm_min * 60.0f + tm->tm_sec + tv.tv_usec / 1000000.0f;
#endif
```

### Label Parsing

Added lookahead check for label syntax:
```rust
// Check for label: identifier followed by colon (e.g., "myLabel:")
if let Some(next) = self.peek_ahead(1) && next.kind == TokenKind::Colon {
    let name_token = self.advance().expect("identifier");
    let name = name_token.text.to_string();
    self.advance(); // consume the colon
    return Ok(Statement::new(StatementKind::Label { name }, span));
}
```

## Files Modified

- `src/lexer/token.rs` - Added SYSTEM token
- `src/ast/stmt.rs` - Added System statement kind
- `src/parser/statements.rs` - Added parse_system(), label parsing
- `src/semantic/typed_ir.rs` - Added System typed statement
- `src/semantic/checker/statements.rs` - Added System type checking
- `src/semantic/mod.rs` - Added TRIM$ registration
- `src/codegen/c_backend/stmt.rs` - Added SYSTEM codegen
- `src/codegen/c_backend/expr.rs` - Added TIMER, DATE$, TIME$, TRIM$ mappings
- `src/codegen/c_backend/runtime.rs` - Added qb_timer, qb_date, qb_time functions
- `tests/integration_tests.rs` - Added 18 new tests
- `tests/golden/*.golden` - Updated for new runtime
- `docs/ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md` - Updated status
- `fuzz/` - New fuzz testing directory

## Files Created

- `fuzz/Cargo.toml`
- `fuzz/.gitignore`
- `fuzz/fuzz_targets/fuzz_lexer.rs`
- `fuzz/fuzz_targets/fuzz_parser.rs`
- `fuzz/fuzz_targets/fuzz_full_pipeline.rs`

## Commands Reference

```bash
# Run integration tests
cargo test --test integration_tests

# Run specific test module
cargo test --test integration_tests console_input

# Update golden tests
UPDATE_GOLDEN=1 cargo test --test golden_tests

# Check coverage
cargo llvm-cov --workspace

# Run fuzz tests (requires nightly)
cd fuzz && cargo +nightly fuzz run fuzz_lexer
```
