# Session 018: String Functions and RND/RANDOMIZE Implementation

**Date:** 2026-01-18
**Focus:** Wiring up string functions and implementing RND/RANDOMIZE

## Summary

Implemented missing string functions and RND/RANDOMIZE support:
- Fixed LEN function parsing (keyword vs identifier issue)
- Updated type signatures for string functions to use LONG instead of INTEGER
- Added RANDOMIZE statement support (parsing, semantic analysis, codegen)
- Updated RND to work without arguments (like QB64)
- Enabled 5 previously ignored tests

## Test Status After This Session

- **Integration tests:** 110 tests (2 ignored)
- **Golden tests:** 10 tests (updated)
- **Property tests:** 19 tests
- **Runtime tests:** 37 tests
- **Benchmarks:** 30 benchmarks

## Accomplishments

### 1. Fixed LEN Function Parsing

**Problem:** LEN is tokenized as a keyword (for `OPEN...LEN=n` syntax), but it's also a function. The parser wasn't recognizing `LEN(s)` as a function call.

**Solution:** Added `TokenKind::Len => self.parse_builtin_function("LEN")` to the expression parser to handle LEN as both a keyword and a function.

### 2. Fixed String Function Type Signatures

**Problem:** String functions like `LEFT$(s, 5)` failed with "type mismatch: expected INTEGER, found LONG" because:
- Integer literals (like `5`) default to LONG in QB64Fresh
- The function signatures expected INTEGER

**Solution:** Updated all string function registrations in `src/semantic/mod.rs` to use `BasicType::Long` instead of `BasicType::Integer`:
- CHR$, LEFT$, RIGHT$, MID$, INSTR, STRING$, SPACE$

### 3. Implemented RANDOMIZE Statement

Added full support for RANDOMIZE:
- **Lexer:** Added `TokenKind::Randomize` token
- **AST:** Added `StatementKind::Randomize { seed, use_timer }`
- **Parser:** `parse_randomize()` handles all forms:
  - `RANDOMIZE` - defaults to timer
  - `RANDOMIZE TIMER` - uses system time
  - `RANDOMIZE 12345` - uses specific seed
- **Semantic:** Type checks seed expression, warns if STRING
- **Codegen:** Emits `qb_randomize_timer()` or `qb_randomize((double)seed)`
- **C Runtime:** Added `qb_randomize()` and `qb_randomize_timer()` with xorshift64 RNG

### 4. Fixed RND Function

**Problem:** RND is registered with 1 parameter, but QB64 allows `RND` without arguments.

**Solution:**
- Changed semantic registration to 0 parameters
- Updated codegen to emit `qb_rnd(1.0f)` when called without arguments
- Updated C runtime `qb_rnd()` to use proper xorshift64 algorithm

### 5. Enabled Previously Ignored Tests

Removed `#[ignore]` from 5 tests:
- `string_functions` - LEN, LEFT$, RIGHT$, MID$, UCASE$, LCASE$
- `chr_asc_functions` - CHR$, ASC
- `instr_function` - INSTR (fixed to use 3-argument form)
- `space_string_functions` - SPACE$, STRING$
- `rnd_function` - RANDOMIZE TIMER, RND

## Technical Insights

### Keyword vs Function Ambiguity

In BASIC, some keywords are also functions:
- `LEN` - keyword in `OPEN...LEN=n`, function in `LEN(s)`
- `TIMER` - could be function or keyword

The parser handles this by checking the token kind in `parse_primary_expr()` and routing to `parse_builtin_function()` for keywords that are also functions.

### QB64 Type System

QB64 uses LONG (32-bit) as the default integer type, not INTEGER (16-bit):
- Integer literals like `42` are LONG by default
- This differs from classic QBasic
- Function signatures must accommodate this

### xorshift64 RNG

The RND implementation uses xorshift64, a fast pseudo-random number generator:
```c
qb_rng_state ^= qb_rng_state << 13;
qb_rng_state ^= qb_rng_state >> 7;
qb_rng_state ^= qb_rng_state << 17;
```

This provides good statistical properties while being very fast.

## Files Modified

- `src/lexer/token.rs` - Added RANDOMIZE token
- `src/ast/stmt.rs` - Added Randomize statement kind
- `src/parser/expressions.rs` - Added LEN as builtin function
- `src/parser/statements.rs` - Added parse_randomize()
- `src/semantic/mod.rs` - Fixed string function types, RND registration
- `src/semantic/typed_ir.rs` - Added Randomize statement kind
- `src/semantic/checker/statements.rs` - Added Randomize type checking
- `src/codegen/c_backend/expr.rs` - RND defaults to 1.0f argument
- `src/codegen/c_backend/stmt.rs` - Added RANDOMIZE codegen
- `src/codegen/c_backend/runtime.rs` - Added RNG functions
- `tests/integration_tests.rs` - Enabled 5 tests

## Golden Tests Updated

Updated all 10 golden test files due to C runtime changes (added RNG state variable and functions).

## Remaining Ignored Tests

- `program_with_system` - SYSTEM statement not implemented
- `labeled_data` - Label syntax for DATA statements not implemented

## Commands Reference

```bash
# Test string functions
cargo test --test integration_tests string_functions

# Test RND
cargo test --test integration_tests rnd_function

# Update golden tests
UPDATE_GOLDEN=1 cargo test --test golden_tests
```
