# Session 021: Keyword-as-Identifier Parser Fix

**Date:** 2026-01-18
**Focus:** Fix parser to allow BASIC keywords as parameter/variable names

## Problem

The parser was rejecting valid BASIC syntax like:
```basic
DECLARE SUB Greet(name AS STRING)
```

Error: `expected parameter name, found Name`

## Root Cause

The lexer tokenizes `name` as `TokenKind::Name` (the NAME statement keyword for file renaming), not as an identifier. When `parse_declare_params()` called `expect(&TokenKind::Identifier, ...)`, it failed because the token was a keyword.

This is a classic lexer ambiguity in BASIC - many keywords can also be used as identifiers. Context determines meaning.

## Solution

Added a new helper system in `src/parser/tokens.rs`:

1. **`token_kind_is_name(kind: &TokenKind) -> bool`** - Checks if a token kind can be used as an identifier name. Returns true for `Identifier` and many keywords that are commonly used as variable names (NAME, INPUT, OUTPUT, PRINT, etc.)

2. **`is_name_token(&self) -> bool`** - Instance method to check current token

3. **`expect_name(&mut self, expected_desc: &str) -> Result<&Token, ()>`** - Like `expect()` but accepts keywords that are valid as names

## Changes

- `src/parser/tokens.rs`: Added keyword-as-identifier handling helpers
- `src/parser/procedures.rs`: Changed `parse_parameter_list()` to use `expect_name()`
- `src/parser/statements.rs`: Changed `parse_declare_params()` and `parse_external_param_list()` to use `expect_name()`

## Testing

Verified parsing now works correctly:
```basic
DECLARE SUB Test(input AS INTEGER, name AS STRING, output AS DOUBLE)
DECLARE FUNCTION AddNums%(a AS INTEGER, b AS INTEGER)
SUB Greet(name AS STRING)
  PRINT "Hello"
END SUB
```

## Note on Separate Issue

During testing, noticed that implicit SUB calls without parentheses (`Greet "World"`) don't parse. This is a separate existing issue unrelated to the keyword-as-identifier fix. The explicit form `CALL Greet("World")` works correctly.

## Session 021 Continued: Expression Parser Fix

### Additional Problem Found

The initial fix only addressed parameter declarations. When using keywords like `name` as variables in expressions (e.g., `PRINT "Hello, "; name`), they still failed with "invalid expression" because the expression parser's `parse_prefix()` function only handled `TokenKind::Identifier`, not keyword tokens that can be used as names.

### Solution

Extended `parse_prefix()` in `src/parser/expressions.rs` to call `is_name_token()` for any token that isn't handled by explicit cases:

```rust
// Keywords that can be used as variable names in expression context
_ if self.is_name_token() => self.parse_identifier_or_call(),
```

This reuses the same `is_name_token()` helper that was added for parameter parsing, ensuring consistent behavior across both contexts.

### Testing Verified

Full compilation pipeline now works:
```basic
DECLARE SUB Greet(name AS STRING)
SUB Greet(name AS STRING)
  PRINT "Hello, "; name
END SUB
CALL Greet("World")
```

All tests pass (461+ tests).

## Key Insight

`★ Insight ─────────────────────────────────────`
BASIC's case-insensitivity and permissive naming means the lexer must tokenize keywords as keywords, but the parser must be context-aware about where identifiers are expected. This is why `expect_name()` and the `is_name_token()` check in expressions use a whitelist of keywords that are semantically valid in name positions, rather than accepting ALL keywords (which would break control flow parsing).

The fix required changes in TWO places:
1. Parameter declarations (initial fix in session 021)
2. Expression parsing (the `parse_prefix()` function's fallback case)

This is a common pattern when handling BASIC's keyword-as-identifier ambiguity - every place that expects an identifier needs to consider that keywords might be valid there.
`─────────────────────────────────────────────────`
