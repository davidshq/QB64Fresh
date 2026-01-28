# Code Generation Write Helpers

The write helpers module (`src/codegen/c_backend/write_helpers.rs`) provides error-handling wrappers around Rust's `write!` and `writeln!` macros to ensure consistent error handling throughout code generation.

## Purpose

While writing to a `String` should never fail in practice, returning `Result` ensures:
- Consistent error handling patterns across codegen
- Future-proofing for potential streaming refactor (writing to `Write` trait)
- Follows Rust best practices (no `unwrap()` in production code)

## API

### Functions

```rust
/// Writes formatted data to a string, returning an error if writing fails.
pub fn write_code(
    output: &mut String,
    args: std::fmt::Arguments<'_>
) -> Result<(), CodeGenError>

/// Writes formatted data followed by a newline to a string.
pub fn writeln_code(
    output: &mut String,
    args: std::fmt::Arguments<'_>
) -> Result<(), CodeGenError>
```

### Macros

```rust
/// Macro for writing formatted code with error handling.
write_code!(output, "int {} = {};", var_name, value)?;

/// Macro for writing formatted code with newline and error handling.
writeln_code!(output, "int x = 5;")?;
```

## Usage

The helpers are used throughout codegen:
- `src/codegen/c_backend/stmt/mod.rs` - Statement emission
- `src/codegen/c_backend/expr.rs` - Expression emission
- `src/codegen/c_backend/runtime/mod.rs` - Runtime code generation

### Example

```rust
// Before:
write!(output, "int {} = {};", var, val).unwrap();

// After:
write_code!(output, "int {} = {};", var, val)?;
```

## Design Decisions

1. **Error Handling:** While writing to a `String` should never fail in practice, returning `Result` ensures consistent error handling patterns across codegen

2. **Macro Wrapper:** The macros (`write_code!`, `writeln_code!`) provide drop-in replacements for `write!`/`writeln!`

3. **Inline Functions:** Both functions are marked `#[inline]` for zero-cost abstraction

## Current Limitations

- Still uses `String` accumulation (not streaming)
- All code must be generated before writing (no incremental output)
- Memory usage grows with program size

## Future Enhancements

The module is designed to be extended for streaming:

```rust
// Potential future API:
pub fn write_code<W: Write>(
    writer: &mut W,
    args: std::fmt::Arguments<'_>
) -> Result<(), CodeGenError> {
    writer.write_fmt(args).map_err(|e| {
        CodeGenError::new(CodeGenErrorKind::IoError(format!("failed to write: {}", e)))
    })
}
```

This would allow:
- Streaming output directly to files
- Better memory usage for large programs
- Incremental code generation

## Impact

- ✅ Better error handling in codegen (no `unwrap()` calls)
- ✅ Foundation for future streaming refactor
- ✅ Consistent error handling patterns
- ✅ Reduces `unwrap()` usage in codegen

---

*Last updated: 2026-01-28*
