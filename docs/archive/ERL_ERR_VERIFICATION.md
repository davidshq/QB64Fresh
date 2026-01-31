# ERL / ERR / _ERRORLINE / _ERRORMESSAGE$ Verification

**Status:** Verified 2026-01-31 (Phase 4.1)

This document records verification that the QB64 error-inquiry features are implemented end-to-end.

## Summary

| BASIC | Purpose | C runtime | Codegen | Builtins |
|-------|---------|-----------|---------|----------|
| `ERR` | Error code of last error | `qb_err_code()` | `expr.rs` → `qb_err_code` | `builtins.rs` |
| `ERL` | Line number of last error | `qb_err_line()` | `expr.rs` → `qb_err_line` | `builtins.rs` |
| `_ERRORLINE` | Line number (64-bit) | `qb_errorline()` | `expr.rs` → `qb_errorline` | `builtins.rs` |
| `_ERRORMESSAGE$` | Error message string | `qb_errormessage()` | `expr.rs` → `qb_errormessage` | `builtins.rs` |

## Implementation Locations

### Semantic (builtins)

- **File:** `src/semantic/builtins.rs`
- **Registration:** `ERR` and `ERL` (no args, Integer); `_ERRORLINE` (no args, Long); `_ERRORMESSAGE$` (no args, String).

### Code generation

- **File:** `src/codegen/c_backend/expr.rs`
- **Mapping:** Built-in function names are mapped to C runtime names (e.g. `"ERR"` → `"qb_err_code"`).

### Runtime (inline and external)

- **File (inline):** `src/codegen/c_backend/runtime/error.rs` — emits C declarations and definitions for `_qb_err`, `_qb_erl`, `qb_err_code()`, `qb_err_line()`, `qb_errorline()`, `qb_errormessage()`, and error message table.
- **External:** `runtime/include/qb64fresh_rt.h` and `runtime/src/lib.rs` expose the same API for linking.

### Error handling flow

- **File:** `src/codegen/c_backend/stmt/error_jump.rs` — ON ERROR handler and RESUME set/use `_qb_err` and `_qb_erl` so that `ERR` and `ERL` are correct in the handler and after RESUME.

## Verification

- Builtins: registered with correct types.
- Codegen: expression emitter maps each name to the corresponding C function.
- Runtime: error module emits all four C symbols; external runtime declares and defines them.
- Error jump: handler and RESUME paths document that ERR/ERL are set for the handler.

No additional code changes required for 4.1; verification complete.
