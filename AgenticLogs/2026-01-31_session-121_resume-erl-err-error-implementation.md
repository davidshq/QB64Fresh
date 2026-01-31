# Session 121: RESUME, ERL, ERR/ERROR – Full Implementation

**Date:** 2026-01-31  
**Scope:** QB64PE_MISSING_FEATURES.md items 114–118 (error handling).

## Summary

Implemented and documented RESUME (retry), RESUME NEXT, RESUME label, ERL, and ERR/ERROR so they are fully supported and aligned with QB64 behavior.

## Changes

### 1. RESUME (retry) and RESUME label

- **emit_error_pending_goto_handler** now takes an optional `retry_label: Option<&str>`. When provided (external runtime), emitted code sets `_qb_error_line = &&retry_label` before `qb_commit_error(); goto *_qb_error_handler`, so the handler can later do RESUME (retry) and jump back to that label.
- All call sites that can trigger an error (file_io: OPEN, CLOSE, LOCK, UNLOCK, PRINT #, WRITE #, INPUT #, LINE INPUT #, GET, PUT, SEEK; system: KILL, RENAME, MKDIR, RMDIR, CHDIR, SHELL) now emit a unique retry label before the failing call and pass it to `emit_error_pending_goto_handler`.
- **emit_resume** for RESUME (no target) and RESUME 0: emit `if (_qb_error_line) { void* _r = _qb_error_line; _qb_error_line = NULL; goto *_r; }` so retry works and the pointer is cleared after use.
- **emit_error_stmt**: before `qb_error(code)` we emit a retry label and set `_qb_error_line = &&retry_label` when jumping to the handler (inline and external), so RESUME (retry) after ERROR retries the ERROR statement.

### 2. RESUME NEXT and RESUME label – ERR/ERL preserved

- Inline runtime: removed `_qb_err = 0` (and any _qb_erl clear) on RESUME NEXT and RESUME label so ERR/ERL keep the last error (QB64 behavior).
- External runtime: RESUME NEXT still calls `qb_clear_error()` (clears pending only; ERR/ERL remain set). RESUME label also calls `qb_clear_error()` then `goto label`.

### 3. ERR / ERL / ERROR

- **ERR** and **ERL**: Already mapped to `qb_err_code()` and `qb_err_line()` (external) and `_qb_err` / `_qb_erl` (inline). No code change; behavior confirmed and documented.
- **ERROR** statement: Already emitted as `qb_error(code)` plus jump to handler; now also sets `_qb_error_line` for RESUME (retry).

### 4. File I/O and system codegen

- File I/O helpers that call `emit_error_pending_goto_handler` now take `&mut self`, generate a retry label with `next_label("err_retry")`, emit the label line, then pass `Some(&retry_label)` to the handler.

### 5. Tests and docs

- **QB64PE_MISSING_FEATURES.md**: RESUME, RESUME NEXT, RESUME label, ERL, and ERR/ERROR marked as ✅ Implemented with short notes.
- **integration_tests.rs**: Added tests for ERR/ERL (`qb_err_code`/`qb_err_line`), RESUME NEXT, RESUME (retry) and RESUME label, and ERROR statement.

## Files touched

- `src/codegen/c_backend/stmt/error_jump.rs` – retry_label in handler, RESUME behavior, ERROR stmt retry label.
- `src/codegen/c_backend/file_io.rs` – retry labels and `&mut self` for all I/O helpers that use the error handler.
- `src/codegen/c_backend/stmt/system.rs` – retry labels for KILL, RENAME, MKDIR, RMDIR, CHDIR, SHELL.
- `docs/ThingsToDo/QB64PE_MISSING_FEATURES.md` – status table updated.
- `tests/integration_tests.rs` – new tests for ERR, ERL, RESUME, ERROR.

## Notes

- ERL (error line) is set by the runtime when it calls `qb_set_error(code, line)`; many runtime call sites pass `line = 0`. Source-line mapping would require codegen to emit line info at each statement and is not done here.
- RESUME (retry) after a runtime-originated error (e.g. OPEN failure) retries from the label immediately before the failing call. After ERROR n, RESUME (retry) retries the ERROR statement.
