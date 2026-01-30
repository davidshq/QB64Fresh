# Session 099: ERR/ERL in ON ERROR handler + File I/O (LINE INPUT #) fix

**Date:** 2026-01-30

## Goal

1. **ERR/ERL in ON ERROR handler** — Ensure QB64Fresh sets error state and jumps to the handler so `PRINT "handler: ERR="; ERR; " ERL="; ERL` shows correct values.
2. **File I/O (LINE INPUT #)** — Fix `05_file_io.bas` so output is `first: line1` / `second: line2` instead of `first: first: ` / `second: second: `.

## Accomplished

### 1. ERR/ERL in ON ERROR handler

- **Cause:** After `ERROR 5`, we emitted `qb_error(5)` but never jumped to the handler. Execution continued to the next statement, so the handler never ran and ERR/ERL were never printed.
- **Fix (codegen):** In `src/codegen/c_backend/stmt/error_jump.rs` `emit_error_stmt`, after `qb_error(code)` we now emit the jump:
  - **Inline runtime:** `if (_qb_error_handler) { goto *_qb_error_handler; }`
  - **External runtime:** `if (qb_error_pending()) { if (_qb_error_handler) { qb_commit_error(); goto *_qb_error_handler; } }`
- **Fix (inline runtime):** In `src/codegen/c_backend/runtime/error.rs`, `qb_error()` now sets `_qb_erl = 0` so ERL is defined when the handler runs.
- **Result:** `03_on_error_resume_next.bas` now prints `before`, then `handler: ERR=5 ERL=0`, then RESUME NEXT continues.

### 2. File I/O (LINE INPUT #)

- **Cause:** In the **inline** runtime, `qb_file_line_input` did `*s = qb_string_new(buf)`. `qb_string_new()` registers the string in the temp pool. A later `qbs_cleanup()` released that string, so `s` became a dangling pointer. The next `qb_string_new("first: ")` (for the PRINT) reused that memory, so `s` appeared to contain `"first: "`.
- **Fix:** In `src/codegen/c_backend/runtime/file.rs`:
  - **LINE INPUT #:** Before assigning, release old `*s`; then assign `*s = qb_string_retain(qb_string_new(buf))` so the variable holds a reference and cleanup does not free it.
  - **INPUT # (string):** Same pattern for `qb_file_input_string`: release old `*s`, then `*s = qb_string_retain(qb_string_new(buf))`.
- **Result:** `05_file_io.bas` now prints `first: line1`, `second: line2`, `done`.

## Files touched

- `src/codegen/c_backend/stmt/error_jump.rs` — emit jump to handler after ERROR statement
- `src/codegen/c_backend/runtime/error.rs` — set `_qb_erl` in `qb_error()`
- `src/codegen/c_backend/runtime/file.rs` — LINE INPUT # and INPUT # string: release old, retain new

## Decisions

- ERR/ERL: Jump is emitted in codegen so it works for both inline and external runtime; external uses `qb_commit_error()` before goto so ERR/ERL are committed.
- File I/O: Retain on assign so the variable owns a reference and temp-pool cleanup does not free the LINE INPUT # / INPUT # result.
