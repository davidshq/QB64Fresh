# Session 045: Windows-Specific Path Handling

**Date:** 2026-01-24

## Summary

Implemented consistent `\`→`/` path normalization on **non-Windows** for all file I/O operations that take path strings. Previously only OPEN (qb_file_open), $INCLUDE, and _FILEEXISTS/_DIREXISTS did this; KILL, NAME, MKDIR, CHDIR, legacy OPEN (qb_file_open_legacy), BLOAD, BSAVE, _READFILE$, and _WRITEFILE did not.

## Changes

### Inline C runtime (codegen)

- **`src/codegen/c_backend/runtime/system.rs`**
  - **qb_chdir, qb_mkdir, qb_file_kill:** On `#ifndef _WIN32`, `strdup` the path, `_qb_normalize_path_inplace`, call the OS function, `free`. On Windows, use the path as-is.
  - **qb_file_rename:** Added (was previously undefined in inline runtime). Same normalization for both old and new paths on non-Windows. Uses `rename()`.

- **`src/codegen/c_backend/runtime/legacy.rs`**
  - **qb_bload, qb_bsave:** After `fopen(filename, ...)` fails, on non-Windows try `_qb_normalize_path(filename)`, `fopen(norm, ...)`, `free(norm)`.

- **`src/codegen/c_backend/runtime/graphics.rs`**
  - **qb_file_open_legacy:** After initial `fopen(fname, fmode)` and after the `fopen(fname, "w+b")` fallback, on non-Windows if `!f` try `_qb_normalize_path(fname)` and `fopen(norm, ...)`.
  - **qb_readfile (_READFILE$), qb_writefile (_WRITEFILE):** After `fopen(path->data, "rb"/"wb")` fails, on non-Windows try `_qb_normalize_path(path->data)` and `fopen(norm, ...)`.

### Rust runtime (--runtime external)

- **`runtime/src/io.rs`**
  - **`normalize_path_for_fs(s: &str) -> Cow<str>`:** On `#[cfg(not(target_os = "windows"))]` replaces `\` with `/`; on Windows returns `Cow::Borrowed(s)`.
  - **qb_file_kill, qb_file_rename, qb_mkdir, qb_rmdir, qb_chdir, qb_file_exists, qb_dir_exists:** Use `normalize_path_for_fs` before passing the path to `std::fs::*` or `std::env::set_current_dir`.

### Unchanged (already normalizing)

- **$INCLUDE** (`preprocessor.rs`): `parse_include_directive` already does `path.replace('\\', "/")`.
- **OPEN** (`file.rs` qb_file_open): Already uses `_qb_normalize_path` fallback on non-Windows.
- **_FILEEXISTS / _DIREXISTS** (inline in `system.rs`): Already had a normalized-path fallback.

## Tests

- `cargo test --test golden_tests`: Pass (golden files updated for modified C output).
- `cargo test --test integration_tests "generates_ffi_call"`: KILL, NAME, MKDIR, CHDIR, RMDIR codegen tests pass.
- `cargo build -p qb64fresh-runtime`: Pass.

## FUTURE.md

- **Windows-specific path handling** item marked done (moved to completed / removed from Platform-Specific).
