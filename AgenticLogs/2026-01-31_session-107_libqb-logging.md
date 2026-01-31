# Session 107 — Scoped Logging (libqb logging.h)

**Date:** 2026-01-31  
**Focus:** Implement LIBQB section 22 (logging.h — Scoped Logging) per LIBQB_FUNCTIONALITY.md lines 167–176.

## Summary

- Implemented full libqb-style scoped logging: `loglevel` (Trace, Information, Warning, Error), `logscope` (Runtime, QB64, Libqb, Audio, Image).
- Runtime: Rust module `logging` + `logging_ffi` (libqb_log_init, qb_log_message, libqb_log_qbs); init wired into qb_runtime_init.
- Header: QB_LOGLEVEL_* / QB_LOGSCOPE_* macros, function declarations, libqb_log_with_scope_* and libqb_log_trace/info/warn/error macros.
- Inline runtime: codegen emits full C implementation (libqb_log, libqb_log_qb64, libqb_log_qbs, va_list/vsnprintf, macros).
- External runtime: optional C shim `runtime/c_src/logging.c` implements variadic libqb_log/libqb_log_qb64 calling qb_log_message (compile and link when using external runtime).

## Changes

### Runtime

- **`runtime/src/logging.rs`**: New module — LogLevel/LogScope, init, log_message, min_level (atomic).
- **`runtime/src/logging_ffi.rs`**: New module — libqb_log_init, qb_log_message, libqb_log_qbs; C-callable.
- **`runtime/src/lib.rs`**: pub mod logging, logging_ffi; re-exports; qb_runtime_init() calls logging::init().
- **`runtime/include/qb64fresh_rt.h`**: New “Scoped Logging” section — QB_LOGLEVEL_* / QB_LOGSCOPE_* #defines, libqb_log_init, libqb_log, libqb_log_qb64, libqb_log_qbs, qb_log_message, qb_log_set_min_level, qb_log_get_min_level; macros libqb_log_with_scope_* and libqb_log_trace/info/warn/error.
- **`runtime/c_src/logging.c`**: New file — variadic libqb_log and libqb_log_qb64 (vsnprintf → qb_log_message). For external runtime: compile and link this file.

### Codegen

- **`src/codegen/c_backend/runtime/logging.rs`**: New module — emit_logging() emits C for loglevel/logscope #defines, libqb_log_init, _qb_log_write, libqb_log, libqb_log_qb64, libqb_log_qbs, and all macros.
- **`src/codegen/c_backend/runtime/mod.rs`**: mod logging; emit_logging() in emit_runtime_declarations; #include <stdarg.h> for va_list.

### Docs

- **`docs/QB64pe/LIBQB_FUNCTIONALITY.md`**: Section 22 (logging.h) — all items marked 🟢.

## Verification

- `cargo build -p qb64fresh-runtime` and `cargo build -p qb64fresh` succeed.
