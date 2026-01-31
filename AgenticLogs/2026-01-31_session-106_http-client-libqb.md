# Session 106 — HTTP Client (libqb_http_*)

**Date:** 2026-01-31  
**Focus:** Implement LIBQB section 19 (http.h — HTTP Client) per LIBQB_FUNCTIONALITY.md lines 201–215.

## Summary

- Implemented full libqb_http_* API in QB64Fresh runtime using reqwest (blocking + rustls).
- Added C FFI and `qb64fresh_rt.h` declarations; init/stop wired into `qb_runtime_init` / `qb_runtime_shutdown`.
- Updated LIBQB_FUNCTIONALITY.md and LIBQB_FUNCTIONALITY_COMPLETED.md.

## Changes

### Runtime

- **`runtime/Cargo.toml`**: Added optional `reqwest` (blocking, rustls-tls); default feature `http` enables it.
- **`runtime/src/http.rs`**: New module — handle table, blocking GET on open, buffer for get_length/get/get_fixed, status/effective URL/content-length. Stub `http_open` when `http` feature off.
- **`runtime/src/http_ffi.rs`**: New module — C FFI with exact libqb names: `libqb_http_init`, `libqb_http_stop`, `libqb_http_open`, `libqb_http_close`, `libqb_http_connected`, `libqb_http_get_length`, `libqb_http_get_content_length`, `libqb_http_get_status_code`, `libqb_http_get_url`, `libqb_http_get`, `libqb_http_get_fixed`.
- **`runtime/include/qb64fresh_rt.h`**: New “HTTP Client” section with declarations for all above.
- **`runtime/src/lib.rs`**: `pub mod http`, `pub mod http_ffi`, re-exports; `qb_runtime_init()` calls `http::http_init()`, `qb_runtime_shutdown()` calls `http::http_stop()`.

### Docs

- **`docs/QB64pe/LIBQB_FUNCTIONALITY.md`**: Section 19 (http.h) — all items marked 🟢.
- **`docs/QB64pe/LIBQB_FUNCTIONALITY_COMPLETED.md`**: New section 19 (http.h) with completed table.

## Design

- **Open**: Blocking GET with reqwest; full response body buffered; status, effective URL, Content-Length stored. Matches libqb “open URL, then read” semantics.
- **Connected**: Returns 1 if handle exists and not closed, 0 if closed, -1 if invalid.
- **get_url**: Effective URL stored as `CString` per handle; pointer valid until handle closed (no free by caller).
- **Redirects**: `Policy::limited(10)` (reqwest 0.12 API).

## Verification

- `cargo build -p qb64fresh-runtime` succeeds (default features include `http`).
