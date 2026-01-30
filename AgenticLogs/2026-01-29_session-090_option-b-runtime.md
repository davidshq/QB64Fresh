# Session 090: Option B Runtime (Error-Pending, evnt, RESUME)

**Date:** 2026-01-29  
**Focus:** Implement OPTION_B_IMPLEMENTATION_PLAN Phase 1–2 and Phase 4 (minimum).

## Summary

Implemented the minimum Option B runtime features from [OPTION_B_IMPLEMENTATION_PLAN.md](OPTION_B_IMPLEMENTATION_PLAN.md):

1. **Phase 1.1 – Error-pending system**  
   Runtime now exposes:
   - `qb_error_pending()` – non-zero if an error is pending
   - `qb_set_error(code, line)` – set pending error (e.g. from failed OPEN)
   - `qb_clear_error()` – clear pending (RESUME NEXT)
   - `qb_commit_error()` – copy pending to ERR/ERL then clear (call before goto handler)

2. **Phase 2.1 – Debug event hooks**  
   - Global `qbevent` (uint32_t) and `qb_evnt(line, incline, incfile)` (no-op) for future IDE/debugger.

3. **Phase 4.1 – RESUME support**  
   Codegen for RESUME NEXT and RESUME &lt;label&gt; now emits `qb_clear_error()` when `--runtime external`.

4. **Runtime failures set error**  
   `qb_file_open` on failure now calls `qb_set_error(53, 0)` (File not found).

5. **External runtime ERROR statement**  
   Inline `qb_error(code)` for external mode now calls `qb_set_error((uint32_t)code, 0)` instead of exit.

## Files Changed

- `runtime/src/lib.rs` – error state (NEW_ERROR, NEW_ERROR_LINE), qb_error_pending, qb_set_error, qb_clear_error, qb_commit_error; qbevent, qb_evnt; test.
- `runtime/include/qb64fresh_rt.h` – declarations for error-pending and evnt APIs.
- `runtime/src/io/file.rs` – on open failure call `crate::qb_set_error(53, 0)`.
- `src/codegen/c_backend/stmt/error_jump.rs` – RESUME NEXT / RESUME label emit `qb_clear_error()` when external.
- `src/codegen/c_backend/runtime/mod.rs` – external `qb_error()` calls `qb_set_error` instead of exit.
- `docs/OPTION_B_IMPLEMENTATION_PLAN.md` – marked Step 1.1, 2.1, 4.1 and success criteria (partial) done.

## Verification

- `cargo build -p qb64fresh-runtime` – succeeds.
- `cargo test -p qb64fresh-runtime test_error_pending_clear_commit` – (run to confirm).

## Review (same session)

- **Bug (fixed):** `qb_errormessage()` for unknown error codes used `format!("Error {}", code).as_ptr()` — Rust `String` is not null-terminated, so `qb_string_new`/`CStr::from_ptr` could read past the buffer. Replaced with `CString::new(...)` and pass `.as_ptr()` so the C string is null-terminated.
- **Improvement:** Added error 53 => "File not found" to the message table (OPEN failure).
- **Documentation:** Noted in codegen that RESUME (retry) with external runtime does not set `_qb_error_line` until Phase 1.3.

## Step 1.2 (same session) — Set error from runtime code

- **File I/O** (`runtime/src/io/file.rs`): When OPEN has invalid fnum or null/empty filename → 52 or 64. When FIELD/PRINT #/INPUT #/EOF/LOF/LOC have bad file number or file not open → 52. When PRINT # has no writer or INPUT # has no reader → 54.
- **Memory** (`runtime/src/memory.rs`): When _MEMNEW has invalid size or layout → 5; when alloc returns null → 7.
- **Error messages** (`runtime/src/lib.rs`): Added 52 Bad file number, 54 Bad file mode, 57 Device I/O error, 62 Input past end, 64 Bad file name to `qb_errormessage()`.
- **Plan:** Marked Step 1.2 done in `docs/OPTION_B_IMPLEMENTATION_PLAN.md`.

## Step 7.1 — String API audit (same session)

- **Task:** List string-related symbols in `qb64pe_fresh.c`; ensure each is in `qb64fresh_rt.h` + runtime or inlined in codegen.
- **Result:** All symbols accounted for. Runtime provides `qb_string_*`, `qb_left`, `qb_right`, `qb_lcase`, `qb_string_compare`, `qb_print_string`, `qb_file_open`, `qb_font_get`, etc. Generated C defines inline: `qbs_tmp_base_get`, `qbs_tmp_register`, `qbs_cleanup`, `qb_len_str`, `qb_mid_assign`, `qb_instr2`, `qb_controlchr`, `qb_file_open_legacy`, `qb_mki`, `qb_string_fill_str`, and many QB64pe helpers. No `qbs_set`/`qbs_add`; codegen uses `qb_string_*` and tmp pool; kept as-is.
- **Verification:** Link `qb64pe_fresh.c` with `libqb64fresh_rt` succeeds (no undefined symbols). `./qb64pe_fresh --help` and `./qb64pe_fresh -v` show correct string output. Step 7.1 marked done in `docs/OPTION_B_IMPLEMENTATION_PLAN.md`.

## Review again (same session) — Error handling consistency

- **Gap (fixed):** Several file I/O functions that take `fnum` did not set `qb_set_error(52/54)` on bad file number or file not open, unlike PRINT #/INPUT #/EOF/LOF/LOC. Updated for consistency:
  - **qb_file_seek**, **qb_file_seek_record**: set 52 for bad fnum or file not open.
  - **qb_file_get**, **qb_file_get_string**: set 52 for bad fnum or file not open; 54 if no reader.
  - **qb_file_put**, **qb_file_put_string**: set 52 for bad fnum or file not open; 54 if no writer.
  - **qb_file_write_char**: set 52 for bad fnum or file not open; 54 if no writer.
- **Verified:** Error codes 5, 7, 52, 53, 54, 57, 62, 64 are all present in `qb_errormessage()`. 57 and 62 are reserved for future use (device I/O, input past end). Return values after `qb_set_error` are defined (e.g. qb_eof/qb_lof/qb_loc return -1/0; qb_memnew returns default QbMem).
- **Build:** `cargo build -p qb64fresh-runtime` succeeds; no new lints in `runtime/src/io/file.rs`.

## Review again (third pass) — Remaining gaps

- **Gap (fixed):** Two more file I/O functions were missing error handling:
  - **qb_file_write_string**: now sets 52 for bad fnum, 52 for file not open, 54 if no writer; null `s` still returns without setting error.
  - **qb_file_write_number**: now sets 52 for bad fnum, 52 for file not open, 54 if no writer.
- **qb_file_open_str**: null `filename` now sets 52 (Bad file number) before returning, matching qb_file_open’s handling of null/empty args.

## Next Steps (Optional)

- ~~Phase 1.3: Codegen emit `if (qb_error_pending()) { qb_commit_error(); goto handler; }` after OPEN and other failing calls.~~ **Done (2026-01-30).** Added `emit_error_pending_goto_handler()` in `stmt/error_jump.rs`; called after all file I/O (OPEN, CLOSE, PRINT #, WRITE #, INPUT #, LINE INPUT #, GET, PUT, SEEK) and system ops (KILL, RENAME, MKDIR, RMDIR, CHDIR, SHELL) when `--runtime external`. Inline runtime unchanged (no-op).
- Phase 2.2: Codegen wrap statements in do { …; if (!qbevent) break; qb_evnt(line, file); } while (r).
- Phase 8: Full QB64pe IDE build and run test.
