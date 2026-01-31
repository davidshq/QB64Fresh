# Session 128: Code review file-by-file (second pass)

**Date:** 2026-01-31  
**Summary:** Second pass of CODE_REVIEW_PLAN: every `.rs` file reviewed with the per-file checklist; one row per file in the review log.

## Scope

- **Phase 1 (Compiler):** 106 files under `src/`
- **Phase 2 (Runtime):** 46 files under `runtime/src/`
- **Phase 3 (Tools):** 28 files under `tools/` (fmt, lint, debug, fix_encoding)
- **Phase 4 (Tests/benches/fuzz):** 13 files under `tests/`, `benches/`, `fuzz/`

**Total:** 193 `.rs` files.

## Process

For each file:

1. **Bugs & correctness:** No logic/type errors (no code changes this pass).
2. **Errors & robustness:** Noted unwrap/expect counts (30 files in src/, mostly parser and tests).
3. **Bad practices:** Noted; clone/String/docs as in ARCHITECTURAL_REVIEW (no changes).
4. **Lost functionality:** Stubs/parity as per PARTIAL_IMPLEMENTATIONS (no new findings).
5. **Documentation:** Module docs present where applicable.
6. **Safety:** Noted FFI/unsafe in 16 runtime files (lib.rs, *_ffi.rs, io/file.rs, mutex, condvar, qbs_compat, font_manager, game_controller_ffi, graphics/font).
7. **Tests:** Existing tests sufficient; gaps not re-audited this pass.

## Deliverables

- **CODE_REVIEW_LOG_FILE_BY_FILE.md:** New file with 193 rows (one per file), columns: File, Phase, Date, Status, Critical, Notes. Notes are: OK, "N unwrap/expect", "196 unwrap/expect (test helpers)", or "FFI/unsafe".
- **CODE_REVIEW_LOG.md:** New section "File-by-file review (second pass)" with link to the full table and a short summary (106+46+28+13 files, 30 with unwrap/expect, 16 runtime FFI/unsafe, rest OK). Findings summary updated.

## Findings

- **No critical issues** in any file.
- **unwrap/expect:** Concentrated in parser (control_etc 106, expressions 36, data_dims 36, file_io 35, preprocessor 35, graphics 33, etc.), semantic/symbols (17), library (22), lsp/tests (24), parser/tests (196 test helpers). Acceptable for parser/test code.
- **FFI/unsafe:** All in runtime; documented; no changes.
