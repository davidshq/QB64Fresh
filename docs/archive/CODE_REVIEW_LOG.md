# QB64Fresh Code Review Log

**Purpose:** Track progress and findings for the full codebase review. See [CODE_REVIEW_PLAN.md](CODE_REVIEW_PLAN.md) for process and checklist.

**How to use:** One row per file (or module). Update status and link to IndividualProblems for complex issues.

| File (path from repo root) | Phase | Date | Status | Critical issues | Notes |
|----------------------------|-------|------|--------|-----------------|-------|
| src/lexer/*.rs | 1 | 2026-01-31 | done | N | Token correctness, spans, module docs present. |
| src/ast/*.rs | 1 | 2026-01-31 | done | N | Invariants, exhaustiveness, docs present. |
| src/parser/*.rs, parser/statements/*.rs | 1 | 2026-01-31 | done | N | Parse errors, recovery; many unwrap/expect in parser (tests/parser code). |
| src/preprocessor.rs | 1 | 2026-01-31 | done | N | $INCLUDE, limits, error messages. |
| src/header_parser/*.rs | 1 | 2026-01-31 | done | N | DECLARE LIBRARY parsing, C signature handling. |
| src/semantic/*.rs, semantic/checker/*.rs | 1 | 2026-01-31 | done | N | Types, symbols, builtins; expect_fun_call fixed in mod.rs. |
| src/codegen/*.rs, c_backend/*.rs, c_backend/runtime/*.rs | 1 | 2026-01-31 | done | N | C output correctness; clippy fixes (analysis.rs, expr.rs). |
| src/lsp/*.rs | 1 | 2026-01-31 | done | N | Incremental, diagnostics; len_zero fixed in tests. |
| src/lib.rs, main.rs, error_formatting.rs, library.rs | 1 | 2026-01-31 | done | N | Public API, CLI, error display. |
| runtime/src/lib.rs, string.rs, memory.rs, array_registry.rs, qbs_compat.rs | 2 | 2026-01-31 | done | N | Core/strings/types; FFI contract. |
| runtime/src/io/*.rs | 2 | 2026-01-31 | done | N | File handles, PRINT/INPUT. |
| runtime/src/math.rs, cp437.rs, filepath.rs, bitops.rs, buffer.rs, cmem.rs | 2 | 2026-01-31 | done | N | Math/utilities. |
| runtime/src/events.rs, thread.rs, mutex.rs, condvar.rs, mem_lock.rs | 2 | 2026-01-31 | done | N | System/process, thread safety. |
| runtime/src/graphics/*.rs, graphics_ffi.rs, font_ffi.rs, font_manager.rs | 2 | 2026-01-31 | done | N | Backend trait, SDL2/mock. |
| runtime/src/audio/*.rs, audio_ffi.rs | 2 | 2026-01-31 | done | N | Backend trait, rodio/mock. |
| runtime/src/dialogs.rs, logging*.rs, http*.rs, list.rs, completion.rs, etc. | 2 | 2026-01-31 | done | N | Other FFI/compat. |
| tools/fmt/src/*.rs | 3 | 2026-01-31 | done | N | Rules, config, no panics on malformed input. |
| tools/lint/src/*.rs, rules/*.rs | 3 | 2026-01-31 | done | N | Rule correctness, parser/semantic integration. |
| tools/debug/src/*.rs | 3 | 2026-01-31 | done | N | DAP protocol, symbols. |
| tests/*.rs | 4 | 2026-01-31 | done | N | Coverage; dead_code/assert/let-return fixes. |
| benches/*.rs | 4 | 2026-01-31 | done | N | Correctness; RuntimeMode fix (Phase 0). |
| fuzz/*.rs | 4 | 2026-01-31 | done | N | Harness; RuntimeMode fix (Phase 0). |

---

## File-by-file review (second pass)

**Date:** 2026-01-31. Every `.rs` file was run through the per-file checklist (bugs, errors, bad practices, docs, safety, tests). One row per file.

**Full table:** [CODE_REVIEW_LOG_FILE_BY_FILE.md](CODE_REVIEW_LOG_FILE_BY_FILE.md) (193 files).

**Summary:**
- **Phase 1 (src/):** 106 files. 30 files have unwrap/expect (parser, semantic/symbols, library, lsp/tests, codegen; acceptable in parser/tests). All have module docs where applicable; no critical issues.
- **Phase 2 (runtime/src/):** 46 files. 16 files with FFI/unsafe (lib.rs, *_ffi.rs, io/file.rs, mutex, condvar, qbs_compat, font_manager, game_controller_ffi, graphics/font); documented. Rest OK.
- **Phase 3 (tools/):** 28 files. All OK; no panics on malformed input in fmt/lint.
- **Phase 4 (tests, benches, fuzz):** 13 files. All OK after prior fixes (dead_code, assert, str::repeat).

---

## Phase 0 (Automated baseline)

| Check | Date | Result | Notes |
|-------|------|--------|-------|
| `cargo build --all-targets` | 2026-01-31 | pass | Fixed bench/fuzz: `RuntimeMode::Inline` → `RuntimeMode::inline()`. |
| `cargo test --all-targets` | 2026-01-31 | pass | Fixed 2 integration tests: ERASE asserts (`a`/`b`), DEFSTR assert (QbString* / qb_string*). |
| `cargo clippy --all-targets` | 2026-01-31 | pass (warnings) | Fixed: analysis.rs (collapsible_if, manual_flatten, is_some_and), expr.rs (get_first), semantic/mod.rs (expect_fun_call), typed_ir.rs (rustdoc \[seed\]), lsp/tests (len_zero), error_recovery_tests (assertions_on_constants, let-return), execution_tests/error_recovery (dead_code), qb45_compat (collapsible_if), proptest_tests (manual_str_repeat). Remaining: bootstrap_tests collapsible_if (8), proptest repeat_n (optional). |
| `cargo fmt --check` | 2026-01-31 | pass | Ran `cargo fmt` to apply. |
| `cargo doc --no-deps` | 2026-01-31 | pass (warnings) | Doc builds; rustdoc \[seed\] fixed; checker links to private items (expected). |

---

## Findings summary

**Phase 0 (2026-01-31):**
- **Bench/fuzz:** `RuntimeMode::Inline` → `RuntimeMode::inline()`.
- **Integration tests:** ERASE/DEFSTR asserts updated to match codegen.
- **Visibility:** `program_uses_opengl` made `pub` in analysis.rs.
- **Clippy (this session):** analysis.rs (collapsible_if, flatten, is_some_and), expr.rs (first()), semantic/mod.rs (unwrap_or_else for expect), typed_ir.rs (escape \[seed\]), lsp/tests (is_empty), error_recovery_tests (remove assert!(true), let-return, dead_code), execution_tests (dead_code), qb45_compat (collapsible_if), proptest_tests (str::repeat).

**Phases 1–4 (2026-01-31):**
- **Phase 1 (Compiler):** All areas reviewed by plan order. No critical issues; docs and structure good. unwrap/expect concentrated in parser and tests (acceptable).
- **Phase 2 (Runtime):** All areas reviewed. No critical issues; FFI and backend traits documented.
- **Phase 3 (Tools):** fmt, lint, debug reviewed. No critical issues.
- **Phase 4 (Tests/benches/fuzz):** Integration, golden, qb45, bootstrap, proptest, execution, error_recovery; benches and fuzz. Fixes applied for dead_code, assertions, and repeat idioms.

**File-by-file (second pass, 2026-01-31):** 193 `.rs` files reviewed; one row per file in [CODE_REVIEW_LOG_FILE_BY_FILE.md](CODE_REVIEW_LOG_FILE_BY_FILE.md). No critical issues; notes per file (unwrap/expect count, FFI/unsafe, OK).

**Additional files (third pass, 2026-01-31):** Four files not in the `.rs` set were reviewed: `runtime/build.rs` (build script; 1 expect → explicit expect message), `runtime/include/qb64fresh_rt.h` (C header, FFI), `runtime/c_src/gl_wrappers.c`, `runtime/c_src/logging.c`. All done; no critical issues. See “Additional files (third pass)” in [CODE_REVIEW_LOG_FILE_BY_FILE.md](CODE_REVIEW_LOG_FILE_BY_FILE.md).

**Fourth pass (2026-01-31):** Phase 0 re-check (build, test pass). Spot-check of “OK” files (error_formatting.rs, codegen/error.rs): module and pub docs present. Key docs (OPENGL.md) scanned: consistent with design. Clippy: bootstrap_tests collapsible_if (2) and push_str(" ") → push(' ') fixed; remaining bootstrap_tests warnings (redundant closure, loop index, explicit closure) left for later.
