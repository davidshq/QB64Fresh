# Session 060: Parser Module Unit Tests

**Date:** 2026-01-25  
**Scope:** Implement CODEBASE_REVIEW_CONSOLIDATED Low Priority #2 and Recommended Action #4 — add explicit unit tests for parser modules (graphics, audio, system, file_io, and `parser/statements/` submodules).

## Summary

- **file_io_tests:** `test_parse_open_legacy` for legacy `OPEN mode$, [#]filenum, filename[, reclen]` (OpenFileLegacy). Covers `parse_open_legacy` in `parser/file_io.rs`.
- **statements_tests** (new `mod` in `parser/tests.rs`): 12 tests for `parser/statements/`:
  - **assignments:** SWAP, MID$=, LSET, RSET
  - **data_dims:** RESTORE, RESTORE label
  - **control_etc:** SLEEP, SLEEP n, ERASE
  - **declare:** DECLARE SUB, DECLARE SUB with params, DECLARE FUNCTION

Graphics, audio, and system already had `graphics_tests`, `audio_tests`, and `system_tests` with broad coverage; no new tests added there.

## Outcomes

- 12 new tests in `statements_tests`; 1 new test in `file_io_tests`.
- `cargo test --lib parser::tests`: 196 passed, 1 ignored.
- CODEBASE_REVIEW_CONSOLIDATED: Low Priority #2 and Recommended #4 marked addressed/done.
