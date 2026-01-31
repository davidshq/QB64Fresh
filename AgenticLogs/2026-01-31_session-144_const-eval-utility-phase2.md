# Session 144: Phase 2 Const Eval Utility – Full Pass

**Date:** 2026-01-31  
**Goal:** Resolve remaining errors for Phase 2 Const Eval Utility so it passes (QB64Fresh + C compile).

## Summary

Phase 2 Const Eval Utility (`02_utilities_const_eval.bas`) was marked PARTIAL with “Dependencies resolved, some errors remain.” The remaining errors were **C compilation** conflicts: the generated C (external runtime) included `qb64fresh_rt.h`, which declares functions like `qb_environ`, `qb_base64encode`, etc. with **`const QbString*`** parameters, while the codegen’s external-runtime helpers emitted definitions with **`QbString*`** (non-const). The compiler reported “conflicting types” for those functions.

## Approach

Per project rules: **fix the code generator, not the generated C.** All changes were in the codegen’s external-runtime path.

1. **Root cause:** In `src/codegen/c_backend/runtime/mod.rs`, the External runtime branch emits helper implementations (qb_environ, qb_base64encode, qb_base64decode, qb_deflate, qb_inflate, qb_adler32, qb_crc32, qb_md5, qb_len_str, qb_fullpath, qb_instr2, qb_mid2, qb_asc2, qb_cvl, qb_cvi, qb_readfile, qb_writefile, qb_val_uint64, qb_val_int64). Those helpers were emitted with `QbString*` for read-only string parameters, while `qb64fresh_rt.h` declares them with `const QbString*`.

2. **Fix:** For every such helper, read-only string parameters were changed from `QbString*` to `const QbString*` in the emitted C so the definitions match the header.

3. **qb_string_retain:** After the const change, `qb_fullpath` returned `qb_string_retain(path)` where `path` is `const QbString*`. The header declares `qb_string_retain(QbString* s)`, so the compiler warned about discarding the const qualifier. The emitted code was updated to `qb_string_retain((QbString*)path)` in both `#ifdef _WIN32` and `#else` branches (retain does not modify the string; the cast is safe).

## Files Touched

- **`src/codegen/c_backend/runtime/mod.rs`**  
  External-runtime helpers: all read-only string parameters now use `const QbString*`; `qb_fullpath` uses `(QbString*)path` when calling `qb_string_retain`.

## Verification

- `cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_const_eval.bas --emit-c -o tests/qb64pe_incremental/02_utilities_const_eval.c` → success.
- `gcc -c tests/qb64pe_incremental/02_utilities_const_eval.c -I runtime/include -o /tmp/02_const_eval.o` → **0 errors, 0 warnings.**

## Doc Updates

- **`docs/archive/QB64PE_INCREMENTAL_TESTING.md`**  
  Phase 2 Const Eval Utility status updated from PARTIAL to **PASSES**; notes updated to describe the const-qualifier fix.

## Result

Phase 2 Const Eval Utility now **passes**: QB64Fresh emits C successfully and the generated C compiles cleanly with the external runtime header. All Phase 2 utilities (hash, type, elements, const_eval) are now passing.
