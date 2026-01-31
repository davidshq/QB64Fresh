# CMEM, mem_lock/_MEM, qbs Compatibility Status

**Status:** Optional — only if QB64pe binary compatibility is required (2026-01-31)

This document records the current status of CMEM, mem_lock/_MEM, and qbs-compatible string system for Option B (complete runtime library). These are **optional** and only needed when compiling/linking QB64pe IDE or other QB64pe binaries with QB64Fresh + our runtime.

## Current State

### Inline runtime (generated C)

- **CMEM:** The inline runtime in `src/codegen/c_backend/runtime/` emits a `qb_cmem` buffer, `cmem` macro, `qb_defseg`/`dblock`, and DEF SEG–style addressing when legacy mode is used. See generated golden C (e.g. `tests/golden/function.golden`) for `QB_CMEM_SIZE`, `qb_cmem`, `cmem`, `qb_defseg`, `dblock`.
- **mem_lock / _MEM:** The inline runtime emits `qb_mem` struct and helpers: `qb_memnew`, `qb_memfree`, `qb_memget`, `qb_memput`, `qb_memcopy`, `qb_memfill`, `qb_mem_of`, `qb_memexists`, `qb_memelement`, typed get/put/fill, etc. These provide _MEM-style handles and operations for generated code.
- **qbs:** QB64Fresh uses its own string representation (qb_string, qb_str_from_c, etc.). It is **not** binary-compatible with QB64pe’s qbs layout, tmp pool, or fixed-length string ABI.

### External runtime (libqb64fresh_rt)

- The Rust runtime (`runtime/`) does not currently expose a QB64pe-identical CMEM region or qbs layout. Memory and string APIs are sufficient for programs compiled by QB64Fresh and linked with `libqb64fresh_rt`, but not for dropping in as a full libqb replacement in QB64pe builds without further work.

## When to Implement Full Compatibility

- **CMEM / mem_lock / _MEM:** Already present in generated inline C. Full parity with QB64pe (same sizes, offsets, and behavior) is only required if we need to run unmodified QB64pe-generated object files or share memory layouts.
- **qbs compatibility:** Required only if:
  - QB64pe IDE (or other QB64pe-compiled code) is built with QB64Fresh as the compiler but still links against QB64pe’s runtime, or
  - We aim to replace QB64pe’s runtime DLL/so with libqb64fresh_rt without recompiling QB64pe-generated code.

If the goal is “QB64Fresh compiles BASIC and links with libqb64fresh_rt,” the current design is sufficient. If the goal is “QB64pe binary compatibility (same .o/.obj and runtime ABI),” then qbs layout, tmp pool, CMEM layout, and _MEM behavior would need to be aligned with QB64pe (see Tier 1 in TODO_CONSOLIDATED.md).

## Reference

- **TODO_CONSOLIDATED.md:** Tier 1 (qbs, CMEM), Phases 5–7.
- **Inline runtime:** `src/codegen/c_backend/runtime/` (memory, legacy).
- **Golden C:** `tests/golden/*.golden` (search for `cmem`, `qb_mem`, `qb_cmem`).
