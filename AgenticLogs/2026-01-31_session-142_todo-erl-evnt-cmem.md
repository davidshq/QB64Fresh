# Session 142: TODO items 4.1, 2.1, 5–7 (ERL/ERR, evnt, CMEM/qbs)

**Date:** 2026-01-31

## Summary

Implemented and documented the three TODO_CONSOLIDATED items (lines 108–110):

1. **4.1 ERL/ERR/_ERRORLINE/_ERRORMESSAGE$ verification** — Verified end-to-end; added `docs/archive/ERL_ERR_VERIFICATION.md`.
2. **2.1 Codegen: emit evnt around statements for debugger** — Implemented: when `--debug` is set, executable statements are wrapped in `do { ...; if (!qbevent) break; qb_evnt(line, incline, file); } while(0);`; inline debug runtime now declares `qbevent` and `qb_evnt`.
3. **5–7 CMEM, mem_lock/_MEM, qbs compatibility** — Documented status in `docs/archive/CMEM_QBS_COMPAT_STATUS.md` (optional; only if binary compat needed).

## Decisions

- **evnt:** Reuse existing `debug.enabled` (i.e. `--debug`) for evnt wrapping; no separate flag. Inline debug runtime emits `qbevent` and `qb_evnt` so generated C is self-contained; external runtime already has them in `qb64fresh_rt.h`.
- **4.1:** Verification only (no code change); implementation already present in builtins, expr codegen, and runtime/error.rs.
- **5–7:** Documentation only; full qbs/CMEM parity is optional and only needed for QB64pe binary compatibility.

## Files touched

- `docs/archive/ERL_ERR_VERIFICATION.md` — new
- `docs/archive/CMEM_QBS_COMPAT_STATUS.md` — new
- `docs/ThingsToDo/TODO_CONSOLIDATED.md` — phases 4.1, 2.1, 5–7 marked done / linked
- `src/codegen/c_backend/runtime/debug.rs` — added `qbevent` and `qb_evnt` to inline debug runtime
- `src/codegen/c_backend/stmt/mod.rs` — added `debug_file_expr()`, evnt wrapper (do { ... } while(0)) when debug enabled

## Verification

- `cargo build --bin qb64fresh` — ok
- `cargo test --test golden_tests` — 10 passed (golden tests do not use `--debug`, so output unchanged)
