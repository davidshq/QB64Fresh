# Session 107 — qblist.h Handle List

**Date:** 2026-01-31

## Summary

Implemented qblist.h handle list (thread-safe list) in QB64Fresh runtime: `list_new`, `list_new_threadsafe`, `list_destroy`, `list_add`, `list_remove`, `list_get`, `list_get_index`.

## Decisions

- **Opaque list:** C header declares `struct list` with no fields; implementation is in Rust. Matches QB64pe API (list* only) without exposing internal layout.
- **Thread-safe:** `list_new_threadsafe` uses a single `Mutex` to guard add/remove (QB64pe uses separate lock_add/lock_remove).
- **Semantics:** Index 0 unused; indices 1-based. Each slot is [user_data | stored_index]. Reuse freed slots before growing; grow by doubling capacity.

## Changes

- **runtime/src/list.rs** (new): Handle list implementation (List, grow_and_append, list_add_impl, list_remove_impl, and C FFI for list_new, list_new_threadsafe, list_destroy, list_add, list_remove, list_get, list_get_index).
- **runtime/src/lib.rs**: Added `mod list` and `pub use list::*`.
- **runtime/include/qb64fresh_rt.h**: Added "Handle List (qblist.h)" section with `struct list` and the seven function declarations.
- **docs/QB64pe/LIBQB_FUNCTIONALITY.md**: Section 24 — all list functions marked ✅.
- **docs/QB64pe/LIBQB_FUNCTIONALITY_COMPLETED.md**: Added section 24 with 🟢 for all list functions.

## Notes

- Runtime crate currently has pre-existing build errors in `logging_ffi.rs` and `audio_ffi.rs`; list module itself compiles and has no linter errors.
- Inline codegen runtime does not emit list_*; list API is for external runtime (e.g. future graphics/sound handle tables).
