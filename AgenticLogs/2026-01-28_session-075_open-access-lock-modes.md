# Session 075: OPEN File Access and Lock Modes

**Date:** 2026-01-28  
**Brief:** Implemented OPEN file access and lock modes — pass access/lock from codegen to runtime and apply flock on Unix.

## Accomplished

- **FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md item 2** — OPEN access and lock modes now passed to runtime and honored.
- **Runtime API extended:**
  - `qb_file_open(fnum, filename, mode, access, lock)` and `qb_file_open_str(..., access, lock)` in header and both runtimes.
  - Constants: `QB_FILE_ACCESS_DEFAULT/READ/WRITE/READ_WRITE`, `QB_FILE_LOCK_DEFAULT/SHARED/LOCK_READ/LOCK_WRITE/LOCK_READ_WRITE/ONLY`.
- **Codegen** (`src/codegen/c_backend/file_io.rs`): Removed “not yet implemented” comment; added `access_const_c` and `lock_const_c` helpers; emit access and lock constants on every OPEN call (default when `None`).
- **Rust runtime** (`runtime/src/io.rs`): `qb_file_open` / `qb_file_open_str` take access and lock; apply `flock` on Unix only for explicit lock modes (LOCK_READ/WRITE/READ_WRITE/ONLY); DEFAULT and SHARED = no lock (match inline C). Windows: stub (TODO LockFileEx).
- **Inline C runtime** (`src/codegen/c_backend/runtime/file.rs`): Same signature; `#include <sys/file.h>` on non-Windows; after `fopen`, call `flock(fd, LOCK_EX)` only when lock is one of LOCK_READ, LOCK_WRITE, LOCK_READ_WRITE, ONLY (not for unknown values).
- **Legacy OPEN** (`qb_file_open_legacy`): Calls `qb_file_open(..., 0, 0)` for default access/lock.

## Files Touched

- `runtime/include/qb64fresh_rt.h` — access/lock #defines and new parameters.
- `runtime/src/io.rs` — access/lock params, apply_flock (Unix), qb_file_open_str forwarding.
- `src/codegen/c_backend/file_io.rs` — access_const_c, lock_const_c, emit 5-arg open calls.
- `src/codegen/c_backend/runtime/file.rs` — #defines, qb_file_open 5-arg, flock block, sys/file.h include.
- `src/codegen/c_backend/runtime/mod.rs` — qb_file_open_legacy calls with 0, 0.
- `docs/ThingsToDo/FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md` — item 2 marked FIXED.

## Verification

- `cargo build -p qb64fresh-runtime` and `cargo build --bin qb64fresh` succeed.
- Emit test: `OPEN "x" FOR BINARY ACCESS READ WRITE LOCK WRITE AS #1` produces `qb_file_open_str(1LL, ..., "r+b", QB_FILE_ACCESS_READ_WRITE, QB_FILE_LOCK_WRITE)`.

## Review (post-implementation)

- **Rust vs inline C**: Aligned behavior — both runtimes now apply no lock for DEFAULT and SHARED; both apply LOCK_EX only for LOCK_READ, LOCK_WRITE, LOCK_READ_WRITE, ONLY.
- **Invalid lock value**: Inline C now checks explicit lock values (2,3,4,5) before calling flock, so unknown/garbage lock values do not trigger LOCK_EX.

## Related

- [FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md](../docs/ThingsToDo/FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md) — item 2
- [BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md](../docs/BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md)
