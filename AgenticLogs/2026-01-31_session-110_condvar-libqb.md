# Session 110: libqb condvar (Condition Variable)

**Date:** 2026-01-31

## Goal

Implement condvar.h compatibility (LIBQB_FUNCTIONALITY.md §34): opaque condvar, new/free, wait(condvar, mutex), signal, broadcast.

## Done

- **runtime/src/condvar.rs** — `LibqbCondvar` wrapping `pthread_cond_t`; `libqb_condvar_new` / `libqb_condvar_free`; `wait(&self, *mut LibqbMutex)` using `pthread_cond_wait`; `signal()` / `broadcast()`.
- **runtime/src/condvar_ffi.rs** — C FFI: `libqb_condvar_new`, `libqb_condvar_free`, `libqb_condvar_wait`, `libqb_condvar_signal`, `libqb_condvar_broadcast`.
- **runtime/src/mutex.rs** — Added `pub(crate) fn as_pthread_ptr(&self)` so condvar can pass the mutex’s `pthread_mutex_t*` to `pthread_cond_wait` without exposing `LibqbMutex::inner`.
- **runtime/include/qb64fresh_rt.h** — New section: `struct libqb_condvar`, declarations for all five functions.
- **runtime/src/lib.rs** — `pub mod condvar`, `pub mod condvar_ffi`, `pub use condvar_ffi::*` (no `condvar::*` to avoid duplicate symbols with FFI).
- **docs/QB64pe/LIBQB_FUNCTIONALITY.md** — §34 condvar table updated: all items 🟢.

## Decisions

- Follow mutex pattern: opaque type, alloc/init in `_new`, destroy/dealloc in `_free`, null checks in FFI.
- Condvar wait takes `(condvar, mutex)`; mutex must be our `libqb_mutex` so we use `LibqbMutex::as_pthread_ptr()` for the inner `pthread_mutex_t*`.
- Re-export only `condvar_ffi::*` at crate root to avoid name clash with condvar’s `libqb_condvar_new`/`_free`.

## Build

`cargo build -p qb64fresh-runtime` succeeds (warnings only, no errors).
