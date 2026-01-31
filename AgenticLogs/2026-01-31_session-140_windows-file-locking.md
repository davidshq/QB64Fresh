# Session 140 – Windows LockFileEx / LOCK UNLOCK parity

**Date:** 2026-01-31  
**Purpose:** Implement Windows file locking for LOCK/UNLOCK and OPEN lock mode (item 4 from session 134 REVIEW_IN_MORNING).

## Summary

- **Scope:** LOCK/UNLOCK statement parity on Windows using LockFile/UnlockFile; OPEN lock mode applies exclusive lock on Windows (same semantics as Unix flock).
- **Where:** `runtime/src/io/file.rs`, `runtime/Cargo.toml`.

## Changes

1. **Windows OPEN lock (`apply_flock`)**  
   Replaced stub with real locking: when OPEN uses a lock mode (LOCK READ/WRITE/READ WRITE/ONLY), we now call Windows `LockFile` on the primary file handle for the entire file (0, 0, 0xFFFFFFFF, 0xFFFFFFFF). SHARED/DEFAULT still apply no lock.

2. **`qb_file_lock` / `qb_file_unlock` in Rust runtime**  
   The external runtime (libqb64fresh_rt) did not implement these; they were only in the inline C runtime. Added:
   - **Unix:** entire file via `flock(LOCK_EX/LOCK_UN)`; range via `fcntl(F_SETLK, struct flock)` with F_WRLCK/F_UNLCK.
   - **Windows:** `LockFile` / `UnlockFile` with start/length as low/high DWORDs; entire file when start/end = -1 (0xFFFFFFFF length).

3. **Dependency**  
   `[target.'cfg(windows)'.dependencies]` in runtime `Cargo.toml`: `winapi` with features `fileapi`, `handleapi`, `minwindef`, `errhandlingapi` for LockFile, UnlockFile, GetLastError, ERROR_*.

## Return values (unchanged)

- 0 = success  
- -2 = invalid handle  
- -4 = illegal function call (bad start/end)  
- -7 = permission denied  
- -9 = access error (Windows only)

## Notes

- Inline C runtime already had Windows LockFile/UnlockFile in the codegen; the gap was only in the Rust external runtime and in Windows `apply_flock` at OPEN time.
- Full LOCK #n, start, end is supported (range locking on Unix with fcntl, on Windows with LockFile/UnlockFile).
