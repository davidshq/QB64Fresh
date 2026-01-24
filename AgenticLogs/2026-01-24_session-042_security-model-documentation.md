# Session 042: Security Model Documentation

**Date:** 2026-01-24  
**Focus:** Document security model for SHELL and file operations (FUTURE.md item 2).

## Summary

Created `docs/SECURITY_MODEL.md` describing how **SHELL** / **_SHELLHIDE** and **file operations** (OPEN, KILL, NAME, MKDIR, RMDIR, CHDIR, BLOAD, BSAVE, _FILEEXISTS, _DIREXISTS, _FILES$, _DIR$) interact with the OS, what risks they pose, and that there is **no sandbox**. Cross-linked from ARCHITECTURE and FUTURE; removed the "Document security model" item from High Priority.

## Decisions

- **Scope:** SHELL, _SHELLHIDE, and all path-taking file operations. Networking, graphics, audio, DECLARE LIBRARY, and general process identity are mentioned only in passing.
- **Tone:** Factual: describe implementation and host behavior, list risks and programmer recommendations, state that this matches traditional BASIC. No promises of future sandboxing; a short "Possible future mitigations" section lists ideas only.
- **Location:** Standalone `docs/SECURITY_MODEL.md` rather than an ADR (ADR = architectural decision; this is reference/specification).

## What Was Documented

### SHELL and _SHELLHIDE

- **Behavior:** SHELL with/without command; SHELL as function; _SHELLHIDE (hidden on Windows, nulled stdout/stderr on Unix).
- **Implementation:** Codegen emits `qb_shell(...->data)` / `qb_shell_hide(...->data)`. Runtime: `sh -c` (Unix) / `cmd /C` (Windows); command string passed through with **no sanitization**.
- **Risks:** Command injection when the string is built from user or untrusted input; inherent capability to run any command the process can.
- **Recommendations:** Avoid building SHELL commands from user input; prefer file I/O and built-ins where possible; document and review SHELL in untrusted or multi-tenant use.

### File Operations

- **Operations:** OPEN (inline C `fopen`), KILL (`std::fs::remove_file`), NAME (`std::fs::rename`), MKDIR (`std::fs::create_dir`), RMDIR (`std::fs::remove_dir`), CHDIR (`std::env::set_current_dir`), BLOAD/BSAVE (inline C `fopen`), _FILEEXISTS, _DIREXISTS, _FILES$, _DIR$.
- **Path handling:** No validation or sandbox; `..` and absolute paths are allowed; resolution is by the OS. **KILL:** no glob expansion (runtime uses `remove_file` only; `KILL "*.tmp"` targets a literal file named `*.tmp`), unlike QB64.
- **Risks:** Path traversal, symlink following, CHDIR affecting global state, overwrite/deletion.
- **Recommendations:** Avoid paths from unsanitized input; use _FILEEXISTS/_DIREXISTS before destructive ops; use CHDIR with care in shared code.

### Relation to Host and Future Work

- **Process:** Programs run with the identity of the executing user; containment is via OS/deployment (containers, restricted accounts, etc.), not the compiler/runtime.
- **Future (not implemented):** `--no-shell`, configurable runtime policies, path allowlists/restrictions—recorded for clarity only.

## Files Created/Modified

| File | Action |
|------|--------|
| `docs/SECURITY_MODEL.md` | Created (~220 lines) |
| `docs/ARCHITECTURE.md` | Added Related Documents link to SECURITY_MODEL.md |
| `docs/ThingsToDo/FUTURE.md` | Removed "Document security model" from High Priority; renumbered; added Version History entry |

## References

- CODEBASE_REVIEW_CONSOLIDATED: SHELL command injection (§3), path traversal (§4)
- Runtime: `runtime/src/io.rs` (qb_shell, qb_shell_hide, qb_file_kill, qb_file_rename, qb_mkdir, qb_rmdir, qb_chdir, qb_file_exists, qb_direxists, qb_dir, qb_files)
- Codegen: `src/codegen/c_backend/stmt/mod.rs`, `src/codegen/c_backend/runtime/file.rs`, `src/codegen/c_backend/file_io.rs`
