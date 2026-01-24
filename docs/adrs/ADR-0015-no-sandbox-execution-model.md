# ADR-0015: No-Sandbox Execution Model

## Status

**Accepted** - January 24, 2026

## Context

QB64Fresh compiles BASIC to C and links to a runtime that performs shell and filesystem operations via the host OS. We must decide whether to provide a sandbox (e.g. restrict SHELL, constrain file paths) or to match the traditional BASIC model where the program has the same privileges as the process.

QBasic, QB64, and QB64PE do not sandbox: SHELL runs commands, file operations use OS APIs directly, and paths are not restricted. Users and auditors need a clear statement of our behavior and how to reason about security.

## Decision

**We do not provide a sandbox. Compiled programs run with the same privileges as the process that executes them. We match traditional BASIC and QB64 semantics and document the behavior, risks, and recommendations in a standalone specification.**

- **SHELL / _SHELLHIDE:** The command string is passed to the system shell (`sh -c` / `cmd /C`) with no sanitization or allowlisting.
- **File operations:** OPEN, KILL, NAME, MKDIR, RMDIR, CHDIR, BLOAD, BSAVE, _FILEEXISTS, _DIREXISTS, _FILES$, _DIR$ use `fopen`, `std::fs::*`, and `std::env::set_current_dir` directly. There is no path validation, sandboxing, or traversal checks beyond what the OS does; `..` and absolute paths are allowed.
- **Containment:** If limits are needed, they must be applied at the OS or deployment level (containers, restricted accounts, etc.), not by the compiler or runtime.

The full specification—behavior, implementation notes, risks, and programmer recommendations—is in [SECURITY_MODEL.md](../SECURITY_MODEL.md).

## Consequences

### Positive

- Matches user expectations from QBasic and QB64
- No extra runtime or complexity for sandboxing
- Clear contract: same capabilities as the process
- SECURITY_MODEL.md gives a single reference for auditors and developers

### Negative

- No in-process mitigation for command injection, path traversal, or misuse of SHELL/file ops; securing programs depends on source trust and OS/deployment controls
- Programs that compose SHELL or paths from untrusted input are inherently risky; we document but do not enforce

## References

- [SECURITY_MODEL.md](../SECURITY_MODEL.md) – Full specification: SHELL, file ops, paths, risks, recommendations, code references
- [ADR-0008](ADR-0008-c-interoperability.md) – C interop safety; links to SECURITY_MODEL for SHELL/file/process model
