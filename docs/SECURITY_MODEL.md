# QB64Fresh Security Model: SHELL and File Operations

*Last updated: 2026-01-24*

The architectural decision (no sandbox, match traditional BASIC) is in [ADR-0015: No-Sandbox Execution Model](adrs/ADR-0015-no-sandbox-execution-model.md).

This document describes the security properties of **SHELL** (and _SHELLHIDE) and **file operations** (OPEN, KILL, NAME, MKDIR, RMDIR, CHDIR, BLOAD, BSAVE, and related functions). It is intended for developers, auditors, and anyone deploying QB64Fresh programs in security-sensitive environments.

---

## Overview

QB64Fresh compiles BASIC to C and links against a runtime that performs system integration (shell, filesystem) using the host OS APIs. **There is no sandbox**: compiled programs run with the same privileges as the process that executes them. This matches traditional BASIC semantics (QBasic, QB64, QB64PE) and is by design.

| Area        | Behavior | Sandboxing |
|-------------|----------|------------|
| SHELL       | Executes commands via shell (`sh -c` / `cmd /C`) | None |
| File I/O    | Direct `fopen` / `std::fs::*` / `remove_file`, etc. | None |
| Paths       | No validation or restriction; `..` and absolute paths allowed | None |

---

## 1. SHELL and _SHELLHIDE

### 1.1 Behavior

- **SHELL** `[command$]`  
  - With a command: runs it via the system shell.  
  - Without a command: starts an interactive shell (`sh` on Unix, `cmd` on Windows).
- **SHELL** as a function: `SHELL(command$)` returns the exit code of the executed command.
- **_SHELLHIDE** `command$`  
  - Same execution as SHELL, but on Windows hides the console window; on Unix, stdout/stderr are discarded.

### 1.2 Implementation

- **Codegen:** The command expression is emitted as-is into `qb_shell(...->data)` or `qb_shell_hide(...->data)` (see `src/codegen/c_backend/stmt/mod.rs`).
- **Runtime:**  
  - Unix: `std::process::Command::new("sh").args(["-c", cmd_str]).status()`  
  - Windows: `std::process::Command::new("cmd").args(["/C", cmd_str]).status()`  

  The string is passed directly to the shell. There is **no parsing, sanitization, or allowlisting** of `cmd_str`.

### 1.3 Security Considerations

| Risk | Description |
|------|-------------|
| **Command injection** | If the command string is built from user or external input (e.g. `SHELL "echo " + user$` or `SHELL "ls " + path$`), an attacker can inject shell metacharacters (`;`, `|`, `&`, `` ` ``, `$()`, newlines, etc.) to run arbitrary commands. |
| **Inherent capability** | Even with constant strings, `SHELL` can run any command the process is allowed to run (e.g. `rm -rf`, `format`, installers). This is expected for a BASIC-like environment. |

**Examples of dangerous patterns:**

```basic
' BAD: user-controlled
INPUT "Path: ", path$
SHELL "ls -la " + path$     ' path$ could be "; rm -rf /"

' BAD: file-based
LINE INPUT #1, cmd$
SHELL cmd$

' Intentional but powerful (no injection)
SHELL "sudo apt update"
```

### 1.4 Recommendations for Programmers

1. **Avoid constructing SHELL commands from user or untrusted input.** If needed, use a fixed command and pass arguments via a controlled mechanism (e.g. exec-style argument list) rather than string concatenation into a shell command.
2. **Prefer file I/O and built-in primitives** (e.g. `_FILEEXISTS`, `_FILES$`, `KILL`, `OPEN`) instead of calling out to shell tools when the goal is file manipulation or discovery.
3. **Document and review** any use of SHELL in applications that handle untrusted input or run in shared/multi-tenant environments.

---

## 2. File Operations

### 2.1 Operations in Scope

| Statement / function | Runtime / codegen | Host API |
|---------------------|-------------------|----------|
| **OPEN** `path` FOR *mode* AS #*n* | `qb_file_open` (inline C) | `fopen(path, mode)` |
| **CLOSE** #*n* | `qb_file_close` | `fclose` |
| **KILL** `path` | `qb_file_kill` (runtime) | `std::fs::remove_file` |
| **NAME** `old` **AS** `new` | `qb_file_rename` (runtime) | `std::fs::rename` |
| **MKDIR** `path` | `qb_mkdir` (runtime) | `std::fs::create_dir` |
| **RMDIR** `path` | `qb_rmdir` (runtime) | `std::fs::remove_dir` |
| **CHDIR** `path` | `qb_chdir` (runtime) | `std::env::set_current_dir` |
| **BLOAD** `path` [, *addr*] | `qb_bload` (inline C) | `fopen` + read |
| **BSAVE** `path`, *addr*, *len* | `qb_bsave` (inline C) | `fopen` + write |
| **_FILEEXISTS** | `qb_file_exists` (runtime) | `Path::exists` |
| **_DIREXISTS** | `qb_direxists` (runtime) | `Path::is_dir` |
| **_FILES$**, **_DIR$** | `qb_files`, `qb_dir` (runtime) | `std::fs::read_dir` and similar |

All path arguments are expressions. They are passed through to the runtime or generated C as string data (e.g. `->data` for QbString); there is **no path normalization, sandboxing, or check for path traversal** in the compiler or in the runtime beyond what the OS does.

### 2.2 Path Handling

- **Path traversal:** `../` and absolute paths (e.g. `/etc/passwd`, `C:\Windows\System32`) are accepted. Resolution is done entirely by the OS.
- **OPEN / BLOAD / BSAVE:** The inline C runtime uses `fopen(filename, mode)`. On non-Windows, it may also try a normalized path (backslash → forward slash) if the first `fopen` fails; the path is not otherwise restricted.
- **KILL, NAME, MKDIR, RMDIR, CHDIR:** Implemented in `runtime/src/io.rs` via `std::fs::*` and `std::env::set_current_dir`. No extra checks.
- **KILL and globbing:** The runtime uses `std::fs::remove_file` only. **Glob patterns (e.g. `KILL "*.tmp"`) are not expanded**; such a path is passed as the literal filename `"*.tmp"`, so only a file named `*.tmp` would be removed. This differs from QB64/QB64PE, where `KILL "*.tmp"` can delete multiple files.

### 2.3 Security Considerations

| Risk | Description |
|------|-------------|
| **Path traversal** | Paths built from user input (e.g. `OPEN userpath$ + "\config.txt"`) can escape the intended directory (e.g. `../../etc/passwd`). |
| **Symlinks** | The runtime and C library follow symlinks. Operations on attacker-controlled symlinks can read/write/overwrite unexpected files. |
| **CHDIR and global state** | `CHDIR` changes the process’s current directory. This affects all subsequent relative `OPEN`, `KILL`, `MKDIR`, etc. Bugs or untrusted code can redirect operations to unintended locations. |
| **Overwrite and deletion** | `OPEN ... FOR OUTPUT`, **NAME**, **KILL**, **BSAVE**, and similar can overwrite or delete files writable by the process. |

### 2.4 Recommendations for Programmers

1. **Avoid building paths from unsanitized user input.** Prefer fixed base directories and allowlisted names, or validate and reject `..` and absolute paths if you need to restrict access.
2. **Use `_FILEEXISTS` / `_DIREXISTS`** before **KILL**, **NAME**, or overwriting **OPEN** when the path comes from configuration or user input, to fail predictably rather than depending on OS errors.
3. **Be cautious with CHDIR** in libraries or reusable procedures; document and isolate its use so it does not change global state in surprising ways.

---

## 3. Relation to the Host

- **Process identity:** The compiled program runs as the user (and, if applicable, group) that executes the binary. There is no separate “QB64Fresh permission set”.
- **Containment:** If you need limits on filesystem or shell access, you must use OS- or deployment-level mechanisms (e.g. containers, VMs, restricted accounts, mandatory access control, namespaces, or dedicated “run” users with minimal privileges).

---

## 4. Compile-Time Option: `--no-shell` (Implemented)

The **`--no-shell`** flag disables SHELL and _SHELLHIDE at compile time. If the program uses either construct (statement or function form), code generation fails with a clear error.

- **Usage:** `qb64fresh program.bas --emit-c --no-shell`
- **Effect:** SHELL, _SHELLHIDE (statements and `SHELL(cmd$)` / `_SHELLHIDE(cmd$)` function calls) are rejected with: *SHELL and _SHELLHIDE are disabled by --no-shell*.
- **Use case:** Builds that must not run external commands (e.g. sandboxed or lock-down deployments).

---

## 5. Possible Future Mitigations (Not Implemented)

The codebase and design do not currently include the following; they are documented here for clarity and as options for future work:

| Option | Description |
|--------|-------------|
| **Configurable runtime policies** | Runtime flags or environment variables to disallow or restrict SHELL, or to constrain file operations to a given directory. |
| **Path allowlists/restrictions** | Optional path validation in the runtime (e.g. reject `..` or paths outside a given root) for high-assurance or educational use. |

Any such feature would be documented separately and would not change the default “full access” behavior described in this document.

---

## 5. References

- **Parser:** `src/parser/system.rs` (KILL, NAME, MKDIR, RMDIR, CHDIR, SHELL, _SHELLHIDE), `src/parser/file_io.rs` (OPEN, etc.)
- **Codegen:** `src/codegen/c_backend/stmt/mod.rs` (SHELL, file ops), `src/codegen/c_backend/file_io.rs` (OPEN), `src/codegen/c_backend/runtime/file.rs` (inline `qb_file_open`, etc.)
- **Runtime:** `runtime/src/io.rs` (`qb_shell`, `qb_shell_hide`, `qb_file_kill`, `qb_file_rename`, `qb_mkdir`, `qb_rmdir`, `qb_chdir`, `qb_file_exists`, `qb_direxists`, `qb_dir`, `qb_files`)
- **Design discussion:** `docs/ThingsToDo/CODEBASE_REVIEW_CONSOLIDATED.md` (SHELL command injection, path traversal)

---

## 7. Summary

| Topic | Default behavior | Sandboxing |
|-------|------------------|------------|
| **SHELL / _SHELLHIDE** | Command string passed to `sh -c` / `cmd /C` with no sanitization. Full command execution under process privileges. | None |
| **File operations** | Paths passed to `fopen` / `std::fs::*` / `std::env::set_current_dir` with no path validation or restriction. `..` and absolute paths allowed. **KILL** does not expand globs. | None |

This matches the traditional BASIC and QB64 model: the program has the same capabilities as the process. Securing QB64Fresh programs relies on **trust in the source (and, if applicable, the build)** and **OS-level and deployment-level controls**, not on compiler or runtime sandboxing.
