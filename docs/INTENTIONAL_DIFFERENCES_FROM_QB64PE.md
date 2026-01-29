# Intentional Differences from QB64pe

This document lists **design choices** where QB64Fresh deliberately behaves differently from QB64pe. These are not bugs or missing features—they are stricter rules, excluded features, or different-by-design behavior we have chosen for clarity, safety, or portability.

**See also:**

- [adrs/ADR-0016-intentional-behavioral-differences.md](adrs/ADR-0016-intentional-behavioral-differences.md) — Policy for documenting and enforcing intentional differences
- [QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md](QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md) — All semantic/behavioral differences (architectural and intentional)
- [adrs/ADR-0014-scope-and-excluded-features.md](adrs/ADR-0014-scope-and-excluded-features.md) — Scope and excluded features (OpenGL, legacy hardware)
- [ThingsToDo/FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md](ThingsToDo/FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md) — Fix list that references some of these choices

---

## Summary Table

| Area | QB64pe | QB64Fresh | Rationale |
|------|--------|-----------|-----------|
| [Cross-procedure GOTO](#1-cross-procedure-goto--labels) | Creates forward-reference label in current scope when name exists elsewhere | Error: label must be defined in current procedure | Stricter; avoids hiding typos or wrong-scope intent |
| [Raw OpenGL (`_GL*`)](#2-raw-opengl-_gl) | Hundreds of `_GL*` commands | Excluded | Portable graphics (SDL2/winit); use DECLARE LIBRARY for OpenGL |
| [Legacy / DOS features](#3-legacy--dos-features) | Some implemented, some error | Stub-only (compile, no-op or undefined at runtime) | Portability and security; documented non-functional |
| [Error reporting](#4-error-reporting) | Often stops at first error | Collects multiple errors per run | Better debugging experience |
| [RND / RANDOMIZE](#5-rnd--randomize) | QB64pe PRNG and sequence | Different PRNG → different sequence | Acceptable; document for compatibility level |
| [Identifier mangling](#6-identifier-mangling) | QB64pe C++ mangling conventions | C-safe mangling (different names) | Implementation choice for C backend |
| [RUN statement](#7-run-statement) | RUN (restart), RUN line/label, RUN "file" | RUN with no args = no-op; RUN "file" = run then exit; no RUN line/label | Minimal implementation; full parity deferred |

---

## 1. Cross-Procedure GOTO / Labels

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **When** | GOTO/GOSUB to a label name that exists only in another procedure | Same |
| **QB64pe** | Creates a *new* label in the current procedure (forward reference). Compiles; resolution is by scope. | — |
| **QB64Fresh** | **Error:** "undefined label" — label must be defined in the *current* procedure. | — |
| **Rationale** | QB64pe’s permissive behavior can hide bugs (e.g. typo or intending the other procedure’s label). We require the label to exist in the current scope and do *not* create a forward-reference label when the name exists elsewhere. |

We may improve the *message* (e.g. "GOTO target must be in the same procedure (label 'X' is defined in another procedure)") but we do **not** emulate creating a label in the current scope. See [FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md §5](ThingsToDo/FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md).

---

## 2. Raw OpenGL (`_GL*`)

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **QB64pe** | Hundreds of `_GL*` commands (e.g. `_GLBEGIN`, `_GLEND`, `_GLVERTEX3F`) | — |
| **QB64Fresh** | **Excluded.** Not parsed or implemented. | — |
| **Rationale** | We use a portable graphics stack (SDL2/winit), not raw OpenGL. Supporting `_GL*` would tie us to an OpenGL-specific backend. Users who need raw OpenGL can call it via `DECLARE LIBRARY`. |

See [ADR-0014](adrs/ADR-0014-scope-and-excluded-features.md) and [ThingsToDo/OPENGL_SUPPORT.md](ThingsToDo/OPENGL_SUPPORT.md).

---

## 3. Legacy / DOS Features

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **Examples** | Port I/O (`INP`, `OUT`, `WAIT`), `INTERRUPT`/`INTERRUPTX`, `PEN`, `IOCTL`, `ERDEV`/`ERDEV$`, some `ON COM`/`ON UEVENT`-style handlers | Same symbols accepted at compile time |
| **QB64pe** | Some implemented, some emit compile errors | — |
| **QB64Fresh** | **Stub-only:** symbols compile; runtime behavior is no-op or undefined. Documented as non-functional. | — |
| **Rationale** | These target legacy hardware or DOS. We keep compile-time compatibility so old code compiles, but we do not implement behavior that is unsafe or non-portable. |

See [ADR-0014](adrs/ADR-0014-scope-and-excluded-features.md).

---

## 4. Error Reporting

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **QB64pe** | Typically stops at first error (limited recovery) | — |
| **QB64Fresh** | **Collects multiple errors** per compilation and reports them together. | — |
| **Rationale** | Showing all errors at once improves the debugging experience; we do not emulate single-error-stop. |

---

## 5. RND / RANDOMIZE

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **QB64pe** | Uses its own PRNG and seed semantics | — |
| **QB64Fresh** | **Different PRNG** → different number sequence for the same seed. | — |
| **Rationale** | Acceptable for our compatibility level. Document so users do not rely on bit-identical sequences. |

See [BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md](BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md).

---

## 6. Identifier Mangling

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **QB64pe** | C++-oriented mangling for procedures, labels, etc. | — |
| **QB64Fresh** | **C-safe mangling** (different names in generated C). | — |
| **Rationale** | We emit C, not C++; our mangling is chosen for C compatibility and uniqueness. No intent to match QB64pe’s symbol names. |

---

## 7. RUN Statement

| | QB64pe | QB64Fresh |
|---|--------|-----------|
| **RUN (no args)** | Restarts the current program (sub_run_init, sub_clear, QBMAIN or goto S_0). | **No-op.** We do not support restart yet. |
| **RUN &lt;line&gt; / &lt;label&gt;** | Clears state and jumps to the given line or label. | **Not supported.** We treat RUN with one argument as a string (filename) only. |
| **RUN "file"** | Runs the given program via WinExec/system, then exits. | **Implemented:** we run the program via `system()` and then exit. |

**Rationale:** Minimal implementation so that RUN links and has defined behavior. RUN "file" matches QB64pe (run program, then exit). RUN with no args and RUN line/label are deferred; documenting here so porting code knows the difference.

---

## Adding to This Document

When we **intentionally** adopt a behavior that differs from QB64pe (stricter, excluded, or different-by-design), add a short section here with:

1. **Area** and one-line summary
2. **QB64pe** vs **QB64Fresh** behavior
3. **Rationale**
4. References to ADRs or other docs if applicable

Do *not* list:

- **Unintentional** bugs or missing behavior we plan to fix
- **Architectural** differences that do not reflect a deliberate *choice* to differ (those belong in [QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md](QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md))
