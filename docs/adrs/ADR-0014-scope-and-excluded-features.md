# ADR-0014: Scope and Intentionally Excluded Features

## Status

**Accepted** - January 24, 2026

## Context

QB64pe and QBasic include features that target legacy hardware, obsolete operating systems, or implementation choices (e.g., raw OpenGL) that do not align with QB64Fresh’s goals: a portable, SDL2/winit-based runtime, and a focus on QB45-style compatibility plus a sustainable subset of QB64 extensions.

We need an explicit scope for what we support, stub, or exclude so that users and contributors understand the boundaries and the rationale.

## Decision

**We define a clear scope: support QB45 compatibility and a curated set of QB64 extensions; explicitly exclude raw OpenGL and certain legacy hardware/DOS features; provide stub-only implementations for some legacy symbols to ease porting while documenting that they are non-functional.**

### 1. Raw OpenGL (`_GL*`)

QB64pe provides hundreds of `_GL*` commands (e.g. `_GLBEGIN`, `_GLEND`, `_GLVERTEX3F`). These are **optional** in QB64Fresh: excluded by default, included only when the program uses `_GL*` or `SUB _GL` (or `--opengl` is set) and the runtime is built with the `opengl` feature.

| When enabled | Behavior |
|---------------|----------|
| Compiler | Registers OpenGL built-ins; enforces “_GL* only in SUB _GL” (error 270); emits `call_gl*` under `#ifdef QB64FRESH_OPENGL`. |
| Runtime | Feature `opengl`: builds vendored `gl_wrappers.c`, exposes `sub_gl_called`; `_GLCOMPAT` returns 1; `_GLRENDER` stores mode (main-loop SUB _GL invocation is follow-up). |
| Default | No OpenGL built-ins or link dependency; `_GLRENDER`/`_GLCOMPAT` remain no-op stubs. |

**Escape hatch:** Users can still call OpenGL via `DECLARE LIBRARY` for custom setups. See [OPENGL_GLUT_DESIGN.md](../ThingsToDo/OPENGL_GLUT_DESIGN.md) for the full design.

### 2. Legacy Hardware and DOS-Only Features

The following are **stub-only** for compile-time compatibility. The runtime does not implement real behavior; they are documented as non-functional.

| Area | Examples | Reason |
|------|----------|--------|
| Port I/O | `INP`, `OUT`, `WAIT` | Blocked or unusable on modern OSes; security and portability. |
| System interrupts | `INTERRUPT`, `INTERRUPTX` | x86 BIOS/DOS; not applicable on modern systems. |
| Light pen | `PEN` | Hardware effectively obsolete. |
| DOS device control | `IOCTL`, `ERDEV`, `ERDEV$` | DOS-specific. |
| Event handlers | `ON COM`, `ON UEVENT`, `ON SIGNAL` | Not implemented in QB64pe either; low demand and non-trivial to support. |

Stubs allow existing code that references these to compile; runtime behavior is undefined or no-op. See `src/codegen/c_backend/runtime/legacy.rs` and related docs.

### 3. InForm and Bundled QB64-Specific Stacks

- **InForm** (WYSIWYG UI designer/GUI engine) is an **external** project; it is not part of QB64pe’s tree. We do not bundle or officially support it. If we add GUI support later, the approach will be decided separately (see [INFORM_FUNCTIONALITY.md](../ThingsToDo/INFORM_FUNCTIONALITY.md), [INFORM_EXPERT_DISCUSSION.md](../ThingsToDo/INFORM_EXPERT_DISCUSSION.md)).
- **QB64-bundled stacks** (e.g. certain OpenGL bindings, InForm) that depend on QB64pe internals are out of scope; users should use `DECLARE LIBRARY`, SDL2, or other portable APIs.

### 4. GOSUB and GCC-Dependent Codegen

GOSUB/RETURN use GCC’s computed goto extension. This works with GCC and Clang but **not** MSVC. Supporting MSVC would require an alternative (e.g. switch-based dispatch). We **accept this tradeoff** for now; MSVC support is low priority. This is an implementation detail of the C backend, not a change to language scope. See [ADR-0002](ADR-0002-code-generation-backend.md) for codegen and compiler details.

### 5. Features Not Supported for QB64 Parity

We aim to **match** QB64, not exceed it in backwards compatibility. The following are not in QB64 (or not exposed as language features), so we do **not** implement them for parity.

| Feature | QB64 status | May revisit as modern functionality? |
|---------|-------------|--------------------------------------|
| **Hardware acceleration (user toggle)** | QB64 does not expose a "hardware acceleration" user option; it uses the implementation default (e.g. OpenGL when available). | Maybe. A dedicated toggle or GPGPU path could be a quality-of-implementation improvement, not QB compatibility. |
| **Touch / multi-touch input API** | QB64 has no dedicated touch API; `_SCREENCLICK` simulates a click, it does not read touch hardware. | Yes. Touch would support mobile and cross-platform as **modern functionality**, not QB/QuickBasic compatibility. |
| **`_THREAD` and thread primitives** | QB64 has no `_THREAD`, THREADCREATE, THREADWAIT, or mutex/semaphore as BASIC keywords; threading is internal (e.g. curl, timer) only. | Yes. Multithreading would be **modern functionality** for parallel programming, not backwards compatibility. |
| **Compiler optimizations** | QB64 does not implement dead code elimination, loop optimization, or inlining; it relies on the C compiler. | Yes. Our own optimizations would be **quality-of-implementation** (better generated code), not parity with QB64. |

**Rationale:** Implementing these would go beyond QB64 parity. We record them here so that (a) users know we are not missing QB64 compatibility by omitting them, and (b) we can revisit selected items later as **modern functionality** (touch, threads) or **tooling improvements** (optimizations, GPU path), not as additional QuickBasic or QB64 compatibility.

## Consequences

### Positive

- Clear boundaries reduce “why doesn’t X work?” confusion
- Excluding `_GL*` keeps the graphics layer portable and backend-agnostic
- Stub-only legacy symbols ease porting of old sources without committing to obsolete or unsafe behavior
- DECLARE LIBRARY remains the escape hatch for OpenGL, custom drivers, etc.

### Negative

- Code relying on `_GL*`, port I/O, or DOS features will not run as in QB64pe; may need rewrites or DECLARE LIBRARY
- Stub behavior (no-op or undefined) can hide bugs if authors assume real semantics

## References

- [ThingsToDo](../ThingsToDo/) (e.g. TODO_CONSOLIDATED.md, OPTION_B_*) – Remaining features, DECLARE LIBRARY; parity exclusions (hardware accel, touch, _THREAD, optimizations) are in this ADR. GOSUB/compiler: ADR-0002.
- [QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md](../QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md) – Documented intentional behavioral differences (stricter GOTO, multiple errors, RND, etc.)
- [ADR-0006](ADR-0006-graphics-system.md) – Graphics architecture (SDL2, trait-based)
- [ADR-0008](ADR-0008-c-interoperability.md) – DECLARE LIBRARY for raw GL or low-level access
