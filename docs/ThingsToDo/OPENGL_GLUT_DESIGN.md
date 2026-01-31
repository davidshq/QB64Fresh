# OpenGL / GLUT Design: Modular, Optional _GL* Support

## Summary

This document describes how to add QB64pe-style OpenGL (`_GL*`) and GLUT-style functionality to QB64Fresh in a **well-architected, modular** way, so that OpenGL is **only included when actually used** (or when explicitly requested).

**Current state:** ADR-0014 intentionally excludes raw `_GL*` from QB64Fresh. This design amends that by making OpenGL an **optional extension** rather than a permanent exclusion: programs that do not use OpenGL incur no dependency or code size cost.

**Scope:** We implement **only** the OpenGL/GLUT functionality that QB64pe implements—nothing beyond it. No extra GL/GLUT APIs, no newer OpenGL versions than QB64pe uses, no additional convenience routines. The goal is QB64pe parity for `_GL*` and `SUB _GL`, so existing QB64pe OpenGL code runs on QB64Fresh, not to extend the API.

**Status:** Optional OpenGL is implemented (phases 1–5). For how to enable it, rules, and building, see the user-facing doc [docs/OPENGL.md](../OPENGL.md). ADR-0014 was updated to reflect optional rather than excluded.

---

## 1. QB64pe OpenGL Model (Reference)

### 1.1 API surface

- **Hundreds of built-ins** derived from `gl.h`: `_GLBEGIN`, `_GLEND`, `_GLVERTEX3F`, `_GLCLEARCOLOR`, `_GLTEXIMAGE2D`, etc.
- **Constants** from `gl.h`: `GL_TRIANGLES`, `GL_QUADS`, `GL_TEXTURE_2D`, `GL_RGBA`, etc.
- **Special sub:** `SUB _GL` — the only place where `_GL*` commands are allowed. Called automatically by the runtime when the display is updated (e.g. vsync).
- **Rendering order:** `_GLRENDER _BEHIND` | `_ONTOP` | `_ONLY` — controls whether OpenGL draws behind, on top of, or instead of the normal 2D layer.
- **Compatibility check:** `_GLCOMPAT` — returns whether OpenGL is available.

### 1.2 QB64pe implementation

- **Compile time (BASIC):** `gl_scan_header` parses `internal/c/parts/core/gl_header_for_parsing/gl.h` and populates `GL_COMMANDS()` and `GL_DEFINES()`. OpenGL functions are registered as built-ins with type mapping (e.g. `GLenum` → `_UNSIGNED LONG`).
- **Generated C:** Each `_GL*` call becomes a call to a wrapper like `call_glBegin((GLenum)a)`. Wrappers are in `gl_helper_code.h` (generated from the same gl.h parse).
- **Runtime rule:** Every wrapper checks `if (!sub_gl_called) error(270);` so that `_GL*` is only valid inside `SUB _GL`. `sub_gl_called` is set when the runtime invokes `SUB _GL`.
- **Context:** QB64pe creates an OpenGL context (e.g. via SDL2 or platform API) and calls `SUB _GL` from the main loop when `_GLRENDER` has been used.

### 1.3 Scope of “GLUT”

QB64pe does not expose a separate “GLUT” API; it exposes OpenGL (`gl.h`) and a small amount of GLU (e.g. `_gluPerspective` in the sample). “GLUT functionality” here means: **same _GL* and SUB _GL model as QB64pe**, so user code that works in QB64pe can be targeted. If we later add GLUT-specific routines, they can live in the same optional layer.

### 1.4 Type mapping (QB64pe reference)

OpenGL types map to QB64 types as follows (QB64pe reference; we use the same mapping for parity):

| OpenGL Type | QB64 Type | Suffix | C Type |
|------------|----------|--------|--------|
| `GLenum` | `_UNSIGNED LONG` | `~&` | `uint32` |
| `GLbitfield` | `_UNSIGNED LONG` | `~&` | `uint32` |
| `GLuint` | `_UNSIGNED LONG` | `~&` | `uint32` |
| `GLint` | `LONG` | `&` | `int32` |
| `GLsizei` | `LONG` | `&` | `int32` |
| `GLboolean` | `_UNSIGNED _BYTE` | `~%%` | `uint8` |
| `GLubyte` | `_UNSIGNED _BYTE` | `~%%` | `uint8` |
| `GLfloat` | `SINGLE` | `!` | `float` |
| `GLclampf` | `SINGLE` | `!` | `float` |
| `GLdouble` | `DOUBLE` | `#` | `double` |
| `GLclampd` | `DOUBLE` | `#` | `double` |
| `GLvoid*` | `_OFFSET` | `%&` | `ptrszint` |

---

## 2. Design Principles

1. **Optional at compile time:** No OpenGL built-ins, codegen, or link dependency unless the program uses `_GL*` or `SUB _GL`, or the user passes an explicit flag (e.g. `--opengl`).
2. **Optional at runtime:** The runtime library does not depend on OpenGL unless built with an OpenGL feature (e.g. `opengl` feature). Programs that don’t use OpenGL don’t link against GL.
3. **Modular:** OpenGL lives in dedicated modules (compiler: builtins + codegen; runtime: context + C shim). No mixing of OpenGL into core SDL2 2D path except where we need a shared window/context.
4. **Parity where intended:** Same rules as QB64pe: `_GL*` only valid inside `SUB _GL`; `_GLRENDER` controls order; `_GLCOMPAT` reports availability. Constants and function signatures match gl.h-derived mappings.
5. **QB64pe parity only:** Implement only what QB64pe implements—no extra OpenGL/GLUT APIs, no newer GL versions, no additional helpers. The API surface is defined by QB64pe’s gl.h parse and built-in set, not by our own extensions.

---

## 3. Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Source (.bas)                                                           │
│ SUB _GL … _GLBEGIN GL_TRIANGLES … _GLEND … END SUB                      │
└─────────────────────────────────────────────────────────────────────────┘
                                      │
                    ┌─────────────────┴─────────────────┐
                    │ Compiler (optional OpenGL layer)   │
                    │ • Detect _GL* / SUB _GL           │
                    │ • Register _GL* built-ins (list   │
                    │   or gl.h-derived)                 │
                    │ • Enforce: _GL* only in SUB _GL    │
                    │ • Codegen: emit call_gl* or gl*   │
                    │   under #ifdef QB64FRESH_OPENGL   │
                    └─────────────────┬─────────────────┘
                                      │
┌─────────────────────────────────────┴───────────────────────────────────┐
│ Generated C                                                            │
│ #ifdef QB64FRESH_OPENGL                                                │
│   call_glBegin(GL_TRIANGLES); … call_glVertex3f(…); call_glEnd();       │
│ #endif                                                                 │
│ + optional: gl_constants.h / gl_wrappers.c (or inline)                │
└─────────────────────────────────────┬───────────────────────────────────┘
                                      │
                    ┌─────────────────┴─────────────────┐
                    │ Runtime (optional OpenGL support) │
                    │ • Feature "opengl" → link GL     │
                    │ • SDL2 window + GL context when   │
                    │   _GLRENDER used                   │
                    │ • sub_gl_called set when SUB _GL   │
                    │   is invoked                       │
                    │ • call_gl* wrappers (C) or direct  │
                    │   gl* + error 270 check            │
                    └───────────────────────────────────┘
```

---

## 4. Compiler Side (QB64Fresh)

### 4.1 “Only when used” strategies

- **Option A — Use detection (recommended):**  
  After parsing/semantic analysis, if the program contains any `_GL*` call or a `SUB _GL` definition, set an internal “uses OpenGL” flag. Only then:
  - Register OpenGL built-ins (see below).
  - Emit OpenGL code and `#define QB64FRESH_OPENGL` in generated C.
  - Build/link step adds OpenGL when this flag is set (e.g. pass `-DQB64FRESH_OPENGL` and link `-lGL`).

- **Option B — Explicit flag:**  
  User passes `--opengl`. Compiler always enables OpenGL built-ins and codegen for that compile. Use if we don’t want to rely on detection.

- **Option C — Hybrid:**  
  Default = Option A (detect). Override: `--no-opengl` to disable even if source has `_GL*`/`SUB _GL`; `--opengl` to enable even if source doesn’t (e.g. for libraries).

Recommendation: **Option A** for “only included when actually used”; optionally add `--opengl`/`--no-opengl` overrides.

### 4.2 OpenGL built-in registry

- **Location:** Dedicated module, e.g. `src/semantic/builtins_opengl.rs` (or `src/semantic/opengl.rs`), compiled only when OpenGL support is enabled (feature or conditional).
- **Content:** A list of OpenGL subs/functions and constants:
  - **Functions/subs:** Either (1) hand-maintained list of the most common ~200–400 gl.h entries, or (2) a build-time step that parses a vendored/copy of `gl.h` and generates Rust code that registers each `_GL*` with the correct parameter types and return type (QB64 type mapping as in QB64pe: `GLenum` → Long, `GLfloat` → Single, etc.).
- **Constants:** Same: either hand-maintained list of common `GL_*` constants or generated from gl.h `#define`s. Generated C will emit `#define GL_TRIANGLES 0x0004` etc. (or include a small gl_constants.h).

We already have `_GLRENDER` and `_GLCOMPAT`; those stay. When OpenGL is enabled, we add the full set of `_GL*` built-ins and `GL_*` constants.

### 4.3 SUB _GL and scope rules

- **Parser:** Recognize `SUB _GL` as a special sub (name `_GL`).
- **Semantic:** Enforce that any call to an OpenGL built-in (`_GL*`) appears only inside the body of `SUB _GL`. Otherwise, report the same error as QB64pe (e.g. “GL command outside SUB _GL”). We already have error codes 270/271; use them.
- **Codegen:** When emitting the call to `SUB _GL`, the runtime will set `sub_gl_called = 1` before calling and clear after return (or the C wrapper does the check).

### 4.4 Code generation

- **Guarding:** All OpenGL-related emitted C (calls to `call_gl*` or `gl*`, and any included headers) sit under `#ifdef QB64FRESH_OPENGL`. So a build without `-DQB64FRESH_OPENGL` never references GL.
- **Naming:** Map `_GLBEGIN` → `call_glBegin` (or direct `glBegin`) with the same parameter order and types as in gl.h. Constants: `_GL_TRIANGLES` → `GL_TRIANGLES` (we emit the constant or include a tiny header).
- **Wrappers:** Either (1) emit a small C file (or inline) that defines `call_gl*(...)` with `if (!sub_gl_called) error(270);` then `gl*(...)`, or (2) link to a small “qb64fresh_gl” shim in the runtime that provides these wrappers when feature `opengl` is on.

---

## 5. Runtime Side (qb64fresh-runtime)

### 5.1 Feature flag

- Add feature `opengl` in `runtime/Cargo.toml`. When `opengl` is enabled:
  - Build/link OpenGL (e.g. `libGL` or platform equivalent). On Linux this may be via `pkg-config --libs gl` or similar; we can use a crate that links GL or a small C shim.
  - Include C code (or Rust FFI) that provides `call_gl*` wrappers and `sub_gl_called` (or equivalent).
  - When creating the main window (SDL2), create an OpenGL context when the program has requested `_GLRENDER` (e.g. we already have `qb_glrender` stub; it would then create/enable the GL context and set a flag so the main loop calls `SUB _GL`).

- Default features: **do not** include `opengl` in the default set, so “only included when used” holds at link time unless the user or the build explicitly enables it.

### 5.2 Context and SUB _GL invocation

- **Context creation:** When `_GLRENDER` is called with a mode other than “off”, the runtime creates an OpenGL context associated with the same window used for 2D (SDL2: `SDL_GL_CreateContext`). If `opengl` feature is disabled, `_GLRENDER` remains a no-op and `_GLCOMPAT` returns 0 (or equivalent).
- **When to create context:** Lazily on first `display()` after `gl_render_mode() >= 0`. The graphics backend (`runtime/src/graphics/sdl2.rs`) checks `gl_render_mode()` in `display()`; when active it calls `display_gl()`, which creates the GL context on first use via `window.gl_create_context()`, then makes it current, invokes SUB _GL, and swaps buffers.
- **Main loop:** The “main loop” is the existing SCREEN/graphics pipeline: every `display()` (from `_DISPLAY`, autodisplay, or other triggers) is the frame. When OpenGL is active, that frame uses `display_gl()`: make the GL context current, set `sub_gl_called = 1`, call the generated SUB _GL via `invoke_sub_gl()` (which calls the callback registered with `qb_gl_register_sub_gl`), set `sub_gl_called = 0`, then `gl_swap_window()`. No separate GL thread or timer—SUB _GL runs on each display update when _GLRENDER is on.
- **Ordering with 2D:** _ONLY (mode 2) is implemented: GL-only frame (no 2D). _BEHIND (0) and _ONTOP (1) would require 2D+GL composition (e.g. render 2D to texture, composite in GL); that is deferred; currently any mode ≥ 0 uses the same GL-only path.
- **Shutdown:** The backend drops the GL context before destroying the window (`shutdown()` sets `gl_context = None` before `canvas = None`) so SDL2 teardown order is correct.
- **sub_gl_called:** Must be visible to the C wrappers. Expose it from the runtime (e.g. a `bool` or `int` in a small C bridge) so that `call_gl*` can run `if (!sub_gl_called) error(270);` before calling `gl*`. Implemented in `runtime/src/gl_ffi.rs`; `invoke_sub_gl()` sets/clears it.

### 5.3 C shim (call_gl* and constants)

- **Option A — Generated at build time:** A build script parses gl.h and generates `gl_wrappers.c` (and optionally `gl_constants.h`) into `target/` or a crate tree. The runtime’s `opengl` feature compiles and links this file.
- **Option B — Vendored helper:** Ship a version of QB64pe’s `gl_helper_code.h` (or a subset) and gl.h-derived constants as part of the repo, and compile them only when `opengl` is enabled. Less flexible for new GL versions but simpler.
- **Option C — Rust FFI:** Use a Rust OpenGL crate (e.g. `gl` or `glow`) and implement `call_gl*` in Rust, each checking a Rust-held `sub_gl_called` then calling the crate. This avoids C but may require more boilerplate for hundreds of functions.

Recommendation: Start with **Option B** (vendored subset of wrappers + constants) for a manageable first milestone; consider Option A for full gl.h coverage later.

---

## 6. Build / Link Integration

- **Compiler output:** When OpenGL is used, the compiler can:
  - Emit a note in a manifest or sidecar file (e.g. “requires_gl: true”), or
  - Emit `#define QB64FRESH_OPENGL` in the generated C so the build knows to link GL.
- **Build script / Makefile / user:** If `QB64FRESH_OPENGL` is defined, add `-lGL` (and on some systems `-lGLU` if we add GLU) and any include path for gl.h. The runtime built with `opengl` feature provides the call_gl* symbols and sub_gl_called.

---

## 7. ADR and Documentation Updates

- **ADR-0014:** Update to state that raw `_GL*` is **optional**: excluded by default, but when the program uses OpenGL (or `--opengl` is set), the compiler and runtime can include the OpenGL layer. The “DECLARE LIBRARY for raw GL” escape hatch remains for custom setups.
- **Docs:** Add a short “OpenGL support” section: how to enable it (use `_GL*`/`SUB _GL` or `--opengl`), that it requires the runtime `opengl` feature and system GL, and link to this design.

---

## 8. Implementation Phases

| Phase | Description | Status |
|-------|-------------|--------|
| **1** | Compiler: Add `uses_opengl` detection (scan for `_GL*` calls and `SUB _GL`). Add `--opengl`/`--no-opengl` flags. When enabled, define `QB64FRESH_OPENGL` in generated C. No new built-ins yet. | Done |
| **2** | Compiler: Add optional module `builtins_opengl.rs` with a **small** set of built-ins (e.g. `_GLBEGIN`, `_GLEND`, `_GLVERTEX3F`, `_GLCLEAR`, `_GLCOLOR*`, plus a few dozen constants). Enforce “_GL* only in SUB _GL” in semantic. Codegen: emit `call_gl*` under `#ifdef QB64FRESH_OPENGL`. | Done |
| **3** | Runtime: Add feature `opengl`. Vendored `gl_wrappers.c` (minimal subset) + constants, build only with `opengl`. Expose `sub_gl_called`. `_GLCOMPAT` returns 1 when opengl; `_GLRENDER` stores mode (main-loop SUB _GL invocation is follow-up). | Done |
| **4** | Expand built-in set (e.g. parse gl.h at build time or add larger hand-maintained list) and constants so that typical QB64pe OpenGL samples (e.g. `open_gl/simple_example.bas`) work. | Deferred (current set covers simple_example.bas surface) |
| **5** | Documentation, ADR update, and CI: build with and without `opengl`; run one OpenGL test when `opengl` is enabled. | Done (ADR-0014 updated; CI can add `--features opengl` build when desired) |

---

## 9. Current implementation status

| Area | Status |
|------|--------|
| **Compiler** | `uses_opengl` detection; `--opengl` / `--no-opengl`; `builtins_opengl.rs` (_GL* / _GLU* / GL_*); "_GL* only in SUB _GL" enforced. Codegen: `call_gl*` under `#ifdef QB64FRESH_OPENGL`. |
| **_GLRENDER** | Builtin; codegen emits `qb_glrender(mode)`. Inline: stub. External with `opengl`: stores mode; `display()` invokes SUB _GL each frame when mode ≥ 0 (GL-only path; _BEHIND/_ONTOP composition deferred). |
| **_GLCOMPAT** | Builtin; codegen emits `qb_glcompat()`. Inline / no opengl: returns 0. External with `opengl`: returns 1. |
| **Runtime** | Feature `opengl`; `gl_wrappers.c`; `sub_gl_called`; _GLCOMPAT returns 1 when opengl; _GLRENDER stores mode. |
| **CI** | Job `runtime-opengl` builds runtime with `--features opengl`. |

When OpenGL is **not** used: _GLRENDER and _GLCOMPAT are stubs. When OpenGL **is** used: compiler emits `call_gl*` and `QB64FRESH_OPENGL`; runtime built with `opengl` provides wrappers. See [docs/OPENGL.md](../OPENGL.md) for user-facing usage.

---

## 10. QB64pe header analysis and code reuse

**QB64pe gl.h** (`QB64pe/internal/c/parts/core/gl_header_for_parsing/gl.h`): public domain; ~337 functions, ~590 constants; OpenGL 1.3 compatibility profile.

**Reuse:** (1) **Header** — copy or vendor for parsing/generation. (2) **Type mapping** — port QB64pe’s mapping (see §1.4). (3) **Wrapper pattern** — same as QB64pe: `if (!sub_gl_called) error(270);` then `gl*(...)`. (4) **Parsing logic** — reference only (QB64pe is BASIC); implement in Rust. (5) **OpenGL loading** — GLEW in QB64pe; we may use Rust `gl`/`glow` or a C shim.

**Rust `gl` crate:** ~95–99% compatibility possible; match QB64pe’s OpenGL version (e.g. 1.3) and naming (_GL* / GL_*) via configuration and codegen.

---

## 11. Challenges and estimated effort

**Challenges:** Header/API surface size; pointer parameters (_OFFSET / arrays); state and context (sub_gl_called, GL context); cross-platform (SDL2 handles context). With optional, use-detection design and vendored wrappers (Option B), risk is bounded.

**Effort (reference):** Full implementation from scratch was estimated ~12–18 days; with reuse of header, type mapping, and pattern ~11–17 days. Phases 1–5 are done; Phase 4 (expand built-in set) is deferred as needed.

---

## 12. Alternatives not chosen

**WebGL / OpenGL ES:** Simpler API and web-friendly, but not QB64pe-compatible; rejected for parity goal.

**Vulkan:** Modern and performant, but not compatible with QB64pe’s OpenGL; rejected for parity goal.

**DECLARE LIBRARY:** Remains the escape hatch for custom GL setups; see ADR-0008.

---

## 13. File Layout (Suggested)

```
QB64Fresh/
├── src/
│   ├── semantic/
│   │   ├── builtins.rs          # unchanged; calls into builtins_opengl when uses_opengl
│   │   └── builtins_opengl.rs    # NEW (optional): _GL* + GL_* registration
│   ├── codegen/c_backend/
│   │   ├── runtime/
│   │   │   ├── mod.rs            # conditionally include opengl.rs
│   │   │   └── opengl.rs         # NEW (optional): emit call_gl* and #ifdef QB64FRESH_OPENGL
│   │   └── ...
│   └── ...
├── runtime/
│   ├── Cargo.toml                # add feature "opengl"
│   ├── src/
│   │   ├── lib.rs                # #[cfg(feature = "opengl")] mod gl_ffi;
│   │   ├── graphics/
│   │   │   └── sdl2.rs           # when opengl: create GL context, call SUB _GL in loop
│   │   └── gl_ffi.rs             # NEW (optional): sub_gl_called, wrappers or link to C
│   └── c_src/                    # or a dedicated gl/ directory
│       └── gl_wrappers.c         # NEW (optional): call_gl* implementations
└── docs/
    └── ThingsToDo/
        └── OPENGL_GLUT_DESIGN.md # this file
```

---

## 14. Summary

- **Modular:** OpenGL is a separate compiler module (builtins + codegen) and runtime feature (context + C shim), not mixed into core 2D graphics.
- **Optional:** Enabled only when the source uses `_GL*`/`SUB _GL` (or `--opengl`) and when the runtime is built with the `opengl` feature.
- **Only when used:** No OpenGL code or link dependency for programs that don’t use OpenGL.
- **Parity:** Same rules as QB64pe (SUB _GL, _GLRENDER, _GLCOMPAT, error 270), with the same gl.h-based API surface, so existing QB64pe OpenGL code can be targeted with minimal changes.

This design keeps the existing SDL2/GraphicsBackend architecture intact and adds OpenGL as an optional, well-bounded layer on top.
