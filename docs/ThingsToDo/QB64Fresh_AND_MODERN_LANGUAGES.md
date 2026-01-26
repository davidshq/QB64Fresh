# QB64Fresh and Modern Languages

**A candid comparison of strengths, weaknesses, viability, and fit within the current language landscape.**

**Last Updated:** 2026-01-26

---

## 1. What QB64Fresh Is

QB64Fresh is a **ground-up rewrite** of QB64: a BASIC compiler that targets QBasic/QuickBASIC 4.5 compatibility and QB64 extensions, producing **native executables** via C code generation. It is implemented in Rust (~60K lines across compiler, runtime, and tooling) and is *not* a fork—it is a clean reimplementation informed by the original.

**Lineage:**
- **QB4.5 (1988)** — Interpreted BASIC for MS-DOS
- **QB64 (2008)** — Compiler that extended QB4.5 with modern features (graphics, networking, 64-bit) while preserving compatibility
- **QB64Fresh (2024–)** — Rust-based rewrite with modern tooling (LSP, any editor), trait-based backends, and an educational, maintainable codebase

**Key facts:**
- Compiles to C, then to native code via GCC/Clang
- 99.1% QB4.5 test compatibility; can compile the 59K-line QB64pe compiler (bootstrap)
- 240+ built-in functions; graphics (SDL2), audio (Rodio), file I/O, networking, DECLARE LIBRARY for C interop
- LSP-based: VSCode, Vim, Emacs, or any LSP-capable editor

---

## 2. Strengths (Relative to Modern Languages)

### 2.1 Simplicity and Learnability

- **Keywords and syntax:** `PRINT`, `INPUT`, `IF...THEN`, `FOR...NEXT` are readable and close to natural flow. No need to learn `fn`, `impl`, `Result`, or `async` before writing a first program.
- **Low ceremony:** Variables can be used without declarations; types can be inferred or indicated with suffixes (`x$`, `n%`). A working program is only a few lines.
- **Procedural, linear model:** No mandatory OOP, no required design patterns. Good for teaching sequencing, conditionals, loops, and subroutines before abstraction.

**Comparison:** Easier initial curve than Rust, C++, or Java; on a par with early Python or Lua for “get something on screen quickly.”

### 2.2 Native Performance Without a VM

- **Compiled to C:** No interpreter or JIT; final binaries are native. Predictable performance and no GC pauses.
- **Deterministic:** No background GC, no JIT warm-up. Suited to games, demos, and tools where latency and frame timing matter.

**Comparison:** Closer to C/Go/Rust than to Python, Ruby, or JavaScript in execution model.

### 2.3 Legacy and Ecosystem Continuity

- **Huge existing corpus:** Decades of QB4.5 and QB64 code (games, utilities, educational material) that can be compiled with little or no change.
- **Bootstrap proof:** Compiling the 59K-line QB64pe compiler shows that non-trivial, real-world BASIC codebases run on QB64Fresh.
- **Migration path:** QB64 users get a familiar language with a different (Rust) implementation and better tooling, not a new language.

**Comparison:** Few modern languages offer this degree of backward compatibility with 1980s–1990s code.

### 2.4 Modern Tooling Without Lock‑In

- **LSP-first:** No built-in IDE; use VSCode, Vim, Emacs, or any LSP client. Go-to-definition, find references, rename, hover, completion, diagnostics.
- **Standard workflows:** Formatter (`qb64fresh-fmt`), linter (`qb64fresh-lint`), debugger infrastructure. Fits into scripts, CI, and existing editors.

**Comparison:** More like Go or Rust (tooling decoupled from IDE) than QB64pe or many classic BASIC environments.

### 2.5 Batteries‑Included for Graphics and Audio

- **Built-in primitives:** `SCREEN`, `LINE`, `CIRCLE`, `PAINT`, `PSET`, `_PUTIMAGE`; `BEEP`, `SOUND`, `PLAY`, `_SNDOPEN`, `_SNDPLAY`; mouse and keyboard (`_MOUSEX`, `_KEYHIT`).
- **No framework required:** 2D graphics and audio work out of the box, without choosing and learning a separate engine or binding.

**Comparison:** More like QB64 or Lua+Love2D than Python (where you pick Pygame, etc.) or C (where you wire everything yourself).

### 2.6 C Interop (DECLARE LIBRARY)

- **First-class C bindings:** `DECLARE LIBRARY` / `DECLARE DYNAMIC LIBRARY` for static and dynamic C libraries. Type mapping, `ALIAS`, `BYVAL` for calling conventions.
- **Fits the pipeline:** Because the output is C, linking and calling C is straightforward. Good for system APIs, existing C libraries, and escape hatches.

**Comparison:** Simpler surface than Rust FFI or ctypes; less flexible than a full FFI layer, but enough for many integration tasks.

### 2.7 Clean, Educational Implementation

- **Rust codebase:** Clear phases (lexer → parser → semantic → codegen), traits for backends, strong typing, and no legacy C++.
- **Documentation and structure:** ADRs, architecture docs, and a codebase intended to teach compiler and language-implementation ideas.

**Comparison:** More readable and maintainable than the original QB64pe compiler; useful as a reference for “how to build a BASIC compiler.”

---

## 3. Weaknesses (Relative to Modern Languages)

### 3.1 Language Limitations Inherited from BASIC

- **No first-class functions:** No function pointers, no higher-order functions, no closures. Callbacks and strategy-style patterns are awkward.
- **Limited abstraction:** No generics, no modules/namespaces, no interfaces or traits. Code reuse is mostly via `SUB`/`FUNCTION` and `TYPE`.
- **OOP is optional and simple:** `TYPE` with methods exists but is not central. No inheritance, no encapsulation discipline enforced by the language.
- **GOTO and line-number culture:** `GOTO`, `GOSUB`, `ON ERROR GOTO` are supported for compatibility. They enable quick patches but can hurt structure in large programs.

**Comparison:** Less expressive than Python, JavaScript, Rust, or Go for building large, modular, highly abstract systems.

### 3.2 Niche Ecosystem and Community

- **Small ecosystem:** No “pip”, “npm”, or “crates.io” for BASIC. Most libraries are C (via DECLARE LIBRARY) or project-specific.
- **Smaller community:** Active mainly in retro, education, and QB64 migration. Not a default choice for new commercial or open-source products.

**Comparison:** Closer to Lua or Nim in “small but dedicated” than to Python, JavaScript, or Rust.

### 3.3 Not Designed for Dominant Modern Domains

- **Web:** No built-in HTTP, HTML, or DOM. Server-side would require C libraries and DECLARE LIBRARY; not a natural fit.
- **Mobile / app frameworks:** No story for iOS/Android or cross-platform UI frameworks. Desktop-only by default.
- **Data science / ML:** No pandas, numpy, or ML stacks. Would need to call C/C++ libraries and manage data manually.
- **Distributed / cloud-native:** No async runtime, no standard concurrency model. Multi-threading (`_THREAD`) is planned, not current.

**Comparison:** Intentionally not competing with Python, JavaScript/TypeScript, or Go in their primary domains.

### 3.4 Gaps vs. QB64pe and Modern BASIC

- **Not yet:** hardware-accelerated backends, `_THREAD`, full `_MEM` model. (Alpha blending is done: _BLEND, _DONTBLEND, _CLEARCOLOR.) Some QB64-specific libs (e.g. InForm, raw OpenGL) are out of scope or require DECLARE LIBRARY and custom C.
- **Behavioral differences:** A few edge cases in semantics, graphics, or built-ins. Documented in [QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md](../QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md).

**Comparison:** Better for “typical” QB4.5/QB64 workloads than for the full superset of QB64pe + all extensions.

### 3.5 Dependency on C Toolchain and Native Build

- **Requires GCC/Clang:** End users (or packagers) need a C compiler. No single-file, download-and-run story like many interpreters.
- **Platform-specific builds:** Graphics and audio may need system libs (e.g. SDL2, ALSA). Cross-compilation and distribution take more setup than with pure interpreter or JS/Python runtimes.

**Comparison:** More like C, C++, or Go than like Python or Node for deployment friction.

---

## 4. Viability

### 4.1 Where QB64Fresh Is Viable

| Use Case | Fit | Notes |
|----------|-----|-------|
| **Legacy QB4.5/QB64 maintenance** | Strong | High compatibility; modern tooling and build; can gradually replace QB64pe. |
| **Programming education (first language)** | Strong | Simple syntax, immediate feedback, graphics/audio without extra frameworks. |
| **Retro computing and demos** | Strong | Native perf, deterministic timing, 2D graphics and sound built in. |
| **Game prototyping and small 2D games** | Good | Enough built-ins for many classic-style games; DECLARE LIBRARY for more. |
| **Small utilities and scripts** | Good | If a C toolchain is acceptable; good for CLI and local automation. |
| **Compilers and language experiments** | Good | Bootstrap shows that a large, complex BASIC codebase can be the compiler itself. |

### 4.2 Where QB64Fresh Is a Poor Fit

| Use Case | Fit | Reason |
|----------|-----|--------|
| **Web frontend or backend** | Poor | No standard web stack; would need to lean heavily on C and custom bindings. |
| **Mobile or cross-platform apps** | Poor | No UI or mobile framework; desktop and native focus. |
| **Data science, ML, analytics** | Poor | No numerical or ML ecosystem; would require C libs and manual integration. |
| **Large, modular, long-lived product code** | Weak | Limited abstraction and modularity; better to use a language with modules, strong typing, and rich abstraction. |
| **Embedded (resource‑tight)** | Weak | Depends on C runtime, SDL2, etc.; not aimed at bare-metal or tiny MCUs. |

### 4.3 Sustainability

- **Open implementation:** Rust codebase, clear structure, and docs lower the barrier to contribution and long-term maintenance.
- **Bootstrap and tests:** 1,390+ tests, QB4.5 compat suite (122/141 qbasic_testcases, 86.5%), and bootstrap of QB64pe provide confidence for refactors and evolution.
- **Scope discipline:** By not chasing web, mobile, or ML, the project can stay focused and avoid feature creep.

---

## 5. Fit Within the Current Language Landscape

### 5.1 By Paradigm and Domain

```
                    High abstraction
                            │
        Rust, Haskell ●     │     ● TypeScript, Kotlin
                            │
        Go, Zig ●           │           ● Python, Ruby
                            │
   ──────────────●──────────┼──────────●────────────────  Mainstream
                  C         │    JavaScript
                            │
        QB64Fresh ●         │     ● Lua
                            │
                    Low abstraction
                            │
        ◄───────────────────┼───────────────────►
           Systems /       │      Scripting /
           native perf     │      dynamic / embedded
```

QB64Fresh sits in a **procedural, “low-abstraction but batteries-included”** niche: more structured than shell or minimal BASIC, but less abstract than Python or Rust. It is closer to C or early Pascal in “how you think” than to Python or JavaScript.

### 5.2 By Intended User

| If the user wants… | Closer choices | QB64Fresh when… |
|--------------------|----------------|------------------|
| Easiest path to “ something on screen” | Python, Scratch, Lua | They prefer BASIC syntax or have existing BASIC. |
| Native perf and low-level control | C, Rust, Zig | They don’t need Rust-level safety or C-level control. |
| To run or modernize old QB/QB64 code | QB64pe, VB6 (where possible) | They want a Rust-based, LSP-first, maintainable toolchain. |
| To teach sequencing and procedures | Python, Lua, JavaScript | They want compiled, native execution and built-in graphics. |
| A small, embeddable language | Lua, Squirrel | Not a fit; QB64Fresh is a compiler, not an embeddable VM. |
| Web, mobile, or cloud by default | JS/TS, Python, Go, etc. | Not a fit for those domains. |

### 5.3 Niche in One Sentence

**QB64Fresh is a viable choice when you want a simple, compiled, QB-compatible language with built-in 2D graphics and audio, modern editor tooling, and a clean implementation—especially for education, legacy QB code, retro-style games, or as a reference compiler—and when you are not targeting web, mobile, data science, or large-scale software architecture.**

---

## 6. Summary

| Dimension | Assessment |
|-----------|------------|
| **Strengths** | Simplicity, native performance, legacy compatibility, LSP-based tooling, built-in graphics/audio, C interop, clean Rust implementation. |
| **Weaknesses** | BASIC’s limited abstraction, small ecosystem, not aimed at web/mobile/data/ML, dependency on C toolchain and system libs. |
| **Viability** | Strong for education, legacy QB, retro and small 2D games, and tooling; weak for web, mobile, data science, and very large systems. |
| **Place among languages** | Procedural, low-abstraction, batteries-included compiler between “scripting” and “systems”; complements rather than replaces dominant modern languages. |

QB64Fresh does not try to be a general-purpose, do-everything language. It focuses on a narrow band: **QB-style BASIC with modern tooling and a maintainable, educational implementation**. Within that band, it is technically solid, well-tested, and viable for production use in the right kinds of projects.

---

## 7. Related Documents

- [ARCHITECTURE.md](ARCHITECTURE.md) — Compiler pipeline and design
- [QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md](../QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md) — QB64Fresh vs QB64pe semantics
- [QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md](../QB64pe/QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md) — Moving from QB64 to QB64Fresh
- [DEVELOPMENT.md](DEVELOPMENT.md) — Contributing and development setup
- [docs/adrs/](adrs/README.md) — Architecture decision records
