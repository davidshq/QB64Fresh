# Runtime Architecture: Expert Perspectives

QB64Fresh's dual-runtime design: why it exists, trade-offs, and improvements.

## The Architecture

```
┌────────────────────────────────────────────────────────────────────────┐
│                        QB64Fresh Compiler                              │
│                                                                        │
│   BASIC Source → Lexer → Parser → Semantic → CodeGen → C Output       │
│                                                        │               │
│                                              ┌─────────┴─────────┐     │
│                                              ▼                   ▼     │
│                                        --runtime           --runtime   │
│                                          inline             external   │
└────────────────────────────────────────────────────────────────────────┘
                                              │                   │
                                              ▼                   ▼
┌─────────────────────────────────┐  ┌─────────────────────────────────┐
│   Inline Runtime (emits C)      │  │   External Runtime (Rust)        │
│                                 │  │                                 │
│  src/codegen/c_backend/runtime/ │  │  runtime/src/                    │
│  ├── types, strings, io, file   │  │  ├── string.rs     (real impl)   │
│  ├── keyboard, memory, timing   │  │  ├── io.rs, math.rs              │
│  ├── arrays, math, error        │  │  ├── graphics/    (SDL2)        │
│  ├── graphics.rs (stubs!)       │  │  ├── audio/       (rodio)       │
│  ├── audio.rs    (stubs!)       │  │  └── *_ffi.rs     (C bindings)  │
│  └── legacy, system, debug      │  │                                 │
│                                 │  │  Output: .c + libqb64fresh_rt.a │
│  Output: Self-contained .c      │  │  Graphics: Real SDL2 windows    │
│  Graphics: Frame limit + stubs  │  │  Audio: Real sound playback     │
│  Audio: Silent                  │  │  Contract: runtime/include/     │
│  (QB64FRESH_MAX_FRAMES=1000)    │  │          qb64fresh_rt.h         │
└─────────────────────────────────┘  └─────────────────────────────────┘
```

---

## 🦀 Rust Engineer

**Concern:** Memory safety, FFI, idiomatic Rust

- **Like:** External runtime: proper `#[no_mangle] extern "C"`, null checks, ref-counted strings.
- **Concern:** Inline runtime is C in Rust strings—no compiler checks; bugs surface only at GCC. Two implementations → drift (fix in one, forget the other).
- **Recommendation:** Generate inline C from Rust, or at least share tests between both.

---

## 🔤 Language Engineer

**Concern:** Semantic correctness, QB64 compatibility

- **Like:** Inline lets us test compilation (types, control flow, expressions) without SDL2.
- **Concern:** Semantic drift: who verifies INKEY$, POINT, etc. match? Stub POINT returns 0; no framebuffer can hide bugs.
- **Recommendation:** Conformance tests for both runtimes. Document stub-only behavior.

---

## 🖥️ QB64 Expert

**Concern:** Compatibility with QB64pe

- **Like:** Mirrors QB64pe’s split (libqb vs libqb_gfx). Inline stubs respect QB64 quirks (e.g. INKEY$ = CHR$(0)+scan_code for extended keys).
- **Concern:** `_MEM` and QB64pe-specific features get dummy stubs; bootstrap needs “good enough” for compilation, not full behavior.
- **Recommendation:** Bootstrap first. Document which QB64pe features are compile-time vs runtime. Compiler needs file I/O and strings, not `_SNDPLAY`.

---

## 🏗️ Software Architect

**Concern:** Maintainability, extensibility

- **Like:** Clear separation: compiler | inline (portable C) | external (full impl). GraphicsBackend trait (SDL2, Mock) enables headless tests and future backends.
- **Concern:** Inline ~9k lines *emitting* C overlaps `runtime/` ~16k; no shared contract enforces `qb_left` etc. match.
- **Recommendation:** **A)** Generate inline from Rust. **B)** Use `qb64fresh_rt.h` as the single API contract. **C)** Accept duplication, document that inline is intentionally minimal. Prefer **C** with clear docs.

---

## 🔧 Pragmatic Engineer

**Concern:** Does it work? Can we ship?

| Feature | Inline | External | Bootstrap? |
|---------|--------|----------|------------|
| Strings, I/O, keyboard, math | ✅ | ✅ | ✅ |
| Graphics, audio | ⚠️ Stubs | ✅ | ❌ |

Bootstrap needs only the ✅ rows; stubs are enough. Duplication (inline vs external) serves different goals: portability, CI, headless vs full graphics/audio. Unifying now would slow progress.

- **Recommendation:** Document the split (this doc), add conformance + CI, focus on bootstrap, revisit architecture (e.g. generate-inline) when it hurts.

---

## Consensus & Action Items

| Expert | Verdict | Priority |
|--------|---------|----------|
| Rust | C-in-strings risky; share tests | Medium |
| Language | Conformance tests | High |
| QB64 | Bootstrap first | High |
| Architect | Accept duplication + document | Medium |
| Pragmatic | Ship, fix later | High |

**Actions:** Document stub-only features ([STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md)). Long-term: consider generating inline from Rust if duplication hurts.

---

## When to Use Each Runtime

**Inline:** CI, syntax checks, teaching, compiler build, distributing .c, headless debugging.

**External:** Graphics, audio, full QB64 behavior, performance, runtime development.

---

## Related

- [GRAPHICS.md](GRAPHICS.md) — Stubs, QB64FRESH_MAX_FRAMES, backends
- [runtime/include/qb64fresh_rt.h](../runtime/include/qb64fresh_rt.h) — C API
- [STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md)

*Last updated: 2026-01-26*
