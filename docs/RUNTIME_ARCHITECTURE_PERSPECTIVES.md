# Runtime Architecture: Expert Perspectives

This document explores QB64Fresh's dual-runtime architecture through the lens of different engineering disciplines. The goal is to understand why this design exists, its trade-offs, and potential improvements.

## The Architecture in Question

QB64Fresh has two runtime implementations:

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
│   Inline Runtime (C Stubs)      │  │   External Runtime (Rust)       │
│                                 │  │                                 │
│  src/codegen/c_backend/runtime/ │  │  runtime/src/                   │
│  ├── strings.rs  (emits C)      │  │  ├── string.rs   (real impl)    │
│  ├── io.rs       (emits C)      │  │  ├── io.rs       (real impl)    │
│  ├── graphics.rs (stubs!)       │  │  ├── graphics/sdl2.rs (SDL2!)   │
│  └── audio.rs    (stubs!)       │  │  └── audio/rodio.rs   (real!)   │
│                                 │  │                                 │
│  Output: Self-contained .c      │  │  Output: .c + libqb64fresh_rt.a │
│  Graphics: Frame counting only  │  │  Graphics: Real SDL2 windows    │
│  Audio: Silent                  │  │  Audio: Real sound playback     │
└─────────────────────────────────┘  └─────────────────────────────────┘
```

---

## 🦀 The Rust Engineer's Perspective

**Primary Concern:** Memory safety, FFI correctness, idiomatic Rust

### What I Like

The external runtime in `runtime/src/` is well-structured Rust:

```rust
// Reference-counted strings with proper cleanup
pub struct QbString { ... }

#[no_mangle]
pub unsafe extern "C" fn qb_string_release(s: *mut QbString) {
    if s.is_null() { return; }
    let header = get_header_mut(s as *mut c_char);
    header.ref_count -= 1;
    if header.ref_count == 0 {
        // Proper deallocation
    }
}
```

The `#[no_mangle]` and `extern "C"` annotations are correct for FFI. The null checks are good defensive programming.

### What Concerns Me

**1. The inline runtime is C code embedded in Rust strings:**

```rust
// This is... not great
writeln!(output, "void qb_print_string(qb_string* s) {{").unwrap();
writeln!(output, "    if (s && s->data) printf(\"%s\", s->data);").unwrap();
writeln!(output, "}}").unwrap();
```

No compiler checks this C code. Typos, memory bugs, undefined behavior - none of it gets caught until GCC compiles the output.

**2. Two implementations means two places for bugs:**

If we fix a bug in the Rust runtime's `qb_instr()`, we might forget to fix it in the inline C version. They can drift apart silently.

### My Recommendation

Consider generating the inline C from the Rust code, or at minimum, share test cases between both implementations. The duplication is a maintenance burden.

---

## 🔤 The Language Engineer's Perspective

**Primary Concern:** Semantic correctness, QB64 compatibility, edge cases

### What I Like

The separation allows testing compilation without graphics dependencies. I can verify that:
- Type checking works
- Control flow is correct
- Expressions evaluate properly

All without needing SDL2 installed.

### What Concerns Me

**Semantic drift between implementations:**

```basic
' What does this return on each runtime?
DIM x AS STRING
x = INKEY$
IF x <> "" THEN PRINT ASC(x)
```

The inline runtime returns:
- Arrow keys: `CHR$(0) + CHR$(72)` for Up

The external runtime returns:
- Arrow keys: `CHR$(0) + CHR$(72)` for Up

Good - they match! But who verifies this? Where's the conformance test suite?

**Missing QB64 semantics in stubs:**

```basic
SCREEN 13
PSET (100, 100), 15
x = POINT(100, 100)  ' Should return 15
```

In inline mode, `POINT` returns 0 because there's no framebuffer. This could mask bugs in programs that depend on reading back pixel values.

### My Recommendation

Create a compatibility test suite that runs against BOTH runtimes. Programs that don't use graphics should produce identical output. Document which features are stub-only.

---

## 🖥️ The QB64 Expert's Perspective

**Primary Concern:** Compatibility with real QB64pe behavior

### Historical Context

QB64pe itself has a similar split! It has:
- `libqb.cpp` - The core runtime
- `libqb_gfx.cpp` - Graphics (optional, can be stubbed)

The QB64Fresh architecture mirrors this, which is good for compatibility reasoning.

### What I Like

The inline stubs handle the tricky QB64 conventions:

```c
// QB64's INKEY$ returns CHR$(0) + scan_code for extended keys
if (ch == 0 || ch == 224) {
    char buf[3] = {0, (char)_getch(), 0};
    return qb_string_new(buf);
}
```

This matches QB64pe's behavior. Arrow keys, function keys, etc. all work correctly.

### What Concerns Me

**QB64pe-specific features may behave differently:**

```basic
' QB64pe-specific
DIM m AS _MEM
m = _MEM(array())
```

The `_MEM` functions are complex and interact with QB64's internal memory model. The stubs return dummy values, but real programs might depend on actual memory addresses.

**The bootstrap chicken-and-egg:**

We're trying to compile QB64pe with QB64Fresh. But QB64pe uses every obscure feature of QB64. The stubs need to be "good enough" for compilation, even if they're not fully functional.

### My Recommendation

Focus on making the bootstrap work first. Document which QB64pe features are used during compilation vs. runtime. The compiler itself doesn't need `_SNDPLAY` - it just needs file I/O and string handling.

---

## 🏗️ The Software Architect's Perspective

**Primary Concern:** System design, maintainability, extensibility

### The Good

**Clear separation of concerns:**

```
Compiler (src/)           - Language processing
Inline Runtime (codegen/) - Portable C generation
External Runtime (runtime/) - Full-featured implementation
```

Each has a distinct responsibility. The compiler doesn't know which runtime will be used.

**The trait-based graphics backend is excellent:**

```rust
pub trait GraphicsBackend {
    fn init(&mut self, width: u32, height: u32) -> Result<()>;
    fn cls(&mut self) -> Result<()>;
    fn pset(&mut self, x: i32, y: i32, color: u32) -> Result<()>;
    // ...
}

// Implementations
pub struct Sdl2Backend { ... }
pub struct MockBackend { ... }  // For testing!
```

This allows headless testing and future backend additions (Vulkan? WebGPU?).

### What Concerns Me

**The inline runtime violates DRY:**

We have ~2000 lines of C-as-Rust-strings that duplicate the ~15000 lines of Rust runtime. When someone adds a feature, they must update both.

**No shared interface definition:**

```rust
// Rust runtime
pub extern "C" fn qb_left(s: *const QbString, n: i32) -> *mut QbString

// Inline runtime (in strings!)
"qb_string* qb_left(qb_string* s, int32_t n) {"
```

These SHOULD be the same signature, but there's no mechanism ensuring they are.

### My Recommendation

**Option A: Generate inline from external**

Write a tool that reads the Rust runtime and generates equivalent C. Complex but eliminates duplication.

**Option B: Shared header file**

Create `qb64fresh_rt.h` as the source of truth. Both implementations must conform to it. The header already exists - enforce it!

**Option C: Accept the duplication**

Document it clearly. The inline runtime is intentionally minimal. Don't try to achieve feature parity - let them serve different purposes.

I lean toward **Option C** with clear documentation.

---

## 🔧 The Pragmatic Engineer's Perspective

**Primary Concern:** Does it work? Can we ship it?

### Reality Check

Let's be honest about what we have:

| Feature | Inline | External | Needed for Bootstrap? |
|---------|--------|----------|----------------------|
| Strings | ✅ Full | ✅ Full | ✅ Yes |
| File I/O | ✅ Full | ✅ Full | ✅ Yes |
| Console I/O | ✅ Full | ✅ Full | ✅ Yes |
| Keyboard | ✅ Full | ✅ Full | ✅ Yes |
| Math | ✅ Full | ✅ Full | ✅ Yes |
| Graphics | ⚠️ Stubs | ✅ SDL2 | ❌ No |
| Audio | ⚠️ Stubs | ✅ Rodio | ❌ No |

**The bootstrap doesn't need graphics or audio.** QB64pe is a compiler - it reads files, processes text, writes files. The inline stubs are sufficient.

### What Actually Matters Right Now

1. **Fix the INI file reading hang** - This is blocking the bootstrap
2. **Ensure file I/O edge cases work** - Binary GET/PUT, random access
3. **Keep keyboard input working** - For interactive prompts

Graphics and audio can wait until someone wants to compile a game.

### The Duplication Isn't Hurting Us

Yes, there are two implementations. But:
- The inline runtime is ~2000 lines of simple C
- It rarely changes (core string/file ops are stable)
- It serves a different purpose (portability vs. features)

Premature unification would slow us down. Ship the bootstrap first, then consider refactoring.

### My Recommendation

1. **Document the split clearly** (this document helps!)
2. **Add a CI job** that compiles a test program with BOTH runtimes
3. **Focus on the bootstrap** - Get QB64pe compiling itself
4. **Revisit architecture later** - When we have working software, we can refactor

---

## Consensus Summary

| Expert | Verdict | Priority |
|--------|---------|----------|
| Rust Engineer | Concerned about C-in-strings | Medium - testing helps |
| Language Engineer | Wants conformance tests | High - catches drift |
| QB64 Expert | Bootstrap-focused | High - practical |
| Architect | Accepts duplication with docs | Medium - document it |
| Pragmatic Engineer | Ship it, fix later | High - unblock progress |

### Action Items

1. **Immediate:** Fix the INI file hang blocking QB64pe bootstrap
2. **Short-term:** Add conformance tests running both runtimes
3. **Medium-term:** Document which features are stub-only
4. **Long-term:** Consider generating inline C from Rust (if duplication becomes painful)

---

## Appendix: When to Use Each Runtime

### Use Inline Runtime When:
- Running in CI/CD (no display server)
- Quick syntax checking
- Teaching/learning (simple setup)
- Compiling compilers (no graphics needed)
- Distributing generated C code

### Use External Runtime When:
- Building graphical applications
- Playing audio
- Need full QB64 compatibility
- Performance matters (optimized Rust)
- Developing the runtime itself
