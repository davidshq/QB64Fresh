# ADR-0002: Code Generation Backend

## Status

**Accepted** - January 16, 2026

## Context

After parsing and semantic analysis, we need to generate executable code. QB64pe transpiles to C++, which has proven effective but couples the implementation to that approach.

Key considerations:
- Need proven, portable solution
- Should support future alternative backends
- Must balance simplicity with extensibility
- Should produce debuggable output

## Decision

**We chose C as the intermediate language with trait-based backend abstraction**.

### Architecture

```rust
pub trait CodeGenerator {
    fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, CodeGenError>;
}
```

**Implementation Strategy:**
- Build ONE backend (C) following YAGNI principle
- Design the `CodeGenerator` trait interface cleanly
- Future contributors can add LLVM, Cranelift, etc. as separate implementations
- Adding a backend = adding code, not changing existing code (Open/Closed principle)

### Rationale

1. **C backend is proven**: QB64pe's success with C++ demonstrates viability
2. **Simple to implement**: Emitting C text is straightforward, no complex APIs
3. **Highly portable**: C compilers available on all platforms
4. **Debuggable**: Generated C is human-readable for troubleshooting
5. **Trait abstraction enables future backends**: Can add LLVM, Cranelift, or others without breaking changes

### Alternatives Considered

- **LLVM**: Complex API, large dependency (~100MB+), steep learning curve
- **Cranelift**: Less mature ecosystem, smaller community
- **Direct machine code**: Enormous implementation effort, platform-specific
- **WebAssembly**: Interesting but doesn't match QB64's target use cases

## Consequences

### Positive

- Proven approach reduces risk
- Generated C code is debuggable by users
- C compiler availability ensures portability
- Trait-based design allows future evolution
- Can leverage existing C compiler optimizations
- Simple text generation is easy to test

### Negative

- Depends on external C compiler (gcc/clang/msvc)
- Compilation is two-phase (BASIC→C→native)
- Generated C may be verbose compared to hand-written
- Some advanced optimizations require LLVM-level IR
- C's type system doesn't perfectly map to QB64's semantics
