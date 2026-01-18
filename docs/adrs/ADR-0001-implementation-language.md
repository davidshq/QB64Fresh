# ADR-0001: Implementation Language

## Status

**Accepted** - January 16, 2026

## Context

QB64Fresh is a complete rewrite of QB64, and we needed to choose an implementation language. The original QB64pe is self-hosted (written in QB64 itself), which creates bootstrapping complexity and limits the use of modern tooling.

Key considerations:
- Need memory safety and predictable performance
- Must support complex AST representation
- Should have good compiler tooling ecosystem
- Should enable modern IDE features (LSP)
- Educational value for contributors

## Decision

**We chose Rust** as the implementation language for QB64Fresh.

### Rationale

1. **Memory safety without garbage collection**: Predictable performance characteristics crucial for a compiler
2. **Excellent pattern matching via `enum`**: Perfect for AST representation and compiler phases
3. **Strong compiler tooling ecosystem**: 
   - `logos` for lexing
   - `chumsky` for parsing
   - `ariadne` for error diagnostics
   - `tower-lsp` for LSP implementation
4. **Good FFI to C**: Enables integration with C runtime libraries
5. **Cargo**: Excellent build system and dependency management
6. **Educational value**: Demonstrates idiomatic Rust patterns for compiler construction

### Alternatives Considered

- **Go**: Less expressive type system, weaker pattern matching
- **TypeScript**: Runtime overhead, less suitable for systems programming
- **C++**: Manual memory management risks, more complex tooling
- **Self-hosted QB64**: Bootstrapping complexity, limited tooling

## Consequences

### Positive

- Memory safety guarantees prevent entire classes of bugs
- Pattern matching makes compiler phases clear and maintainable
- Rich ecosystem of battle-tested crates
- Strong community support for compiler development
- Excellent error messages help contributors

### Negative

- Learning curve for contributors not familiar with Rust
- Longer compile times compared to interpreted languages
- Borrow checker can be challenging for certain compiler patterns
- Smaller potential contributor pool than languages like Python or JavaScript
