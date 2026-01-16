# QB64Fresh - Project Plan

## Project Goal

Create a complete, ground-up rewrite of QB64 - a modern BASIC compiler that compiles QBasic/QuickBASIC-compatible code to native executables.

## Key Principles

### What We're Building
- A modern, clean BASIC compiler
- Language Server Protocol (LSP) implementation for IDE integration
- VSCode extension (and potentially other editor integrations)
- Focus on the compiler intelligence, not IDE reproduction

### What We're NOT Doing
- Not forking or patching the existing codebase
- Not reproducing the built-in IDE (QB64pe has one; we'll use LSP instead)
- Not constrained to the same implementation language
- Not constrained to the same dependencies or architecture

---

## Architecture Analysis Summary (Completed)

Analysis of QB64pe revealed the following key insights:

### QB64pe Component Sizes
| Component | Lines | Language |
|-----------|-------|----------|
| Main Compiler | ~24,000 | QB64 |
| IDE | ~862,000 | QB64 |
| Built-in Functions | ~123,000 | QB64 |
| Runtime Library | ~31,000 | C++ |

### QB64pe Architecture Pattern
- **Self-hosted**: QB64 compiler written in QB64 itself
- **C++ Intermediate**: BASIC → C++ → Native executable
- **No AST**: Direct line-by-line code generation
- **Tightly Coupled IDE**: 862K lines of IDE code in the compiler

### Strengths to Preserve
1. Complete QB4.5/QBasic compatibility
2. Extensive QB64 extensions (_MEM, _SND*, graphics, etc.)
3. Cross-platform support (Windows, Linux, macOS)
4. Comprehensive test suite

### Weaknesses to Address
1. Monolithic compiler (24K lines in one file)
2. No AST limits IDE features and error recovery
3. IDE tightly coupled with compiler
4. No LSP support for modern editors
5. GOTO-heavy legacy code style

### Key QB64 Extensions to Support
- `_MEM` operations (direct memory access)
- `_SND*` functions (audio)
- `_LOADIMAGE`, `_PUTIMAGE`, `_NEWIMAGE` (graphics)
- `_OFFSET` type (pointer-sized integers)
- `$INCLUDE`, `$IF` preprocessor directives
- OpenGL integration
- Console support

---

## Technology Decisions (Complete)

### Decision 1: Implementation Language

**Choice: Rust**

**Rationale:**
- Memory safety without garbage collection - predictable performance
- Excellent pattern matching via `enum` - perfect for AST representation
- Strong compiler tooling ecosystem (`logos`, `chumsky`, `ariadne`, `tower-lsp`)
- Good FFI to C for runtime library integration
- Cargo provides excellent build system and dependency management
- Educational value - demonstrates idiomatic Rust patterns

**Considered alternatives:** Go (less expressive), TypeScript (runtime overhead), C++ (manual memory management risks)

**Status:** Decided

### Decision 2: Code Generation Backend

**Choice: C Intermediate with trait-based backend abstraction**

**Rationale:**
- C backend is proven (QB64pe uses C++), simple to implement, highly portable
- Emitting C text is straightforward - no complex APIs to learn
- Generated C is debuggable/readable
- Trait-based interface allows future backends without changing existing code:

```rust
pub trait CodeGenerator {
    fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, CodeGenError>;
}
```

**Architecture:**
- Build ONE backend (C) following YAGNI
- Design the `CodeGenerator` trait interface cleanly
- Future contributors can add LLVM, Cranelift, etc. as separate implementations
- Adding a backend = adding code, not changing existing code (Open/Closed principle)

**Considered alternatives:** LLVM (complex, large dependency), Cranelift (less mature), direct machine code (enormous effort)

**Status:** Decided

### Decision 3: Runtime Library Approach

**Choice: Hybrid Rust + established crates**

**Rationale:**
- Write runtime "glue" in Rust for clean, safe code
- Leverage battle-tested Rust crates for heavy lifting:
  - **Graphics/Windowing**: `sdl2` or `winit` + `pixels`/`softbuffer`
  - **Audio**: `rodio` (built on `cpal`)
  - **Image loading**: `image` crate
  - **Font rendering**: `fontdue` or `rusttype`
- Avoids inheriting QB64pe's C++ technical debt
- Unified Rust codebase is easier to maintain and understand

**Tradeoffs acknowledged:**
- May have subtle behavioral differences from QB64pe
- Need to carefully map QB64 semantics to library capabilities
- Graphics modes and SCREEN compatibility will need careful design

**Implementation approach:**
- Define Rust traits for runtime capabilities (similar to codegen backend)
- Implement using chosen crates
- Test against QB64pe's test suite for compatibility
- Document any intentional deviations

**Status:** Decided

### Decision 4: Build System and Tooling

**Choice: Cargo (Rust's standard build system)**

**Rationale:**
- Cargo is the standard for Rust projects - excellent dependency management
- Supports workspaces for multi-crate projects
- Built-in test runner, benchmarking, documentation generation
- `cargo fmt` and `cargo clippy` for code quality

**Additional tooling:**
- `just` or `make` for orchestrating multi-step builds (compile QB64 code → run C compiler)
- CI/CD via GitHub Actions with standard Rust workflows

**Status:** Decided

### Decision 5: Testing Framework

**Choice: Built-in Rust testing + QB64pe test suite**

**Rationale:**
- Rust's built-in `#[test]` is sufficient for unit tests
- `#[cfg(test)]` modules keep tests close to code
- Integration tests in `tests/` directory

**Testing strategy:**
1. **Unit tests**: Each compiler phase tested in isolation
2. **Integration tests**: End-to-end compilation of sample programs
3. **Compatibility tests**: Run QB64pe's test suite against QB64Fresh
4. **Golden tests**: Compare output against expected results

**Key test categories from QB64pe to port:**
- `tests/compile_tests/` - Feature-specific compilation tests
- `tests/qbasic_testcases/` - Classic QBasic program compatibility

**Status:** Decided

---

## Phase 1: Architecture Analysis (Completed)

**Goal:** Deeply understand QB64pe's architecture, compilation pipeline, and design decisions.

### Tasks - All Complete
- [x] Map major components and their responsibilities
- [x] Trace the compilation pipeline (source → executable)
- [x] Document the type system and semantic analysis
- [x] Understand the runtime library
- [x] Identify strengths to preserve and weaknesses to address
- [x] Note QB64-specific extensions to QBasic

### Deliverables - All Complete
- [x] `AgenticLogs/2026-01-16_session-001_project-setup.md`
- [x] `docs/QB64PE_ARCHITECTURE_ANALYSIS.md`

---

## Phase 2: Design (Current)

**Goal:** Define QB64Fresh's architecture based on analysis findings.

### Tasks
1. Choose implementation language (with rationale)
2. Design module structure
3. Define compilation pipeline
4. Plan LSP integration from the start
5. Design testing strategy
6. Establish coding conventions

### Deliverables
- Technical design document
- Module interface specifications
- Technology decision records

---

## Phase 3: Implementation

**Goal:** Build QB64Fresh incrementally, with tests alongside.

### Planned Order
1. **Lexer/Tokenizer** - Convert source text to tokens
2. **Parser** - Build Abstract Syntax Tree (AST)
3. **Semantic Analysis** - Type checking, symbol resolution
4. **Code Generation** - Produce executable output
5. **Runtime Library** - Built-in functions, I/O, graphics
6. **LSP Server** - IDE integration
7. **VSCode Extension** - Editor support

### Approach
- Test-driven where practical
- Each component should be independently testable
- Regular integration testing
- Compatibility testing against QBasic programs

---

## Phase 4: Validation

**Goal:** Ensure compatibility and correctness.

### Tasks
1. Run QB64pe test suite against QB64Fresh
2. Test classic QBasic programs
3. Performance benchmarking
4. Edge case and error handling validation

---

## Timeline

No time estimates - we proceed methodically, prioritizing correctness over speed.

---

## Progress Tracking

All work is documented in `AgenticLogs/` with session logs capturing:
- Decisions made
- Code written
- Problems encountered and solutions
- Insights and learnings

---

*Document created: 2026-01-16*
*Last updated: 2026-01-16*
