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

## Technology Decisions (In Progress)

### Decision 1: Implementation Language

**Options:**
| Language | Pros | Cons |
|----------|------|------|
| **Rust** | Memory safety, excellent for compilers, good tooling, pattern matching | Steeper learning curve, longer compile times |
| **Go** | Simple, fast compilation, good concurrency | Less expressive type system, no generics until recently |
| **TypeScript** | LSP-first natural fit, wide ecosystem | Runtime overhead, not traditional for compilers |
| **C++** | Familiar (runtime is C++), mature tooling | Manual memory management, complexity |
| **Zig** | Simple, C interop, no hidden control flow | Younger ecosystem |

**Status:** Pending discussion

### Decision 2: Code Generation Backend

**Options:**
| Approach | Pros | Cons |
|----------|------|------|
| **C Intermediate** | Simple, portable, proven (QB64pe uses this) | Extra compilation step, limited optimization control |
| **LLVM** | Excellent optimization, multiple targets, JIT possible | Complex integration, large dependency |
| **Cranelift** | Rust-native, fast compilation, simpler than LLVM | Less optimization than LLVM, newer |
| **Direct x86/ARM** | Maximum control, no dependencies | Enormous effort, platform-specific |
| **WebAssembly** | Browser deployment, sandboxed | Limited system access, graphics challenges |

**Status:** Pending discussion

### Decision 3: Runtime Library Approach

**Options:**
| Approach | Pros | Cons |
|----------|------|------|
| **Port existing C++** | Known working, compatible | Carries technical debt |
| **Rewrite in impl language** | Clean design, consistent codebase | Significant effort |
| **Hybrid** | Pragmatic, incremental | Two codebases to maintain |
| **Wrap existing via FFI** | Quick start, full compatibility | FFI complexity, maintenance burden |

**Status:** Pending discussion

### Decision 4: Build System and Tooling

**Status:** Depends on implementation language choice

### Decision 5: Testing Framework

**Status:** Depends on implementation language choice

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
