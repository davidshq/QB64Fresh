# QB64Fresh Rewrite - Session 001: Project Setup
**Date:** 2026-01-16
**Session Focus:** Initial project configuration and planning

---

## Overview

This log documents the beginning of a complete ground-up rewrite of QB64 (a modern BASIC compiler). The goal is to analyze the existing QB64 Phoenix Edition codebase and create a fresh, clean implementation.

## Project Structure

```
qb64contain/              # Working directory for Claude Code
├── QB64pe/               # Source: Existing QB64 Phoenix Edition (analysis target)
├── QB64Fresh/            # Destination: New implementation (rewrite)
│   └── AgenticLogs/      # This directory - session logs and documentation
```

## Session Log

### 1. Working Directory Configuration

**Decision:** Run Claude Code from `qb64contain/` (parent directory)

**Rationale:**
- Equal access to both `QB64pe/` (read/analyze) and `QB64Fresh/` (write new code)
- Natural workflow for "study original → write new" pattern
- Clean separation between original and rewrite
- Easy cross-reference and comparison

### 2. Directory Verification

- Confirmed `QB64pe/` contains complete QB64 Phoenix Edition source:
  - `source/` - Main source code
  - `internal/` - Internal components
  - `tests/` - Test suite
  - Build files (Makefile, setup scripts)
  - Documentation and licenses

- Confirmed `QB64Fresh/` is empty (git repo initialized, no commits)
  - Ready for fresh implementation

### 3. AgenticLogs Directory Created

Purpose: Maintain transparent, reviewable documentation of the entire rewrite process.

Security considerations for logs:
- Use relative paths from project root only
- No system paths, usernames, or sensitive environment details
- No credentials or API keys
- Focus on technical decisions and code-related information

### 4. Claude Code Configuration

Created `CLAUDE.md` with project-specific instructions:
- Guidelines for reading QB64pe (analysis) vs writing QB64Fresh (implementation)
- Logging requirements
- Code style preferences

### 5. Project Plan Created

Created `PROJECT_PLAN.md` outlining:
- Phase 1: Architecture Analysis (this session)
- Phase 2: Design
- Phase 3: Implementation (Lexer → Parser → Semantic → CodeGen → Runtime → LSP)
- Phase 4: Validation

### 6. Architecture Analysis Completed

Comprehensive analysis documented in `docs/QB64PE_ARCHITECTURE_ANALYSIS.md`

**Key Findings:**

#### Component Size Analysis
| Component | Lines | Language |
|-----------|-------|----------|
| Main Compiler | ~24,000 | QB64 |
| IDE | ~862,000 | QB64 |
| Built-in Functions | ~123,000 | QB64 |
| Runtime Library | ~31,000 | C++ |

#### Architecture Pattern
- **Self-hosted**: QB64 compiler is written in QB64 itself
- **C++ Intermediate**: Compiles BASIC → C++ → Native executable
- **No AST**: Direct code generation without intermediate representation
- **Line-by-line**: Processes source code one line at a time

#### Compilation Pipeline
1. Preprocessor ($IF, $INCLUDE, etc.)
2. Lexical Analysis (element extraction)
3. Parsing & Semantic Analysis
4. C++ Code Generation
5. C++ Compilation (GCC/MinGW/Clang)
6. Native Executable Output

#### Runtime Library Structure (`internal/c/`)
- `libqb.cpp` - Main runtime (31K lines)
- `libqb/src/` - Modular components:
  - graphics.cpp (105K) - Rendering
  - shell.cpp (54K) - Process execution
  - filesystem.cpp (36K) - File I/O
  - And 30+ more modules

#### External Dependencies
- FreeGLUT (OpenGL windowing)
- STB libraries (image loading)
- miniaudio (audio playback)
- FreeType (font rendering)
- zlib (compression)

#### Identified Strengths
1. Complete QB4.5/QBasic compatibility
2. Extensive QB64 extensions
3. Cross-platform support
4. Large test suite

#### Identified Weaknesses (Opportunities for QB64Fresh)
1. Monolithic 24K line compiler file
2. No proper AST - limits IDE features
3. Massive IDE tightly coupled with compiler
4. No LSP support
5. Limited error recovery
6. GOTO-heavy legacy code style

---

## Key Decisions Made

1. **Working Directory**: Stay in `qb64contain/` for equal access to both codebases
2. **No IDE Reproduction**: Will implement LSP + VSCode extension instead
3. **Not Constrained**: Free to choose different language, dependencies, architecture
4. **Logging Approach**: AgenticLogs for transparency and reproducibility

---

## Artifacts Created

- `QB64Fresh/PROJECT_PLAN.md` - Project roadmap
- `QB64Fresh/CLAUDE.md` - Claude Code configuration
- `QB64Fresh/AgenticLogs/` - This session log
- `QB64Fresh/docs/QB64PE_ARCHITECTURE_ANALYSIS.md` - Comprehensive architecture analysis

---

## Technology Decisions (All Complete)

### Decision 1: Implementation Language
**Choice:** Rust

**Rationale:**
- Memory safety without GC - predictable performance
- Excellent pattern matching via `enum` - perfect for AST work
- Strong ecosystem: `logos` (lexer), `chumsky` (parser), `ariadne` (errors), `tower-lsp`
- Good FFI to C for runtime integration
- Educational value - demonstrates idiomatic Rust

### Decision 2: Code Generation Backend
**Choice:** C intermediate with trait-based abstraction

**Rationale:**
- C backend is proven (QB64pe uses C++), simple, portable
- Trait interface allows future backends without changing existing code
- YAGNI compliant - build one backend, but design interface cleanly

```rust
pub trait CodeGenerator {
    fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, CodeGenError>;
}
```

### Decision 3: Runtime Library Approach
**Choice:** Hybrid Rust + established crates

**Rationale:**
- Write runtime in Rust for unified, safe codebase
- Use proven crates: `sdl2`/`winit` (graphics), `rodio` (audio), `image` (images)
- Avoids inheriting QB64pe's C++ technical debt
- May have subtle behavioral differences - document and test carefully

### Decision 4: Build System
**Choice:** Cargo (Rust standard)

### Decision 5: Testing Framework
**Choice:** Built-in Rust testing + QB64pe test suite compatibility

---

## Project Principles Established

1. **Educational Value**: Code should teach Rust and compiler construction
2. **Pragmatic Engineering**: Balance good design with YAGNI
3. **Clean Architecture**: Natural compiler phases with clear boundaries
4. **Regular Logging**: Update AgenticLogs throughout sessions, not just at end

---

## Development Environment Setup

### Verified Existing Tools
- Rust 1.88.0, Cargo 1.88.0, clippy, rustfmt, rust-src
- GCC 13.3.0 (for C backend)
- VS Code 1.108.1

### Tooling Research & Corrections

During setup, we verified current best practices and found several outdated recommendations:

| Original Recommendation | Status | Correction |
|------------------------|--------|------------|
| `cargo-watch` | Deprecated | Use `bacon` instead |
| `cargo-tree` | Unnecessary | Built into cargo since 1.44 |
| `serayuzgur.crates` | Deprecated | Use `fill-labs.dependi` |

### Installed Tools

**VS Code Extensions:**
- `rust-lang.rust-analyzer` - Rust language support
- `tamasfe.even-better-toml` - TOML file support
- `fill-labs.dependi` - Dependency management
- `vadimcn.vscode-lldb` - Debugging

**Cargo Tools:**
- `bacon` - File watcher with TUI (replaces cargo-watch)
- `cargo-nextest` - Faster test runner
- `cargo-audit` - Security scanning

### Developer Documentation Created

- `README.md` - Project overview and entry point
- `DEVELOPMENT.md` - Comprehensive developer onboarding guide

---

## Logging System Established

Created tiered logging system:
- **Tier 1**: `AgenticLogs/` - High-level session documentation
- **Tier 2**: `AgenticLogs/IndividualProblems/` - Detailed troubleshooting
- **Tier 3**: `FullLogs/` - Raw interaction logs (git-ignored)

---

## Implementation Started

### Rust Project Initialized

Created `Cargo.toml` with initial dependencies:
- `logos` 0.14 - Lexer generation
- `ariadne` 0.4 - Error reporting
- `clap` 4 - CLI argument parsing
- `thiserror` 1.0 - Error handling
- `log` + `env_logger` - Logging

### Lexer Module Implemented

Created `src/lexer/` with:
- `mod.rs` - Lexer wrapper with iterator interface
- `token.rs` - Token definitions using logos macros

**Token categories implemented:**
- Control flow keywords (IF, THEN, ELSE, FOR, WHILE, etc.)
- Declaration keywords (DIM, AS, CONST, SUB, FUNCTION, etc.)
- Type keywords (INTEGER, LONG, STRING, plus QB64 extensions)
- I/O keywords (PRINT, INPUT, OPEN, etc.)
- Operators (arithmetic, comparison, logical)
- Literals (integers, floats, hex, octal, binary, strings)
- Punctuation and delimiters
- Comments (both ' and REM styles)
- QB64 metacommands ($INCLUDE, etc.)

### Tests Passing

- 14 unit tests
- 7 doc tests
- All passing

### CLI Working

Basic CLI implemented with:
- `qb64fresh <file.bas>` - Shows compilation status
- `qb64fresh <file.bas> --tokens` - Dumps lexer output
- `qb64fresh <file.bas> --verbose` - Verbose mode

### Example File Created

`examples/hello.bas` - Test file demonstrating various BASIC constructs

---

## Next Steps

1. Implement parser (AST construction)
2. Define AST types
3. Build expression parser with precedence climbing

---

## Technical Notes

- AI Assistant: Claude Opus 4.5
- Project Type: Compiler/Language implementation (BASIC → executable)
- Target: Complete rewrite, not a fork or patch
- IDE Strategy: LSP-based, not built-in IDE
- Implementation: Rust with C code generation

---

*Session completed: 2026-01-16*
