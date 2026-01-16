# QB64Fresh - Claude Code Project Configuration

## Project Overview

QB64Fresh is a complete ground-up rewrite of QB64, a modern BASIC compiler. This is NOT a fork - it's a fresh implementation informed by analyzing the original QB64 Phoenix Edition.

**Implementation Language:** Rust
**Code Generation:** C intermediate (with trait-based abstraction for future backends)
**IDE Strategy:** LSP-based (no built-in IDE)

## Directory Context

- **QB64pe/** - Original QB64 Phoenix Edition source (READ ONLY - for analysis)
- **QB64Fresh/** - New implementation (this project - WRITE HERE)

---
## Logging System (IMPORTANT)

**THE INFORMATION IN THIS SECTION IS QUINTESSENTIAL, MAKE SURE YOU ALWAYS FOLLOW IT. WE CAN FIX OTHER THINGS, BUT WE NEED TO KNOW WHAT HAPPENED!**

We use a **tiered logging system** to balance readability with completeness:

### Tier 1: AgenticLogs/ (Version Controlled)
**Purpose:** High-level session documentation for public review

**Format:** `YYYY-MM-DD_session-NNN_brief-description.md`

**Content:**
- Decisions made with rationale
- Major implementation milestones
- High-level problem summaries (link to Tier 2 for details)
- What was accomplished

**Update frequency:** Throughout each session, not just at end

### Tier 2: AgenticLogs/IndividualProblems/ (Version Controlled)
**Purpose:** Detailed troubleshooting documentation for education

**Format:** `YYYY-MM-DD_problem-brief-name.md` (use template in `_TEMPLATE.md`)

**When to create:**
- Problem investigation becomes lengthy (3+ attempts)
- Dead ends and false starts have educational value
- Root cause wasn't obvious and discovery process is interesting

**Content:**
- Full investigation path including failed attempts
- What we tried and why
- What we learned from each attempt
- Root cause and resolution
- Key takeaways for future reference

**Workflow:**
1. Start documenting in AgenticLogs as normal
2. If problem becomes complex, create IndividualProblem doc
3. Add brief summary + link in AgenticLogs
4. Continue detailed documentation in IndividualProblem doc

### Tier 3: FullLogs/ (NOT Version Controlled)
**Purpose:** Raw interaction history for reconstruction when needed

**Content:** Complete session transcripts

**Usage guidelines:**
- Write to FullLogs continuously during sessions
- DO NOT read from FullLogs routinely (too verbose)
- Only consult when:
  - Creating IndividualProblem docs that span sessions
  - Need exact details that weren't captured in other tiers
  - Debugging a problem requires exact reproduction steps
- Try documentation, web search, and refined logs FIRST

**Why separate:** Raw logs are too verbose for version control and regular use, but valuable as backup for detailed reconstruction.

### Logging Best Practices
- Sanitize all logs: relative paths only, no sensitive system info
- Link between tiers when referencing related content
- Err on the side of more detail in IndividualProblems - it's educational
- AgenticLogs should be readable standalone (no required links to understand)

### Session Start Checklist

**At the START of every session, Claude MUST:**

1. **Add a logging todo item** to the todo list:
   ```
   - [ ] Update AgenticLogs (keep in_progress throughout session)
   ```

2. **Check for existing session logs** - determine the next session number:
   ```bash
   ls AgenticLogs/
   ```

3. **Create the session log file early** - don't wait until the end

4. **For complex problems** - create IndividualProblems doc AS SOON AS the problem requires 3+ attempts, not after resolution

### Pre-Commit Checklist

**BEFORE making any git commit, Claude MUST verify:**

1. **AgenticLogs updated?**
   - [ ] Session log exists for today's work
   - [ ] Major decisions and milestones documented
   - [ ] Any complex problems have IndividualProblems entries

2. **FullLogs updated?**
   - [ ] Session transcript written to FullLogs/

3. **If logs are NOT updated:**
   - STOP and update them BEFORE committing
   - This is a hard requirement, not optional

**This checklist is a safety net. Ideally, logs are updated incrementally throughout the session, making this just a quick verification.**

---

## Core Principles

### 1. Educational Value

This project should serve as an excellent learning resource for:
- **Rust learners** - Idiomatic Rust patterns, ownership, traits, error handling
- **Compiler enthusiasts** - How to build a real language from scratch
- **Software architects** - Clean separation of concerns, extensible design

**In practice:**
- Write clear, well-commented code (explain the "why", not just the "what")
- Use idiomatic Rust patterns and explain them when non-obvious
- Structure code so each module demonstrates a compiler concept
- Include doc comments that teach, not just describe
- Reference relevant compiler theory where appropriate

### 2. Pragmatic Engineering

Balance well-designed software with practical delivery:

**DO:**
- Build clean abstractions at natural boundaries
- Write tests for complex logic
- Design interfaces before implementations
- Refactor when it genuinely improves the code

**DON'T:**
- Over-engineer for hypothetical futures (YAGNI)
- Add abstraction layers "just in case"
- Optimize before profiling
- Gold-plate features nobody asked for

**The test:** If an abstraction doesn't make the current code simpler or more testable, it's probably premature.

### 3. Clean Architecture

Follow the natural compiler pipeline with clear boundaries:

```
Source → Lexer → Parser → AST → Semantic Analysis → Typed IR → CodeGen → Output
                                                              ↑
                                                    Backend trait interface
```

Each phase should be:
- Independently testable
- Single responsibility
- Well-documented

---

## Code Style Guidelines

### Rust Conventions
- Follow standard Rust formatting (`cargo fmt`)
- Use `clippy` lints
- Prefer `Result` over panics for recoverable errors
- Use meaningful type names that reflect domain concepts

### Documentation
- All public items get doc comments
- Module-level docs explain the "what" and "why"
- Include examples in doc comments where helpful
- Link to relevant compiler concepts/theory

### Naming
```rust
// Types: PascalCase, domain-specific
struct BinaryExpr { ... }
enum Statement { ... }

// Functions: snake_case, verb phrases
fn parse_expression() -> Result<Expr, ParseError>
fn emit_c_code(ir: &TypedProgram) -> String

// Modules: snake_case, noun phrases
mod lexer;
mod semantic_analysis;
mod c_backend;
```

### Error Handling
- Use custom error types with good messages
- Errors should help users fix their code
- Include source locations in all diagnostics

---

## Architecture Decisions Record

### Decision 1: Implementation Language
**Choice:** Rust
**Rationale:** Memory safety, excellent pattern matching for AST work, strong ecosystem for compiler tooling (logos, chumsky, ariadne), good FFI for C runtime integration.

### Decision 2: Code Generation Backend
**Choice:** C intermediate with trait-based abstraction
**Rationale:**
- C backend is proven (QB64pe uses C++), simple, portable
- Trait interface allows future backends (LLVM, Cranelift) without changing existing code
- Follows YAGNI - build one backend, but design the interface cleanly

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

---

## Key Files Reference

### Current Implementation (as of 2026-01-16)

| File | Purpose | Status |
|------|---------|--------|
| `src/lib.rs` | Library root, module exports, prelude | ✓ Complete |
| `src/main.rs` | CLI entry point (--tokens, --ast, --typed-ir, --emit-c) | ✓ Complete |
| `src/lexer/mod.rs` | Lexer wrapper with iterator interface | ✓ Complete |
| `src/lexer/token.rs` | Token definitions using logos macros | ✓ Complete |
| `src/ast/mod.rs` | AST root: Span, Program types | ✓ Complete |
| `src/ast/expr.rs` | Expression AST nodes | ✓ Complete |
| `src/ast/stmt.rs` | Statement AST nodes | ✓ Complete |
| `src/parser/mod.rs` | Pratt parser + recursive descent (~1600 lines) | ✓ Complete |
| `src/parser/error.rs` | Parse error types with spans | ✓ Complete |
| `src/semantic/mod.rs` | Semantic analyzer entry point, built-ins | ✓ Complete |
| `src/semantic/error.rs` | Semantic error types with spans | ✓ Complete |
| `src/semantic/types.rs` | BasicType enum, type inference, conversions | ✓ Complete |
| `src/semantic/symbols.rs` | Symbol table with scope management | ✓ Complete |
| `src/semantic/checker.rs` | Type checker for expressions/statements | ✓ Complete |
| `src/semantic/typed_ir.rs` | Typed IR output for codegen | ✓ Complete |
| `src/codegen/mod.rs` | CodeGenerator trait, GeneratedOutput | ✓ Complete |
| `src/codegen/error.rs` | Code generation error types | ✓ Complete |
| `src/codegen/c_backend.rs` | C code generation (~1100 lines) | ✓ Complete |
| `examples/hello.bas` | Test BASIC file for development | ✓ Complete |
| `examples/simple.bas` | Simpler test BASIC file | ✓ Complete |

### Configuration Files

| File | Purpose |
|------|---------|
| `Cargo.toml` | Rust package manifest and dependencies |
| `rustfmt.toml` | Code formatting configuration |
| `.editorconfig` | Cross-editor formatting rules |
| `.vscode/launch.json` | VS Code debug configurations |
| `.vscode/extensions.json` | Recommended VS Code extensions |
| `.github/workflows/ci.yml` | GitHub Actions CI pipeline |

### Documentation

| File | Purpose |
|------|---------|
| `CLAUDE.md` | AI assistant configuration (this file) |
| `DEVELOPMENT.md` | Developer onboarding guide |
| `PROJECT_PLAN.md` | Project roadmap and phases |
| `docs/QB64PE_ARCHITECTURE_ANALYSIS.md` | Original QB64 architecture analysis |
| `docs/PARSER_PLAN.md` | Detailed parser implementation plan |
| `docs/QB64_SYNTAX_REFERENCE.md` | QB64 language syntax quick reference |

### Project Structure
```
src/
├── ast/          # ✓ AST type definitions (complete)
├── parser/       # ✓ Pratt parser + recursive descent (complete)
├── semantic/     # ✓ Type checking, symbol resolution (complete)
├── codegen/      # ✓ Backend trait + C implementation (complete)
│   ├── mod.rs    # CodeGenerator trait
│   ├── error.rs  # CodeGenError types
│   └── c_backend.rs  # C code generation
├── runtime/      # Runtime library interface (TODO)
└── lsp/          # Language server (TODO)
```

---

## Lessons Learned

### Tooling (Verified 2026-01)

**Cargo tools:**
- `cargo-watch` is **deprecated** - use `bacon` instead
- `cargo-tree` is built into cargo - no install needed, just use `cargo tree`
- `cargo-nextest` is genuinely faster (up to 3x) - worth using
- `cargo-audit` is essential for security scanning

**VS Code extensions:**
- `serayuzgur.crates` is deprecated → use `fill-labs.dependi` instead
- `rust-analyzer` is the only Rust extension needed (the old `rust-lang.rust` is deprecated)
- `errorlens` is nice-to-have but not essential - rust-analyzer's diagnostics are sufficient

**When recommending tools:**
- Always verify current status before recommending - ecosystems change
- Check for deprecation notices in READMEs
- Search for "[tool] vs [alternative] [current year]" to find recent comparisons

### Rust Patterns for This Project

**Borrow checker with error handling:**
When you need token/borrowed data for error messages while also accessing `self.errors`:
```rust
// DON'T - closure captures self.errors while self is borrowed
let token = self.peek().ok_or_else(|| {
    self.errors.push(ParseError::eof("expression"));
})?;

// DO - extract values first, then handle error
let token = match self.peek() {
    Some(t) => t,
    None => {
        self.errors.push(ParseError::eof("expression"));
        return Err(());
    }
};

// For complex cases, clone needed values before error handling:
let token_kind = token.kind.clone();
let token_span: Span = token.span.clone().into();
// Now token borrow is released, can use self.errors
```

**Range<T> is not Copy:**
Even when T is Copy, Range<T> must be cloned:
```rust
// DON'T
let span: Span = token.span.into();  // Error: can't move from borrow

// DO
let span: Span = token.span.clone().into();
```

### Common Pitfalls

**API assumptions:** Always read the actual type definitions before writing dependent code. The parser was written assuming `Token<'a>` but Token owns its data - causing 34+ compilation errors.

**Closure borrowing:** Closures in `.ok_or_else()`, `.map_err()`, etc. capture their environment. If that conflicts with an existing borrow, use explicit `match` instead.

---

## Reference Material

When implementing, refer to:
- `QB64pe/source/qb64pe.bas` - Original compiler logic
- `QB64pe/source/subs_functions/` - Built-in function definitions
- `QB64pe/internal/c/libqb*` - Runtime library implementation
- `QB64pe/tests/` - Compatibility test cases

### Compatibility Test Suite

`QB64pe/tests/qbasic_testcases/` contains **143 BASIC programs**:

| Directory | Relevance | Notes |
|-----------|-----------|-------|
| `qb45com/` | **High** | QB4.5 compatibility - our core target |
| `misc/` | Medium | Mixed; many use QB64-specific extensions |
| `n54/`, `pete/`, `thebob/` | Medium | Contributor collections; check for QB64 extensions |
| `open_gl/` | **Skip** | Uses `_GL` commands - we're using SDL2/winit, not raw OpenGL |

**Future milestone:** Start with `qb45com/` for core language compatibility. Programs using `_` prefixed commands (like `_SNDPLAYFILE`, `_GL*`, `_UNSIGNED`) are QB64 extensions that may not be in our initial scope.

**THE MOST IMPORTANT PART OF THIS ENTIRE DOCUMENT IS THE Logging System SECTION, MAKE SURE YOU ALWAYS, ALWAYS, ALWAYS FOLLOW IT!**
