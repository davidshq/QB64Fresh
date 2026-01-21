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

2. **If logs are NOT updated:**
   - STOP and update them BEFORE committing
   - This is a hard requirement, not optional

**This checklist is a safety net. Ideally, logs are updated incrementally throughout the session, making this just a quick verification.**

---

## TODO and Completed Item Maintenance

**When marking items as completed in TODO.md or TESTING_INFRASTRUCTURE_PLAN.md:**

1. **Move completed items to their respective archive files:**
   - `TODO.md` completed items → `TODO-completed.md`
   - `TESTING_INFRASTRUCTURE_PLAN.md` completed items → `docs/ThingsToDo/TESTING-COMPLETED.md`

2. **Format for completed items:**
   - Use `[x]` checkbox syntax
   - Include any relevant metrics or dates (e.g., "**81.63%** achieved!")
   - Group with related completed items in the archive

3. **Keep TODO files clean:**
   - Only uncompleted `[ ]` items should remain in TODO.md and TESTING_INFRASTRUCTURE_PLAN.md
   - Remove redundant checkboxes and strikethrough text after moving
   - Update session numbers and dates in file headers

4. **Session end check:**
   - Before ending a session, scan TODO.md for any `[x]` items and move them
   - This keeps the active TODO focused on remaining work

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

### Documentation (MANDATORY - Follow These Standards)

**CRITICAL:** All code must follow Rust community documentation best practices. Documentation is not optional - it's part of delivering quality code. Use `#![warn(missing_docs)]` in modules to enforce this.

#### Comment Types

**`///` - Item Documentation (Doc Comments)**
Use for the item that follows (functions, structs, enums, traits, type aliases):

```rust
/// Parses a BASIC expression using Pratt parsing.
///
/// Handles operator precedence, function calls, and array access.
/// Returns a typed AST node with source location information.
///
/// # Arguments
///
/// * `min_precedence` - Minimum precedence level for this parse context
///
/// # Returns
///
/// The parsed expression, or `Err(())` if parsing failed (errors accumulated in `self.errors`).
///
/// # Example
///
/// ```ignore
/// let expr = parser.parse_expression(Precedence::Lowest)?;
/// assert!(matches!(expr.kind, ExprKind::Binary { .. }));
/// ```
pub fn parse_expression(&mut self, min_precedence: Precedence) -> Result<Expr, ()> {
    // ...
}
```

**`//!` - Module/Crate Documentation**
Use at the top of a file to document the containing module:

```rust
//! # Lexical Analysis
//!
//! This module tokenizes QB64 BASIC source code using the `logos` crate.
//!
//! ## Design Notes
//!
//! - Case-insensitive keyword matching
//! - Preserves original source spans for error reporting
//! - Handles BASIC-specific tokens (type suffixes like `$`, `%`, `&`)
//!
//! ## Example
//!
//! ```
//! use qb64fresh::lexer::lex;
//! let tokens = lex("PRINT \"Hello\"");
//! ```
```

#### Standard Documentation Sections

Use these conventional headings consistently:

| Section | When to Use | Required? |
|---------|-------------|-----------|
| `# Arguments` | Functions with parameters | Yes, if params exist |
| `# Returns` | Functions returning values | Yes, if non-void |
| `# Example` / `# Examples` | Public APIs | Strongly encouraged |
| `# Panics` | Functions that can panic | Yes, if it panics |
| `# Errors` | Functions returning `Result` | Yes, list error cases |
| `# Safety` | `unsafe` functions | **Mandatory** |

#### Documentation Requirements by Item Type

**Modules (`//!`):**
- Brief one-line summary
- Purpose and responsibilities
- Key concepts or design notes
- Usage example (can use `ignore` if complex setup needed)
- Links to related modules

**Structs/Enums:**
- Brief one-line summary
- Field/variant documentation for public fields
- Example of construction and use

**Functions/Methods:**
- Brief one-line summary (appears in rustdoc listings)
- Detailed behavior description
- All parameters documented
- Return value documented
- Example (runnable if possible)
- Error conditions for `Result` returns

**Error Enums:**
- Each variant MUST be documented explaining:
  - What condition causes this error
  - What the user should do to fix it
  - Example of code that triggers it (where helpful)

```rust
/// Errors that can occur during parsing.
#[derive(Debug, Clone)]
pub enum ParseError {
    /// Expected a specific token but found something else.
    ///
    /// This typically occurs when syntax is malformed, such as
    /// missing parentheses or incorrect keyword ordering.
    ///
    /// # Example
    ///
    /// ```basic
    /// IF x > 5   ' Missing THEN keyword
    /// ```
    UnexpectedToken {
        expected: String,
        found: TokenKind,
        span: Span,
    },

    /// Reached end of input while expecting more tokens.
    ///
    /// Usually indicates an unclosed block (IF without END IF,
    /// FOR without NEXT, etc.) or incomplete expression.
    UnexpectedEof {
        expected: String,
        span: Span,
    },
}
```

#### Best Practices

1. **First line is a summary** - Keep concise; it appears in search results and module listings

2. **Examples are tests** - Code in `# Examples` blocks runs during `cargo test --doc`
   - Use `ignore` for examples that need external setup
   - Use `no_run` for examples that compile but shouldn't execute
   - Use `should_panic` for examples demonstrating panic behavior

3. **Link to related items** - Use backtick syntax for auto-linking:
   ```rust
   /// See [`Parser::parse_expression`] for expression handling.
   /// Returns a [`TypedExpr`] with the inferred [`BasicType`].
   ```

4. **Document the "why"** - Implementation details that aren't obvious:
   ```rust
   /// Uses Pratt parsing for correct operator precedence.
   /// Array access uses FunctionCall AST node because syntax is identical.
   ```

5. **ASCII diagrams for architecture** - Include in module docs:
   ```rust
   //! ## Pipeline
   //!
   //! ```text
   //! Source → Lexer → Parser → AST → Semantic → TypedIR → CodeGen → C
   //! ```
   ```

6. **Avoid documenting the obvious** - Don't write:
   ```rust
   /// Returns the name.  // BAD - says nothing useful
   fn name(&self) -> &str

   /// Returns the variable name including any type suffix (e.g., "count%").
   fn name(&self) -> &str  // GOOD - explains what "name" means in context
   ```

#### Enforcement

- Run `cargo doc --no-deps` to verify documentation builds
- Run `cargo test --doc` to verify examples compile and run
- Consider `#![warn(missing_docs)]` at crate root for public API enforcement

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

### Decision 4: Build System and Tooling
**Choice:** Cargo as primary build system with auxiliary orchestration
**Rationale:**
- Standard Rust tooling reduces friction for contributors
- Workspace support for multi-crate project (compiler + runtime + LSP)
- Integrated testing, formatting (`cargo fmt`), linting (`cargo clippy`)
- Cross-platform consistency

### Decision 5: Testing Framework
**Choice:** Rust's built-in testing + QB64pe test suite
**Rationale:**
- Unit tests (`#[test]`) for each compiler phase
- Integration tests for end-to-end compilation
- Golden tests for regression detection
- Compatibility tests using QB64pe's `qbasic_testcases/`

### Decision 6: Graphics System
**Choice:** Trait-based pluggable backend with SDL2 primary implementation
**Rationale:**
- `GraphicsBackend` trait enables mock backend for CI/headless testing
- SDL2 proven by QB64pe, cross-platform, well-maintained
- Feature flags for compile-time backend selection
- See `runtime/src/graphics/` for implementation

### Decision 7: Audio System
**Choice:** Trait-based pluggable backend mirroring graphics, rodio planned
**Rationale:**
- `AudioBackend` trait consistent with graphics architecture
- rodio is pure Rust, simpler integration than SDL2_mixer
- Mock backend enables CI testing without audio hardware
- Handle-based API matches QB64 semantics
- See `runtime/src/audio/` for implementation

### Decision 8: C Interoperability (DECLARE LIBRARY)
**Choice:** First-class language feature with direct C code generation
**Rationale:**
- Natural fit since we emit C code anyway
- Syntax matches QB64 for compatibility
- Enables system API calls and third-party library integration
- Supports static and dynamic libraries, ALIAS for name mangling
- See `docs/adrs/ADR-0008-c-interoperability.md` for type mapping details

*For detailed ADRs with full rationale and consequences, see `docs/adrs/`.*

---

## Key Files Reference

### Current Implementation (as of 2026-01-21)

| File | Purpose | Status |
|------|---------|--------|
| `src/lib.rs` | Library root, module exports, prelude | ✓ Complete |
| `src/main.rs` | CLI entry point (--tokens, --ast, --typed-ir, --emit-c) | ✓ Complete |
| `src/preprocessor.rs` | $INCLUDE directive processing | ✓ Complete |
| `src/lexer/mod.rs` | Lexer wrapper with iterator interface | ✓ Complete |
| `src/lexer/token.rs` | Token definitions using logos macros | ✓ Complete |
| `src/ast/mod.rs` | AST root: Span, Program types | ✓ Complete |
| `src/ast/expr.rs` | Expression AST nodes | ✓ Complete |
| `src/ast/stmt.rs` | Statement AST nodes | ✓ Complete |
| `src/parser/mod.rs` | Parser entry point and tests | ✓ Complete |
| `src/parser/tokens.rs` | Token navigation utilities | ✓ Complete |
| `src/parser/expressions.rs` | Pratt parser for expressions | ✓ Complete |
| `src/parser/statements.rs` | Statement parsing (core) | ✓ Complete |
| `src/parser/graphics.rs` | Graphics statement parsing | ✓ Complete |
| `src/parser/audio.rs` | Audio statement parsing | ✓ Complete |
| `src/parser/file_io.rs` | File I/O statement parsing | ✓ Complete |
| `src/parser/system.rs` | System statement parsing | ✓ Complete |
| `src/parser/control_flow.rs` | IF/FOR/WHILE/DO/SELECT parsing | ✓ Complete |
| `src/parser/procedures.rs` | SUB/FUNCTION/TYPE definitions | ✓ Complete |
| `src/parser/directives.rs` | Preprocessor directives ($IF, $LET) | ✓ Complete |
| `src/parser/error.rs` | Parse error types with spans | ✓ Complete |
| `src/semantic/mod.rs` | Semantic analyzer entry point, built-ins | ✓ Complete |
| `src/semantic/error.rs` | Semantic error types with spans | ✓ Complete |
| `src/semantic/types.rs` | BasicType enum, type inference, conversions | ✓ Complete |
| `src/semantic/symbols.rs` | Symbol table with scope management | ✓ Complete |
| `src/semantic/typed_ir.rs` | Typed IR output for codegen | ✓ Complete |
| `src/semantic/checker/mod.rs` | Type checker entry point | ✓ Complete |
| `src/semantic/checker/expressions.rs` | Expression type checking | ✓ Complete |
| `src/semantic/checker/statements.rs` | Statement type checking | ✓ Complete |
| `src/semantic/checker/control_flow.rs` | Control flow type checking | ✓ Complete |
| `src/semantic/checker/assignments.rs` | Assignment validation | ✓ Complete |
| `src/semantic/checker/definitions.rs` | Definition handling | ✓ Complete |
| `src/semantic/checker/const_eval.rs` | Constant evaluation | ✓ Complete |
| `src/codegen/mod.rs` | CodeGenerator trait, GeneratedOutput | ✓ Complete |
| `src/codegen/error.rs` | Code generation error types | ✓ Complete |
| `src/codegen/c_backend/mod.rs` | C backend entry point | ✓ Complete |
| `src/codegen/c_backend/expr.rs` | Expression code generation | ✓ Complete |
| `src/codegen/c_backend/stmt.rs` | Statement code generation (core) | ✓ Complete |
| `src/codegen/c_backend/file_io.rs` | File I/O helpers (OPEN, CLOSE, GET, PUT) | ✓ Complete |
| `src/codegen/c_backend/types.rs` | Type mapping utilities | ✓ Complete |
| `src/codegen/c_backend/runtime.rs` | Inline C runtime library | ✓ Complete |
| `src/codegen/c_backend/analysis.rs` | DATA/label collection | ✓ Complete |
| `src/codegen/c_backend/const_fold.rs` | Constant folding optimization | ✓ Complete |
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
| `docs/QB64PE_ARCHITECTURE_ANALYSIS.md` | Original QB64 architecture analysis |
| `docs/PARSER_PLAN.md` | Detailed parser implementation plan |
| `docs/QB64_SYNTAX_REFERENCE.md` | QB64 language syntax quick reference |

### Project Structure
```
QB64Fresh/                    # Main compiler workspace
├── src/
│   ├── ast/                  # ✓ AST type definitions
│   │   ├── mod.rs            # Span, Program types
│   │   ├── expr.rs           # Expression nodes
│   │   └── stmt.rs           # Statement nodes
│   ├── lexer/                # ✓ Logos-based tokenizer
│   │   ├── mod.rs            # Lexer wrapper
│   │   └── token.rs          # Token definitions
│   ├── parser/               # ✓ Pratt parser + recursive descent
│   │   ├── mod.rs            # Entry point, tests
│   │   ├── tokens.rs         # Token navigation
│   │   ├── expressions.rs    # Expression parsing
│   │   ├── statements.rs     # Statement parsing (core)
│   │   ├── control_flow.rs   # IF/FOR/WHILE/DO/SELECT
│   │   ├── procedures.rs     # SUB/FUNCTION/TYPE
│   │   ├── directives.rs     # $IF, $LET, $CHECKING
│   │   ├── graphics.rs       # SCREEN, LINE, CIRCLE, etc.
│   │   ├── audio.rs          # BEEP, SOUND, PLAY, _SND*
│   │   ├── file_io.rs        # OPEN, CLOSE, GET, PUT, SEEK
│   │   ├── system.rs         # SHELL, KILL, NAME, MKDIR
│   │   └── error.rs          # Parse errors
│   ├── semantic/             # ✓ Type checking, symbol resolution
│   │   ├── mod.rs            # Entry point, built-ins
│   │   ├── types.rs          # BasicType enum
│   │   ├── symbols.rs        # Symbol table
│   │   ├── typed_ir.rs       # Typed IR output
│   │   ├── error.rs          # Semantic errors
│   │   └── checker/          # Type checker modules
│   │       ├── mod.rs        # Checker entry point
│   │       ├── expressions.rs
│   │       ├── statements.rs
│   │       ├── control_flow.rs
│   │       ├── assignments.rs
│   │       ├── definitions.rs
│   │       └── const_eval.rs
│   ├── codegen/              # ✓ Backend trait + C implementation
│   │   ├── mod.rs            # CodeGenerator trait
│   │   ├── error.rs          # CodeGenError types
│   │   └── c_backend/        # C code generation
│   │       ├── mod.rs        # Backend entry point
│   │       ├── expr.rs       # Expression codegen
│   │       ├── stmt.rs       # Statement codegen (core)
│   │       ├── file_io.rs    # File I/O helpers
│   │       ├── types.rs      # Type mapping
│   │       ├── runtime.rs    # Inline C runtime
│   │       ├── analysis.rs   # DATA/label collection
│   │       └── const_fold.rs # Constant folding optimization
│   ├── lsp/                  # ✓ Language Server Protocol
│   │   ├── mod.rs            # LSP server implementation
│   │   └── main.rs           # qb64fresh-lsp binary entry
│   ├── preprocessor.rs       # ✓ $INCLUDE directive handling
│   ├── lib.rs                # Library crate root
│   └── main.rs               # qb64fresh binary entry
├── runtime/                  # ✓ Runtime library (workspace member)
│   ├── src/
│   │   ├── lib.rs            # Crate root
│   │   ├── string.rs         # Reference-counted strings
│   │   ├── io.rs             # PRINT, INPUT, console
│   │   ├── math.rs           # Math functions
│   │   ├── graphics_ffi.rs   # Graphics FFI bindings
│   │   ├── audio_ffi.rs      # Audio FFI bindings
│   │   ├── graphics/         # Graphics backend system
│   │   │   ├── mod.rs        # GraphicsBackend trait
│   │   │   ├── sdl2.rs       # SDL2 implementation
│   │   │   ├── mock.rs       # Mock for testing
│   │   │   ├── font.rs       # Font rendering
│   │   │   └── error.rs      # Graphics errors
│   │   └── audio/            # Audio backend system
│   │       ├── mod.rs        # AudioBackend trait
│   │       ├── rodio_backend.rs  # Rodio implementation
│   │       ├── mock.rs       # Mock for testing
│   │       └── error.rs      # Audio errors
│   └── include/
│       └── qb64fresh_rt.h    # C header for FFI
└── examples/                 # Test BASIC files

vscode-qb64fresh/             # VSCode extension (sibling project)
├── src/extension.ts          # LSP client
├── syntaxes/                 # TextMate grammar
└── package.json              # Extension manifest
```

### Dual Binary Architecture
- `qb64fresh` - Compiler CLI (lexer → parser → semantic → codegen)
- `qb64fresh-lsp` - Language server for IDE integration (stdio JSON-RPC)

### Runtime Modes
Code generation supports two modes via `--runtime` flag:
- `inline` (default) - Self-contained C with embedded runtime
- `external` - Links against `libqb64fresh_rt` static library

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

## Critical Codebase Knowledge (For Context Recovery)

**IMPORTANT: At session start or after context recovery, DO THIS FIRST:**
1. Read `src/ast/expr.rs` (ExprKind variants)
2. Read `src/ast/stmt.rs` lines 1-200 (key StatementKind variants)
3. Read `src/semantic/typed_ir.rs` lines 1-250 (TypedExprKind, TypedStatementKind)
4. Skim `src/lexer/token.rs` for TokenKind variants you need

This takes ~30 seconds and prevents 10+ minutes of fumbling with wrong variant names.

### AST Type Definitions

**Expressions (`src/ast/expr.rs`):**
```rust
pub enum ExprKind {
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Identifier(String),
    Binary { left: Box<Expr>, op: BinaryOp, right: Box<Expr> },
    Unary { op: UnaryOp, operand: Box<Expr> },
    Grouped(Box<Expr>),
    FunctionCall { name: String, args: Vec<Expr> },  // Also used for array access!
    FieldAccess { object: Box<Expr>, field: String },
}
```

**Key insight:** There is NO separate `ArrayElement` variant. Array access uses `FunctionCall` because the syntax is identical: `arr(i)` vs `func(i)`.

**Statements (`src/ast/stmt.rs`):** ~100 variants including:
- `Let { name, value }` - simple assignment
- `ArrayAssignment { name, indices, value }` - array element assignment
- `MidAssignment { target: Expr, start, length, value }` - MID$ statement (target is Expr for array support)
- `Dim`, `Redim { preserve, shared, variables }`, `Const`
- Control flow: `If`, `For`, `While`, `DoLoop`, `Select`
- Graphics: `Circle { step, ... }`, `Paint { step, ... }`, `Line`, `Pset`, etc.

### Typed IR (`src/semantic/typed_ir.rs`)

Mirrors AST but with type information:
- `TypedExpr` has `kind: TypedExprKind` and `basic_type: BasicType`
- `TypedStatement` has `kind: TypedStatementKind`
- When adding AST features, also update TypedIR

### Adding a New Language Feature

1. **AST** (`src/ast/stmt.rs` or `expr.rs`): Add variant
2. **Parser** (`src/parser/*.rs`): Parse the syntax, produce AST
3. **TypedIR** (`src/semantic/typed_ir.rs`): Add typed variant
4. **Semantic checker** (`src/semantic/checker/*.rs`): Type-check, produce TypedIR
5. **Codegen** (`src/codegen/c_backend/stmt.rs` or `expr.rs`): Emit C code

### Token Definitions (`src/lexer/token.rs`)

Uses `logos` crate. Tokens defined with `#[token(...)]` or `#[regex(...)]` attributes:
```rust
#[derive(Logos)]
pub enum TokenKind {
    #[token("IF", ignore(ascii_case))]
    If,
    #[regex(r"[A-Za-z_][A-Za-z0-9_.]*[$%&!#]?", priority = 3)]
    Identifier,
    // ... ~200 token types
}
```

**Type suffixes:** `$` (string), `%` (integer), `&` (long), `!` (single), `#` (double) are part of the identifier token.

### Common Patterns

**Parser helper methods (`src/parser/tokens.rs`):**
- `peek()` - look at current token
- `peek_ahead(n)` - look n tokens ahead
- `advance()` - consume and return current token
- `match_token(&TokenKind)` - consume if matches, return bool
- `expect(&TokenKind, msg)` - consume or error

**Semantic checker (`src/semantic/checker/`):**
- `check_expr(&Expr) -> TypedExpr`
- `check_statement(&Statement) -> TypedStatement`
- Methods return typed versions, push errors to `self.errors`

### Testing Commands

```bash
# Test a single file through stages
cargo run --bin qb64fresh -- file.bas --tokens    # Lexer output
cargo run --bin qb64fresh -- file.bas --ast       # Parser output
cargo run --bin qb64fresh -- file.bas --typed-ir  # Semantic output
cargo run --bin qb64fresh -- file.bas --emit-c    # C code output

# Run QB45 compatibility tests
cargo test --test qb45_compat -- --nocapture

# Test from file (avoids shell escaping issues with $)
echo 'MID$(arr$(1), 1, 2) = "x"' > /tmp/test.bas
cargo run --bin qb64fresh -- /tmp/test.bas --ast
```

**Important:** When testing code with `$` characters, use files or heredocs, not echo pipes (shell interprets `$`).

### QB45 Compatibility Status

Current: **114/115 files (99.1%)** (excluding open_gl which uses intentionally unsupported `_GL*` commands)

Remaining issues (as of 2026-01-21):
- Parser: 0 failures ✓ ALL RESOLVED
- Semantic: 1 failure (misc/frog.bas - bug in original code, not compiler limitation)
- Lexer: 0 failures ✓ ALL RESOLVED

Test files: `/home/dave/repos/qb64contain/QB64pe/tests/qbasic_testcases/`

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
