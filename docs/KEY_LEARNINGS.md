# Key Learnings

This document tracks practical, actionable insights discovered during QB64Fresh development. These are learnings that are genuinely useful for future work, not just historical notes.

**Purpose:** Quick reference for common patterns, gotchas, and best practices that save time and prevent mistakes.

---

## Table of Contents

- [Rust Patterns](#rust-patterns)
- [Tooling](#tooling)
- [Architecture & Design](#architecture--design)
- [Debugging & Troubleshooting](#debugging--troubleshooting)
- [Compiler-Specific Insights](#compiler-specific-insights)
- [Performance](#performance)
- [Testing](#testing)

---

## Rust Patterns

### Borrow Checker with Error Handling

**Problem:** Need to use borrowed data in error messages while also mutating `self.errors`.

**Anti-pattern:**
```rust
// DON'T - closure captures self.errors while self is borrowed
let token = self.peek().ok_or_else(|| {
    self.errors.push(ParseError::eof("expression"));
})?;
```

**Solution:**
```rust
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

**When to use:** Any time you need borrowed data for error messages in methods that mutate `self`.

---

### Range<T> is Not Copy

**Problem:** Even when `T` is `Copy`, `Range<T>` must be cloned.

**Anti-pattern:**
```rust
// DON'T
let span: Span = token.span.into();  // Error: can't move from borrow
```

**Solution:**
```rust
// DO
let span: Span = token.span.clone().into();
```

**When to use:** Converting `Range<usize>` to `Span` or other range types.

---

### Closure Borrowing Conflicts

**Problem:** Closures in `.ok_or_else()`, `.map_err()`, etc. capture their environment, which can conflict with existing borrows.

**Solution:** Use explicit `match` instead of closure-based error handling when borrow conflicts occur.

**When to use:** Error handling in methods that need to access multiple `&mut self` fields.

---

### API Assumptions Lead to Compilation Errors

**Problem:** Assuming API structure without reading actual type definitions.

**Example:** Parser was written assuming `Token<'a>` (lifetime parameter) but `Token` actually owns its data, causing 34+ compilation errors.

**Solution:** Always read the actual type definitions before writing dependent code. Check:
- Does it have lifetime parameters?
- Does it own or borrow data?
- What are the actual field types?

**When to use:** Before writing code that depends on types you haven't used recently.

---

## Tooling

### Cargo Tools (Verified 2026-01)

**Deprecated/Changed:**
- `cargo-watch` is **deprecated** → use `bacon` instead
- `cargo-tree` is built into cargo - no install needed, just use `cargo tree`

**Recommended:**
- `cargo-nextest` is genuinely faster (up to 3x) - worth using
- `cargo-audit` is essential for security scanning

**Best Practice:** Always verify current status before recommending tools - ecosystems change. Check for deprecation notices in READMEs and search for "[tool] vs [alternative] [current year]" to find recent comparisons.

---

### VS Code Extensions

**Deprecated:**
- `serayuzgur.crates` is deprecated → use `fill-labs.dependi` instead
- `rust-lang.rust` (old extension) is deprecated → `rust-analyzer` is the only Rust extension needed

**Nice-to-have:**
- `errorlens` is nice-to-have but not essential - rust-analyzer's diagnostics are sufficient

---

## Architecture & Design

### Array Access Uses FunctionCall AST Node

**Key Insight:** There is NO separate `ArrayElement` variant in the AST. Array access uses `FunctionCall` because the syntax is identical: `arr(i)` vs `func(i)`.

**Implication:** Semantic analysis must distinguish between function calls and array access by checking symbol table (does identifier refer to a function or an array?).

**Location:** `src/ast/expr.rs` - `ExprKind::FunctionCall`

---

### Error Handling: Accumulate, Don't Fail Fast

**Pattern:** Parsers and semantic checkers accumulate errors in a `Vec<Error>` rather than failing on first error.

**Rationale:** 
- Better user experience - show all errors at once
- Enables better error recovery
- Allows partial compilation results for IDE features

**Implementation:** Methods return `Result<T, ()>` where `()` indicates "error was added to `self.errors`, continue processing".

---

### Trait-Based Backend Abstraction

**Pattern:** Use traits for code generation backends (e.g., `CodeGenerator` trait) even if only one implementation exists initially.

**Rationale:**
- Clean separation of concerns
- Enables future backends (LLVM, Cranelift) without changing existing code
- Makes testing easier (can mock backends)
- Follows YAGNI principle - design the interface cleanly, but only build one backend

**Example:** `src/codegen/mod.rs` - `CodeGenerator` trait with `CBackend` implementation.

---

## Debugging & Troubleshooting

### Context Recovery Checklist

**When starting a session or recovering context, read these files first (~30 seconds):**
1. `src/ast/expr.rs` - ExprKind variants
2. `src/ast/stmt.rs` lines 1-200 - key StatementKind variants
3. `src/semantic/typed_ir.rs` lines 1-250 - TypedExprKind, TypedStatementKind
4. `src/lexer/token.rs` - TokenKind variants you need

**Why:** Prevents 10+ minutes of fumbling with wrong variant names.

---

### Memory Limits for Compiler Execution

**Critical:** Both QB64Fresh and QB64pe can consume 25GB+ memory and crash the system.

**Solution:** Always use memory limits:
```bash
bash -c 'ulimit -v 16777216 && ./qb64fresh input.bas --emit-c -o output.c'
bash -c 'ulimit -v 16777216 && ./qb64pe_fresh -x input.bas -o output'
```

**See:** `docs/MEMORY_LIMITS.md` for details.

---

### Testing Code with `$` Characters

**Problem:** Shell interprets `$` in command-line arguments.

**Solution:** Use files or heredocs instead of echo pipes:
```bash
# DON'T
echo 'MID$(arr$(1), 1, 2) = "x"' | cargo run --bin qb64fresh -- --ast

# DO
echo 'MID$(arr$(1), 1, 2) = "x"' > /tmp/test.bas
cargo run --bin qb64fresh -- /tmp/test.bas --ast
```

---

## Compiler-Specific Insights

### Type Suffixes in Identifiers

**Pattern:** Type suffixes (`$`, `%`, `&`, `!`, `#`) are part of the identifier token, not separate tokens.

**Implication:** Lexer produces `Identifier("count%")`, semantic analysis must parse the suffix to determine type.

**Location:** `src/lexer/token.rs` - identifier regex includes suffix pattern.

---

### MID$ Assignment Target is Expression

**Pattern:** `MidAssignment` statement has `target: Expr` (not just identifier) to support array elements: `MID$(arr$(i), 1, 2) = "x"`.

**Implication:** Semantic checker must handle both simple variables and array elements as MID$ targets.

**Location:** `src/ast/stmt.rs` - `StatementKind::MidAssignment`

---

### Preprocessor vs Parser Directives

**Distinction:**
- **Preprocessor directives** (`$INCLUDE`) - handled before lexing, can affect token stream
- **Parser directives** (`$IF`, `$LET`, `$CHECKING`) - parsed as statements, affect compilation but not tokenization

**Location:** 
- Preprocessor: `src/preprocessor.rs`
- Parser directives: `src/parser/directives.rs`

---

## Performance

### Constant Folding Optimization

**Pattern:** Perform constant folding during code generation, not semantic analysis.

**Rationale:**
- Keeps semantic analysis focused on type checking
- Allows codegen to optimize based on target architecture
- Easier to disable for debugging

**Location:** `src/codegen/c_backend/const_fold.rs`

---

## Testing

### QB45 Compatibility Test Suite

**Location:** `QB64pe/tests/qbasic_testcases/`

**Structure:**
- `qb45com/` - **High priority** - QB4.5 compatibility (core target)
- `misc/` - Medium priority - mixed, many use QB64-specific extensions
- `n54/`, `pete/`, `thebob/` - Medium priority - contributor collections
- `open_gl/` - **Skip** - uses `_GL` commands (we use SDL2/winit, not raw OpenGL)

**Current Status:** 114/115 files (99.1%) passing (excluding open_gl)

---

### Golden Tests for Regression Detection

**Pattern:** Use golden tests (expected output files) for compiler output validation.

**Benefits:**
- Catches regressions automatically
- Documents expected behavior
- Easy to update when behavior intentionally changes

**Location:** `tests/` directory with `.golden` and `.output` files.

---

## Contributing to This Document

**When to add a learning:**
- It's genuinely useful for future work (saves time, prevents mistakes)
- It's not obvious from reading the code
- It's a pattern that might be reused
- It's a gotcha that caused significant debugging time

**When NOT to add:**
- One-off solutions to specific bugs (those belong in session logs)
- Obvious things that are clear from code/documentation
- Temporary workarounds that will be fixed

**Format:**
- Use clear headings and categories
- Include code examples for patterns
- Explain the "why" not just the "what"
- Add "When to use" guidance where applicable

---

## Related Documents

- `CLAUDE.md` - Project configuration (includes some learnings, but this doc is more comprehensive)
- `AgenticLogs/` - Session logs with detailed problem-solving
- `AgenticLogs/IndividualProblems/` - Deep dives into complex issues
- `docs/adrs/` - Architecture Decision Records
