# Rust File Size Analysis: Multi-Perspective Recommendations

**Project:** QB64Fresh Compiler
**Date:** 2026-01-23
**Total Codebase:** ~27,000 lines across 48 Rust files

## Executive Summary

After analyzing the codebase from multiple perspectives, the consensus is: **targeted refactoring of 2-3 files would provide meaningful benefits, but wholesale restructuring is not recommended.**

---

## Current State

### Files Over 2,000 Lines (Primary Concerns)

| File | Lines | Purpose |
|------|-------|---------|
| `codegen/c_backend/runtime.rs` | 5,660 | Emits inline C runtime library |
| `codegen/c_backend/stmt.rs` | 4,215 | Generates C code for statements |
| `parser/statements.rs` | 3,945 | Parses QB64 statements |
| `semantic/mod.rs` | 3,228 | Semantic analysis orchestrator |
| `semantic/checker/statements.rs` | 3,096 | Type-checks statements |
| `parser/mod.rs` | 2,451 | Parser entry point |
| `ast/stmt.rs` | 2,237 | Statement AST definitions |
| `lsp/mod.rs` | 2,206 | Language Server Protocol |
| `semantic/typed_ir.rs` | 2,033 | Typed intermediate representation |

---

## Perspective Analysis

### 1. The Pragmatic Developer

> "Does it work? Is it causing actual problems? What's the cost-benefit of changing it?"

**Assessment:**

The large files aren't blocking development. The codebase compiles, tests pass, and new features are being added successfully. The main pain points are:

- **Navigation:** Finding specific functionality in 5,000-line files requires grep/search
- **Cognitive load:** Understanding the full context of runtime.rs or stmt.rs requires significant mental effort
- **Merge conflicts:** Multiple developers touching the same large file creates friction (though this is a solo project currently)

**Recommendation:** Split only if you're about to do significant work in that area anyway. "Refactoring for its own sake" has negative ROI.

**Priority files:**
1. `runtime.rs` - Only if you're adding new runtime features
2. `stmt.rs` - Only if you're adding many new statements

---

### 2. The Software Architect

> "What's the long-term maintainability? How does the structure communicate intent?"

**Assessment:**

The architecture is fundamentally sound:
- Clean pipeline: Lexer → Parser → Semantic → Codegen
- Good use of traits (`CodeGenerator`) for abstraction
- Appropriate module boundaries at the macro level

The large files are mostly **"unavoidable catalogs"**:
- `ast/stmt.rs` - Lists all 100+ statement types (must be centralized)
- `runtime.rs` - C code templates for each runtime function
- `stmt.rs` - Mapping from typed IR to C code for each statement

These follow the "expression problem" pattern - QB64 has ~200 language constructs, and somewhere you must handle all 200.

**However,** the files could be organized as **directory modules** without losing cohesion:

```
codegen/c_backend/
├── runtime/
│   ├── mod.rs          # Public interface, emit_runtime()
│   ├── strings.rs      # String manipulation functions
│   ├── io.rs           # PRINT/INPUT runtime support
│   ├── math.rs         # Mathematical functions
│   ├── file.rs         # File I/O support
│   └── graphics.rs     # Graphics stubs
```

**Key insight:** The issue isn't coupling - it's that Rust's module system makes "one file = one concern" the path of least resistance, even when one concern is 5,000 lines.

**Recommendation:** Refactor `runtime.rs` as a proof-of-concept. If the pattern works well, apply to `stmt.rs`. Leave the rest alone.

---

### 3. The Rust Expert

> "Does the structure follow Rust idioms? What does the Rust community do?"

**Assessment:**

Large files are common in Rust codebases:
- `rustc` has files over 10,000 lines
- `serde` has similarly large enum definitions
- The Rust compiler itself uses "mod.rs + submodules" extensively

**Rust-specific considerations:**

1. **Orphan rules:** Trait implementations must be in the same crate as either the trait or the type. Splitting files doesn't help here.

2. **Visibility:** Rust's `pub(crate)` and `pub(super)` make internal organization flexible. You can split into submodules while keeping the public API unchanged.

3. **Compile times:** Splitting files can actually *help* incremental compilation. Rust recompiles changed files and their dependents. Smaller files = more granular recompilation.

4. **Pattern matching exhaustiveness:** The large `match` statements in stmt.rs are idiomatic Rust. Splitting the *handling* into separate files is fine; the dispatch must remain centralized for exhaustiveness checking.

**Recommended Rust pattern for splitting:**

```rust
// In codegen/c_backend/stmt/mod.rs
mod control_flow;
mod io;
mod graphics;

impl StmtEmitter {
    pub fn emit_stmt(&mut self, stmt: &TypedStatement) {
        match &stmt.kind {
            // Control flow - delegate to submodule
            TypedStatementKind::If { .. } => self.emit_if(stmt),
            TypedStatementKind::For { .. } => self.emit_for(stmt),
            // ... (dispatch stays centralized)
        }
    }
}

// In codegen/c_backend/stmt/control_flow.rs
impl StmtEmitter {
    pub(super) fn emit_if(&mut self, stmt: &TypedStatement) {
        // Implementation here
    }
}
```

**Recommendation:** Use `pub(super)` visibility for implementation details. Keep the match dispatch in mod.rs. This is idiomatic and maintains exhaustiveness checking.

---

### 4. The Claude/LLM Expert

> "How does file size affect AI-assisted development?"

**Assessment:**

This perspective is crucial for this project. Large files create real friction with LLM tools:

| Issue | Impact | Severity |
|-------|--------|----------|
| Context window limits | Can't see entire file at once | Medium |
| Attention dilution | Key details lost in large contexts | High |
| Token costs | Reading large files uses more tokens | Medium |
| Edit precision | More potential for incorrect edits | High |

**Specific problems observed:**

1. **runtime.rs at 5,660 lines:** When asking Claude to modify a function, it must either:
   - Read the entire file (expensive, dilutes attention)
   - Read a portion and risk missing related code

2. **Cross-references:** Functions in runtime.rs call each other. Without seeing the full file, Claude may miss dependencies.

3. **Pattern consistency:** When adding new runtime functions, Claude needs to see existing patterns. In a 5,000-line file, it sees fewer complete examples.

**What helps Claude work effectively:**

- **Files under 500 lines:** Full context fits in a single read
- **Files 500-1,500 lines:** Workable with targeted reads
- **Files over 2,000 lines:** Requires multiple reads, increased error risk

**Recommendation:** Splitting files improves AI-assisted development significantly. The 500-1,500 line range is the sweet spot.

---

### 5. The "What Changed?" Analyst

> "Has the situation changed since we last evaluated this?"

**Changes since project inception:**

1. **Codebase grew:** The compiler now has ~27,000 lines vs. earlier stages
2. **More runtime functions:** runtime.rs continues to grow as QB64 coverage increases
3. **Claude Code became primary dev tool:** AI assistance is now central to development workflow
4. **LSP was added:** 2,200 lines of protocol handling that didn't exist before

**What's different now:**

- The AI-assisted development argument is stronger than before
- The files continue to grow, not shrink
- More features = more statements = larger switch statements

---

## Unified Recommendations

Based on all perspectives, here is the prioritized action plan:

### Tier 1: High Value, Clear Benefit

#### 1. Split `runtime.rs` (5,660 lines → ~8 files of 500-900 lines)

**Why:** Highest ROI. The file is already organized by subsystem. Each `emit_*` function is independent.

**Structure:**
```
codegen/c_backend/runtime/
├── mod.rs          (~200 lines) - Public API, emit_runtime()
├── types.rs        (~400 lines) - Type definitions, QB64String
├── strings.rs      (~800 lines) - String manipulation functions
├── io.rs           (~600 lines) - PRINT/INPUT support
├── math.rs         (~500 lines) - Math and type conversion
├── file.rs         (~500 lines) - File I/O functions
├── arrays.rs       (~400 lines) - Array functions
├── timing.rs       (~300 lines) - Timer, SLEEP, DATE$, TIME$
├── memory.rs       (~300 lines) - PEEK, POKE, VARPTR
├── legacy.rs       (~400 lines) - DEF SEG, legacy support
└── control.rs      (~300 lines) - GOSUB stack, error handling
```

**Effort:** Medium (mostly moving code, few interface changes)

**Risk:** Low (no logic changes, just reorganization)

---

### Tier 2: Medium Value, More Effort

#### 2. Split `stmt.rs` (4,215 lines → ~6 files)

**Why:** Second largest file. Natural domain boundaries exist.

**Structure:**
```
codegen/c_backend/stmt/
├── mod.rs              - StmtEmitter struct, emit_stmt() dispatcher
├── control_flow.rs     - IF, FOR, WHILE, DO, SELECT, GOTO, GOSUB
├── io.rs               - PRINT, INPUT
├── declarations.rs     - DIM, LET, CONST, SWAP
├── procedures.rs       - SUB/FUNCTION definitions
├── graphics.rs         - CIRCLE, LINE, PSET, PAINT, etc.
└── file_io.rs          - OPEN, CLOSE, GET, PUT, file statements
```

**Effort:** Medium-high (shared state in StmtEmitter needs careful handling)

**Risk:** Medium (tighter coupling than runtime.rs)

---

### Tier 3: Lower Priority

#### 3. Split `lsp/mod.rs` (2,206 lines → ~5 files)

**Why:** Cleanly separable by LSP feature

**When:** Next time significant LSP work is needed

#### 4. Consider splitting `parser/statements.rs` (3,945 lines)

**Why:** Large but already follows patterns from existing submodules (graphics.rs, audio.rs, file_io.rs)

**When:** If parser changes become frequent

---

### Tier 4: Leave Alone

These files are large but should NOT be split:

| File | Lines | Reason |
|------|-------|--------|
| `ast/stmt.rs` | 2,237 | Enum definition - must be centralized |
| `semantic/mod.rs` | 3,228 | Already uses submodules effectively |
| `semantic/typed_ir.rs` | 2,033 | IR definition - must be centralized |
| `lexer/token.rs` | 1,591 | Token catalog - must be centralized |

These are **definition files** where the size reflects language complexity, not poor organization.

---

## Implementation Strategy

### Approach A: Opportunistic (Recommended)

Split files when you're about to do significant work in them:

1. Before adding new runtime functions → split `runtime.rs`
2. Before adding new statement types → split `stmt.rs`
3. Before major LSP features → split `lsp/mod.rs`

**Pros:** Zero wasted effort, changes happen when context is fresh
**Cons:** Files stay large until that work happens

### Approach B: Dedicated Refactoring Sprint

Set aside time to split `runtime.rs` and `stmt.rs` together.

**Pros:** Get the benefits immediately, consistent structure
**Cons:** No immediate feature value, risk of introducing bugs

### Approach C: Hybrid

1. **Now:** Split `runtime.rs` (clear win, low risk)
2. **Later:** Split `stmt.rs` opportunistically
3. **Much later:** Consider LSP split if needed

---

## Conclusion

The consensus across all perspectives:

1. **runtime.rs should be split.** It's the largest file, has clear internal boundaries, and the AI-assistance argument is compelling.

2. **stmt.rs is a good candidate** but has tighter coupling. Split when convenient.

3. **Most other large files should stay as-is.** They're definition catalogs where size reflects language complexity.

4. **The architecture is sound.** This is reorganization, not redesign.

The key insight from the "Claude expert" perspective tips the balance: in an AI-assisted development workflow, file sizes in the 500-1,500 line range significantly improve development velocity. This wasn't true when we first evaluated, but it's true now.

---

## Appendix: Quick Reference

### Files to Split (in order)
1. `runtime.rs` (5,660 → 8-10 files)
2. `stmt.rs` (4,215 → 5-6 files)
3. `lsp/mod.rs` (2,206 → 4-5 files)

### Files to Leave Alone
- `ast/stmt.rs` - Enum definition
- `semantic/typed_ir.rs` - IR definition
- `semantic/mod.rs` - Already uses submodules
- `lexer/token.rs` - Token catalog
- `parser/mod.rs` - Entry point, acceptable size

### Ideal File Size Targets
- **Sweet spot:** 500-1,500 lines
- **Acceptable:** up to 2,000 lines
- **Problematic:** over 3,000 lines
