# Strategic Guidance: Multi-Perspective Codebase Review

**Date:** 2026-01-31  
**Purpose:** Four expert perspectives (Software Architect, Pragmatic Engineer, Rust Engineer, Language Specialist) on QB64Fresh with actionable strategic guidance.

---

## 1. Software Architect

### Strengths

- **Clear pipeline:** `Source → Lexer → Parser → AST → Semantic → Typed IR → CodeGen → C` is explicit in `lib.rs` and respected across modules. Boundaries between phases are well-defined.
- **Trait-based codegen:** `CodeGenerator` trait and `GeneratedOutput` allow future backends (LLVM, Cranelift) without changing callers. Open/Closed principle is applied.
- **Two-pass semantic:** Declaration collection then type checking supports forward references and keeps symbol resolution coherent.
- **LSP as first-class:** Semantic analyzer exposes `find_definition`, `get_hover_info`, `get_document_symbols`; the compiler is designed for IDE use, not just batch compilation.
- **Documented ADRs:** Architecture decisions are recorded in `docs/adrs/`, which helps onboarding and prevents ad-hoc re-architecture.

### Strategic Recommendations


2. **Define a small “compiler API” surface.**  
   Today, the main entry is “parse → analyze → generate.” For tools (formatter, linter, debugger, LSP), a thin facade that exposes “parse only,” “parse + analyze,” “analyze from AST,” etc., would stabilize how other crates/tools depend on the compiler and avoid them reaching into internal modules.

3. **Finish the file-splitting strategy.**  
   Per `FILE_SPLITTING_ANALYSIS.md`, `codegen/c_backend/stmt/mod.rs` (~1,768 lines) should become a thin dispatcher (<50 arms, ~500 lines). The semantic `checker/statements.rs` is already thinned to 11 delegation arms. Completing the stmt codegen thinning will keep “one place per statement category” and avoid a single 2k-line file as the bottleneck for reviews and merges.

4. **Consider a shared diagnostic type.**  
   Parse, semantic, and codegen each have their own error types and collection (e.g., `Vec<SemanticError>`). A unified `CompilerDiagnostic` (span, severity, message, phase) would simplify error formatting, LSP publishing, and any future “compile and show all diagnostics” API without forcing a single error enum.

---

## 2. Pragmatic Engineer

### Strategic Recommendations

4. **Document “how to add a feature” in one place.**  
   CLAUDE.md already has “Adding a New Language Feature” (AST → Parser → TypedIR → Semantic checker → Codegen). Turn that into a single `docs/ADDING_A_LANGUAGE_FEATURE.md` with concrete file names and a minimal example (e.g., one new statement). Reduces context-switching for contributors.

---

## 3. Rust Engineer

### Strategic Recommendations

1. **Unify codegen error with `std::error::Error` and optional `thiserror`.**  
   `CodeGenError` implements `std::error::Error` but is a manual struct/enum. If you want consistent reporting (e.g., same formatting as parse/semantic), consider `thiserror` for `CodeGenErrorKind` and a `#[source]` chain if codegen ever wraps other errors. Low priority if current formatting is sufficient.

2. **Consider `Default` for large codegen state only where it helps.**  
   `CodeGenState` and `ProcedureContext` have `new()`; they’re not `Default`. That’s fine. Only add `Default` if you start building these in multiple places and want `..Default::default()` for clarity.

3. **Keep parser helpers in one place.**  
   `parser/tokens.rs` (peek, advance, match_token, expect) is the right abstraction. Avoid duplicating token-walking logic in subparsers; keep using the same helpers so that behavior (e.g., error recovery) is consistent.

---

## 4. Language Specialist

### Strategic Recommendations

1. **Maintain a single “language status” view.**  
   A short document or table: which statements/functions/operators are implemented, which are parsed but not yet codegen’d, which are explicitly out of scope (e.g., certain `_GL*`). FILE_SPLITTING_ANALYSIS and QB45 compatibility stats are close; a dedicated “Language coverage” or “Feature matrix” would help users and contributors.

---

## Summary: Priority Actions

| Priority | Action | Owner perspective |
|----------|--------|-------------------|
| **Medium** | Add `docs/ADDING_A_LANGUAGE_FEATURE.md` from CLAUDE.md flow | Pragmatic |
| **Medium** | Single “language coverage / feature matrix” doc | Language |
| **Low** | Consider unified `CompilerDiagnostic` for all phases | Architect |

---

*This review reflects the codebase as of 2026-01-31, including the expr split (session 148), semantic checker statement thinning, and FILE_SPLITTING_ANALYSIS. Revisit when stmt thinning is complete or when adding a new backend or major language area.*
