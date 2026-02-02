# Session 149: Strategic guidance — multi-perspective review

**Date:** 2026-01-31

## Goal

Have four expert perspectives (Software Architect, Pragmatic Engineer, Rust Engineer, Language Specialist) review the QB64Fresh codebase and provide strategic guidance.

## Outcome

Created **`docs/STRATEGIC_GUIDANCE_MULTI_PERSPECTIVE_REVIEW.md`** with:

1. **Software Architect** — Pipeline clarity, trait-based codegen, two-pass semantic, LSP integration; recommendations: formalize typed IR contract, thin compiler API surface, finish stmt codegen thinning, consider unified diagnostic type.
2. **Pragmatic Engineer** — Fix root causes (no patching generated C), incremental thinning, guards (memory/source limits); recommendations: finish stmt thinning next, avoid premature splitting of “medium” files, keep tests close, document “how to add a feature” in one place.
3. **Rust Engineer** — AST ownership, error collection, thiserror, module layout; recommendations: optional thiserror for codegen errors, keep parser helpers in one place, clippy/fmt as gate.
4. **Language Specialist** — AST reflects BASIC (array vs function call), type system and builtins; recommendations: single “language status” view, document array vs function resolution, clarify BYREF/BYVAL usage, keep QB64pe as reference only.

## Priority actions (from doc)

- **High:** Thin `codegen/c_backend/stmt/mod.rs` to <50 arms, ~500 lines; document typed IR contract.
- **Medium:** Add `docs/ADDING_A_LANGUAGE_FEATURE.md`; add language coverage / feature matrix.
- **Low:** Consider unified `CompilerDiagnostic`; document array vs function call resolution.

## References

- `docs/ThingsToDo/FILE_SPLITTING_ANALYSIS.md` — file-splitting plan (stmt thinning in progress).
- `AgenticLogs/2026-01-31_session-148_expr-split-file-splitting.md` — expr split completed.
