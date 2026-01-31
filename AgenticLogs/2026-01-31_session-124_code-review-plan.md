# Session 124: Full Codebase Review Plan

**Date:** 2026-01-31  
**Brief:** Create a plan to iterate through every code file in QB64Fresh and evaluate for bugs, errors, bad practices, lost functionality, etc.

## Accomplished

- **Created [CODE_REVIEW_PLAN.md](../docs/ThingsToDo/CODE_REVIEW_PLAN.md)** in `docs/ThingsToDo/`:
  - Evaluation dimensions: bugs/correctness, errors/robustness, bad practices, lost functionality, documentation, safety, tests.
  - Phased approach: Phase 0 (automated baseline), Phase 1 (compiler `src/`), Phase 2 (runtime), Phase 3 (tools), Phase 4 (tests/benches/fuzz).
  - File inventory: ~194 `.rs` files; ordered by pipeline/dependency for compiler and runtime.
  - Per-file checklist template and review log/output format.
  - Success criteria and iteration/maintenance notes.
- **Created [CODE_REVIEW_LOG.md](../docs/ThingsToDo/CODE_REVIEW_LOG.md)** as a tracking table for progress and Phase 0 results.

## Decisions

- Scope: all Rust (`.rs`) in QB64Fresh only; excludes generated C, BASIC fixtures, third-party.
- Plan ties to existing docs: ARCHITECTURAL_REVIEW, PARTIAL_IMPLEMENTATIONS, BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS, CLAUDE.md.
- Review log lives in `docs/ThingsToDo/` next to the plan; complex issues get `AgenticLogs/IndividualProblems/` entries and links from the log.

## References

- [CODE_REVIEW_PLAN.md](../docs/ThingsToDo/CODE_REVIEW_PLAN.md) – full process
- [CODE_REVIEW_LOG.md](../docs/ThingsToDo/CODE_REVIEW_LOG.md) – progress and findings
- [ARCHITECTURAL_REVIEW.md](../docs/ThingsToDo/ARCHITECTURAL_REVIEW.md) – pending recommendations (§3 clone/docs/idiom)
- [PARTIAL_IMPLEMENTATIONS.md](../docs/ThingsToDo/PARTIAL_IMPLEMENTATIONS.md) – stub/parity context
