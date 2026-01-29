# Session 083: ADR Codebase Review (More ADRs)

**Date:** 2026-01-28  
**Session:** 083  
**Focus:** Review entire QB64Fresh codebase for ADRs to add or update

## Summary

Full review of src/, docs/, and existing ADRs led to:

### Updates

- **ADR-0011 (Error handling):** Marked ariadne CLI output and Suggestions ("did you mean") as complete. Added `src/error_formatting.rs` and `src/semantic/suggestions.rs` to Files; removed completed items from Future Work.
- **ADR-0003 (Runtime):** Added "Runtime delivery modes" section: inline (embedded C stubs, CI/headless) vs external (libqb64fresh_rt, full graphics/audio). Referenced ARCHITECTURE.md and PARTIAL_IMPLEMENTATIONS.md.

### New ADRs

- **ADR-0017: Generated code is ephemeral** — Fix code generator, never patch generated C. Formalizes .cursor rule qb64fresh-codegen.mdc for all contributors and tooling.
- **ADR-0018: Compiler execution resource limits** — Recommend/require memory limits (ulimit) when running QB64Fresh or QB64pe. References MEMORY_LIMITS.md and CLAUDE.md.

### Index / docs

- docs/adrs/README.md — Added ADR-0017, ADR-0018 to index.
- docs/DOCS-README.md — 16 → 18 ADRs; added ADR-0016, 0017, 0018 to key decisions list.
- docs/ARCHITECTURE.md — Added ADR-0017, ADR-0018 to ADR table.

## Not added (rationale)

- **Semantic analysis / Typed IR architecture:** Described in ARCHITECTURE.md; no separate ADR. Considered implementation detail rather than a contested decision.
- **Codegen stmt/ modularization:** Covered by ADR-0002 and ARCHITECTURE; no separate "codegen modularization" ADR.
- **Header parser:** Already covered in ADR-0008.

## Files Touched

- docs/adrs/ADR-0011-error-handling.md
- docs/adrs/ADR-0003-runtime-library-approach.md
- docs/adrs/ADR-0017-generated-code-is-ephemeral.md (new)
- docs/adrs/ADR-0018-compiler-execution-resource-limits.md (new)
- docs/adrs/README.md
- docs/DOCS-README.md
- docs/ARCHITECTURE.md
