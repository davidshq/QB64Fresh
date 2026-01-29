# Session 082: ADR Updates and New ADR-0016

**Date:** 2026-01-28  
**Session:** 082  
**Focus:** Add/update ADRs to reflect recent decisions and docs

## Summary

- **ADR-0009 (LSP):** Updated to document incremental analysis (incremental lex/parse/semantic from first affected statement) implemented in session 072. Adjusted capabilities table, Document Synchronization section, rationale, Future Work (marked incremental analysis done), and consequences.
- **ADR-0008 (C Interop):** Added link to docs/BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md for known limitations and problematic BASIC→C items.
- **ADR-0014 (Scope):** Added reference to docs/INTENTIONAL_DIFFERENCES_FROM_QB64PE.md in References.
- **ADR-0016 (new):** Created "Intentional Behavioral Differences from QB64pe" — policy for documenting and enforcing deliberate deviations (stricter GOTO, multiple errors, RND, mangling, etc.); canonical list remains docs/INTENTIONAL_DIFFERENCES_FROM_QB64PE.md.
- **Index/docs:** Updated docs/adrs/README.md, DOCS-README.md, ARCHITECTURE.md for 16 ADRs; added ADR-0016 to INTENTIONAL_DIFFERENCES "See also."

## Files Touched

- docs/adrs/ADR-0009-lsp-architecture.md
- docs/adrs/ADR-0008-c-interoperability.md
- docs/adrs/ADR-0014-scope-and-excluded-features.md
- docs/adrs/ADR-0016-intentional-behavioral-differences.md (new)
- docs/adrs/README.md
- docs/DOCS-README.md
- docs/ARCHITECTURE.md
- docs/INTENTIONAL_DIFFERENCES_FROM_QB64PE.md
