# Session 151: Compiler API facade

**Date:** 2026-01-31  
**Goal:** Define a small “compiler API” surface per STRATEGIC_GUIDANCE_MULTI_PERSPECTIVE_REVIEW.md (item 2).

## Summary

Added a thin facade module `src/compiler_api.rs` that exposes stable entry points for tools (formatter, linter, debugger, LSP) so they can run compiler phases without reaching into internal modules.

## Decisions

- **Single module:** All API entry points live in `compiler_api`; no new crate. Keeps the surface in one place and avoids versioning/workspace churn.
- **Options structs:** `ParseOptions` (preprocess, base_path, source_path) and `CompileOptions` (parse opts + runtime_mode, embedded_files, opengl, no_shell) so callers can configure without touching internal types.
- **Error types:** `ParseApiError` (Preprocessor | Parse) and `CompileApiError` (Parse | Semantic | Codegen) with `From<ParseApiError>` for `CompileApiError` so `?` works in `compile()`.
- **Prelude:** Re-exported compiler_api functions and options in `prelude` so `use qb64fresh::prelude::*` gives access to the API.

## API surface

| Entry point           | Input           | Output           | Use case                    |
|-----------------------|-----------------|------------------|-----------------------------|
| `parse`               | source + opts   | Program          | Parse only (optional $INCLUDE) |
| `parse_tokens`        | tokens          | Program          | Parse from already-lexed tokens |
| `analyze`             | Program         | TypedProgram     | Analyze from AST only       |
| `parse_and_analyze`   | source + opts   | TypedProgram     | Parse + analyze in one call  |
| `compile`             | source + opts   | GeneratedOutput  | Full pipeline               |

## Files touched

- **Added:** `src/compiler_api.rs` – facade with `ParseOptions`, `CompileOptions`, `ParseApiError`, `CompileApiError`, and the five entry points; unit tests for each.
- **Modified:** `src/lib.rs` – `pub mod compiler_api`; prelude re-exports; lib doc updated to point tools at `compiler_api`.

## Follow-up

- Tools (LSP, lint, fmt) can migrate to `compiler_api::parse` / `parse_and_analyze` when convenient; existing use of lexer/parser/semantic remains valid.
- Optional: document “preferred API for tools” in CLAUDE.md or docs/ADDING_A_LANGUAGE_FEATURE.md.
