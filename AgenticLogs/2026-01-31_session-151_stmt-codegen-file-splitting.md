# Session 151: Stmt codegen file-splitting (thin dispatcher)

**Date:** 2026-01-31

## Goal

Per `FILE_SPLITTING_ANALYSIS.md` and `STRATEGIC_GUIDANCE_MULTI_PERSPECTIVE_REVIEW.md`: thin `codegen/c_backend/stmt/mod.rs` to a dispatcher (<50 arms, ~500 lines) by moving emitter state and helpers into a dedicated submodule.

## Plan

1. Add `stmt/state.rs` holding:
   - Context structs: `LoopContext`, `CodeGenState`, `ProcedureContext`, `GlobalSymbols`, `DataContext`, `EventContext`, `DebugContext`, `Config`
   - `StmtEmitter` struct and impl (constructor, `emit_expr`, `proc_label`, `indent_str`, `is_executable_statement`, `debug_file_expr`, `emit_debug_line`)
2. Keep in `mod.rs`: module wiring, re-exports, `EvntIndentGuard`, and the `emit_stmt` match dispatcher only.
3. Re-export `StmtEmitter` and `LoopContext` from `state` so `control_flow.rs` and parent continue to work.

## Outcome

- **`stmt/mod.rs`**: thin dispatcher **381 lines** (target <500). Contains only: module doc, submodule declarations, re-exports, `EvntIndentGuard`, and `impl StmtEmitter { emit_stmt(...) }` (match + evnt wrapper).
- **`stmt/state.rs`** (new): **403 lines**. Holds all context structs (`LoopContext`, `CodeGenState`, `ProcedureContext`, `GlobalSymbols`, `DataContext`, `EventContext`, `DebugContext`, `Config`), `StmtEmitter` struct, and helper impl (`new`, `with_runtime_mode`, `next_label`, `emit_expr`, `proc_label`, `indent_str`, `is_executable_statement`, `debug_file_expr`, `emit_debug_line`). Context types and `StmtEmitter` are `pub(in crate::codegen::c_backend)`; re-exported from `stmt/mod.rs` so `c_backend` and `file_io` can use them.
- One place per statement category unchanged; submodules unchanged; `control_flow` still uses `super::LoopContext` (resolved via re-export).
- **Tests:** 435 passed.
