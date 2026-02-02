# Session 148: expr.rs file splitting (FILE_SPLITTING_ANALYSIS)

**Date:** 2026-01-31

## Goal

Split `src/codegen/c_backend/expr.rs` (~3,231 lines) into category-based submodules per `docs/ThingsToDo/FILE_SPLITTING_ANALYSIS.md`: thin dispatcher in `expr/mod.rs` plus `literals`, `binary`, `unary`, `calls`, `special`, and `helpers`.

## Plan

- **expr/mod.rs** — `emit_expr`, `emit_expr_external`, `emit_expr_internal`; match on `TypedExprKind` and delegate to submodules; re-export public API.
- **expr/helpers.rs** — `escape_string`, `unwrap_qb_str_from_c`, `needs_fixed_string_conversion`, `c_function_name`, `opengl_c_name`, `emit_string_data_access`.
- **expr/literals.rs** — Integer, float, string literals and variable reference.
- **expr/binary.rs** — Binary ops (arithmetic, comparison, string concat); `emit_binary_expr`, `c_binary_op`, `emit_string_comparison`.
- **expr/unary.rs** — Unary ops and grouped.
- **expr/calls.rs** — FunctionCall, ArrayAccess, FieldAccess, ArrayRef, ExternalFunctionCall, ProcPtr.
- **expr/special.rs** — Convert, CvFunc, MkDollarFunc, CastFunc, ValWithType, MemGetTyped.

## Status

Done. `src/codegen/c_backend/expr.rs` was replaced by `src/codegen/c_backend/expr/`:

- **expr/mod.rs** — Thin dispatcher: `ExprEmitCtx`, `emit_expr`, `emit_expr_external`, match on `TypedExprKind` delegating to submodules; re-exports `c_function_name`, `escape_string`, `unwrap_qb_str_from_c`, `emit_string_data_access`.
- **expr/helpers.rs** — `escape_string`, `unwrap_qb_str_from_c`, `needs_fixed_string_conversion`, `c_function_name`, `opengl_c_name`, `emit_string_data_access` (C name mapping and string helpers).
- **expr/literals.rs** — Integer, float, string literals and variable reference (`emit_integer_literal`, `emit_float_literal`, `emit_string_literal`, `emit_variable`).
- **expr/binary.rs** — Binary ops: `emit_binary_expr`, `c_binary_op`, `emit_string_comparison`.
- **expr/unary.rs** — Unary and grouped: `format_unary`, `format_grouped`.
- **expr/calls.rs** — Function calls (full built-in and user-defined handling), `emit_array_access`, `emit_external_function_call`, `emit_field_access`, `emit_array_ref`, `emit_proc_ptr`.
- **expr/special.rs** — Convert, CvFunc, MkDollarFunc, CastFunc, ValWithType, MemGetTyped.

Visibility: helpers’ `c_function_name`, `escape_string`, `emit_string_data_access` set to `pub(crate)` so stmt/analysis can use them via `expr::` re-exports.

**Tests:** `cargo test --lib` (419 passed) and `cargo test --test qb45_compat` (5 passed) run successfully.

---

## Post-split review (errors, bugs, bad practices, lost functionality)

**Fixed:**

1. **Lost functionality — expr unit tests**  
   The original `expr.rs` had a `#[cfg(test)] mod tests` with 10 tests (integer/string literals, power/EQV/IMP constant-folded and with variable, escape_string, c_function_name). Those were dropped when the file was removed. **Restored** in `expr/mod.rs` so all 11 tests (including new `test_unwrap_qb_str_from_c`) run.

2. **Bug — `unwrap_qb_str_from_c` malformed input**  
   If the string was `qb_str_from_c(x` (no closing `)`), `end_pos` stayed 0 and the function returned `""`. **Fixed** by initializing `end_pos = stripped.len()` so when no matching `)` is found we return the whole inner slice instead of an empty string. Added `test_unwrap_qb_str_from_c` to cover normal, nested, and malformed cases.

**Checked — no issues:**

- **Public API:** `emit_expr`, `emit_expr_external`, `c_function_name`, `escape_string`, `unwrap_qb_str_from_c`, `emit_string_data_access` are used by stmt/mod.rs, stmt/io.rs, stmt/definitions.rs, stmt/audio.rs, stmt/graphics.rs, stmt/misc.rs, stmt/system.rs, analysis.rs; all go through the expr re-exports and compile.
- **wrap_string_temps:** User-defined BYREF args and built-in args in `emit_function_call` use `ctx.emit(arg, wrap_string_temps)` where appropriate (lines 786 and 935 in calls.rs); no wrong `false` vs `wrap_string_temps`.
- **No stray emit_expr/emit_expr_internal:** All recursive emission in expr goes through `ctx.emit(...)`; no leftover direct calls.
- **Visibility:** Helpers used from outside expr are `pub(crate)`; expr re-exports are consistent.
- **RuntimeMode path:** `helpers::emit_string_data_access` uses `super::super::RuntimeMode` (expr → c_backend); correct.

---

## Stmt/mod.rs and semantic checker thinning (FILE_SPLITTING_ANALYSIS §2–3)

Same session: thinned the codegen statement dispatcher and the semantic checker statement dispatcher per `docs/ThingsToDo/FILE_SPLITTING_ANALYSIS.md` (items 2 and 3).

### 2. Codegen `stmt/mod.rs` thinning

**First pass (earlier in session):**
- **Assignments:** Added `assignments::emit_assignments_stmt`; moved `emit_mid_assignment` and `emit_asc_assignment` into `assignments.rs`. Replaced 6 assignment arms in `mod.rs` with one delegation.
- **Console I/O:** Added `io::emit_console_io_stmt` and helpers in `io.rs`. Replaced 4 arms with one.
- **Control flow:** Added `control_flow::emit_control_flow_stmt`. Replaced 19 arms with one.
- **File I/O:** Added `super::file_io::emit_file_io_stmt`. Replaced 14 arms with one.
- **Error/jump:** Added `error_jump::emit_error_jump_stmt`. Replaced 6 arms with one.
- **DEF FN:** Added `def_fn::emit_def_fn_stmt`. Replaced 2 arms with one.
- **Meta:** First meta block delegates to `meta::emit_meta_stmt`.

**Second pass (FILE_SPLITTING_ANALYSIS §2 target &lt;50 arms, &lt;~500 lines):**
- **Definitions:** Added `definitions::emit_definitions_stmt` handling SubDefinition, FunctionDefinition, Dim, Const, DefType, Define, OptionBase, OptionExplicit, OptionExplicitArray, CommonStmt, SharedStmt, StaticStmt, Redim. Replaced ~14 inline arms with one delegation.
- **Data:** Added `data::emit_data_related_stmt` for Read, Restore, Randomize. Replaced 3 arms with one.
- **Call:** New `call.rs` with `emit_call_stmt`; Call block (~267 lines) moved out of `mod.rs`. misc delegates Call to `call::emit_call_stmt`.
- **Misc extended:** misc now handles Label, Comment, Expression, IncludeDirective, DefSeg, Poke, MemPutTyped, ConditionalBlock, ConditionalBlockResolved, plus existing Swap, Continue, Run, Chain, events, window/desktop, AssertStmt. Removed duplicate inline arms (Run through AssertStmt, ~200 lines) from `mod.rs`.

**Result:** `mod.rs` reduced from **1,768 lines** to **~760 lines**; dispatcher has few delegation arms (assignments, io, control_flow, definitions, data, misc, file_io, error_jump, def_fn, graphics, audio, system, meta). All tests pass (`cargo test --lib` 430 passed).

### 3. Semantic checker `statements.rs` thinning — **Completed**

- **System submodule:** Added `statements/system.rs` with `check_system_stmt` handling Kill, Rename, Mkdir, … DeclareFunction; extended to also handle System, Sleep, Wait, Delay, Limit, Erase, KeyClear, Run, Chain, Tron, Troff.
- **IO submodule:** Extended `statements/io.rs` so `check_io_stmt` handles console I/O (Print, PrintUsing, Input, LineInput) and legacy file/string (Lprint, FilesStmt, FieldStmt, Lset, Rset) in addition to file I/O (OpenFile … FileSeek). Main dispatcher now has one arm for all I/O.
- **Misc submodule:** Extended `statements/misc.rs` with event/port/interrupt (OnKey, KeyControl, OnTimer, … OutPort, InterruptStmt, InterruptXStmt), legacy file/runtime (IoctlStmt, FreeStmt, ClearStmt, ResetStmt), window/desktop (TitleStmt, ScreenMoveStmt, … ConsoleStmt), and assert/meta (AssertStmt, MetaAsserts, … MetaUseLibrary). Main dispatcher has one arm for all misc.
- **Dispatcher:** Main match in `statements.rs` now has **11 delegation arms** only (assignments, io, control_flow, definitions, data, error_flow, misc, graphics, audio, system, misc events/meta). No inline statement handling remains.

**Result:** Thin dispatcher target (&lt;50 arms) achieved with 11 arms. `docs/ThingsToDo/FILE_SPLITTING_ANALYSIS.md` §3 updated to Done.
