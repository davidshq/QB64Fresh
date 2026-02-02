# File Splitting Analysis

**Date:** 2026-01-31  
**Purpose:** Identify source files that would benefit from splitting to improve maintainability, reviewability, and tool/context efficiency.

---

## Executive Summary

This analysis covers the QB64Fresh compiler (`src/`), runtime (`runtime/src/`), and tools (`tools/`). Line counts are from a full tree scan of `*.rs` files.

**Findings:**

| Priority | Count | Action |
|----------|--------|--------|
| **Medium** | 4 | Consider splitting when touching the area |

**High-priority status (as of 2026-01-31):**

3. **`src/semantic/checker/statements.rs`** (1,339 lines) — Target: thin dispatcher (<50 arms). See §3.

---

## Methodology

1. **Line counts** — `find … -exec wc -l {} \;` over all `*.rs` in `src/`, `runtime/src/`, and `tools/`.
2. **Dispatcher size** — Grep for match arms (e.g. `TypedStatementKind::`, `TypedExprKind::`) in codegen and checker.
3. **Criteria for “split candidate”:**
   - **Size:** >1,500 lines (or >2,000 for cohesive single-purpose files).
   - **Structure:** Monolithic match dispatcher with many arms, or several distinct responsibilities under one file.
   - **Benefit:** Splitting would improve navigation, reduce merge conflicts, and keep single modules within typical context windows.
4. **Verdicts:** High / Medium / Low / Defer / Keep. “Keep” = single enum or one clear abstraction (e.g. `ast/stmt.rs`). “Defer” = could split later but not urgent.

---

## File Inventory (Largest First)

### Compiler (`src/`)

| Lines | Path | Notes |
|------|------|--------|
| 2,491 | `parser/tests.rs` | Test helpers/cases. Lower priority |
| 2,271 | `semantic/builtins.rs` | Builtin registry. **Medium** |
| 2,223 | `semantic/typed_ir.rs` | Typed IR types. **Medium** |
| 1,339 | `semantic/checker/statements.rs` | Stmt type-check dispatcher; delegates to statements/*. **High (thin further)** |

---

## High Priority: Split or Finish Thinning

---

## Medium Priority: Consider When Touching

---

### 5. `src/semantic/typed_ir.rs` (2,223 lines)

**Content:** Typed IR type definitions (`TypedExprKind`, `TypedStatementKind`, and related structs). Single, large type “catalog.”

**Split idea:** Rust enums cannot be split across files. Optional: move related structs and helpers into `typed_ir/` submodules (e.g. `typed_ir/expr.rs`, `typed_ir/stmt.rs`) and keep the main enums in `mod.rs` with re-exports. Only do this if editing becomes painful.

---

### 6. `runtime/src/graphics_ffi.rs` (2,943 lines)

**Content:** FFI bindings for graphics (many `#[no_mangle] pub extern "C"` functions).

**Split idea:** If maintained often, split by domain: e.g. `graphics_ffi/primitives.rs`, `graphics_ffi/images.rs`, `graphics_ffi/text.rs`, `graphics_ffi/state.rs`, with `graphics_ffi/mod.rs` re-exporting. Otherwise defer.

---

## Implementation Plan

### Phase 1: High priority (recommended order)

3. **`statements.rs`** — **In progress**  
   - Move remaining arms into existing submodules so that the main match only delegates. Submodule `statements/system.rs` added 2026-01-31.  
   - **Done when:** &lt;50 match arms. Current: 1,339 lines.

---

## Success Criteria

- **expr:** Met. See High Priority §1.
- **stmt:** &lt;50 match arms, &lt;~500 lines. **Achieved:** ~381 lines (state in `stmt/state.rs`). See §2.
- **statements:** &lt;50 match arms (current: 1,339). See §3.
- All existing tests and (if applicable) golden outputs pass after each change.
- No intentional behavior or performance regressions.

---

## Summary Table

| File | Lines | Arms | Verdict | Next step |
|------|-------|------|---------|-----------|
| `semantic/checker/statements.rs` | 1,339 | many | **Thin** | Move remaining arms into submodules (+ system.rs) |
| `semantic/builtins.rs` | 2,271 | — | **Medium** | Split by category when touching |
| `semantic/typed_ir.rs` | 2,223 | — | **Medium** | Optional typed_ir/ submodules |
| `runtime/graphics_ffi.rs` | 2,943 | — | **Medium** | Split by domain if it grows |
| `tools/debug/server.rs` | 1,263 | — | **Medium** | Split by capability when needed |
---

*Analysis generated 2026-01-31; updated 2026-01-31 after expr split. `expr.rs` removed; `expr/` module in place. Re-run line counts and dispatcher-arm counts when revisiting.*
