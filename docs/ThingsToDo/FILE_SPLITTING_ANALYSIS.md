# File Splitting Analysis

**Date:** 2026-01-31  
**Purpose:** Identify source files that would benefit from splitting to improve maintainability, reviewability, and tool/context efficiency.

---

## Executive Summary

This analysis covers the QB64Fresh compiler (`src/`), runtime (`runtime/src/`), and tools (`tools/`). Line counts are from a full tree scan of `*.rs` files.

**Findings:**

| Priority | Count | Action |
|----------|--------|--------|
| **High** | 3 | Split or continue reducing dispatcher size |
| **Medium** | 4 | Consider splitting when touching the area |
| **Low / Defer** | 5 | Monitor; keep or split only if clear benefit |
| **Keep as-is** | 3 | Single enum or cohesive abstraction |

**High-priority candidates:**

1. **`src/codegen/c_backend/expr.rs`** (3,231 lines, ~56 match arms) — Largest single file; expression codegen dispatcher. Not yet split into submodules; strong candidate for category-based split (literals, binary/unary, calls, string ops, etc.).
2. **`src/codegen/c_backend/stmt/mod.rs`** (2,311 lines, ~126 match arms) — Already partially split (graphics, audio, system, meta, misc, etc.); dispatcher still large. Target: thin dispatcher (<50 arms).
3. **`src/semantic/checker/statements.rs`** (1,533 lines, ~59 match arms) — Already split into submodules; dispatcher close to target. Target: thin dispatcher (<50 arms).

**Already split (no further action):** `runtime/src/io` — previously one large file; now `io/mod.rs`, `io/print.rs`, `io/input.rs`, `io/file.rs`.

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
| 3,231 | `codegen/c_backend/expr.rs` | Expression codegen; single match, ~56 arms. **High** |
| 2,491 | `parser/tests.rs` | Test helpers/cases. Lower priority |
| 2,376 | `ast/stmt.rs` | Single statement enum. **Keep** |
| 2,311 | `codegen/c_backend/stmt/mod.rs` | Stmt codegen dispatcher, ~126 arms. **High** |
| 2,271 | `semantic/builtins.rs` | Builtin registry. **Medium** |
| 2,223 | `semantic/typed_ir.rs` | Typed IR types. **Medium** |
| 1,617 | `lexer/token.rs` | Token enum. **Keep** |
| 1,606 | `codegen/c_backend/runtime/mod.rs` | Runtime header orchestration; already uses submodules. **Defer** |
| 1,634 | `codegen/c_backend/runtime/graphics.rs` | Inline C graphics stubs. Cohesive. **Defer** |
| 1,533 | `semantic/checker/statements.rs` | Stmt type-check dispatcher, ~59 arms. **High** |
| 1,529 | `semantic/symbols.rs` | Symbol table. Single concern. **Defer** |
| 1,508 | `preprocessor.rs` | Preprocessing. Single concern. **Defer** |
| 1,402 | `codegen/c_backend/analysis.rs` | DATA/label analysis. **Defer** |
| 1,365 | `codegen/c_backend/mod.rs` | C backend root. **Defer** |
| 1,247 | `codegen/c_backend/stmt/definitions.rs` | DIM/SUB/FUNCTION codegen. **Defer** |
| 1,215 | `header_parser/parser.rs` | Header parser. **Defer** |
| 1,212 | `codegen/c_backend/runtime/legacy.rs` | Legacy C stubs. **Defer** |
| 1,136 | `codegen/c_backend/implicit_vars.rs` | Implicit variable logic. **Defer** |
| 1,185 | `semantic/checker/expressions.rs` | Expression type-check. **Defer** |
| 1,085 | `semantic/checker/assignments.rs` | Assignment type-check. **Defer** |
| 1,061 | `semantic/checker/const_eval.rs` | Constant evaluation. **Defer** |
| 1,042 | `lsp/mod.rs` | LSP server. **Defer** |
| 964 | `semantic/checker/mod.rs` | Checker root. **Defer** |

### Runtime (`runtime/src/`)

| Lines | Path | Notes |
|------|------|--------|
| 3,992 | `graphics/sdl2.rs` | SDL2 backend; one cohesive impl. **Defer** |
| 2,943 | `graphics_ffi.rs` | Graphics FFI. **Medium** (split by domain if it grows) |
| 2,441 | `io/file.rs` | File I/O (already split from old `io.rs`). OK |
| 2,138 | `string.rs` | Core string type. Cohesive. **Defer** |
| 1,782 | `io/input.rs` | Input/keyboard. OK |
| 1,141 | `audio/rodio_backend.rs` | Audio backend. Cohesive. **Defer** |
| 1,121 | `graphics/mod.rs` | Graphics trait + delegation. **Defer** |
| 931 | `math.rs` | Math functions. **Defer** |
| 758 | `events.rs` | Event handling. **Defer** |
| 533 | `lib.rs` | Crate root. OK |

### Tools (`tools/`)

| Lines | Path | Notes |
|------|------|--------|
| 1,263 | `debug/src/server.rs` | DAP server. **Medium** |
| 947 | `debug/src/lib.rs` | Debug lib root. **Defer** |
| 914 | `fix_encoding.rs` | Standalone script. **Defer** |
| 853 | `debug/src/dap.rs` | DAP types. **Defer** |
| 850 | `debug/src/watch.rs` | Watch expressions. **Defer** |
| 836 | `debug/src/symbols.rs` | Debug symbols. **Defer** |
| 813 | `debug/src/sources.rs` | Source management. **Defer** |

---

## High Priority: Split or Finish Thinning

### 1. `src/codegen/c_backend/expr.rs` (3,231 lines, ~56 match arms)

**Current state:** Single file; one large `emit_expr`-style match on `TypedExprKind` (~56 arms). Handles literals, binary/unary ops, function calls, string ops, array access, etc.

**Why split:**
- Largest file in the repo; hard to navigate and review.
- One function carries many parameters and branches; cognitive load is high.
- Aligns with existing pattern in `stmt/`: category-based submodules + thin dispatcher.

**Suggested structure:**

- **`expr/mod.rs`** — Thin dispatcher: match on `TypedExprKind`, delegate to:
  - `expr/literals.rs` — Integer, float, string literals
  - `expr/binary.rs` — Binary ops (arithmetic, comparison, string concat)
  - `expr/unary.rs` — Unary ops
  - `expr/calls.rs` — Function calls, array access, field access
  - `expr/special.rs` — SHELL, _SHELLHIDE, type conversions, etc.
- Keep shared helpers (e.g. `c_identifier`, string escaping) in a small `expr/helpers.rs` or in existing `types.rs`/`const_fold.rs` where they already live.

**Effort:** Medium (half day). **Risk:** Low if tests and golden outputs are run after each extracted module. **Benefit:** High; largest file becomes a small dispatcher + focused modules.

---

### 2. `src/codegen/c_backend/stmt/mod.rs` (2,311 lines, ~126 match arms)

**Current state:** Already split into submodules (assignments, audio, control_flow, data, def_fn, definitions, error_jump, graphics, io, meta, misc, system). Dispatcher in `mod.rs` still has ~126 arms.

**Why continue:** Dispatcher is still the main source of size and complexity; target is &lt;50 arms so that `mod.rs` is a thin coordinator.

**Approach:** Move more arms into existing submodules by adding module-level “emit_*” helpers that handle a subset of `TypedStatementKind` (e.g. all graphics in `graphics.rs`, all system in `system.rs`). In `mod.rs`, replace those arms with a single call to the helper. Repeat until `mod.rs` is &lt;~400 lines and &lt;50 arms.

**Effort:** Low–medium (1–2 hours). **Risk:** Low. **Benefit:** Clear ownership per statement category; easier to find and change code.

---

### 3. `src/semantic/checker/statements.rs` (1,533 lines, ~59 match arms)

**Current state:** Dispatcher already delegates to submodules (assignments, audio, control_flow, data, definitions, error_flow, graphics, io, misc). ~59 arms remain in the main match.

**Why continue:** 59 is close to the &lt;50 target; a small push will make the file a thin dispatcher.

**Approach:** Move a few more categories fully into submodules (e.g. ensure all I/O, all graphics, all “misc” go through one arm each that call into the submodule). Re-check arm count and trim any remaining inline branches.

**Effort:** Low (&lt;1 hour). **Risk:** Low. **Benefit:** Consistent with codegen; checker and codegen both use thin dispatchers.

---

## Medium Priority: Consider When Touching

### 4. `src/semantic/builtins.rs` (2,271 lines)

**Content:** Builtin function registry (name, types, overloads). Many entries, but repetitive.

**Split idea:** If it grows or becomes hard to scan, split by category: e.g. `builtins/string.rs`, `builtins/math.rs`, `builtins/io.rs`, `builtins/graphics.rs`, etc., with a single `builtins/mod.rs` that aggregates and re-exports. Not urgent.

---

### 5. `src/semantic/typed_ir.rs` (2,223 lines)

**Content:** Typed IR type definitions (`TypedExprKind`, `TypedStatementKind`, and related structs). Single, large type “catalog.”

**Split idea:** Rust enums cannot be split across files. Optional: move related structs and helpers into `typed_ir/` submodules (e.g. `typed_ir/expr.rs`, `typed_ir/stmt.rs`) and keep the main enums in `mod.rs` with re-exports. Only do this if editing becomes painful.

---

### 6. `runtime/src/graphics_ffi.rs` (2,943 lines)

**Content:** FFI bindings for graphics (many `#[no_mangle] pub extern "C"` functions).

**Split idea:** If maintained often, split by domain: e.g. `graphics_ffi/primitives.rs`, `graphics_ffi/images.rs`, `graphics_ffi/text.rs`, `graphics_ffi/state.rs`, with `graphics_ffi/mod.rs` re-exporting. Otherwise defer.

---

### 7. `tools/debug/src/server.rs` (1,263 lines)

**Content:** DAP server implementation (request handling, lifecycle).

**Split idea:** When adding features, consider extracting: request handlers by capability (breakpoints, evaluate, etc.) or by phase (launch, attach, shutdown). No change needed until complexity grows.

---

## Low Priority / Defer

- **`src/ast/stmt.rs`** (2,376) — Single statement enum; splitting enums across files is not idiomatic in Rust. **Keep as-is.** Use comments/sections to keep variants navigable.
- **`src/lexer/token.rs`** (1,617) — Same as above; token enum. **Keep as-is.**
- **`runtime/src/graphics/sdl2.rs`** (3,992) — One SDL2 implementation; cohesive. **Defer** unless it grows past ~5k or becomes hard to work in.
- **`runtime/src/string.rs`** (2,138) — Core string type. **Defer.**
- **`src/parser/tests.rs`** (2,491) — Test code. Can be split by test category later if needed. **Defer.**

---

## Implementation Plan

### Phase 1: High priority (recommended order)

1. **`expr.rs` → `expr/`**  
   - Introduce `expr/mod.rs` and move `emit_expr` + match into it.  
   - Extract 2–3 categories (e.g. literals, binary, calls) into `expr/literals.rs`, `expr/binary.rs`, `expr/calls.rs`.  
   - Wire dispatcher to new modules; run tests.  
   - Extract remaining categories (unary, special) and any helpers.  
   - **Done when:** `expr/mod.rs` is &lt;~500 lines and match has &lt;~20 arms; rest in submodules.

2. **`stmt/mod.rs`**  
   - For each remaining large block of arms in `mod.rs`, add or use a helper in the right submodule (e.g. `graphics::emit_graphics_stmt`) and replace arms with one call.  
   - **Done when:** &lt;50 match arms, file &lt;~500 lines.

3. **`statements.rs`**  
   - Move remaining arms into existing submodules so that the main match only delegates.  
   - **Done when:** &lt;50 match arms.

### Phase 2: Medium / later

- Revisit **builtins.rs**, **typed_ir.rs**, **graphics_ffi.rs**, **debug/server.rs** when doing larger refactors or when file size/editing pain increases.

### Phase 3: Monitor

- **ast/stmt.rs**, **lexer/token.rs** — Keep; no structural split.  
- **graphics/sdl2.rs**, **string.rs**, **parser/tests.rs** — Re-evaluate if they grow or become hard to work in.

---

## Success Criteria

- **expr:** `codegen/c_backend/expr.rs` replaced by `expr/` module with thin dispatcher and &lt;~56 arms total across submodules; no single file &gt;~1,000 lines.
- **stmt:** `stmt/mod.rs` &lt;50 match arms, &lt;~500 lines.
- **statements:** `statements.rs` &lt;50 match arms.
- All existing tests and (if applicable) golden outputs pass after each change.
- No intentional behavior or performance regressions.

---

## Summary Table

| File | Lines | Arms | Verdict | Next step |
|------|-------|------|---------|-----------|
| `codegen/c_backend/expr.rs` | 3,231 | ~56 | **Split** | Create `expr/`, category-based modules |
| `codegen/c_backend/stmt/mod.rs` | 2,311 | ~126 | **Thin** | Move arms into existing submodules |
| `semantic/checker/statements.rs` | 1,533 | ~59 | **Thin** | Move remaining arms into submodules |
| `semantic/builtins.rs` | 2,271 | — | **Medium** | Split by category when touching |
| `semantic/typed_ir.rs` | 2,223 | — | **Medium** | Optional typed_ir/ submodules |
| `runtime/graphics_ffi.rs` | 2,943 | — | **Medium** | Split by domain if it grows |
| `tools/debug/server.rs` | 1,263 | — | **Medium** | Split by capability when needed |
| `ast/stmt.rs` | 2,376 | — | **Keep** | — |
| `lexer/token.rs` | 1,617 | — | **Keep** | — |
| `runtime/graphics/sdl2.rs` | 3,992 | — | **Defer** | Monitor size/navigation |
| `runtime/io/*` | — | — | **Done** | Already split |

---

*Analysis generated 2026-01-31 from current tree. Re-run line counts and dispatcher-arm counts when revisiting.*
