# QB64Fresh: File Size and Split Review

**Purpose:** Identify large source files that make it harder for AI assistants (and humans) to work effectively, and recommend which should be split vs. kept as-is.

**Date:** 2026-01-24 (Updated)

---

## Summary

| Category | Count | Status |
|----------|-------|--------|
| **Do not split** | 5 | Keep enums/Logos definitions in one file |
| **Completed splits** | 7 | ✅ All major refactoring done |
| **Moderate / optional** | 3 | Low priority, split only if needed |

---

## 1. Files That Should NOT Be Split

These are large mainly because they define **one coherent enum or derive macro** that must stay in a single place. Splitting would either break exhaustiveness checking, the `#[derive(Logos)]` contract, or force brittle `#[cfg]`/re-exports.

### `src/ast/stmt.rs` — **2,237 lines**

- **Content:** `StatementKind` enum with ~100+ variants (Print, Let, If, For, MidAssignment, etc.) and their `struct`/field docs.
- **Why not split:** Rust enums are exhaustively matched; splitting variants across files isn't supported. You'd need multiple enums (bad for `match`) or `include!`-style tricks (fragile).
- **Recommendation:** **Keep as-is.** The size is inherent to BASIC's statement set. Use `// region`-style comments or `#[doc]` groups to navigate. Consider `ast/stmt.rs` as the single source of truth for "what statements exist."

### `src/semantic/typed_ir.rs` — **2,033 lines**

- **Content:** `TypedExprKind` and `TypedStatementKind` enums mirroring the AST, plus `TypedArrayDimension`, `TypedParameter`, etc.
- **Why not split:** Same as `ast/stmt.rs`: one family of enums, one place. TypedIR is the main contract between semantic and codegen.
- **Recommendation:** **Keep as-is.** When adding AST statement/expr kinds, this file grows in lockstep; that's expected.

### `src/lexer/token.rs` — **1,591 lines**

- **Content:** `#[derive(Logos)] pub enum TokenKind` with many `#[token(...)]` and `#[regex(...)]` variants.
- **Why not split:** The `logos` crate expects the full enum in one definition. You cannot split `TokenKind` across modules and still `derive(Logos)`.
- **Recommendation:** **Keep as-is.** Size comes from BASIC's keyword and token set. Group with `// ========== Section ==========` and keep token naming consistent.

### `src/ast/expr.rs` — **294 lines**

- **Content:** `ExprKind` enum and `BinaryOp`/`UnaryOp`. Smaller than `stmt.rs` but same "one enum" rule.
- **Recommendation:** **Keep as-is.** Not a pressing size problem; splitting would still violate the "one enum" principle.

### `src/lexer/mod.rs` — **256 lines**

- **Content:** Lexer wrapper, iterator, and `lex()` entry. Modest size.
- **Recommendation:** **Keep as-is.** No need to split.

---

## 3. Moderate / Optional Splits (Low Priority)

These are smaller or have fewer obvious boundaries. Splitting is optional and should be done only if you want to shorten files for a specific workflow.

### `src/codegen/c_backend/expr.rs` — **1,452 lines**

- **Content:** `emit_expr` and a `match` on `TypedExprKind`, plus helpers. Smaller than `stmt.rs` but same pattern.
- **Options:** If it grows toward 2k+ lines, mirror `stmt`: keep the `match` in `expr.rs`, move `emit_*` helpers into `expr/` submodules.
- **Recommendation:** **Leave as-is for now.** Revisit if it crosses ~2,000 lines.

---

### `runtime/src/graphics/sdl2.rs` — **2,144 lines**
### `runtime/src/graphics_ffi.rs` — **1,666 lines**

- **Content:** SDL2-backed implementation of the graphics API; FFI bindings and trampolines.
- **Options:** Split by domain: e.g. `sdl2/drawing.rs`, `sdl2/image.rs`, `sdl2/screen.rs`.
- **Recommendation:** **Low priority** unless you often work in this area.

---

## 4. Metrics Summary

### Before and After Line Counts

| File | Before | After | Reduction |
|------|--------|-------|-----------|
| `codegen/c_backend/runtime.rs` | 5,660 | 367 (mod.rs) | 94% |
| `codegen/c_backend/stmt.rs` | 4,215 | 2,540 (mod.rs) | 40% |
| `parser/statements.rs` | 3,945 | 307 (mod.rs) | 92% |
| `semantic/mod.rs` | 3,228 | 636 | 80% |
| `semantic/checker/statements.rs` | 3,096 | 2,037 | 34% |
| `parser/mod.rs` | 2,451 | 163 | 93% |
| `lsp/mod.rs` | 2,206 | 967 | 56% |

### Files That Stay Large (By Design)

| File | Lines | Reason |
|------|-------|--------|
| `ast/stmt.rs` | 2,237 | Single enum, cannot split |
| `semantic/typed_ir.rs` | 2,033 | Single enum family, cannot split |
| `lexer/token.rs` | 1,591 | Logos derive, cannot split |

---

## 5. Principles Applied

- **One enum, one file:** `StatementKind`, `TypedExprKind`, `TypedStatementKind`, `TokenKind` stay in single modules.
- **Logos:** The full `TokenKind` enum must remain in one `#[derive(Logos)]` block.
- **Dispatcher + helpers:** For `emit_stmt`, `parse_statement`, `check_statement`, keep the central `match` in one place; move helper implementations into submodules.
- **Repetitive / data-like blocks:** `register_builtins`, `get_builtin_signature`, and the `emit_*` C emission blocks were good candidates for extraction.
- **Tests:** Large `#[cfg(test)] mod tests` blocks can move to `module/tests.rs` without affecting production structure.
- **Target size:** Aim for production files in the **< 1,500 lines** range where practical; "do not split" files are exceptions.

---

No urgent refactoring remains. The codebase is now well-organized for AI-assisted development.
