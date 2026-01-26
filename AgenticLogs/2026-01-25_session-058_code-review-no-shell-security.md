# Code Review: `--no-shell` and Security Changes

**Date:** 2026-01-25  
**Scope:** Session 058 — `--no-shell` compile-time flag, ShellDisabled, `emit_expr(_, no_shell)`, SECURITY_MODEL updates, and related edits (incl. `data_dims`).

---

## 1. Correctness

### 1.1 SHELL / _SHELLHIDE Handling

- **ShellCmd / ShellHide:** When `no_shell` is true, you return `Err(ShellDisabled)` before any `emit_expr` or `qb_shell` / `qb_shell_hide` emission. `SHELL` with no command (`qb_shell(NULL)`) is also rejected. ✅
- **FunctionCall in expr:** The `upper_name == "SHELL" || upper_name == "_SHELLHIDE"` check runs after `to_uppercase()`, so it’s case-insensitive. ✅
- **Function vs statement:** Both `SHELL(cmd$)` / `_SHELLHIDE(cmd$)` (in `emit_expr`) and `SHELL` / `_SHELLHIDE` statements (in `emit_stmt`) are rejected when `no_shell` is true. ✅

### 1.2 `no_shell` Propagation

- **StmtEmitter / CBackend:** `no_shell` is set in `CBackend::generate` and passed through `StmtEmitter`. All `emit_expr` call sites in stmt, control_flow, definitions, assignments, io, data, def_fn, error_jump, and file_io use `self.no_shell`. ✅
- **analysis (collect_globals):** Uses `emit_expr(value_expr, false)`. CONST initializers must be constant; `SHELL` is not, so this is acceptable. For strict consistency you could thread `self.no_shell` from `CBackend::generate` into `collect_globals`, but it’s optional.

### 1.3 `false` for `no_shell`

- **analysis:** `false` is correct (no `StmtEmitter` / `no_shell` there; CONST can’t be SHELL).
- **expr doc tests:** `false` is correct (unit tests; no SHELL in those expressions).

### 1.4 Spans and Errors

- `ast::Span` is `Copy`. `stmt.span.clone()` and `expr.span.clone()` are redundant; `stmt.span` and `expr.span` suffice. Harmless.
- `TypedStatement` and `TypedExpr` use `crate::ast::Span`; `CodeGenError::with_span` takes `Span`. Types match. ✅

---

## 2. Minor Cleanups (Optional)

### 2.1 Redundant `.clone()` on `Span`

`Span` is `Copy`, so you can drop `.clone()` when attaching spans to `CodeGenError`:

```rust
// stmt/mod.rs
.with_span(stmt.span)   // was: stmt.span.clone()

// expr.rs FunctionCall
.with_span(expr.span)  // was: expr.span.clone()
```

### 2.2 `--no-shell` Only Affects `--emit-c`

`--no-shell` is only applied when `args.emit_c` is true. With `--ast`, `--tokens`, `--typed-ir`, etc., it is effectively ignored. That’s logically fine, but the help text could clarify that it only has effect with `--emit-c`, e.g.:

```
--no-shell      Disable SHELL/_SHELLHIDE when using --emit-c (compile error if used)
```

### 2.3 `analysis::collect_globals` and `no_shell`

For strict uniformity you could pass `no_shell` from `CBackend::generate` into `collect_globals` and use it in `emit_expr` instead of `false`. Semantics don’t change today (CONST can’t be SHELL), but it would make the rule “every `emit_expr` gets `no_shell` from the top-level codegen context” consistent.

---

## 3. Unrelated Edits (`data_dims`)

### 3.1 `TokenKind::String` → `TokenKind::String_`, `TokenKind::Bit` → `TokenKind::BitType`

These match the lexer (`String_` to avoid the Rust keyword, `BitType` for `_BIT`). ✅

### 3.2 `ok_or_else` → `match` for `peek()`

Replacing `ok_or_else` with a `match` avoids borrowing `self` (from `peek()`) and `self.errors` in the same closure and fixes the borrow-check error. The logic (push error and `return Err(())`) is preserved. ✅

### 3.3 `token.clone()` in the `match`

`peek()` gives `Option<&Token>`. `Some(t) => t.clone()` is a correct way to get an owned `Token` for the `match` and `advance()` in each arm. ✅

---

## 4. Practices and Robustness

### 4.1 `StmtEmitter::no_shell`

`no_shell` is `pub`. Only `CBackend::generate` sets it; other code only reads. For a more encapsulated API you could make it `pub(crate)` or add a getter, but as used it’s fine.

### 4.2 `CBackend::default()` and `new()`

Both set `no_shell: false`. `with_runtime_mode` does too. If `with_no_shell` is not used, the default is to allow SHELL. Matches the intended default. ✅

### 4.3 `CodeGenErrorKind::ShellDisabled`

- New variant is documented and has a clear `Display` message. ✅
- `CodeGenError`’s `Display` already includes `span` when present, so “at 0..15”–style output is fine. ✅

---

## 5. Summary

| Area | Status | Notes |
|------|--------|-------|
| SHELL / _SHELLHIDE | ✅ | Statement and function forms; `SHELL` with no command; case-insensitive. |
| `no_shell` propagation | ✅ | All `emit_expr` in codegen use `self.no_shell` or `no_shell`; analysis uses `false` where CONST can’t be SHELL. |
| `analysis` + `false` | ✅ | Acceptable; optionally thread `no_shell` for consistency. |
| Spans | ✅ | `.clone()` on `Span` is redundant, not wrong. |
| `--no-shell` without `--emit-c` | ✅ | Flag ignored; only codegen checks SHELL. Help text could clarify. |
| `data_dims` TokenKind | ✅ | `String_` and `BitType` match lexer. |
| `data_dims` `ok_or_else` → `match` | ✅ | Borrow fix; behavior unchanged. |

**Verdict:** No bugs found. The two optional cleanups (drop `Span::clone()` where `Span` is `Copy`, and documenting that `--no-shell` only applies with `--emit-c`) are minor.
