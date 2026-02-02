# Review: stmt/mod.rs thinning (errors and bugs)

**Date:** 2026-01-31  
**Scope:** Changes from FILE_SPLITTING_ANALYSIS §2 (definitions, data, call, misc dispatchers).

## Summary

- **Build:** OK (`cargo build`)
- **Tests:** OK (`cargo test --lib` 430 passed, `cargo test --test qb45_compat` 5 passed)
- **No critical bugs found.** One consistency improvement noted below.

---

## Checks performed

### 1. Dispatcher routing (mod.rs)

- **Order:** Assignment → io → control_flow → definitions → data → misc → meta (first block) → file_io → error_jump → def_fn → graphics → audio → system → meta (second block).
- **No double-handling:** `ResumeStmt` and `ErrorStmt` are only in the error_jump arm (not in misc). MouseHide, MouseShow, DeclareLibrary, etc. are only in the system arm (not in the misc arm). So each statement kind is handled by a single arm.
- **Exhaustiveness:** The match on `TypedStatementKind` is exhaustive; every variant is covered by one of the arms.

### 2. definitions::emit_definitions_stmt

- **Const:** Uses `c_type(&value.basic_type)` and `emitter.emit_expr(value)`; types and API are correct.
- **StaticStmt:**  
  - Scalar: `static ty name = init;` with `default_init` for non-strings, `"NULL"` for strings — correct.  
  - Static array (`var.is_static`): `static ty name[dim] = {0};` plus `qb_array_register` — correct.  
  - Dynamic array (`!var.is_static`): `static ty* name = malloc/calloc(...);` plus `qb_array_register`; **does not** call `emit_dim` (original mod.rs also emitted this branch manually), so no double emission.
- **_ => unreachable!:** Only used for definition kinds; caller (mod.rs) only passes those, so appropriate.

### 3. data::emit_data_related_stmt

- **Read / Restore:** Delegates to `emitter.emit_read` and `emitter.emit_restore`; signatures match.
- **Randomize:** Emits `qb_randomize((double)(...))` or `qb_randomize_timer()`; matches previous behavior.
- **_ => unreachable!:** Caller only passes Read, Restore, Randomize; appropriate.

### 4. call::emit_call_stmt

- **Byref / UDT / lvalue:** Logic matches original (arg codes, temp decls, `c_name` mapping).
- **Built-in names:** Same set of special cases (_ICON, _PALETTECOLOR, _KEYDOWN, _KEYUP, _NOTIFYPOPUP, OpenGL, etc.) and same early returns where needed.
- **OpenGL:** `c_name.starts_with("call_gl") && emitter.uses_opengl` and `#ifdef QB64FRESH_OPENGL` — correct.

### 5. misc::emit_misc_stmt

- **Label:** Uses `emitter.proc_label(name)` and `emitter.codegen.emitted_labels.insert(...)`; `proc_label` is now `pub(super)` so this compiles and behavior is unchanged.
- **ConditionalBlock / ConditionalBlockResolved:** Recursively call `emitter.emit_stmt`; correct.
- **Call:** Delegates to `super::call::emit_call_stmt`; no duplication with mod.rs.
- **TypeDefinition / Data:** No-ops; consistent with “handled elsewhere / at global emission”.
- **misc vs system:** Kinds that are in the **system** arm in mod.rs (MouseHide, MouseShow, ClipboardSet, DeclareLibrary, etc.) are **not** in the misc arm, so they are handled only by system. misc implements the same variants for use when invoked from its own match (e.g. from other code paths); in the main dispatcher, only the system arm sees those kinds, so routing is correct.

### 6. proc_label visibility

- **Change:** `fn proc_label` → `pub(super) fn proc_label` in mod.rs.
- **Use:** Only misc uses it for `Label`; no other cross-module use. Appropriate.

---

## Consistency improvement (non-blocking)

- **StaticStmt and parameter shadowing:**  
  `emit_dim` in definitions.rs renames locals that shadow parameters (`current_func_param_names` / `variable_renames`).  
  `emit_definitions_stmt`’s **StaticStmt** branch does not apply this renaming; it uses `c_identifier(&var.name)` only. So a `STATIC` variable that shadows a parameter could, in theory, generate a name collision in C.  
  The **original** mod.rs StaticStmt block also did not apply the shadowing logic (it emitted the static array block manually without going through `emit_dim`). So this is not a regression.  
  **Suggestion (optional):** When emitting StaticStmt in a procedure, apply the same shadowing check and renaming as in `emit_dim` for consistency and to avoid rare collisions.

---

## Conclusion

- No errors or bugs found in the thinning changes.
- Dispatcher routing is correct and non-duplicated.
- definitions, data, call, and misc behave as intended and match previous semantics.
- Optional improvement: align StaticStmt with `emit_dim`’s parameter-shadowing handling for consistency.
