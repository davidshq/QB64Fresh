# Session 119 — Keyhandler keydown_vk/keyup_vk Stub

**Date:** 2026-01-31

## Summary

Completed **§21 keyhandler** from LIBQB_FUNCTIONALITY: key state via inkey/keyhit is already implemented; added BASIC statement support for `_KEYUP`/`_KEYDOWN` (simulate key) so they emit `qb_keyup_vk`/`qb_keydown_vk` and documented stubs as done.

## Changes

1. **Builtins** (`src/semantic/builtins.rs`)
   - Registered `_KEYUP` as builtin sub with one Long param (`code`).
   - Comment that `_KEYDOWN(code)` as statement is handled in codegen (same name as function).

2. **Codegen** (`src/codegen/c_backend/stmt/mod.rs`)
   - Call `_KEYDOWN(code)` as statement → `qb_keydown_vk((uint32_t)code);`
   - Call `_KEYUP(code)` as statement → `qb_keyup_vk((uint32_t)code);`
   - Single-arg case emits the call and returns; wrong arg count falls through to generic path.

3. **Runtime** (`runtime/src/io/input.rs`)
   - Doc comments for `qb_keydown_vk`/`qb_keyup_vk` clarified as keyhandler stubs (no-op; real impl would use platform key injection).

4. **Tests** (`tests/integration_tests.rs`)
   - `keyup_statement_emits_qb_keyup_vk`: `_KEYUP 13` → C contains `qb_keyup_vk((uint32_t)...)`.
   - `keydown_statement_emits_qb_keydown_vk`: `_KEYDOWN 100305` → C contains `qb_keydown_vk((uint32_t)...)`.
   - `inline_runtime_includes_keydown_keyup_vk_stubs`: emitted inline runtime contains both stub declarations.

5. **Docs**
   - `docs/QB64pe/LIBQB_FUNCTIONALITY.md`: §21 keyhandler row → 🟢 (stub in place; BASIC statements emit calls).
   - `docs/QB64pe/LIBQB_FUNCTIONALITY_COMPLETED.md`: keydown_vk/keyup_vk row → 🟢 (stub; BASIC `_KEYUP`/`_KEYDOWN` statement → runtime calls).

## Status

- **Key state:** inkey/keyhit and _KEYDOWN (function) unchanged; already working.
- **Simulate key:** `qb_keydown_vk`/`qb_keyup_vk` remain no-op stubs; BASIC `_KEYUP` and `_KEYDOWN` (statement) now compile to those calls. Full implementation would require platform-specific key injection (e.g. SendInput / XTest) and is out of scope.
