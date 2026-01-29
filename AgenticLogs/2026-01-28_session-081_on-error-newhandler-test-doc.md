# Session 081: ON ERROR GOTO _NEWHANDLER — Test + Doc

**Date:** 2026-01-28

## Goal

Implement fix #8 from FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS: add explicit test(s) for `ON ERROR GOTO _NEWHANDLER handlerlabel` and document that it is supported.

## Done

1. **Parser unit test** — Added `test_parse_on_error_goto_newhandler` in `src/parser/tests.rs`: parses `ON ERROR GOTO _NEWHANDLER handlerlabel` as one statement and asserts target starts with `_NEWHANDLER ` and contains the label.
2. **Integration test** — Added `on_error_goto_newhandler_compiles_and_emits_handler` in `tests/integration_tests.rs` (error_extensions): full pipeline compiles and generated C contains `_qb_error_handler` and the handler label.
3. **Documentation** — Updated:
   - `src/codegen/c_backend/stmt/error_jump.rs`: doc for `emit_on_error_goto` now lists `ON ERROR GOTO _NEWHANDLER label` and explains parser/codegen behavior.
   - `docs/ThingsToDo/PARTIAL_IMPLEMENTATIONS.md`: added row for `ON ERROR GOTO _NEWHANDLER label`.
   - `docs/BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md`: ON ERROR GOTO _NEWHANDLER row updated to "Supported" with test/doc references.
   - `docs/ThingsToDo/FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md`: item 8 marked Done; summary table updated.
4. **Existing coverage** — Bootstrap test `error_handler_syntax` (tests/bootstrap_tests.rs) already covered parse + semantic for ON ERROR GOTO _NEWHANDLER; no change.

## Incidental

- Fixed `src/lsp/tests.rs`: `new_program.statements` → `new_program.0.statements` (parser returns `(Program, Vec<ParseError>)`). Unblocks lib tests.

## Tests

- `cargo test parser::tests::test_parse_on_error_goto_newhandler` — ok
- `cargo test error_extensions::on_error_goto_newhandler_compiles_and_emits_handler` — ok
- `cargo test regression_tests::error_handler_syntax` — ok
