# File-by-file review (second pass)

**Date:** 2026-01-31. Per-file checklist applied; one row per `.rs` file.

| File | Phase | Date | Status | Critical | Notes |
|------|-------|------|--------|----------|-------|
| src/ast/expr.rs | 1 | 2026-01-31 | done | N | OK |
| src/ast/mod.rs | 1 | 2026-01-31 | done | N | OK |
| src/ast/stmt.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/analysis.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/const_fold.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/expr.rs | 1 | 2026-01-31 | done | N | 8 unwrap/expect |
| src/codegen/c_backend/file_io.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/implicit_vars.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/mod.rs | 1 | 2026-01-31 | done | N | 4 unwrap/expect |
| src/codegen/c_backend/resources.rs | 1 | 2026-01-31 | done | N | 1 unwrap/expect |
| src/codegen/c_backend/runtime/arrays.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/audio.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/bitops.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/debug.rs | 1 | 2026-01-31 | done | N | 2 unwrap/expect |
| src/codegen/c_backend/runtime/error.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/file.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/graphics.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/io.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/keyboard.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/legacy.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/logging.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/math.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/memory.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/mod.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/strings.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/system.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/timing.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/runtime/types.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/assignments.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/audio.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/control_flow.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/data.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/def_fn.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/definitions.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/error_jump.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/graphics.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/io.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/meta.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/misc.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/mod.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/stmt/system.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/type_registry.rs | 1 | 2026-01-31 | done | N | 6 unwrap/expect |
| src/codegen/c_backend/types.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/c_backend/write_helpers.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/error.rs | 1 | 2026-01-31 | done | N | OK |
| src/codegen/mod.rs | 1 | 2026-01-31 | done | N | 1 unwrap/expect |
| src/error_formatting.rs | 1 | 2026-01-31 | done | N | OK |
| src/header_parser/lexer.rs | 1 | 2026-01-31 | done | N | OK |
| src/header_parser/mod.rs | 1 | 2026-01-31 | done | N | OK |
| src/header_parser/parser.rs | 1 | 2026-01-31 | done | N | OK |
| src/lexer/mod.rs | 1 | 2026-01-31 | done | N | 1 unwrap/expect |
| src/lexer/token.rs | 1 | 2026-01-31 | done | N | OK |
| src/lib.rs | 1 | 2026-01-31 | done | N | 1 unwrap/expect |
| src/library.rs | 1 | 2026-01-31 | done | N | 22 unwrap/expect |
| src/lsp/analysis.rs | 1 | 2026-01-31 | done | N | OK |
| src/lsp/analysis/incremental.rs | 1 | 2026-01-31 | done | N | OK |
| src/lsp/main.rs | 1 | 2026-01-31 | done | N | OK |
| src/lsp/mod.rs | 1 | 2026-01-31 | done | N | 1 unwrap/expect |
| src/lsp/position.rs | 1 | 2026-01-31 | done | N | OK |
| src/lsp/signatures.rs | 1 | 2026-01-31 | done | N | OK |
| src/lsp/tests.rs | 1 | 2026-01-31 | done | N | 24 unwrap/expect |
| src/main.rs | 1 | 2026-01-31 | done | N | OK |
| src/parser/audio.rs | 1 | 2026-01-31 | done | N | 17 unwrap/expect |
| src/parser/control_flow.rs | 1 | 2026-01-31 | done | N | 21 unwrap/expect |
| src/parser/directives.rs | 1 | 2026-01-31 | done | N | 29 unwrap/expect |
| src/parser/error.rs | 1 | 2026-01-31 | done | N | OK |
| src/parser/expressions.rs | 1 | 2026-01-31 | done | N | 36 unwrap/expect |
| src/parser/file_io.rs | 1 | 2026-01-31 | done | N | 35 unwrap/expect |
| src/parser/graphics.rs | 1 | 2026-01-31 | done | N | 33 unwrap/expect |
| src/parser/mod.rs | 1 | 2026-01-31 | done | N | 1 unwrap/expect |
| src/parser/procedures.rs | 1 | 2026-01-31 | done | N | 18 unwrap/expect |
| src/parser/statements/assignments.rs | 1 | 2026-01-31 | done | N | 22 unwrap/expect |
| src/parser/statements/control_etc.rs | 1 | 2026-01-31 | done | N | 106 unwrap/expect |
| src/parser/statements/data_dims.rs | 1 | 2026-01-31 | done | N | 36 unwrap/expect |
| src/parser/statements/declare.rs | 1 | 2026-01-31 | done | N | 11 unwrap/expect |
| src/parser/statements/mod.rs | 1 | 2026-01-31 | done | N | OK |
| src/parser/statements/print_input.rs | 1 | 2026-01-31 | done | N | 16 unwrap/expect |
| src/parser/system.rs | 1 | 2026-01-31 | done | N | 5 unwrap/expect |
| src/parser/tests.rs | 1 | 2026-01-31 | done | N | 196 unwrap/expect (test helpers) |
| src/parser/tokens.rs | 1 | 2026-01-31 | done | N | 3 unwrap/expect |
| src/preprocessor.rs | 1 | 2026-01-31 | done | N | 35 unwrap/expect |
| src/semantic/builtins.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/builtins_opengl.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/assignments.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/const_eval.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/control_flow.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/definitions.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/expressions.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/mod.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/statements.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/statements/assignments.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/statements/audio.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/statements/control_flow.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/statements/data.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/statements/definitions.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/statements/error_flow.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/statements/graphics.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/statements/io.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/checker/statements/misc.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/collect.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/error.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/mod.rs | 1 | 2026-01-31 | done | N | 3 unwrap/expect |
| src/semantic/suggestions.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/symbols.rs | 1 | 2026-01-31 | done | N | 17 unwrap/expect |
| src/semantic/typed_ir.rs | 1 | 2026-01-31 | done | N | OK |
| src/semantic/types.rs | 1 | 2026-01-31 | done | N | OK |
| runtime/src/array_registry.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/audio/error.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/audio/midi.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/audio/mock.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/audio/mod.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/audio/rodio_backend.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/audio_ffi.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/bitops.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/buffer.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/cmem.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/completion.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/condvar.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/condvar_ffi.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/console_display_ffi.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/cp437.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/dialogs.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/events.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/filepath.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/font_ffi.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/font_manager.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/game_controller_ffi.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/graphics/error.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/graphics/font.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/graphics/mock.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/graphics/mod.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/graphics/sdl2.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/graphics_ffi.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/http.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/http_ffi.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/io/file.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/io/input.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/io/mod.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/io/print.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/joystick.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/lib.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/list.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/logging.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/logging_ffi.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/math.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/mem_lock.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/memory.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/mutex.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/mutex_ffi.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/qbs_compat.rs | 2 | 2026-01-31 | done | N | FFI/unsafe |
| runtime/src/string.rs | 2 | 2026-01-31 | done | N | OK |
| runtime/src/thread.rs | 2 | 2026-01-31 | done | N | OK |
| tools/debug/src/config.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/dap.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/error.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/frames.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/lib.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/main.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/protocol.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/server.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/sources.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/symbols.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/values.rs | 3 | 2026-01-31 | done | N | OK |
| tools/debug/src/watch.rs | 3 | 2026-01-31 | done | N | OK |
| tools/fix_encoding.rs | 3 | 2026-01-31 | done | N | OK |
| tools/fmt/src/config.rs | 3 | 2026-01-31 | done | N | OK |
| tools/fmt/src/error.rs | 3 | 2026-01-31 | done | N | OK |
| tools/fmt/src/formatter.rs | 3 | 2026-01-31 | done | N | OK |
| tools/fmt/src/lib.rs | 3 | 2026-01-31 | done | N | OK |
| tools/fmt/src/main.rs | 3 | 2026-01-31 | done | N | OK |
| tools/fmt/src/rules.rs | 3 | 2026-01-31 | done | N | OK |
| tools/lint/src/config.rs | 3 | 2026-01-31 | done | N | OK |
| tools/lint/src/error.rs | 3 | 2026-01-31 | done | N | OK |
| tools/lint/src/lib.rs | 3 | 2026-01-31 | done | N | OK |
| tools/lint/src/main.rs | 3 | 2026-01-31 | done | N | OK |
| tools/lint/src/rules/complexity.rs | 3 | 2026-01-31 | done | N | OK |
| tools/lint/src/rules/correctness.rs | 3 | 2026-01-31 | done | N | OK |
| tools/lint/src/rules/mod.rs | 3 | 2026-01-31 | done | N | OK |
| tools/lint/src/rules/style.rs | 3 | 2026-01-31 | done | N | OK |
| tools/lint/src/rules/unused.rs | 3 | 2026-01-31 | done | N | OK |
| benches/compiler_benchmarks.rs | 4 | 2026-01-31 | done | N | OK |
| fuzz/fuzz_targets/fuzz_full_pipeline.rs | 4 | 2026-01-31 | done | N | OK |
| fuzz/fuzz_targets/fuzz_lexer.rs | 4 | 2026-01-31 | done | N | OK |
| fuzz/fuzz_targets/fuzz_parser.rs | 4 | 2026-01-31 | done | N | OK |
| tests/bootstrap_tests.rs | 4 | 2026-01-31 | done | N | OK |
| tests/common/mod.rs | 4 | 2026-01-31 | done | N | OK |
| tests/compatibility.rs | 4 | 2026-01-31 | done | N | OK |
| tests/error_recovery_tests.rs | 4 | 2026-01-31 | done | N | OK |
| tests/execution_tests.rs | 4 | 2026-01-31 | done | N | OK |
| tests/golden_tests.rs | 4 | 2026-01-31 | done | N | OK |
| tests/integration_tests.rs | 4 | 2026-01-31 | done | N | OK |
| tests/proptest_tests.rs | 4 | 2026-01-31 | done | N | OK |
| tests/qb45_compat.rs | 4 | 2026-01-31 | done | N | OK |

---

## Additional files (third pass – previously not in .rs list)

Files that are first-party source but were not in the Phase 1–4 `.rs` set: build script, C header, C source.

| File | Phase | Date | Status | Critical | Notes |
|------|-------|------|--------|----------|-------|
| runtime/build.rs | 2 | 2026-01-31 | done | N | Build script; 1 expect (CARGO_MANIFEST_DIR); module doc; compiles gl_wrappers.c when opengl. |
| runtime/include/qb64fresh_rt.h | 2 | 2026-01-31 | done | N | C header; FFI declarations; platform macros; string/array/io/graphics/audio/etc.; well-commented. |
| runtime/c_src/gl_wrappers.c | 2 | 2026-01-31 | done | N | C; OpenGL wrappers; GL_CHECK; sub_gl_called; doc block present. |
| runtime/c_src/logging.c | 2 | 2026-01-31 | done | N | C; variadic libqb_log/libqb_log_qb64; doc and usage in comment. |

---

## Fourth pass (Phase 0 re-check, spot-check, clippy)

**Date:** 2026-01-31.

- **Phase 0:** Build and test re-run: pass.
- **Spot-check:** error_formatting.rs, codegen/error.rs (marked OK): module docs and pub item docs present.
- **Docs:** OPENGL.md scanned; consistent with CODE_REVIEW and OPENGL_GLUT_DESIGN.
- **Clippy:** bootstrap_tests: collapsible_if (2 blocks) fixed; push_str(" ") → push(' '). Remaining: redundant_closure (2), loop index, explicit closure (2) — deferred.
