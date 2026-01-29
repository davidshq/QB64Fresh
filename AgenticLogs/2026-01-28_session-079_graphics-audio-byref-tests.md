# Session 079 — Graphics/Audio Integration Tests & BYREF Verification

**Date:** 2026-01-28

## Goal

Address TODO_CONSOLIDATED.md §5 remaining items (lines 46–48):

1. Graphics backend integration tests (SDL single-init challenge)
2. Audio backend integration tests
3. By-ref parameter codegen (parameters as pointers not dereferenced)

## Plan

1. **Graphics:** Add `serial_test` to runtime dev-dependencies; add `runtime/tests/graphics_integration.rs` with `#[serial]` tests that init/shutdown SDL once per process.
2. **Audio:** Add `runtime/tests/audio_integration.rs` with `#[serial]` tests for rodio init/shutdown.
3. **BYREF:** Verify existing codegen and tests (pointer alias + dereference already implemented in Session 076); run tests.

## Outcomes

1. **Graphics backend integration tests**
   - Added `serial_test` to `runtime/Cargo.toml` dev-dependencies.
   - Created `runtime/tests/graphics_integration.rs` with `#[cfg(feature = "graphics-sdl2")]` and two `#[serial]` tests: `graphics_init_and_shutdown`, `graphics_init_after_shutdown`.
   - Both tests pass (`cargo test -p qb64fresh-runtime --test graphics_integration`).

2. **Audio backend integration tests**
   - Created `runtime/tests/audio_integration.rs` with `#[cfg(feature = "audio-rodio")]` and two `#[serial]` tests: `audio_init_and_shutdown`, `audio_init_after_shutdown`.
   - Both tests pass (`cargo test -p qb64fresh-runtime --test audio_integration`).

3. **By-ref parameter codegen**
   - Already implemented: `definitions.rs` emits pointer alias for BYREF scalars (`int32_t* n = n_ref`); expr/assignments/control_flow dereference via `current_func_byref_scalar_names`.
   - Ran existing tests: `integration_tests::procedures::byref_scalar_sub_compiles_and_emits_write_through` and `execution_tests::execution::byref_scalar_sub_modifies_caller_variable` — both pass.

4. **Runtime fix (unblocking build)**
   - Fixed four `qb_net_openhost(0)` calls in `runtime/src/io.rs` to use `std::ptr::null()` so the runtime compiles (expected `*const i8`, found `usize`).
