# Session 139: Bootstrap full execution test

**Date:** 2026-01-31

## Summary

Implemented the full execution path for `qb64pe_can_compile_hello_world` as described in the TODO (bootstrap_tests.rs lines 471–479).

## Decisions

- **QB64pe `-o` semantics:** QB64pe’s `-o` is the output *executable* name, not a .c file. So the test uses `-x test.bas -o test` and runs `./test` to verify output contains "Hello". No separate “verify test.c” or “gcc test.c” step.
- **Skip conditions:** If `pkg-config sdl2` fails or QB64pe root is missing, the test skips with a clear message instead of failing, so CI/local runs without SDL2 or QB64pe still pass (codegen checks still run).
- **Temp dir:** All artifacts (qb64pe.c, qb64pe_bootstrapped, test.bas, test exe) go under `std::env::temp_dir()/qb64pe_bootstrap_<pid>`.
- **QB64pe cwd:** Bootstrapped QB64pe is run with `current_dir(QB64pe root)` so it can find `internal/temp` and other assets.

## Implementation

1. **Imports:** `std::io::Write`, `std::process::Command` in bootstrap_tests.rs.
2. **Steps in test:**
   - Compile QB64pe in a 16MB-stack thread (same as other bootstrap tests).
   - In-memory check: compile `PRINT "Hello"` with QB64Fresh and assert codegen (qb_print_string, "Hello", int main).
   - Write `result.c_code` to `temp_dir/qb64pe.c`.
   - Build runtime: `cargo build -p qb64fresh-runtime --release --features graphics-sdl2`.
   - Get SDL2 libs via `pkg-config --libs sdl2`; on failure, skip with message.
   - Compile qb64pe.c with gcc: `-I runtime/include`, `-L target/release`, `-lqb64fresh_rt`, SDL2 libs, `-lm -lpthread -ldl`.
   - Write `test.bas` with `PRINT "Hello"`.
   - Run `./qb64pe_bootstrapped -x test.bas -o test` from QB64pe root; if root missing, skip.
   - Assert output executable exists, run it, assert stdout contains "Hello".

## Notes

- Test remains `#[ignore = "Full execution test requires runtime library build, gcc, and SDL2"]`. Run with `cargo test --test bootstrap_tests qb64pe_can_compile_hello_world -- --ignored`.
- Running with `--ignored` can be slow (QB64pe compile is heavy); test compiles successfully.
