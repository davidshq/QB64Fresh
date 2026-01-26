# Testing Infrastructure Plan - Remaining Work

**Created:** 2026-01-18
**Updated:** 2026-01-25
**Purpose:** Track remaining testing infrastructure work for QB64Fresh

> **Note:** 
> - Completed items have been moved to [archive/TESTING-COMPLETED.md](../archive/TESTING-COMPLETED.md)
> - For comprehensive testing documentation, see [TESTING.md](../TESTING.md)

---

## Current State Summary

The **compiler** testing infrastructure is substantially complete:
- **1,390+ tests total** (404 unit, 720 integration, 10 golden, 19 property-based, 3 compatibility, 27 execution, 208 runtime)
- **86.5%** QB64pe compatibility (122/141 files; 10 parser + 9 semantic failures)
- **3 fuzz targets** verified (~4.6M inputs, 0 crashes)

The **runtime library** has comprehensive test coverage:
- **208 runtime tests** (with audio-rodio feature)
- Critical modules like `io.rs` and `string.rs` have thorough testing

### Test Breakdown
| Test Suite | Count | Status | Command |
|------------|-------|--------|---------|
| Compiler unit tests | 404 | ✅ passing | `cargo test -p qb64fresh --lib` |
| Integration tests | 720 | ✅ passing | `cargo test --test integration_tests` |
| Golden tests | 10 | ⚠️ 2 pass, 8 fail (codegen diff) | `cargo test --test golden_tests` |
| Property-based | 19 | ✅ passing | `cargo test --test proptest_tests` |
| Compatibility | 3 | ✅ passing | `cargo test --test compatibility` |
| **Runtime tests** | **208** | ✅ passing | `cargo test -p qb64fresh-runtime --lib` |
| **Execution tests** | **27** | ✅ passing | `cargo test --test execution_tests` |
| QB64pe compat | 122/141 files | 86.5% | `cargo test --test qb45_compat -- --nocapture` |

### Current Test Failures (as of 2026-01-25)

- **Golden tests:** 8 of 10 currently fail (codegen output diffs, e.g. STRIG dispatch ordering); 2 pass. Golden baselines may need refresh after recent codegen changes.
- **Other suites:** All passing. Remaining work is in "Remaining Gaps" and "Remaining QB64pe Failures" below.

### Runtime Test Distribution (Updated 2026-01-25)
| Module | Lines | Tests | Status |
|--------|-------|-------|--------|
| string.rs | 1700+ | **90+** | ✅ Comprehensive (null, refcount, edge cases) |
| math.rs | 392 | **23** | ✅ Comprehensive (edge cases, infinity, NaN, boundaries) |
| io.rs | 2000+ | **60+** | ✅ Comprehensive (print, file, network, shell) |
| graphics_ffi.rs | 1590 | 3 | FFI wrappers minimally tested |
| graphics/sdl2.rs | 2135 | 6 | Basic init/mode tests only |
| graphics/mock.rs | 466 | **10** | ✅ STEP operations, state management |
| graphics/font.rs | 593 | 5 | Font loading tests |
| audio/mock.rs | 428 | **8** | ✅ Lifecycle, playback, errors, multi-sound |
| audio/rodio_backend.rs | 500+ | 6 | ✅ Real audio backend tested |
| audio_ffi.rs | 460 | 1 | FFI wrapper minimal |
| dialogs.rs | 350 | 1 | Dialog stubs minimal |
| joystick.rs | 310 | 2 | Joystick stubs minimal |
| lib.rs | 95 | 1 | Init/shutdown only |

---

## Remaining Gaps

### Medium Priority

| Area | Current State | Risk Level | Recommendation |
|------|---------------|------------|----------------|
| STRING * n assignment | Type mismatch error | Medium | Add implicit padding/conversion *(requires codebase change)* |
| Coverage reporting | **Verified** | Low | `cargo llvm-cov --workspace` runs; already in CI (`.github/workflows/ci.yml`). |
| Graphics integration | **Improved** | Medium | Added 3 SDL2 backend unit tests: `test_argb_to_sdl_color_black_white`, `test_image_buffer_multiple_pixels`, `test_palette_entries` in `runtime/src/graphics/sdl2.rs`. Headless init tests not feasible (SDL single-init, process-wide; circle/line buffer semantics). |
| By-ref parameter codegen | Parameters as pointers not dereferenced | Medium | Add param context to emit_expr *(requires codebase change)* |

---

## Remaining Work

### Medium Term

- [ ] Restore coverage reporting and verify 80%+ coverage *(command `cargo llvm-cov --workspace` and CI job verified 2026-01-25)*
- [ ] STRING * n implicit conversion
- [ ] Graphics backend integration tests (non-headless where possible)
- [ ] Audio backend integration tests

### Future Enhancements

| Area | Priority | Rationale |
|------|----------|-----------|
| STRING * n conversion tests | Medium | Would fix remaining QB64pe failures |
| @ and \| lexer tokens | Low | Would fix 1 QB64pe file |
| Extended ASCII handling | Low | Would fix 1 QB64pe file |
| `cargo-mutants` | Low | Mutation testing - not yet installed |
| Automated comparison with QB64PE output | Low | compile_tests now supported |
| Runtime fuzzing | Low | Fuzz runtime functions directly |

### Remaining QB64pe Failures (19 files)
- Parser failures: 10 files (misc: 1; pete: 2; thebob: 7)
- Semantic failures: 9 files (misc: 9)

---

## References

- **Testing Guide:** [TESTING.md](../TESTING.md) - Comprehensive testing documentation
- **Completed Items:** [archive/TESTING-COMPLETED.md](../archive/TESTING-COMPLETED.md)
- QB64PE Testing Framework: `QB64pe/docs/testing.md`
- QB64PE Test Cases: `QB64pe/tests/compile_tests/`
- QB45 Compatibility Report: `docs/QB45_COMPATIBILITY_REPORT.md`
- Rust Testing Book: https://doc.rust-lang.org/book/ch11-00-testing.html
- Criterion Documentation: https://bheisler.github.io/criterion.rs/book/

---

*Document created as part of QB64Fresh codebase review - 2026-01-18*
*Updated: 2026-01-20 - Moved completed items to TESTING-COMPLETED.md*
*Updated: 2026-01-21 - Moved Session 041/042 completed items to TESTING-COMPLETED.md*
*Updated: 2026-01-22 - Refreshed test counts, documented current failures*
*Updated: 2026-01-25 - Refreshed test counts, QB64pe 122/141 (86.5%); completed items moved to TESTING-COMPLETED*
*Updated: 2026-01-25 - Runtime 195→208; golden 2 pass/8 fail (codegen diff); io/string line counts*
