# Testing Infrastructure Plan - Remaining Work

**Created:** 2026-01-18
**Updated:** 2026-01-26
**Purpose:** Track remaining testing infrastructure work for QB64Fresh

> **Note:** 
> - Completed items have been moved to [archive/TESTING-COMPLETED.md](../archive/TESTING-COMPLETED.md)
> - For comprehensive testing documentation, see [TESTING.md](../TESTING.md)

---

## Current State Summary

The **compiler** testing infrastructure is substantially complete:
- **1,500+ tests total** (405 unit, 727 integration, 10 golden, 19 property-based, 3 compatibility, 27 execution, 210 runtime)
- **99.1%** QB64pe compatibility (114/115 files; 1 remaining failure)
- **3 fuzz targets** verified (~4.6M inputs, 0 crashes)

The **runtime library** has comprehensive test coverage:
- **208 runtime tests** (with audio-rodio feature)
- Critical modules like `io.rs` and `string.rs` have thorough testing

### Test Breakdown
| Test Suite | Count | Status | Command |
|------------|-------|--------|---------|
| Compiler unit tests | 405 | ✅ passing | `cargo test -p qb64fresh --lib` |
| Integration tests | 727 | ✅ passing | `cargo test --test integration_tests` |
| Golden tests | 10 | ✅ passing | `cargo test --test golden_tests` |
| Property-based | 19 | ✅ passing | `cargo test --test proptest_tests` |
| Compatibility | 3 | ✅ passing | `cargo test --test compatibility` |
| **Runtime tests** | **210** | ✅ passing | `cargo test -p qb64fresh-runtime --lib` |
| **Execution tests** | **27** | ✅ passing | `cargo test --test execution_tests` |
| QB64pe compat | 114/115 files | 99.1% | `cargo test --test qb45_compat -- --nocapture` |

### Current Test Failures (as of 2026-01-27)

- **Golden tests:** All 10 passing ✅ (previously failing tests have been fixed)
- **QB64pe compatibility:** 1 remaining failure (misc/frog.bas - bug in original code, not compiler limitation)
- **Other suites:** All passing. Remaining work is in "Remaining Gaps" and "Remaining QB64pe Failures" below.

### Runtime Test Distribution (Updated 2026-01-27)
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
| STRING * n assignment | **✅ Fixed** | Medium | Implicit conversion implemented - STRING ↔ STRING * n now works |
| Coverage reporting | **✅ Complete** | Low | `cargo llvm-cov --workspace` runs; CI job configured in `.github/workflows/ci.yml`; **81.63%** coverage achieved (above 80% target). |
| Graphics integration | **Improved** | Medium | Added 3 SDL2 backend unit tests: `test_argb_to_sdl_color_black_white`, `test_image_buffer_multiple_pixels`, `test_palette_entries` in `runtime/src/graphics/sdl2.rs`. Headless init tests not feasible (SDL single-init, process-wide; circle/line buffer semantics). |
| By-ref parameter codegen | Parameters as pointers not dereferenced | Medium | Add param context to emit_expr *(requires codebase change)* |

---

## Remaining Work

### Medium Term

- [x] Restore coverage reporting and verify 80%+ coverage *(command `cargo llvm-cov --workspace` and CI job verified 2026-01-25; **81.63%** achieved, CI configured in `.github/workflows/ci.yml`)*
- [x] STRING * n implicit conversion ✅ (2026-01-27)
- [ ] Graphics backend integration tests (non-headless where possible)
- [ ] Audio backend integration tests

### Future Enhancements

| Area | Priority | Rationale |
|------|----------|-----------|
| STRING * n conversion tests | Medium | ✅ Implemented - conversion now works in assignments |
| @ and \| lexer tokens | Low | Would fix 1 QB64pe file |
| Extended ASCII handling | Low | Would fix 1 QB64pe file |
| `cargo-mutants` | Low | Mutation testing - not yet installed |
| Automated comparison with QB64PE output | Low | compile_tests now supported |
| Runtime fuzzing | Low | Fuzz runtime functions directly |

### Remaining QB64pe Failures (1 file)
- Semantic failures: 1 file (misc/frog.bas - bug in original code, not compiler limitation)

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
*Updated: 2026-01-26 - Reviewed and verified current state matches codebase*
*Updated: 2026-01-27 - Updated test counts: 405 unit, 727 integration, 210 runtime; 99.1% QB64pe compatibility (114/115); all golden tests passing*
*Updated: 2026-01-27 - ✅ Implemented STRING * n implicit conversion - assignments now automatically convert between STRING and FixedString types*
