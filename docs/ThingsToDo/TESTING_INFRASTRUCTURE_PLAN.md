# Testing Infrastructure Plan - Remaining Work

**Created:** 2026-01-18
**Updated:** 2026-01-22
**Purpose:** Track remaining testing infrastructure work for QB64Fresh

> **Note:** 
> - Completed items have been moved to [archive/TESTING-COMPLETED.md](../archive/TESTING-COMPLETED.md)
> - For comprehensive testing documentation, see [TESTING.md](../../TESTING.md)

---

## Current State Summary

The **compiler** testing infrastructure is substantially complete:
- **1,146+ compiler tests** (384 unit, 720 integration, 10 golden, 19 property-based, 3 compatibility)
- **97.9%** QB64PE compatibility (138/141 files)
- **3 fuzz targets** verified (~4.6M inputs, 0 crashes)

The **runtime library** now has comprehensive test coverage:
- **194 runtime tests** total (with audio-rodio feature)
- Critical modules like `io.rs` and `string.rs` have thorough testing

### Test Breakdown
| Test Suite | Count | Status | Command |
|------------|-------|--------|---------|
| Compiler unit tests | 384 | ⚠️ 1 failing | `cargo test -p qb64fresh --lib` |
| Integration tests | 720 | ⚠️ 3 failing | `cargo test --test integration_tests` |
| Golden tests | 10 | ❌ 8 failing | `cargo test --test golden_tests` |
| Property-based | 19 | ✅ passing | `cargo test --test proptest_tests` |
| Compatibility | 3 | ✅ passing | `cargo test --test compatibility` |
| **Runtime tests** | **194** | ✅ passing | `cargo test -p qb64fresh-runtime --lib` |
| **Execution tests** | **27** | ✅ passing | `cargo test --test execution_tests` |

### Current Test Failures (as of 2026-01-22)
| Test | Location | Issue |
|------|----------|-------|
| `test_invalid_binary_op` | `semantic::checker::tests` | Assertion expecting errors fails |
| `exit_statement` | integration_tests | EXIT codegen issue |
| `statuscode_function` | integration_tests | _STATUSCODE function |
| `mapunicode_statement` | integration_tests | _MAPUNICODE statement |
| Golden tests (8) | golden_tests | Codegen output changed, need UPDATE_GOLDEN=1 |

### Runtime Test Distribution (Updated 2026-01-22)
| Module | Lines | Tests | Status |
|--------|-------|-------|--------|
| string.rs | 758 | **90+** | ✅ Comprehensive (null, refcount, edge cases) |
| math.rs | 392 | **23** | ✅ Comprehensive (edge cases, infinity, NaN, boundaries) |
| io.rs | 850+ | **60+** | ✅ Comprehensive (print, file, network, shell) |
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

### High Priority (Blocking Tests)

| Area | File | Issue | Recommendation |
|------|------|-------|----------------|
| Golden test drift | tests/golden_tests.rs | 8/10 golden tests failing due to codegen changes | Review changes, run `UPDATE_GOLDEN=1 cargo test --test golden_tests` |
| `test_invalid_binary_op` | semantic/checker/mod.rs:538 | Test expects errors but none generated | Update test or fix semantic checking |
| EXIT statement | integration_tests | Codegen incomplete for EXIT | Implement EXIT statement codegen |
| _STATUSCODE function | integration_tests | Function not fully implemented | Complete _STATUSCODE implementation |
| _MAPUNICODE statement | integration_tests | Statement not fully implemented | Complete _MAPUNICODE implementation |

### Medium Priority

| Area | Current State | Risk Level | Recommendation |
|------|---------------|------------|----------------|
| STRING * n assignment | Type mismatch error | Medium | Add implicit padding/conversion |
| Coverage reporting | Ready to run | Low | Run `cargo llvm-cov --workspace` |
| Graphics integration | Mock tests only | Medium | Test actual SDL2 backend where possible |
| By-ref parameter codegen | Parameters as pointers not dereferenced | Medium | Add param context to emit_expr |

### Resolved Issues (from previous versions)
- ✅ **ControlChr** - Now fully implemented (parser, semantic, codegen)
- ✅ **rodio API mismatch** - Updated to rodio 0.21, API compatible

---

## Remaining Work

### Immediate (Fix Failing Tests)

- [ ] Review golden test failures and update if intentional changes
- [ ] Fix `test_invalid_binary_op` semantic checker test
- [ ] Implement EXIT statement codegen
- [ ] Implement _STATUSCODE function
- [ ] Implement _MAPUNICODE statement

### Medium Term

- [ ] Restore coverage reporting and verify 80%+ coverage
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

### Remaining QB64pe Failures (3 files)
- Parser failures: 2 files
- Semantic failures: 1 file

---

## References

- **Testing Guide:** [TESTING.md](../../TESTING.md) - Comprehensive testing documentation
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
*Updated: 2026-01-22 - Refreshed test counts, documented current failures, marked ControlChr and rodio as resolved*
