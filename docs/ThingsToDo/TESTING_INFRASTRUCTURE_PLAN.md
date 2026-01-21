# Testing Infrastructure Plan - Remaining Work

**Created:** 2026-01-18
**Updated:** 2026-01-20
**Purpose:** Track remaining testing infrastructure work for QB64Fresh

> **Note:** Completed items have been moved to [TESTING-COMPLETED.md](TESTING-COMPLETED.md)

---

## Current State Summary

The testing infrastructure is **substantially complete**:
- **820+ tests** total (217 unit, 656 integration, 10 golden, 16 compatibility, 19 property-based, 30 benchmarks, 44 runtime)
- **99.1%** QB64PE compatibility (114/115 files, excluding open_gl)
- **3 fuzz targets** verified (~4.6M inputs, 0 crashes)

---

## Remaining Gaps

| Area | Current State | Risk Level | Recommendation |
|------|---------------|------------|----------------|
| STRING * n assignment | Type mismatch error | Medium | Add implicit padding/conversion |
| Coverage reporting | Ready to run | Low | Run `cargo llvm-cov --workspace` |
| ~~File I/O runtime~~ | ~~Stubs only~~ | ~~Low~~ | ✅ **Done** - 22 codegen tests added |
| ~~Graphics runtime~~ | ~~Stubs only~~ | ~~Low~~ | ✅ **Done** - 43 codegen tests added |
| ~~Sound runtime~~ | ~~Stubs only~~ | ~~Low~~ | ✅ **Done** - 29 codegen tests added |

---

## Remaining Work

### Medium Term
- [ ] Restore coverage reporting and verify 80%+ coverage
- [ ] STRING * n implicit conversion

### Future Enhancements
| Area | Priority | Rationale |
|------|----------|-----------|
| STRING * n conversion tests | Medium | Would fix 6+ QB64pe failures |
| @ and \| lexer tokens | Low | Would fix 1 QB64pe file |
| Extended ASCII handling | Low | Would fix 1 QB64pe file |
| `cargo-mutants` | Low | Mutation testing - not yet installed |
| Automated comparison with QB64PE output | Low | compile_tests now supported |

### Remaining QB64pe Failure (1 file)
- `frog.bas`: Bug in original code (`SCORE > HISCORE` where HISCORE is a UDT array)

---

## Quick Reference: Running Tests

```bash
# Run all tests (compiler + runtime)
cargo test

# Run specific test suites
cargo test -p qb64fresh --test integration_tests    # 539 integration tests
cargo test -p qb64fresh --test golden_tests         # 10 golden tests
cargo test -p qb64fresh --test compatibility        # 16 local fixture tests
cargo test -p qb64fresh --test proptest_tests       # 19 property-based tests
cargo test -p qb64fresh --test qb45_compat          # QB64pe compatibility (141 files)

# Run unit tests only
cargo test -p qb64fresh --lib                       # 217 unit tests

# QB64pe compatibility tests (with output)
cargo test --test qb45_compat -- --nocapture
cargo test --test qb45_compat all_testcases_summary -- --nocapture  # Full summary
cargo test --test qb45_compat diagnose_failures -- --nocapture      # Debug failures
VERBOSE=1 cargo test --test qb45_compat -- --nocapture              # Show passing files

# Run benchmarks
cargo bench                            # Full benchmark suite
cargo bench -- "lexer"                 # Specific benchmark group

# Update golden files (after intentional changes)
UPDATE_GOLDEN=1 cargo test --test golden_tests

# Run with verbose output
cargo test -- --nocapture

# Coverage reporting
cargo llvm-cov --workspace             # Console summary
cargo llvm-cov --workspace --html      # HTML report in target/llvm-cov/html
```

---

## References

- **Completed Items:** [TESTING-COMPLETED.md](TESTING-COMPLETED.md)
- QB64PE Testing Framework: `QB64pe/docs/testing.md`
- QB64PE Test Cases: `QB64pe/tests/compile_tests/`
- QB45 Compatibility Report: `docs/QB45_COMPATIBILITY_REPORT.md`
- Rust Testing Book: https://doc.rust-lang.org/book/ch11-00-testing.html
- Criterion Documentation: https://bheisler.github.io/criterion.rs/book/

---

*Document created as part of QB64Fresh codebase review - 2026-01-18*
*Updated: 2026-01-20 - Moved completed items to TESTING-COMPLETED.md*
