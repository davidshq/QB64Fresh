# Session 078: Regression Tests for REGRESSION_TEST_COVERAGE §9–13

**Date:** 2026-01-28  
**Focus:** Ensure regression tests exist for items 9–13 in REGRESSION_TEST_COVERAGE.md (lines 36–54).

## Summary

- **Items 10–13:** Already implemented. No code changes needed.
- **Item 9 (Parser edge case tests):** Added one explicit regression test.

## Findings

| Item | Description | Status | Test location |
|------|-------------|--------|----------------|
| 9 | Parser edge case tests (3.x) | **Added** | `bootstrap_tests::regression_tests::parser_edge_case_comparison_vs_array_assignment` |
| 10 | Built-in constant registration (5.1) | Already done | `builtin_constant_registration`, `integration_tests::constants::builtin_chr_str_constants_registered` |
| 11 | Error handler syntax (5.3) | Already done | `error_handler_syntax` |
| 12 | Label uniqueness (6.1) | Already done | `label_uniqueness`, `duplicate_label_emission_regression` |
| 13 | Forward declaration (6.2) | Already done | `forward_declarations` |

## Change Made

**New test:** `parser_edge_case_comparison_vs_array_assignment` in `tests/bootstrap_tests.rs` (regression_tests module).

- **Bug:** `x = arr(1) = 5` and `x = ASC("A") = 65` were parsed as array assignment → "expected (, found Equals" (Session 048).
- **Fix:** `is_array_assignment()` now requires `(` immediately after the identifier.
- **Test:** Parses the above plus `arr(1) = 5`; asserts we get exactly one `ArrayAssignment` and at least two `Let` statements (the two comparisons).

Run with:
```bash
cargo test --test bootstrap_tests parser_edge_case_comparison_vs_array_assignment
```

## Doc Update (when REGRESSION_TEST_COVERAGE.md is available)

Update the “Test Gap Analysis” section so that:

- **§9** — Status: Covered. Test: `parser_edge_case_comparison_vs_array_assignment` (Session 048).
- **§10–§13** — Status: Covered; add the test locations listed in the table above.

## Maintenance

- `REGRESSION_TEST_COVERAGE.md` was not found in the repo at session time (possibly unsaved or on another branch). When the file is present, apply the doc update above.
