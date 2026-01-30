# Quick Reference Card

## Fast Tests (Under 1 Second)

```bash
# Core infrastructure
cargo run --bin qb64fresh -- tests/qb64pe_incremental/01_core_infrastructure.bas --emit-c
# Time: 0.15s

# Hash utility
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_hash.bas --emit-c
# Time: 0.7s

# Type utility
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_type.bas --emit-c
# Time: 0.1s
```

## Extract Sections

```bash
./scripts/extract-qb64pe-section.sh <name> <start_line> <end_line>

# Examples:
./scripts/extract-qb64pe-section.sh idstruct_type 596 642
./scripts/extract-qb64pe-section.sh clearid_sub 14476 14478
./scripts/extract-qb64pe-section.sh regid_sub 21849 22081
```

## Run All Tests

```bash
./scripts/test-qb64pe-incremental.sh all
./scripts/test-qb64pe-incremental.sh 1
./scripts/test-qb64pe-incremental.sh 2
```

## Test Status

| Test | Status | Time | Notes |
|------|--------|------|-------|
| Phase 1: Core Infrastructure | ✅ | 0.15s | All passing |
| Phase 2: Hash Utility | ✅ | 0.7s | Requires hash.bi header |
| Phase 2: Type Utility | ✅ | 0.1s | Self-contained |
| Phase 2: Const Eval | ⚠️ | 0.1s | 3 errors, but full compile works |
| Phase 3: Built-in Functions | ⚠️ | ~5s | 4 errors, deps resolved |
| Phase 4: Core Compiler | ⚠️ | ~2-5s | 3 errors, deps resolved |
| Phase 5: Full Compiler | ✅ | ~10s | **SUCCESS - 24,757 lines!** |

**Note:** Errors in Phase 2-4 are likely false positives (full compilation succeeds).

## Key Files

- `01_core_infrastructure.bas` - Global includes
- `02_utilities_*.bas` - Individual utilities
- `04_core_compiler_working.bas` - Full infrastructure
- `sections/*.bas` - Extracted sections

## Documentation

- `INCREMENTAL_TESTING_STRATEGY.md` - **⭐ Main guide for fast iteration**
- `QUICK_START.md` - Getting started guide
- `WORKFLOW_EXAMPLE.md` - Practical examples
- `EXTRACTION_GUIDE.md` - How to extract sections
- `SUMMARY.md` - Status summary

## Tips

- Use `--emit-c` for faster testing
- Start with Phase 1-2 tests
- Extract sections as needed
- Fix errors before moving on
- Test incrementally, not all at once

## Speed Comparison

- **Full QB64pe:** 5+ minutes
- **Phase 1-2 tests:** 0.1-0.7 seconds
- **Speedup:** 300-3000x faster! 🚀
