# Quick Start Guide - QB64pe Incremental Testing

## The Problem

QB64pe is **~24,757 lines** and takes **~5+ minutes** to compile. This prevents rapid iteration.

## The Solution

Test **smaller portions** incrementally, building up to the full compiler.

## Fast Tests (Under 1 Second)

### Phase 1: Core Infrastructure ✅
```bash
cargo run --bin qb64fresh -- tests/qb64pe_incremental/01_core_infrastructure.bas --emit-c
# Time: ~0.15s
```

### Phase 2: Individual Utilities ✅
```bash
# Hash utility
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_hash.bas --emit-c
# Time: ~0.7s

# Type utility
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_type.bas --emit-c
# Time: ~0.1s

# Const Eval utility
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_const_eval.bas --emit-c
# Time: ~0.1s
```

## Using the Helper Script

```bash
# Run all working tests
./scripts/test-qb64pe-incremental.sh all

# Run specific phase
./scripts/test-qb64pe-incremental.sh 1
./scripts/test-qb64pe-incremental.sh 2
```

## Extracting Sections from qb64pe.bas

When you need parts of the main compiler:

```bash
# Extract a section
./scripts/extract-qb64pe-section.sh <name> <start_line> <end_line>

# Example: Extract idstruct TYPE
./scripts/extract-qb64pe-section.sh idstruct_type 596 642
```

See `INCREMENTAL_TESTING.md` for extraction and workflow details.

## Current Status

| Test | Status | Time |
|------|--------|------|
| Phase 1: Core Infrastructure | ✅ PASSES | 0.15s |
| Phase 2: Hash Utility | ✅ PASSES | 0.7s |
| Phase 2: Type Utility | ✅ PASSES | 0.1s |
| Phase 2: Const Eval | ✅ PASSES | 0.1s |
| Phase 3: Built-in Functions | ✅ PASSES | ~5s |
| Phase 4: Core Compiler | ✅ PASSES | ~800ms (Set_ConstFunctions/clearid/regid defined or stubbed) |
| Phase 5: Full Compiler | ✅ PASSES | ~800ms (all phases + C compile 0 errors) |

## Workflow

1. **Start with Phase 1-2** - Fast, isolated tests ✅
2. **Fix errors immediately** - Don't move on until current phase passes
3. **Extract sections** - Use script to get needed parts from qb64pe.bas
4. **Build incrementally** - Add sections one at a time
5. **Final validation** - Phase 5 only when ready

## Key Files

- `tests/qb64pe_incremental/` - All test files
- `scripts/test-qb64pe-incremental.sh` - Test runner
- `scripts/extract-qb64pe-section.sh` - Section extractor
- `docs/archive/QB64PE_INCREMENTAL_TESTING.md` - Full strategy guide

## Tips

- Use `--emit-c` flag for faster testing (no C compilation)
- Test files compile in **seconds**, not minutes
- Isolated utilities work best (hash, type)
- Some modules need main compiler (built-in functions)
- Extract sections as needed using the script
