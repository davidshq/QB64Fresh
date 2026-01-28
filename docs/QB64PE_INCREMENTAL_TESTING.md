# QB64pe Incremental Testing Strategy

## Problem

QB64pe is **~24,757 lines** of BASIC code, which makes compilation very slow (~5+ minutes). This prevents rapid iteration cycles (compile → find error → fix → repeat).

## Solution: Progressive Module Testing

Instead of compiling the entire QB64pe at once, we can test progressively larger subsets:

### Module Breakdown

| Module | Lines | Purpose | Test Priority |
|--------|-------|---------|---------------|
| `qb64pe.bas` (main) | 24,757 | Full compiler | Final validation |
| `ide/ide_methods.bas` | 21,305 | IDE component | **Optional** (commented out) |
| `subs_functions/subs_functions.bas` | 4,342 | Built-in functions | High |
| `utilities/const_eval.bas` | 1,448 | Constant evaluation | High |
| `utilities/type.bas` | 1,384 | Type system | High |
| `utilities/elements.bas` | 1,003 | Element parsing | Medium |
| `utilities/hash.bas` | 516 | Hash table | Medium |
| `global/*.bas` | ~200 | Constants/settings | Low (simple) |

**Key Insight:** The IDE component (`ide_methods.bas`) is **21K lines** and is **commented out** in the main file. The core compiler without IDE is only **~3,500 lines**.

## Testing Strategy

### Phase 1: Core Infrastructure (Fast - ~0.15s) ✅
Test the minimal includes needed for basic compilation:
- `global/version.bas`
- `global/settings.bas`
- `global/constants.bas`
- Basic utilities

**Test file:** `tests/qb64pe_incremental/01_core_infrastructure.bas`
**Status:** PASSES

### Phase 2: Utilities (Fast - ~0.7s) ✅
Add utility modules one at a time:
- `utilities/hash.bi` + `utilities/hash.bas` ✅ (0.7s) - **PASSES**
- `utilities/const_eval.bi` + `utilities/const_eval.bas` ⚠️ (0.1s) - **PARTIAL**
  - Requires `elements.bas` for `pushelement` and `getelements$`
  - Has some array indexing errors (may be QB64pe-specific features)
- `utilities/type.bi` - ⏳ Not yet tested
- `utilities/elements.bas` - ⏳ Not yet tested

**Test files:** `tests/qb64pe_incremental/02_utilities_*.bas`
**Status:** Hash utility passes; const_eval partial; others not yet tested

**Key Findings:**
- `.bi` header files must be included before `.bas` implementation files
- Some utilities have dependencies (const_eval needs elements.bas)

### Phase 3: Built-in Functions (BLOCKED)
Add the built-in function definitions:
- `subs_functions/subs_functions.bas`

**Test file:** `tests/qb64pe_incremental/03_builtin_functions.bas`
**Status:** BLOCKED - Requires main compiler infrastructure

**Finding:** Built-in functions module is tightly coupled to main compiler:
- Needs `clearid` and `regid` SUBs (defined in qb64pe.bas)
- Needs `idstruct` TYPE and `ids()` array (defined in qb64pe.bas)
- Cannot test in isolation

**Recommendation:** Test as part of Phase 4 (core compiler) instead

### Phase 4: Core Compiler (Medium - ~10s)
Main compiler logic without IDE:
- All includes except `ide/*`

**Test file:** `tests/qb64pe_incremental/04_core_compiler.bas`
**Status:** Not yet tested
**Note:** This is the sweet spot for iteration - core compiler without IDE overhead

### Phase 5: Full Compiler (Slow - ~5min)
Complete QB64pe including IDE:
- Everything

**Test file:** `tests/qb64pe_incremental/05_full_compiler.bas` (symlink to original)
**Status:** Not yet tested (too slow for regular iteration)

## Usage

### Quick Iteration (Phase 1-3)
```bash
# Test core infrastructure (0.15s)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/01_core_infrastructure.bas --emit-c

# Test hash utility (0.7s)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_hash.bas --emit-c

# Test built-in functions (when ready)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/03_builtin_functions.bas --emit-c
```

### Medium Testing (Phase 4)
```bash
# Test core compiler (no IDE)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/04_core_compiler.bas --emit-c
```

### Full Validation (Phase 5)
```bash
# Test complete QB64pe (run in background)
bash -c 'ulimit -v 16777216 && cargo run --bin qb64fresh -- tests/qb64pe_incremental/05_full_compiler.bas --emit-c -o /tmp/qb64pe_full.c' &
```

### Using the Helper Script
```bash
# Run all phases 1-4 in order
./scripts/test-qb64pe-incremental.sh all

# Run a specific phase
./scripts/test-qb64pe-incremental.sh 1
./scripts/test-qb64pe-incremental.sh 2
```

## Creating Test Files

Test files are **minimal wrappers** that include the modules we want to test:

```basic
' Test: Core Infrastructure
' Purpose: Verify basic includes and global definitions compile

$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Minimal code to exercise the includes
DIM test AS INTEGER
test = 1
```

**Important:** Include `.bi` header files before `.bas` implementation files.

## Benefits

1. **Fast iteration** - Phase 1-3 tests compile in seconds
2. **Isolated errors** - Know exactly which module has issues
3. **Progressive validation** - Each phase builds on the previous
4. **Parallel development** - Fix errors in one module while testing another

## Workflow

1. **Start with Phase 1** - Get core infrastructure working ✅
2. **Test Phase 2 utilities** - Test isolated utilities (hash works, const_eval partial) ✅
3. **Skip Phase 3** - Built-in functions blocked (needs main compiler) ❌
4. **Build Phase 4** - Extract core compiler sections incrementally ⏳
5. **Final validation** - Run Phase 5 (full compiler) when ready

## Current Test Results

| Phase | Status | Time | Notes |
|-------|--------|------|-------|
| Phase 1: Core Infrastructure | ✅ PASSES | 0.15s | Simple, self-contained |
| Phase 2: Hash Utility | ✅ PASSES | 0.7s | Requires hash.bi header |
| Phase 2: Const Eval Utility | ⚠️ PARTIAL | 0.1s | Dependencies resolved, some errors remain |
| Phase 3: Built-in Functions | ❌ BLOCKED | N/A | Needs main compiler infrastructure |
| Phase 4: Core Compiler | ⏳ TODO | ~10s | Need to extract sections from qb64pe.bas |
| Phase 5: Full Compiler | ⏳ TODO | ~5min | Final validation only |

## Key Learnings

### Include Order Matters
- `.bi` (header) files must come before `.bas` (implementation) files
- Example: `hash.bi` must be included before `hash.bas`

### Dependencies
When a test fails, check:
1. Missing `.bi` header files
2. Missing TYPE definitions
3. Missing global variable declarations
4. Missing helper functions

## Notes

- Test files use **relative paths** from `QB64pe/source/` (use `../../../QB64pe/source/` from test file location)
- IDE component is **optional** - core compiler works without it
- Each test file should have **minimal executable code** to exercise includes
- Use `--emit-c` flag to see generated C (faster than full compilation)
- See `tests/qb64pe_incremental/TESTING_NOTES.md` for detailed test results and findings
