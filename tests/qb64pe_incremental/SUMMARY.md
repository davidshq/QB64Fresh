# QB64pe Incremental Testing - Summary

## Current Status

### ✅ Working (Fast Iteration)
- **Phase 1: Core Infrastructure** - 0.15s ✅
  - Global includes (version, settings, constants)
  - Simple, self-contained
  
- **Phase 2: Hash Utility** - 0.7s ✅
  - Hash table implementation
  - Requires hash.bi header before hash.bas

### ⚠️ Partial
- **Phase 2: Const Eval Utility** - 0.1s ⚠️
  - Requires elements.bas for `pushelement` and `getelements$`
  - Has some array indexing errors (may be QB64pe-specific features)
  - Dependencies resolved, but some compilation errors remain

### ❌ Blocked
- **Phase 3: Built-in Functions** - BLOCKED
  - Requires `clearid`, `regid` SUBs from main qb64pe.bas
  - Requires `idstruct` TYPE and `ids()` array from main qb64pe.bas
  - **Cannot test in isolation** - too tightly coupled to main compiler

## Key Learnings

### What Works
1. **Isolated utilities** (hash.bas) can be tested independently
2. **Global includes** are simple and self-contained
3. **Fast iteration** is possible for Phase 1-2 (under 1 second)

### What Doesn't Work
1. **Built-in functions** module is tightly coupled to main compiler
2. **Main compiler sections** need to be extracted, not just included
3. **Some utilities** have complex dependencies (const_eval needs elements.bas)

## Revised Strategy

### Current Approach (Working)
```
Phase 1: Core Infrastructure (0.15s) ✅
  └─> Global includes only
  
Phase 2: Individual Utilities (0.7s each) ✅
  └─> hash.bas (works)
  └─> const_eval.bas (partial)
  └─> type.bas (not tested)
```

### New Approach Needed
```
Phase 4: Core Compiler Sections
  └─> Extract TYPE definitions from qb64pe.bas
  └─> Extract clearid/regid SUBs
  └─> Test built-in functions with minimal compiler infrastructure
  └─> Build up incrementally
```

## Recommendations

1. **For rapid iteration:** Use Phase 1-2 tests (working well)
2. **For compiler development:** Need to extract sections from qb64pe.bas
3. **For full validation:** Phase 5 (full compiler) only when ready

## Files Created

### Test Files
- `01_core_infrastructure.bas` - ✅ Working (0.15s)
- `02_utilities_hash.bas` - ✅ Working (0.7s)
- `02_utilities_const_eval.bas` - ⚠️ Partial (0.1s)
- `02_utilities_type.bas` - ✅ Working (0.1s)
- `03_builtin_functions.bas` - ❌ Blocked (needs main compiler)
- `04_core_compiler.bas` - ⏳ Placeholder
- `04_core_compiler_minimal.bas` - ⏳ New approach (extract sections)
- `05_full_compiler.bas` - ⏳ Symlink to original

### Extracted Sections
- `sections/idstruct_type.bas` - ✅ Extracted (lines 596-642)

### Scripts
- `scripts/test-qb64pe-incremental.sh` - Test runner
- `scripts/extract-qb64pe-section.sh` - Section extractor ✅

### Documentation
- `README.md` - Test file documentation
- `TESTING_NOTES.md` - Findings and results
- `EXTRACTION_GUIDE.md` - How to extract sections
- `SUMMARY.md` - This file

## Next Actions

1. ✅ Test `02_utilities_type.bas` - PASSES (isolated)
2. ✅ Create extraction script - DONE (`extract-qb64pe-section.sh`)
3. ✅ Extract idstruct TYPE - DONE (`sections/idstruct_type.bas`)
4. ⏳ Extract clearid SUB (line 14476+)
5. ⏳ Extract regid SUB (line 21849+)
6. ⏳ Build Phase 4 test with extracted sections
7. ⏳ Test built-in functions with minimal compiler infrastructure

## Tools Available

### Test Runner
```bash
./scripts/test-qb64pe-incremental.sh [1|2|3|4|all]
```

### Section Extractor
```bash
./scripts/extract-qb64pe-section.sh <name> <start_line> <end_line>
# Example:
./scripts/extract-qb64pe-section.sh idstruct_type 596 642
```

See `EXTRACTION_GUIDE.md` for detailed usage.
