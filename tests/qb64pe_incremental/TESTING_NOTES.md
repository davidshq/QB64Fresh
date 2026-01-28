# QB64pe Incremental Testing - Notes and Findings

## Test Results

### Phase 1: Core Infrastructure ✅
- **File:** `01_core_infrastructure.bas`
- **Status:** PASSES
- **Time:** ~0.15s
- **Includes:** version.bas, settings.bas, constants.bas
- **Notes:** Simple global includes work perfectly

### Phase 2: Utilities

#### 02_utilities_hash.bas ✅
- **Status:** PASSES (after adding hash.bi header)
- **Time:** ~0.2s
- **Includes:** hash.bi, hash.bas
- **Key Finding:** Must include `.bi` header files before `.bas` implementation files
- **Dependencies:** None (self-contained)

#### 02_utilities_const_eval.bas
- **Status:** PARTIAL (has some errors, but dependencies resolved)
- **Time:** ~0.1s
- **Includes:** elements.bas, const_eval.bi, const_eval.bas
- **Key Finding:** Requires elements.bas for `pushelement` and `getelements$` functions
- **Remaining Issues:** Some array indexing errors (may be QB64pe-specific features)
- **Dependencies:** elements.bas (for pushelement, getelements$)

#### 02_utilities_type.bas ✅
- **Status:** PASSES
- **Time:** ~0.1s
- **Includes:** type.bi
- **Dependencies:** None (self-contained)
- **Notes:** Type system constants and UDT storage - works independently

### Phase 3: Built-in Functions
- **Status:** BLOCKED (requires main compiler infrastructure)
- **Dependencies:** 
  - `clearid` and `regid` SUBs (defined in qb64pe.bas)
  - `idstruct` TYPE and `ids()` array (defined in qb64pe.bas)
  - `Set_ConstFunctions` (from const_eval.bas)
- **Finding:** Built-in functions module is tightly coupled to main compiler - can't test in isolation
- **Recommendation:** Test as part of Phase 4 (core compiler) instead

### Phase 4: Core Compiler (No IDE)
- **Status:** Not yet tested
- **Expected time:** ~10s
- **Note:** This is the sweet spot for iteration

### Phase 5: Full Compiler
- **Status:** Not yet tested (too slow for regular iteration)
- **Expected time:** ~5+ minutes

## Key Learnings

### Include Order Matters
- `.bi` (header) files must come before `.bas` (implementation) files
- Example: `hash.bi` must be included before `hash.bas`

### Dependencies
When a test fails, check:
1. Missing `.bi` header files
2. Missing TYPE definitions
3. Missing global variable declarations
4. Missing helper functions (like `hash1char`, `hash2char`)

### Testing Strategy
1. Start with simplest test (Phase 1) ✅
2. Add dependencies incrementally
3. Fix errors before moving to next phase
4. Document dependencies as you discover them

## Common Issues

### Type Errors
If you see "type mismatch" or "undefined variable" errors:
- Check if a `.bi` header file is missing
- Check if TYPE definitions are included
- Check if global variable declarations are included

### Missing Functions
If you see "undefined function" errors:
- Check if helper functions are defined elsewhere
- May need to include additional utility files
- Some functions may be defined inline in the main qb64pe.bas

## Next Steps

1. ✅ Test Phase 1 (core infrastructure) - PASSES
2. ✅ Test Phase 2 hash utility - PASSES
3. ⚠️ Test Phase 2 const_eval utility - PARTIAL (has errors but dependencies resolved)
4. ⏳ Test Phase 2 type utility
5. ⏳ Create Phase 4 test that extracts core compiler logic (Phase 3 blocked - needs main compiler)
6. Document all discovered dependencies

## Revised Strategy

**Phase 3 is blocked** - built-in functions require main compiler infrastructure (`clearid`, `regid`, `idstruct`). 

**New approach:**
- Phase 1-2: Test isolated utilities ✅ (working well)
- Phase 4: Extract and test core compiler sections incrementally
- Phase 5: Full compiler validation

The key is to test **sections** of the main compiler file, not just includes.
