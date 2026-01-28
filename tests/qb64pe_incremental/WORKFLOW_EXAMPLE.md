# Practical Workflow Example

## Scenario: You need to fix a bug in QB64pe compilation

### Problem
QB64pe fails to compile with an error in the hash table code. You need to:
1. Find the error quickly
2. Fix it
3. Verify the fix
4. Repeat until it works

### Solution: Use Incremental Testing

#### Step 1: Test the isolated component (0.7s)
```bash
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_hash.bas --emit-c
```

**Result:** Error found in hash.bas line 123
**Time:** 0.7 seconds

#### Step 2: Fix the error
Edit the hash table code, then test again:
```bash
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_hash.bas --emit-c
```

**Result:** Hash utility now compiles
**Time:** 0.7 seconds per iteration

#### Step 3: Test related components
```bash
# Test type utility (0.1s)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_type.bas --emit-c

# Test core infrastructure (0.15s)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/01_core_infrastructure.bas --emit-c
```

**Time:** 0.25 seconds total

#### Step 4: Move to larger tests
Once isolated components work, test Phase 4:
```bash
cargo run --bin qb64fresh -- tests/qb64pe_incremental/04_core_compiler_working.bas --emit-c
```

**Time:** ~2-5 seconds (still much faster than full QB64pe)

### Comparison

**Old Workflow:**
1. Compile full QB64pe: **5+ minutes**
2. Find error
3. Fix
4. Compile again: **5+ minutes**
5. Repeat...

**Total time for 5 iterations:** 25+ minutes

**New Workflow:**
1. Test isolated component: **0.7 seconds**
2. Find error
3. Fix
4. Test again: **0.7 seconds**
5. Repeat...

**Total time for 5 iterations:** 3.5 seconds

**Speedup: 428x faster!** 🚀

## Scenario: You need to test a specific section of qb64pe.bas

### Problem
You want to test the `clearid` SUB to see if it works correctly.

### Solution: Extract and test

#### Step 1: Extract the section
```bash
./scripts/extract-qb64pe-section.sh clearid_sub 14476 14478
```

**Result:** Created `tests/qb64pe_incremental/sections/clearid_sub.bas`

#### Step 2: Create a test file
Create `test_clearid.bas`:
```basic
DEFLNG A-Z
$CONSOLE

' Core includes
$INCLUDE:'../../../QB64pe/source/global/version.bas'
$INCLUDE:'../../../QB64pe/source/global/settings.bas'
$INCLUDE:'../../../QB64pe/source/global/constants.bas'

' Type system
$INCLUDE:'../../../QB64pe/source/utilities/type.bi'

' Extracted sections
$INCLUDE:'sections/idstruct_type.bas'
$INCLUDE:'sections/ids_init.bas'
$INCLUDE:'sections/clearid_sub.bas'

' Test code
clearid
PRINT "clearid test passed"
```

#### Step 3: Test it
```bash
cargo run --bin qb64fresh -- tests/qb64pe_incremental/test_clearid.bas --emit-c
```

**Time:** ~0.2 seconds

## Scenario: You're adding a new feature

### Problem
You're adding support for a new QB64 feature and need to test it incrementally.

### Solution: Build up incrementally

1. **Start with Phase 1** - Verify basic infrastructure works
2. **Add your feature to a test file** - Create `test_new_feature.bas`
3. **Test in isolation** - Fix errors quickly (seconds, not minutes)
4. **Integrate with Phase 4** - Test with compiler infrastructure
5. **Final validation** - Test with full QB64pe (only when ready)

## Tips

1. **Always start small** - Test isolated components first
2. **Fix errors immediately** - Don't move on until current test passes
3. **Use `--emit-c` flag** - Faster than full compilation
4. **Extract sections as needed** - Use the extraction script
5. **Document dependencies** - Note what each test needs

## Common Patterns

### Testing a utility module
```bash
# Create test file with just the utility
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_hash.bas --emit-c
```

### Testing with dependencies
```bash
# Include required utilities first
# Then include your module
# Test incrementally
```

### Extracting and testing a section
```bash
# Extract
./scripts/extract-qb64pe-section.sh my_section 1000 1100

# Include in test file
$INCLUDE:'sections/my_section.bas'

# Test
cargo run --bin qb64fresh -- my_test.bas --emit-c
```
