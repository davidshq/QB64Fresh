# Incremental Testing Strategy for QB64pe

## Problem

Compiling the full QB64pe (`qb64pe.bas`, 24,757 lines) takes **5+ minutes**, making rapid iteration impossible:
- Find error → Fix → Wait 5+ minutes → Find next error → Repeat
- This workflow is too slow for efficient debugging

## Solution: Incremental Testing

Test **portions** of QB64pe instead of the whole file. This enables:
- **Fast iteration:** 0.1-5 seconds per test instead of 5+ minutes
- **Focused debugging:** Test specific sections that are failing
- **Progressive validation:** Build up from working sections to full compiler

## Quick Start

### Option 1: Use Pre-built Test Files (Fastest)

```bash
# Test core infrastructure (~0.15s)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/01_core_infrastructure.bas --emit-c

# Test utilities (~0.1-0.7s each)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_hash.bas --emit-c
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_type.bas --emit-c

# Test built-in functions (~5s)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/03_builtin_functions.bas --emit-c
```

**Speedup:** 300-3000x faster than full QB64pe!

### Option 2: Extract Specific Sections

When you find an error in the full compile, extract just that section:

```bash
# Extract a TYPE definition (lines 596-642)
./scripts/extract-qb64pe-section.sh idstruct_type 596 642

# Extract a SUB (find end line first)
grep -n "^SUB clearid\|^END SUB" ../QB64pe/source/qb64pe.bas | head -2
./scripts/extract-qb64pe-section.sh clearid_sub 14476 14478

# Test the extracted section
cargo run --bin qb64fresh -- tests/qb64pe_incremental/sections/idstruct_type.bas --emit-c
```

## Workflow: Find → Extract → Fix → Test → Repeat

### Step 1: Run Full Compile (Background)

```bash
# Start full compile in background with progress output
./scripts/test-full-qb64pe-unbuffered.sh > /tmp/qb64pe_full_test.log 2>&1 &

# Or manually with unbuffered output
stdbuf -oL -eL cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c -o /tmp/qb64pe_full_test.c --verbose 2>&1 | tee /tmp/qb64pe_full_test.log
```

**Note:** This takes 5+ minutes, but you can work on other things while it runs.

### Step 2: Identify Errors

When the full compile finishes (or fails), check for errors:

```bash
# Count errors
grep -c "error:" /tmp/qb64pe_full_test.log

# List unique error types
grep "error:" /tmp/qb64pe_full_test.log | sort | uniq

# Find first error location
grep "error:" /tmp/qb64pe_full_test.log | head -5
```

### Step 3: Extract Problematic Section

Find the section causing the error:

```bash
# Find line numbers for a TYPE/SUB/FUNCTION
grep -n "^TYPE idstruct\|^SUB clearid\|^FUNCTION EvaluateFunction" ../QB64pe/source/qb64pe.bas

# Extract the section (adjust end line as needed)
./scripts/extract-qb64pe-section.sh <name> <start_line> <end_line>
```

### Step 4: Test Section in Isolation

```bash
# Test extracted section (fast - seconds, not minutes)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/sections/<name>.bas --emit-c

# If it compiles, check the generated C for issues
cargo run --bin qb64fresh -- tests/qb64pe_incremental/sections/<name>.bas --emit-c -o /tmp/test_section.c
gcc -I runtime/include /tmp/test_section.c -c -o /tmp/test_section.o 2>&1 | head -20
```

### Step 5: Fix and Iterate

1. **Fix the bug** in QB64Fresh source code
2. **Test the section again** (fast iteration)
3. **Repeat** until section compiles cleanly
4. **Move to next error** or test full compile again

## Example: Fixing a Codegen Bug

### Scenario: `d2` undeclared error

```bash
# 1. Full compile shows error at line 9210
grep "d2.*undeclared" /tmp/qb64pe_full_test.log
# Output: /tmp/qb64pe_full_test.c:9210: error: 'd2' undeclared

# 2. Find what section contains line 9210
# (In generated C, map back to BASIC source)
# Error is in field access: id2.specialformat

# 3. Find where id2 is used in QB64pe
grep -n "id2\.specialformat\|\.specialformat" ../QB64pe/source/qb64pe.bas | head -5

# 4. Extract that section (or find the function containing it)
grep -B 20 "id2\.specialformat" ../QB64pe/source/qb64pe.bas | grep "^FUNCTION\|^SUB" | tail -1

# 5. Extract and test
./scripts/extract-qb64pe-section.sh problematic_function <start> <end>
cargo run --bin qb64fresh -- tests/qb64pe_incremental/sections/problematic_function.bas --emit-c

# 6. Fix bug in src/codegen/c_backend/expr.rs
# 7. Test section again (instant feedback)
# 8. Once fixed, test full compile again
```

## Pre-built Test Files

### Phase 1: Core Infrastructure (~0.15s)
- `01_core_infrastructure.bas` - Global includes, constants, settings
- **Use when:** Testing basic $INCLUDE handling

### Phase 2: Utilities (~0.1-0.7s each)
- `02_utilities_hash.bas` - Hash table implementation
- `02_utilities_type.bas` - Type system utilities
- `02_utilities_const_eval.bas` - Constant evaluation
- **Use when:** Testing utility modules individually

### Phase 3: Built-in Functions (~5s)
- `03_builtin_functions.bas` - All built-in SUB/FUNCTION definitions
- **Use when:** Testing function definitions and registration

### Phase 4: Core Compiler (~10s)
- `04_core_compiler.bas` - Main compiler without IDE (3,500 lines vs 24,757)
- **Use when:** Testing compiler logic without IDE overhead
- **Sweet spot:** Large enough to catch most bugs, fast enough for iteration

## Speed Comparison

| Test | Lines | Time | Speedup |
|------|-------|------|---------|
| Full QB64pe | 24,757 | 5+ min | 1x |
| Phase 4 (Core) | 3,500 | ~10s | 30x |
| Phase 3 (Built-ins) | 4,342 | ~5s | 60x |
| Phase 2 (Utilities) | 500-1500 | 0.1-0.7s | 300-3000x |
| Phase 1 (Core) | ~100 | 0.15s | 2000x |
| Extracted Section | 50-200 | 0.1-0.5s | 600-3000x |

## Tips for Fast Iteration

### 1. Start Small
- Always start with the smallest test that reproduces the error
- Extract just the problematic function, not the whole file

### 2. Use `--emit-c` Flag
- Skip C compilation step for faster testing
- Only compile C when you need to verify generated code

### 3. Test Incrementally
- Fix one error at a time
- Test after each fix (don't accumulate fixes)
- Verify fix works before moving to next error

### 4. Extract Dependencies
- If a section needs other code, extract dependencies too
- Use `$INCLUDE` to compose sections
- See `EXTRACTION_GUIDE.md` for details

### 5. Use Background Compiles
- Run full compile in background while working on sections
- Check results when ready, don't wait for it

### 6. Progress Output
- Use `--verbose` flag to see compilation progress
- Use `stdbuf -oL -eL` for real-time output when redirecting
- See `PROGRESS_OUTPUT_GUIDE.md` for details

## When to Test Full QB64pe

Test the full compiler when:
- ✅ All incremental tests pass
- ✅ You've fixed multiple bugs and want to verify
- ✅ You're ready for final validation
- ✅ You have time to wait (5+ minutes)

**Don't** test full QB64pe for:
- ❌ Initial debugging (too slow)
- ❌ Testing a single fix (use extracted section)
- ❌ Rapid iteration (use Phase 1-4 tests)

## Scripts Reference

### Extract Section
```bash
./scripts/extract-qb64pe-section.sh <name> <start_line> <end_line>
```
- Extracts lines from `qb64pe.bas` to `tests/qb64pe_incremental/sections/<name>.bas`
- Includes necessary headers and minimal test code

### Test Full QB64pe
```bash
./scripts/test-full-qb64pe-unbuffered.sh
```
- Runs full compile with progress output
- Uses `stdbuf` for real-time output
- 10-minute timeout

### Test Incremental Files
```bash
./scripts/test-qb64pe-incremental.sh all
./scripts/test-qb64pe-incremental.sh 1  # Phase 1 only
./scripts/test-qb64pe-incremental.sh 2  # Phase 2 only
```

## Related Documentation

- `QUICK_REFERENCE.md` - One-page cheat sheet
- `EXTRACTION_GUIDE.md` - How to extract sections
- `PROGRESS_OUTPUT_GUIDE.md` - Real-time output setup
- `WORKFLOW_EXAMPLE.md` - Practical examples
- `README.md` - Overview of test files

## Summary

**For fast iteration:**
1. Use pre-built Phase 1-4 test files (0.1-10s)
2. Extract specific sections when needed (0.1-0.5s)
3. Fix bugs and test incrementally
4. Only test full QB64pe when ready for final validation

**Result:** 300-3000x faster iteration, same bug-finding power! 🚀
