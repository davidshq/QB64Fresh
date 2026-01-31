# QB64pe Incremental Testing Strategy

**Last Updated:** 2026-01-31  
**Status:** Full compiler compilation successful through all QB64Fresh phases ✅ (C compilation: 0 errors)

**Mission accomplished:** The incremental testing infrastructure is complete. You can test isolated components in **0.1–0.7 seconds** (vs 5+ minutes for full QB64pe), extract sections from qb64pe.bas as needed, and iterate rapidly—**300–3000× faster** for component testing.

## Problem

QB64pe is **~59,000 lines** of BASIC code (39 files with all includes), which makes compilation very slow (~5+ minutes). This prevents rapid iteration cycles (compile → find error → fix → repeat).

**Update (2026-01-28):** QB64Fresh can now compile the full QB64pe through all phases (preprocessing, lexing, parsing, semantic analysis, code generation). The generated C code compiles with **0 errors** (const qualifier fixes in session 071 resolved the remaining C compilation errors). See [BOOTSTRAP_PLAN_FULL.md](BOOTSTRAP_PLAN_FULL.md) §7.1 for status.

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
- `utilities/const_eval.bi` + `utilities/const_eval.bas` ✅ (0.1s) - **PASSES**
  - Requires `elements.bas` for `pushelement` and `getelements$`
  - Fixed: codegen external runtime now uses `const QbString*` for read-only string params to match qb64fresh_rt.h
- `utilities/type.bi` + `utilities/type.bas` ✅ (0.1s) - **PASSES**
- `utilities/elements.bas` ✅ (~5s) - **PASSES** (QB64Fresh all phases; C compile has const qualifier mismatches in inline runtime stubs, same category as elsewhere)

**Test files:** `tests/qb64pe_incremental/02_utilities_*.bas`
**Status:** Hash, type, elements, and const_eval utilities pass through QB64Fresh

**Key Findings:**
- `.bi` header files must be included before `.bas` implementation files
- Some utilities have dependencies (const_eval needs elements.bas)

### Phase 3: Built-in Functions ✅
Add the built-in function definitions:
- `subs_functions/subs_functions.bas`

**Test file:** `tests/qb64pe_incremental/03_builtin_functions.bas`
**Status:** ✅ PASSES - Minimal compiler infrastructure provided via stubs

**Infrastructure:** Phase 3 runs in isolation using:
- Extracted sections: `idstruct_type.bas`, `ids_init.bas`, `clearid_sub.bas`, `regid_sub.bas` (from qb64pe.bas)
- Stub file `sections/phase3_stubs.bas`: `validname`, `tryRemoveSymbol$`, `AddQuotes$`, and shared `subfunc`, `subfuncn` (symbols otherwise defined in qb64pe.bas or utilities not included here)
- Include order: core/utility includes → idstruct/ids → phase3_stubs → clearid/regid → subs_functions.bas

### Phase 4: Core Compiler (Fast - ~800ms) ✅
Main compiler logic without IDE:
- All includes except `ide/*`

**Test file:** `tests/qb64pe_incremental/04_core_compiler.bas`  
**Status:** ✅ **COMPILES SUCCESSFULLY** (tested as part of full compiler)

**Note:** Since the full compiler (Phase 5) now compiles successfully, Phase 4 is validated. The core compiler without IDE is the sweet spot for iteration, but full compiler compilation is now fast enough (~800ms) for regular testing.

### Phase 5: Full Compiler (Fast - ~800ms) ✅
Complete QB64pe including IDE:
- Everything (39 files, ~59,000 lines)

**Test:** Run via bootstrap test or compile QB64pe source directly. Optionally use a symlink `tests/qb64pe_incremental/05_full_compiler.bas` → `../QB64pe/source/qb64pe.bas`.  
**Status:** ✅ **COMPILES SUCCESSFULLY** through all QB64Fresh phases

**Results:**
- ✅ Preprocessing: All `$INCLUDE` directives processed (39 files)
- ✅ Lexing: 0 tokenization errors
- ✅ Parsing: 0 parse errors
- ✅ Semantic Analysis: 0 type checking errors
- ✅ Code Generation: 114,924 lines of C code generated
- ✅ C Compilation: 0 errors (fixed in session 071: const qualifier mismatches)

**Compilation Time:** ~800ms (much faster than expected!)

**Note:** The incremental testing strategy is still valuable for:
- Debugging specific modules in isolation
- Understanding module dependencies
- Fast iteration on utility functions
- But the full compiler now compiles successfully through QB64Fresh phases!

## Usage

### Quick Iteration (Phase 1-3)
```bash
# Test core infrastructure (0.15s)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/01_core_infrastructure.bas --emit-c

# Test hash utility (0.7s)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_hash.bas --emit-c

# Test type utility (0.1s)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/02_utilities_type.bas --emit-c

# Test built-in functions (~5s)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/03_builtin_functions.bas --emit-c
```

### Medium Testing (Phase 4)
```bash
# Test core compiler (no IDE)
cargo run --bin qb64fresh -- tests/qb64pe_incremental/04_core_compiler.bas --emit-c
```

### Full Validation (Phase 5)
```bash
# Via bootstrap test (recommended; uses ../QB64pe/source/qb64pe.bas)
cargo test --test bootstrap_tests qb64pe_compiles_successfully

# Or compile QB64pe source directly (with memory limit; may OOM at 4GB - see MEMORY_LIMITS.md)
bash -c 'ulimit -v 4194304 && cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c -o /tmp/qb64pe_full.c'
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
3. **Phase 3** - Built-in functions pass with stub infrastructure ✅
4. **Phase 4 validated** - Core compiler compiles successfully ✅
5. **Phase 5 validated** - Full compiler compiles successfully through all QB64Fresh phases ✅
6. **C compilation** - ✅ 0 errors (fixed in session 071; see [BOOTSTRAP_PLAN_FULL.md](BOOTSTRAP_PLAN_FULL.md) §7.1)

**Note:** The incremental testing strategy remains valuable for debugging specific modules. The full compiler compiles successfully through all QB64Fresh phases and generated C compiles with 0 errors (see [BOOTSTRAP_PLAN_FULL.md](BOOTSTRAP_PLAN_FULL.md) §7.1).

## Current Test Results

| Phase | Status | Time | Notes |
|-------|--------|------|-------|
| Phase 1: Core Infrastructure | ✅ PASSES | 0.15s | Simple, self-contained |
| Phase 2: Hash Utility | ✅ PASSES | 0.7s | Requires hash.bi header |
| Phase 2: Type Utility | ✅ PASSES | 0.1s | type.bi + type.bas |
| Phase 2: Const Eval Utility | ✅ PASSES | 0.1s | const QbString* fixes in codegen (match header) |
| Phase 3: Built-in Functions | ✅ PASSES | ~5s | Stubs: validname, tryRemoveSymbol$, AddQuotes$, subfunc |
| Phase 4: Core Compiler | ✅ PASSES | ~800ms | Validated as part of full compiler |
| Phase 5: Full Compiler | ✅ PASSES (QB64Fresh) ✅ (C compile) | ~800ms | All QB64Fresh phases pass; C compiles with 0 errors |

**Major Achievement (2026-01-28):** Phase 5 (full compiler) compiles successfully through all QB64Fresh phases and generated C compiles with **0 errors** (const qualifier fixes in session 071). See [BOOTSTRAP_PLAN_FULL.md](BOOTSTRAP_PLAN_FULL.md) §7.1 for details.

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

## Tools

- **Test runner:** `scripts/test-qb64pe-incremental.sh` — run all phases or specific phases (e.g. `./scripts/test-qb64pe-incremental.sh all`).
- **Section extractor:** `scripts/extract-qb64pe-section.sh` — extract line ranges from qb64pe.bas (e.g. `./scripts/extract-qb64pe-section.sh idstruct_type 596 642`, `clearid_sub 14476 14478`, `regid_sub 21849 22081`). Extracted sections go to `tests/qb64pe_incremental/sections/` (e.g. idstruct_type.bas, ids_init.bas, clearid_sub.bas, regid_sub.bas).

## Notes

- Test files use **relative paths** from `QB64pe/source/` (use `../../../QB64pe/source/` from test file location)
- IDE component is **optional** - core compiler works without it
- Each test file should have **minimal executable code** to exercise includes
- Use `--emit-c` flag to see generated C (faster than full compilation)
- See `tests/qb64pe_incremental/INCREMENTAL_TESTING.md` and README for detailed test results and workflow

## Current Status Summary

**QB64Fresh Compilation Phases:** ✅ **ALL PASSING**
- Preprocessing: ✅ Complete (39 files, ~59K lines)
- Lexing: ✅ Complete (0 errors)
- Parsing: ✅ Complete (0 errors)
- Semantic Analysis: ✅ Complete (0 errors)
- Code Generation: ✅ Complete (114,924 lines of C)

**C Compilation:** ✅ **0 errors** (fixed in session 071: const qualifier mismatches for BYREF parameters)

**Impact:** Before: 5+ minutes to test QB64pe. After: 0.1–0.7 s for isolated components → **300–3000× faster iteration**.

**Next Steps:**
1. Build executable (link generated C with runtime library; see [BOOTSTRAP_PLAN_FULL.md](BOOTSTRAP_PLAN_FULL.md) §7.1)
2. Test bootstrapped QB64pe execution

**Related Documentation:**
- [tests/qb64pe_incremental/README.md](../tests/qb64pe_incremental/README.md) - Test file overview and workflow
- [tests/qb64pe_incremental/QUICK_START.md](../tests/qb64pe_incremental/QUICK_START.md) - Quick start and section extraction
- [tests/qb64pe_incremental/QUICK_REFERENCE.md](../tests/qb64pe_incremental/QUICK_REFERENCE.md) - Quick reference
- [tests/qb64pe_incremental/INCREMENTAL_TESTING.md](../tests/qb64pe_incremental/INCREMENTAL_TESTING.md) - Incremental testing details
