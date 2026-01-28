# Plan: Complete QB64PE Compilation Support

**Created:** 2026-01-26  
**Updated:** 2026-01-28  
**Status:** C Code Generation in Progress (69 errors remaining, 91% reduction from initial 807 errors)

## Executive Summary

This document outlines the plan to enable QB64Fresh-compiled QB64pe to successfully compile BASIC programs, achieving full bootstrap validation.

**Important:** QB64pe requires the **external runtime with graphics support** because it has a GUI. The compilation uses `RuntimeMode::External` and the runtime library must be built with `--features graphics-sdl2`.

## Current Status

**Current Status:**
- ⚠️ **69 C compilation errors remaining** (down from 807 initial errors)
- ⚠️ Generated C code: 114,924 lines (compiles with 69 errors)
- ⚠️ Type system issues: Variable shadowing fixed, ParseNum UDT struct added
- ⚠️ Runtime function signature mismatches: Some pointer type incompatibilities remain
- ⚠️ Full execution testing: Blocked until C compilation errors are resolved

## Implementation Phases

### Phase 4: C Code Generation Fixes (IN PROGRESS)

#### 4.1 Type System Fixes ✅ PARTIAL

**Status:** Major progress made, some issues remain

**Remaining:**
- ⚠️ Some type compatibility issues between `qbt_ParseNum*` and `QbString*` (part of 69 errors)
- ⚠️ Complex UDT type mappings may need additional runtime helpers
- ⚠️ Function pointer to integer conversion (1 error)

#### 4.2 Runtime Function Signature Fixes ⚠️ IN PROGRESS

**Status:** Some pointer type mismatches remain

**Remaining Issues:**
- ⚠️ `qb_removestringenclosingpair_str` - incompatible pointer types
- ⚠️ `strcpy` - incompatible pointer types
- ⚠️ `qb_net_openclient` - incompatible pointer types
- ⚠️ Hash table functions (`qb_hashfind`, `qb_sub_hashadd`) - const qualifier issues

**Required Fixes:**
- Review runtime function signatures in `runtime/include/qb64fresh_rt.h`
- Update codegen to properly convert between `QbString*` and `const char*` where needed
- Add conversion helpers if necessary

#### 4.3 Runtime Architecture Improvements (Future)

**Status:** Design phase complete, implementation pending

**Reference:** See `docs/ARCHITECTURAL_REVIEW_ITEM5_IMPLEMENTATION.md` for detailed phased approach:
- **Phase 1:** Quick fixes (forward declarations, type mismatches, emission order)
- **Phase 2:** Dependency tracking (explicit dependencies, emission ordering)
- **Phase 3:** Trait-based architecture (if needed for long-term maintenance)

**When to implement:** After C compilation errors are resolved, if runtime architecture becomes a maintenance burden.

## Implementation Priority

1. **Critical (Blocking):**
   - Phase 4.1: Fix remaining type system issues (part of 69 errors)
   - Phase 4.2: Fix runtime function signature mismatches (pointer type conversions)
   - Phase 4.1: Fix function pointer to integer conversion (1 error)

2. **High Priority:**
   - Phase 4.2: Review and update runtime function signatures
   - Phase 4.2: Add proper type conversion helpers in codegen
   - Phase 3.1: Full execution testing (after C compilation succeeds)

3. **Medium Priority:**
   - Phase 4.3: Consider Phase 1 runtime architecture improvements (forward declarations, emission order)
   - Phase 3.2-3.3: Additional integration tests
   - Phase 5.2: Performance benchmarking

4. **Low Priority:**
   - Phase 4.3: Phase 2-3 runtime architecture improvements (if needed)
   - Phase 5.1: Additional documentation updates

## Success Criteria

**Phase 1 Success:**
- All runtime features needed by QB64pe work correctly
- File I/O operations succeed
- Keyboard input works in console mode (both Unix and Windows)

**Phase 2 Success:**
- QB64pe can compile programs via `-x` flag
- Error reporting works correctly

**Phase 3 Success:**
- Bootstrapped QB64pe compiles "Hello World" program
- Generated executable runs correctly
- QB4.5 test suite subset passes

**Final Success:**
- Bootstrapped QB64pe can compile arbitrary BASIC programs
- Full bootstrap chain works (QB64Fresh → QB64pe → BASIC programs)
- All tests passing

## Estimated Timeline

| Phase | Sessions | Complexity |
|-------|----------|------------|
| Phase 1: Runtime Features | 4-6 | Medium |
| Phase 2: Compiler Features | 2-3 | Low |
| Phase 3: Integration Testing | 2-3 | Medium |
| Phase 4: Documentation | 1-2 | Low |
| **Total** | **9-14 sessions** | |
**Current Focus:**
- 🔄 **Fixing remaining 69 C compilation errors:**
  - Type system issues (type compatibility, function pointer conversions)
  - Runtime function signature mismatches (pointer type conversions needed)
  - Const qualifier issues in hash table functions

**Remaining:**
- ⚠️ C compilation errors - 69 errors blocking full compilation
  - See `docs/QB64PE_COMPILATION_BLOCKING_ISSUES.md` for detailed error analysis
  - Primary issues: Type conversions, pointer type mismatches, const qualifiers
- ⚠️ Full execution testing - Requires successful C compilation first
  - Steps documented in test files
  - **IMPORTANT:** QB64pe requires external runtime with graphics support (it has a GUI)
  - Requires: `cargo build -p qb64fresh-runtime --release --features graphics-sdl2`
  - Then: Compile QB64pe C code and link against runtime with SDL2:
    ```bash
    gcc -I runtime/include qb64pe.c -L target/release -lqb64fresh_rt \
        $(pkg-config --libs sdl2) -lm -lpthread -ldl -o qb64pe_bootstrapped
    ```
  - Finally: Run bootstrapped QB64pe on test programs

## Notes

- Memory issues have been resolved and are no longer blocking
- File I/O is implemented and validated through integration tests
- Keyboard input works on both Unix and Windows
- All runtime features are implemented and validated
- Full execution testing framework is in place with documented prerequisites
- **Major progress:** 91% reduction in C compilation errors (807 → 69)
- Generated C code is 114,924 lines, demonstrating the compiler can handle large programs
- Recent fixes (commits 3fe464b, 1183a3a) addressed the primary blockers (variable shadowing, type names)
- Remaining errors are primarily type system and runtime function signature issues
- See `docs/QB64PE_COMPILATION_BLOCKING_ISSUES.md` for detailed error analysis and recommended fixes
- See `docs/ARCHITECTURAL_REVIEW_ITEM5_IMPLEMENTATION.md` for runtime architecture improvement recommendations
