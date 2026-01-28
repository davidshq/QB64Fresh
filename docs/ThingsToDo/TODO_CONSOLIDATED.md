# QB64Fresh Consolidated TODO and Planning

**Last Updated:** 2026-01-28  
**Purpose:** Single source of truth for all TODO items, priorities, and planning

> **Note:** This file consolidates content from:
> - `TODO.md` - Phase 6 optimization items
> - `TODO_ITEMS.md` - Code-level TODO items
> - `docs/NEXT_STEPS_ANALYSIS.md` - Next steps analysis
> - `docs/ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md` - Testing infrastructure
> - `docs/QB64PE_COMPILATION_PLAN.md` - QB64pe compilation support
> - `docs/ThingsToDo/INSTALLER_PLAN.md` - Distribution and installer planning

---

## Executive Summary

**Current State:** All fundamental architectural issues resolved. QB64pe compilation generates C code successfully but has **69 C compilation errors remaining** (down from 807, 91% reduction). Remaining work focuses on:

1. **QB64pe C compilation error resolution** (high priority - blocking executable build)
2. Code quality and maintainability improvements
3. Performance optimizations
4. Developer experience enhancements

**Grade:** A- (maintained)

---

## Critical Priorities (Blocking)

### 1. QB64pe C Compilation Error Resolution ⚠️ **CRITICAL**

**Status:** 69 errors remaining (91% reduction from 807 initial errors)  
**Impact:** Blocks executable build and full bootstrap validation  
**Effort:** 1-2 weeks  
**Goal:** Compile and run QB64pe using QB64Fresh

#### Compilation Status Overview

QB64Fresh successfully compiles QB64pe through all QB64Fresh phases:

| Phase | Status | Details |
|-------|--------|---------|
| **C Compilation** | ⚠️ In Progress | GCC compiles with **69 errors remaining** (down from 807, 91% reduction) |
| **Linking** | ⚠️ Blocked | Pending successful C compilation |

**Compilation Metrics:**
- Source Files: 39 files
- Source Lines: ~59,000 lines of BASIC
- Preprocessed Size: 2.64 MB (with all `$INCLUDE` files)
- Generated C Code: 114,924 lines (~4.5 MB)
- Compilation Time: ~800ms on modern hardware
- Error Reduction: 91% (807 → 69 errors)

**Progress Made:** (See TODO-completed.md for details)
- Variable shadowing issue resolved (was causing 188 errors)
- ParseNum UDT struct added to runtime header (commit 1183a3a)
- Type name consistency fixed (`QbString*` vs `qb_string*`)
- 91% reduction in errors (807 → 69)

#### Error Categories

**1.1 Type System Issues**
- Type compatibility between `qbt_ParseNum*` and `QbString*` (partially fixed in 1183a3a)
- Verify ParseNum UDT maps to `qbt_ParseNum` in generated C
- Test struct field access (`.typ`, `.f`, `.i`, `.ui`, `.s`)
- Additional UDT mappings and conversion helpers may be needed
- Function pointer to integer conversion (1 error)
  - Error: `assignment to 'int32_t' from 'int32_t (*)(void)' makes integer from pointer without a cast`
  - Fix: Add explicit casts in codegen or handle function pointers as distinct type

**1.2 Runtime Function Signature Mismatches**
**Problem:** Incompatible pointer types (`QbString*` vs `const char*`, struct mismatches)

**Examples:**
- `qb_removestringenclosingpair_str()` - argument type mismatch
- `strcpy()` - pointer type issues
- `qb_net_openclient()` - pointer type issues
- Hash table functions (`qb_hashfind`, `qb_sub_hashadd`) - const qualifier issues

**Fix:** Review `runtime/include/qb64fresh_rt.h`, update codegen for proper conversions

**1.3 Missing Runtime Functions (May Be Needed)**
- String element functions: `qb_elementgetnumericvalue_lng()`, `qb_elementgetstringvalue_lng()`, `qb_elementisnumber_lng()`, `qb_elementisstring_lng()`, `qb_getelement_str()`, `qb_countfunctionelements()`
- Note: Some functions listed in 1.2 (e.g., `qb_hashfind`, `qb_sub_hashadd`, `qb_net_openclient`, `qb_removestringenclosingpair_str`) may need implementation or signature fixes - see section 1.2 for details

**1.4 Metacommands (Needs Verification)**
- ⚠️ `$EXEICON` - Executable icon (parsed, codegen untested)
- ⚠️ `$VERSIONINFO` - Version information (parsed, codegen untested)
- ⚠️ `$DYNAMIC` - Dynamic arrays (syntax exists, commented in source)

#### Code Generation Issues

**String-Based Structured Data**
QB64pe uses strings to pass structured data (e.g., `args AS STRING` in `EvaluateFunction$`). QB64Fresh treats these as regular strings, but runtime expects structured access.

**Fix:** Recognize special string types, generate appropriate struct access, map `qbt_ParseNum` types

**Complex Type Conversions**
Frequent conversions between: string representations of numbers, structured data in strings, function pointers, array access patterns.

**Fix:** Improve type inference, add runtime helpers, better variant type handling

#### Fix Priority

**Priority 1: High Impact**
1. **Runtime function signature mismatches** - Fix const qualifiers, add type conversions
2. **Function pointer handling** - Add explicit casts in codegen
3. **Type system issues** - Verify ParseNum mapping, fix compatibility issues

**Priority 2: Nice to Have**
4. **Metacommand verification** - Test `$EXEICON` and `$VERSIONINFO` codegen

#### Testing Strategy

1. **Incremental:** Start with `EvaluateFunction$` in isolation, test string element functions, expand gradually (see Required Actions & Next Steps)
2. **Runtime:** Build library, verify signatures match generated code, test memory management
3. **Integration:** Compile `qb64pe.bas`, link runtime, run basic functionality tests

#### Required Actions & Next Steps

1. **Fix type system issues**
   - Verify ParseNum UDT mapping, test struct field access (`.typ`, `.f`, `.i`, `.ui`, `.s`)
   - Fix type compatibility between `qbt_ParseNum*` and `QbString*`
   - Add explicit casts for function pointer to integer conversion

2. **Fix runtime function signature mismatches**
   - Review and update runtime function signatures in `runtime/include/qb64fresh_rt.h`
   - Update codegen to properly convert between `QbString*` and `const char*` where needed
   - Fix const qualifier issues in hash table functions (`qb_hashfind`, `qb_sub_hashadd`)
   - Add conversion helpers if necessary

3. **Implement missing runtime functions** (if needed)
   - Add string element functions: `qb_elementgetnumericvalue_lng()`, `qb_elementgetstringvalue_lng()`, `qb_elementisnumber_lng()`, `qb_elementisstring_lng()`, `qb_getelement_str()`, `qb_countfunctionelements()`
   - Fix pointer type issues for `qb_net_openclient()` and `qb_removestringenclosingpair_str()`

4. **Update codegen**
   - Handle structured strings, generate proper conversions
   - Add function pointer casts
   - Recognize special string types, generate appropriate struct access

5. **Iterate and test**
   - Fix remaining 69 errors incrementally
   - Test after each change
   - Start with `EvaluateFunction$` in isolation, expand gradually

#### Related Files

**Source Files:**
- `src/codegen/c_backend/expr.rs` - Function calls, arguments, assignments
- `src/codegen/c_backend/types.rs` - Type mapping and conversions
- `src/semantic/types.rs` - Type inference, function pointers
- `runtime/include/qb64fresh_rt.h` - Runtime declarations
- `runtime/src/` - Runtime implementations

**Reference:**
- `QB64pe/source/qb64pe.bas` - Main source
- `QB64pe/source/utilities/const_eval.bas` - Contains `EvaluateFunction$`
- `/tmp/qb64pe_compiled.c` - Generated C (for analysis)

#### Runtime Requirements

**QB64pe requires external runtime with graphics support** because it has a GUI. The compilation uses `RuntimeMode::External` and requires:

- Runtime library built with: `cargo build -p qb64fresh-runtime --release --features graphics-sdl2`
- SDL2 libraries linked: `$(pkg-config --libs sdl2)`
- Additional dependencies: `-lm -lpthread -ldl -lwayland-client`

**Build Command:**
```bash
gcc -I runtime/include qb64pe.c -L target/release -lqb64fresh_rt \
    $(pkg-config --libs sdl2) -lm -lpthread -ldl -o qb64pe_bootstrapped
```

#### Runtime Features Status

All required runtime features are implemented and validated (see TODO-completed.md for details):
- File I/O - All operations tested in `tests/integration_tests.rs`
- Keyboard Input - Unix and Windows implementations working
- String Operations - All operations tested and working
- Array Operations - All operations tested and working
- Command-Line Mode - Argument parsing works correctly
- Error Handling - ON ERROR GOTO, RESUME, error reporting implemented

---

## Short-term Priorities (Next Month)

### 2. LSP Incremental Parsing ⚠️ **PERFORMANCE CRITICAL**

**Current State:**
- LSP re-parses entire file on every change
- For large files (e.g., QB64pe ~59K lines), this is slow
- User experience suffers from latency

**Impact:**
- **User-facing:** Slow autocomplete, diagnostics, hover
- **Scalability:** Doesn't scale to large codebases
- **Resource usage:** Unnecessary CPU/memory for small edits

**Recommended Approach:**
1. Implement incremental lexing (track changed regions)
2. Implement incremental parsing (re-parse only changed sections)
3. Incremental semantic analysis (re-analyze affected scopes)

**Effort:** High (1-2 weeks)
- Requires significant parser refactoring
- Need to handle edge cases (multi-line changes, etc.)
- Testing complexity increases

**Priority:** High for user experience, but can be deferred if other priorities are more critical

**Related Files:**
- `src/lsp/mod.rs` - LSP server implementation
- `src/lsp/incremental.rs` - Incremental parsing infrastructure (exists but needs completion)

---

---

## Medium-term Priorities (Next Quarter)

### 4. Stream Code Generation ⚠️ **MEMORY OPTIMIZATION**

**Current State:**
- Code generation accumulates entire C output in `String`
- For large programs (QB64pe → 114K lines), this uses significant memory
- `write_helpers.rs` provides error handling but not streaming

**Recommended Approach:**
- Use `Write` trait instead of `String` accumulation
- Stream output directly to file/stream
- Better memory usage for large programs

**Effort:** Medium (1 week)
- Refactor `CBackend::generate()` to accept `Write` trait
- Update all codegen methods to write incrementally
- Testing to ensure output is identical

**Priority:** Medium - Memory optimization, but current approach works

---

### 5. Runtime Mode Abstraction Unification ⚠️ **CODE ORGANIZATION**

**Current State:**
- TypeRegistry helps, but runtime mode differences still handled with if/else
- Inline vs External modes have different code paths
- Could benefit from trait-based abstraction

**Recommended Approach:**
- Consider trait-based runtime mode abstraction
- Or keep current approach if differences are small (YAGNI principle)

**Effort:** Medium-High (1-2 weeks)
- Requires design decision: trait vs current approach
- If trait: significant refactoring
- If current: document rationale

**Priority:** Low-Medium - Code organization, but current approach is pragmatic

---

### 6. Testing Infrastructure Improvements

**Current State:** Testing infrastructure is substantially complete:
- **1,500+ tests total** (405 unit, 727 integration, 10 golden, 19 property-based, 3 compatibility, 27 execution, 210 runtime)
- **99.1%** QB64pe compatibility (114/115 files; 1 remaining failure)
- **3 fuzz targets** verified (~4.6M inputs, 0 crashes)

**Remaining Work:**

**6.1 Graphics Backend Integration Tests**
- Current: 3 SDL2 backend unit tests, 6 basic init/mode tests
- Need: Non-headless integration tests where possible
- Challenge: SDL single-init, process-wide; circle/line buffer semantics

**6.2 Audio Backend Integration Tests**
- Current: 8 mock tests, 6 rodio backend tests
- Need: Additional integration tests

**6.3 By-ref Parameter Codegen**
- Issue: Parameters as pointers not dereferenced
- Fix: Add param context to emit_expr (requires codebase change)

**Note:** Testing infrastructure plan content has been consolidated here. See section 6 for remaining testing work.

---

## Long-term Priorities

### 7. Phase 6: Optimization

**Status:** All Phase 1-5, 7, 8 items completed (see `docs/archive/TODO-completed.md`)

**Remaining Optimization Items:**
- [ ] Dead code elimination *(Medium - 2-3 sessions)*
- [ ] Loop optimization *(Medium - 2-3 sessions)*
- [ ] Inline small functions *(Medium - 2-3 sessions)*

---

### 8. Distribution and Installer

**Status:** Planning phase complete, implementation pending

**Goals:**
1. **No Rust required** — End users run pre-compiled binaries only
2. **Pre-compiled binaries** — Windows, macOS, and Linux (primary architectures)
3. **C compiler handling** — Installer detects, guides, or installs a C compiler
4. **Clear scope** — What we ship, what we expect from the system, what is optional

**Phased Rollout:**

**Phase 1 — Minimal (no installer, binaries only)**
- [ ] GitHub Actions: build `qb64fresh` and `qb64fresh-lsp` for Windows x64, macOS (x64 + arm64), Linux x64
- [ ] Publish tarballs/zips on GitHub Releases with SHA-256
- [ ] README in each archive: how to add to PATH, how to install a C compiler

**Phase 2 — C compiler and runtime**
- [ ] Build and ship `libqb64fresh_rt.a` / `qb64fresh_rt.lib` and `qb64fresh_rt.h`
- [ ] Document `--runtime external`, link flags, and SDL2
- [ ] Implement detect + guide for C compiler on each platform

**Phase 3 — Installers**
- [ ] Windows: NSIS or Inno Setup installer
- [ ] macOS: .pkg or .dmg installer
- [ ] Linux: .deb and .rpm packages

**Phase 4 — Polish**
- [ ] Optional: `qb64fresh-fmt`, `qb64fresh-lint` in "full" install
- [ ] Optional: `qb64fresh doctor` (or `--check-env`) for environment validation
- [ ] Optional: `--build` / `--run` in the compiler
- [ ] Code signing / notarization where feasible

**Note:** Installer plan content has been consolidated here. See section 8 for distribution and installer planning.

---

## Code-level TODO Items

### Code Generation

**Graphics - Line Style Pattern**
- **File:** `src/codegen/c_backend/stmt/mod.rs:1484`
- **Item:** Implement line style pattern support
- **Context:** LINE statement style parameter is currently ignored

**Array I/O - Multi-dimensional Support**
- **File:** `src/codegen/c_backend/stmt/io.rs:89`
- **Item:** Handle multi-dimensional arrays in I/O statements
- **Context:** Currently only handles first index for 1D array syntax (TODO: handle multi-dim)

**String Runtime - Debug Logging**
- **File:** `src/codegen/c_backend/runtime/strings.rs:111`
- **Item:** Consider adding debug logging/warning when string operations occur
- **Context:** Comment in generated C code suggesting future enhancement for string leak detection

### Runtime Library

**Graphics - Per-Image Palettes**
- **File:** `runtime/src/graphics_ffi.rs:1056`
- **Item:** Support per-image palettes in `qb_palettecolor_get`
- **Context:** Currently ignores handle parameter and uses current palette

- **File:** `runtime/src/graphics_ffi.rs:1086`
- **Item:** Support per-image palettes with handle in `qb_palettecolor`
- **Context:** SET operation for palette needs handle support

**Graphics - STEP Behavior**
- **File:** `runtime/src/graphics_ffi.rs:1707`
- **Item:** Track last graphics position for proper STEP behavior
- **Context:** STEP modifier in graphics commands needs last position tracking

**Graphics - Scrolling**
- **File:** `runtime/src/graphics/sdl2.rs:1811`
- **Item:** Scroll if needed (text output)
- **Context:** When cursor moves past bottom, should scroll

- **File:** `runtime/src/graphics/sdl2.rs:1840`
- **Item:** Implement actual scrolling
- **Context:** Cursor row adjustment needs actual scrolling implementation

### Testing

**Bootstrap Tests - Golden File Comparison**
- **File:** `tests/bootstrap_tests.rs:279`
- **Item:** Implement golden file comparison for QB64pe subset
- **Context:** Test is marked `#[ignore]` with `todo!()` macro until golden file strategy is decided

**Bootstrap Tests - Full Execution**
- **File:** `tests/bootstrap_tests.rs:353` (approximate)
- **Item:** Once runtime library is built and QB64pe executable exists, implement full execution test
- **Context:** Requires building runtime with graphics support and compiling QB64pe C output

**Integration Tests - INSTR Function**
- **File:** `tests/integration_tests.rs:2054`
- **Item:** Add support for 2-argument form `INSTR(string, search)`
- **Context:** Currently only supports 3-argument form `INSTR(start, string, search)`

### Debugger

**Expression Evaluation**
- **File:** `tools/debug/src/server.rs:814`
- **Item:** Implement expression evaluation
- **Context:** Debug adapter protocol needs expression evaluation for watch variables

---

## Recommended Next Steps (Prioritized)

### **Option A: QB64pe Bootstrap Completion** (Recommended - Highest Priority)

1. **QB64pe C compilation error resolution** (1-2 weeks)
   - **CRITICAL:** Blocks executable build and full bootstrap validation
   - See section 1 for detailed error analysis and required actions
   - **Impact:** Enables full bootstrap chain validation

2. **Executable build and testing** (2-3 days)
   - Build bootstrapped QB64pe executable
   - Test basic functionality (help, command-line args)
   - Validate runtime execution

3. **Full execution testing** (3-4 days)
   - Test bootstrapped QB64pe compiling simple BASIC programs
   - Validate output correctness
   - Test on representative QB4.5 compatibility suite programs

**Total:** ~2-3 weeks to complete bootstrap validation

---

### **Option B: Code Quality Focus** (After Bootstrap)

1. ✅ **Error recovery tests** - **COMPLETE** (moved to TODO-completed.md)
   - 39 tests passing in `tests/error_recovery_tests.rs`
   - Comprehensive coverage of parser and semantic error recovery
   - Validates error handling works correctly

2. **Cloning audit (hot paths only)** (3-4 days)
   - Focus on performance-critical paths
   - Profile first to identify bottlenecks
   - Incremental improvement

**Total:** ~3-4 days of focused work

---

### **Option C: Performance Focus**

1. **LSP incremental parsing** (1-2 weeks)
   - See section 2 for detailed analysis
   - High user impact, significant effort but transformative

2. **Cloning audit (hot paths)** (3-4 days)
   - Follow-up performance optimization

**Total:** ~2-3 weeks

---

### **Option D: Infrastructure Focus**

1. **Stream code generation** (1 week)
   - See section 4 for detailed analysis
   - Memory optimization, better scalability

**Total:** ~1 week

---

## Decision Matrix

| Priority | Task | Impact | Effort | Risk | Recommended? |
|----------|------|--------|--------|------|--------------|
| **Critical** | QB64pe C compilation errors | High (blocking) | High (1-2 weeks) | Low | ✅ **YES - Start here** |
| **Short-term** | LSP incremental parsing | High (UX) | High (1-2 weeks) | Medium | ⚠️ **If UX is priority** |
| **Medium-term** | Stream codegen | Medium (memory) | Medium (1 week) | Low | ⚠️ **If memory is issue** |
| **Medium-term** | Runtime mode abstraction | Low (organization) | Medium-High | Medium | ❌ **Defer (YAGNI)** |

---

## Summary Statistics

**Total TODO Items Found:** 12 code-level TODOs + 3 from Phase 6 optimization

**By Category:**
- Code Generation: 3 items
- Runtime Library: 5 items
- Testing: 3 items
- Debugger: 1 item
- Optimization: 3 items (Phase 6)

**Priority Areas:**
1. QB64pe C compilation errors (69 errors remaining) - **CRITICAL**
2. Graphics features (palettes, scrolling, STEP behavior) - 5 items
3. Code generation improvements (array I/O, line style pattern) - 2 items
4. Testing infrastructure - 3 items
5. Debugger functionality - 1 item

---

## Related Documentation

### Planning Documents (Consolidated Here)
- [ARCHITECTURAL_REVIEW.md](docs/ARCHITECTURAL_REVIEW.md) - Architectural recommendations
- [ARCHITECTURAL_REVIEW_COMPLETED.md](docs/ARCHITECTURAL_REVIEW_COMPLETED.md) - Completed architectural improvements
- [TODO-completed.md](docs/archive/TODO-completed.md) - Completed TODO items

### Detailed Status Documents (Reference)
- [PARTIAL_IMPLEMENTATIONS.md](docs/ThingsToDo/PARTIAL_IMPLEMENTATIONS.md) - Detailed implementation status by feature category
- [QB64PE_IDE_FUNCTIONALITY_CHECKLIST_TODO.md](docs/ThingsToDo/QB64PE_IDE_FUNCTIONALITY_CHECKLIST_TODO.md) - IDE feature comparison and status
- [BOOTSTRAP_VALIDATION.md](docs/ThingsToDo/BOOTSTRAP_VALIDATION.md) - Bootstrap validation status

### Original Planning Files (Removed - Consolidated Here)
All planning content from the following files has been consolidated into this document:
- `TODO.md` - Phase 6 optimization items
- `TODO_ITEMS.md` - Code-level TODO items
- `docs/NEXT_STEPS_ANALYSIS.md` - Next steps analysis
- `docs/ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md` - Testing infrastructure plan
- `docs/QB64PE_COMPILATION_PLAN.md` - QB64pe compilation support plan
- `docs/ThingsToDo/INSTALLER_PLAN.md` - Distribution and installer planning
- `docs/QB64PE_COMPILATION_STATUS.md` - QB64pe compilation status and metrics (integrated into section 1)
- `docs/QB64PE_COMPILATION_BLOCKING_ISSUES.md` - Detailed error analysis (integrated into section 1)

---

*This consolidated file replaces the need to check multiple TODO/planning files. All planning information is now in one place.*
