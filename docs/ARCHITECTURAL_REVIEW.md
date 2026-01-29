# QB64Fresh Architectural Review

**Date:** 2026-01-28  
**Reviewers:** Software Architect, Rust Expert, Pragmatic Engineer

**Note:** Completed items moved to [ARCHITECTURAL_REVIEW_COMPLETED.md](ARCHITECTURAL_REVIEW_COMPLETED.md).

---

## Overview

This document consolidates architectural recommendations from multiple perspectives into a unified set of actionable items. The codebase is in excellent state (Grade: A-), with remaining issues focused on optimization and organization rather than fundamental architecture.

---

## Medium Priority Recommendations

### 2. Stream Code Generation ⚠️ **PENDING**

**Problem:** Codegen accumulates `String` via `format!()` with no buffer reuse, causing memory pressure for large programs.

**Recommendation:**
- Refactor codegen to use `Write` trait instead of accumulating strings
- Profile to confirm impact before optimization
- Consider `Cow<'static, str>` for generated code strings where appropriate

---

### 3. Rust Code Quality Improvements ⚠️ **PENDING**

#### 3.1 Excessive Cloning

**Problem:** 397 `.clone()` calls across 38 files (e.g., `statements.rs`: 83 clones).

**Recommendation:**
- Audit for unnecessary clones, especially in hot paths
- Use references where possible instead of cloning
- Consider `Rc`/`Arc` for shared AST nodes if profiling shows cloning is a bottleneck
- Note: Some cloning is necessary for AST construction; focus on optimization-critical paths

#### 3.2 Missing `#[must_use]` Attributes

**Problem:** `Result`-returning functions can be accidentally ignored.

**Recommendation:**
- Add `#[must_use]` to functions returning `Result<(), Error>` to prevent accidental error suppression

#### 3.3 String vs `&str` Usage

**Problem:** Functions take `String` when `&str` suffices.

**Recommendation:**
- Use `&str` for input parameters where ownership isn't needed
- Consider `impl Into<String>` for flexibility when both owned and borrowed strings are acceptable
- Use `Cow<str>` for functions that may accept owned or borrowed strings

#### 3.4 Rust Idiom Improvements

**Recommendations:**
- **`SmallVec`**: For small collections (e.g., function parameters) to avoid heap allocation
- **`IndexMap`**: For symbol tables if iteration order matters

---

### 5. Runtime Architecture Improvements ⚠️ **PARTIALLY COMPLETE** (Phase 1 Done)

**Status:** Phase 1 (Quick Fixes) completed 2026-01-28. Phase 2-3 deferred. See [ARCHITECTURAL_REVIEW_ITEM5_IMPLEMENTATION.md](ThingsToDo/ARCHITECTURAL_REVIEW_ITEM5_IMPLEMENTATION.md) for details.

**Phase 1 Completed:**
- ✅ Forward declarations section added to resolve function ordering issues
- ✅ Type mismatches fixed (e.g., `qb_shell_hide` uses `qb_string_data()`)
- ✅ `RuntimeMode` enum implemented with associated data
- ✅ Emission order corrected (forward declarations → types → implementations)
- ✅ Contributed to 91% reduction in QB64pe compilation errors (807 → 69)

**Remaining (Deferred):**
- ⏸️ Phase 2: Dependency tracking (manual ordering sufficient for current runtime size ~50 functions)
- ⏸️ Phase 3: Trait-based architecture (future consideration only)

#### 5.1 Runtime Mode Abstraction

**Current State:** `RuntimeMode` enum implemented with `Inline { type_registry }` and `External { header_path }` variants. Forward declarations handle cross-module dependencies. Manual emission ordering works well for current runtime size.

**Recommendations (Future):**
- Consider Phase 2 dependency tracking if runtime function count grows significantly (>100)
- Consider Phase 3 trait-based architecture only if Phase 2 shows limitations
- Current manual ordering with forward declarations is sufficient

#### 5.2 Runtime Library Organization

**Current State:** Runtime code generation uses forward declarations and explicit emission ordering. String concatenation approach is acceptable for current scale.

**Recommendation (Future):**
- Extract runtime generation into separate crate, or
- Use code generation (macros/build scripts) instead of string concatenation
- Only if runtime grows significantly or maintenance becomes an issue

---

### 6. Memory Safety ⚠️ **PENDING**

**Current:** Minimal `unsafe` code (good).

**Recommendation:**
- Review generated C code for safety issues
- Consider `cbindgen` for FFI safety and consistency
- Add tests that compile generated C to catch issues early