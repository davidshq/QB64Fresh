# QB64Fresh Architectural Review

**Date:** 2026-01-28  
**Reviewers:** Software Architect, Rust Expert, Pragmatic Engineer

**Note:** Completed items moved to [ARCHITECTURAL_REVIEW_COMPLETED.md](ARCHITECTURAL_REVIEW_COMPLETED.md).

---

## Overview

This document consolidates architectural recommendations from multiple perspectives into a unified set of actionable items. The codebase is in excellent state (Grade: A-), with remaining issues focused on optimization and organization rather than fundamental architecture.

**Recent Completion:** Item #1 (LSP Performance Improvement) was completed on 2026-01-28. See [ARCHITECTURAL_REVIEW_COMPLETED.md](ARCHITECTURAL_REVIEW_COMPLETED.md) for details.

---

## High Priority Recommendations

*No high-priority items currently pending. All critical architectural issues have been resolved.*

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

### 4. Error Message Quality ⚠️ **PENDING**

**Current:** Errors have spans/messages but lack context.

**Recommendation:**
- Add "did you mean?" suggestions for typos and similar symbol names
- Show related symbols (e.g., "did you mean `foo`?")
- Use `ariadne` more extensively for better error formatting and context

---

### 5. Runtime Architecture Improvements ⚠️ **PENDING**

#### 5.1 Runtime Mode Abstraction

**Current State:** Runtime code generation split across many files (1219 lines in `runtime/mod.rs`).

**Recommendations:**
- **Prefer enum with associated data** for mutually exclusive modes:
  ```rust
  enum RuntimeMode {
      Inline { type_registry: TypeRegistry },
      External { header_path: PathBuf },
  }
  ```
- Use generics to avoid scattered runtime mode checks
- Ensure abstraction doesn't force unnecessary complexity where enums work
- **Alternative:** Consider macro/code generation for both modes from single source

#### 5.2 Runtime Library Organization

**Recommendation:**
- Extract runtime generation into separate crate, or
- Use code generation (macros/build scripts) instead of string concatenation
- Preserve flexibility for dev/testing (inline) vs production (external)

---

### 6. Memory Safety ⚠️ **PENDING**

**Current:** Minimal `unsafe` code (good).

**Recommendation:**
- Review generated C code for safety issues
- Consider `cbindgen` for FFI safety and consistency
- Add tests that compile generated C to catch issues early