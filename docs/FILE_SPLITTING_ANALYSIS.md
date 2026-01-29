# File Splitting Analysis

**Date:** 2026-01-28  
**Purpose:** Determine which large files should be split to improve maintainability and Claude context access

## Executive Summary

After analyzing files >2000 lines, we recommend splitting **3 files** with high priority and **2 files** with medium priority. The primary issue is massive match dispatchers that have grown beyond maintainable size.

## Analysis Methodology

We evaluated files from three perspectives:
1. **Software Architect** - Structure, maintainability, separation of concerns
2. **Rust Expert** - Rust-specific patterns, module organization best practices
3. **Pragmatic Engineer** - Effort vs. benefit, risk assessment, incremental approach

## Files Analyzed

| File | Lines | Status | Recommendation |
|------|-------|--------|----------------|
| `src/codegen/c_backend/stmt/mod.rs` | 3,239 | **SPLIT** | High priority - 209 match arms |
| `src/semantic/checker/statements.rs` | 2,446 | **SPLIT** | High priority - 312 match arms |
| `src/ast/stmt.rs` | 2,351 | **CONSIDER** | Medium priority - single enum |
| `runtime/src/io.rs` | 3,603 | **SPLIT** | High priority - clear sections |
| `runtime/src/graphics/sdl2.rs` | 3,767 | **DEFER** | Low priority - cohesive implementation |

---

## High Priority Splits

### 1. `src/codegen/c_backend/stmt/mod.rs` (3,239 lines)

**Problem:** The `emit_stmt()` method contains a massive match statement with **209 match arms** covering all statement types. While helper modules exist (assignments, control_flow, data, etc.), the dispatcher itself is monolithic.

**Current Structure:**
```
stmt/
├── mod.rs (3,239 lines) ← PROBLEM: huge match dispatcher
├── assignments.rs
├── control_flow.rs
├── data.rs
├── def_fn.rs
├── definitions.rs
├── error_jump.rs
└── io.rs
```

**Architect Perspective:**
- **Violation:** Single Responsibility Principle - one method handling 200+ cases
- **Impact:** Hard to navigate, high cognitive load, merge conflicts
- **Solution:** Split dispatcher into category-specific modules

**Rust Expert Perspective:**
- **Pattern:** Use module-level dispatch functions, not monolithic match
- **Best Practice:** Group related statement types into separate dispatch modules
- **Example Pattern:**
  ```rust
  // stmt/graphics.rs
  pub fn emit_graphics_stmt(emitter: &mut StmtEmitter, stmt: &TypedStatement, ...) -> Result<...> {
      match stmt.kind {
          TypedStatementKind::Screen { ... } => ...,
          TypedStatementKind::Cls { ... } => ...,
          // All graphics statements
      }
  }
  ```

**Pragmatic Engineer Perspective:**
- **Effort:** Medium (2-3 hours) - mechanical refactoring
- **Risk:** Low - well-tested code, clear boundaries
- **Benefit:** High - dramatically improves maintainability
- **Incremental:** Can split one category at a time

**Recommended Split:**
```
stmt/
├── mod.rs (200-300 lines) ← Thin dispatcher
├── assignments.rs (existing)
├── control_flow.rs (existing)
├── data.rs (existing)
├── def_fn.rs (existing)
├── definitions.rs (existing)
├── error_jump.rs (existing)
├── io.rs (existing)
├── graphics.rs (NEW) ← Graphics statement dispatch
├── audio.rs (NEW) ← Audio statement dispatch
├── system.rs (NEW) ← System statements (SHELL, KILL, etc.)
├── meta.rs (NEW) ← Meta statements ($IF, $LET, etc.)
└── misc.rs (NEW) ← Remaining simple statements
```

**Estimated Reduction:** `mod.rs` from 3,239 → ~250 lines

---

### 2. `src/semantic/checker/statements.rs` (2,446 lines)

**Problem:** Similar to codegen - `check_statement()` has **312 match arms**. While submodules exist (audio, data, error_flow, graphics, io), the dispatcher is still monolithic.

**Current Structure:**
```
checker/
├── statements.rs (2,446 lines) ← PROBLEM: huge match dispatcher
├── statements/
│   ├── audio.rs
│   ├── data.rs
│   ├── error_flow.rs
│   ├── graphics.rs
│   └── io.rs
└── ...
```

**Architect Perspective:**
- **Same issues as codegen** - monolithic dispatcher
- **Additional concern:** Type checking logic is complex, harder to reason about in one place

**Rust Expert Perspective:**
- **Same pattern as codegen** - use module-level dispatch
- **Note:** Semantic checking is more complex than codegen, so splitting is even more valuable

**Pragmatic Engineer Perspective:**
- **Effort:** Medium (2-3 hours)
- **Risk:** Low-Medium - semantic checking is critical but well-tested
- **Benefit:** Very High - type checking is complex, splitting improves clarity

**Recommended Split:**
```
checker/
├── statements.rs (200-300 lines) ← Thin dispatcher
├── statements/
│   ├── audio.rs (existing)
│   ├── data.rs (existing)
│   ├── error_flow.rs (existing)
│   ├── graphics.rs (existing)
│   ├── io.rs (existing)
│   ├── graphics.rs (existing)
│   ├── assignments.rs (NEW) ← Assignment checking
│   ├── control_flow.rs (NEW) ← Control flow checking
│   ├── definitions.rs (NEW) ← Definition checking
│   └── misc.rs (NEW) ← Remaining simple checks
```

**Estimated Reduction:** `statements.rs` from 2,446 → ~250 lines

---

### 3. `runtime/src/io.rs` (3,603 lines)

**Problem:** Single file containing three distinct functional areas: PRINT, INPUT, and file I/O operations.

**Current Structure:**
- Lines 1-100: PRINT functions
- Lines 100-600: INPUT functions (with platform-specific keyboard handling)
- Lines 600-3600: File I/O (OPEN, CLOSE, GET, PUT, SEEK, etc.)

**Architect Perspective:**
- **Clear separation:** Three distinct responsibilities
- **Low coupling:** PRINT, INPUT, and file I/O are independent
- **High cohesion:** Each section is self-contained

**Rust Expert Perspective:**
- **Pattern:** Split by functional area, not by implementation detail
- **Module organization:** Each module can have its own helper modules if needed
- **FFI considerations:** All functions are `#[no_mangle] pub extern "C"` - splitting doesn't affect FFI

**Pragmatic Engineer Perspective:**
- **Effort:** Low (1-2 hours) - clear boundaries, mechanical split
- **Risk:** Very Low - runtime functions are well-isolated
- **Benefit:** High - easier to navigate, test, and maintain
- **Incremental:** Can split one section at a time

**Recommended Split:**
```
runtime/src/
├── io/
│   ├── mod.rs (50-100 lines) ← Re-exports
│   ├── print.rs (100 lines) ← PRINT functions
│   ├── input.rs (500 lines) ← INPUT functions + keyboard handling
│   └── file.rs (3,000 lines) ← File I/O operations
└── io.rs (DELETE)
```

**Estimated Reduction:** Each file <1,000 lines, clear separation

---

## Medium Priority Considerations

### 4. `src/ast/stmt.rs` (2,351 lines)

**Problem:** Single enum with ~100 variants covering all statement types.

**Architect Perspective:**
- **Cohesion:** All variants are statements - semantically cohesive
- **Coupling:** High - many parts of compiler depend on this enum
- **Split difficulty:** Rust enums can't be split across files easily

**Rust Expert Perspective:**
- **Enum limitations:** Can't split enum variants across modules in Rust
- **Alternatives:**
  1. Use `#[non_exhaustive]` and extension enums (complex, not idiomatic)
  2. Group related variants with comments (current approach)
  3. Keep as-is (enum is the right abstraction)

**Pragmatic Engineer Perspective:**
- **Effort:** High (would require architectural changes)
- **Risk:** High (touches core AST, many dependencies)
- **Benefit:** Low-Medium (enum is already well-organized with comments)
- **Recommendation:** **DEFER** - enum is appropriate abstraction, splitting would add complexity

**Verdict:** **KEEP AS-IS** - The enum is the right abstraction. Use comments to organize variants by category.

---

## Low Priority / Defer

### 5. `runtime/src/graphics/sdl2.rs` (3,767 lines)

**Problem:** Large implementation file for SDL2 graphics backend.

**Architect Perspective:**
- **Cohesion:** All code is SDL2-specific implementation - highly cohesive
- **Structure:** Well-organized with helper structs and methods
- **Split difficulty:** Implementation details are interconnected

**Rust Expert Perspective:**
- **Pattern:** Large implementation files are acceptable when cohesive
- **Alternative:** Could split by feature (primitives, images, text, events) but adds complexity

**Pragmatic Engineer Perspective:**
- **Effort:** Medium-High (would require careful analysis of dependencies)
- **Risk:** Medium (graphics is complex, easy to break)
- **Benefit:** Low-Medium (file is well-organized, splitting may not help)
- **Recommendation:** **DEFER** - Revisit if file grows beyond 5,000 lines or becomes hard to navigate

**Verdict:** **DEFER** - Cohesive implementation, well-organized. Monitor for future growth.

---

## Implementation Plan

### Phase 1: High Priority (Recommended First)

1. **Split `stmt/mod.rs` dispatcher** (2-3 hours)
   - Create `graphics.rs`, `audio.rs`, `system.rs`, `meta.rs`, `misc.rs`
   - Move match arms to appropriate modules
   - Update `mod.rs` to call module-level dispatch functions
   - Test: Run full test suite

2. **Split `statements.rs` dispatcher** (2-3 hours)
   - Create `assignments.rs`, `control_flow.rs`, `definitions.rs`, `misc.rs` in `statements/`
   - Move match arms to appropriate modules
   - Update `statements.rs` to call module-level dispatch functions
   - Test: Run full test suite

3. **Split `runtime/src/io.rs`** (1-2 hours)
   - Create `io/` module directory
   - Split into `print.rs`, `input.rs`, `file.rs`
   - Update `runtime/src/lib.rs` imports
   - Test: Run runtime tests

**Total Estimated Time:** 5-8 hours  
**Risk Level:** Low  
**Impact:** High - dramatically improves maintainability

### Phase 2: Monitor and Reassess

- Monitor `ast/stmt.rs` - if it grows beyond 3,000 lines, reconsider
- Monitor `graphics/sdl2.rs` - if it grows beyond 5,000 lines or becomes hard to navigate, split by feature area

---

## Success Criteria

After splitting:
- ✅ No file >2,000 lines (except well-justified exceptions)
- ✅ Match dispatchers <50 arms per file
- ✅ Clear module boundaries with single responsibility
- ✅ All tests pass
- ✅ No performance regression
- ✅ Easier navigation and code review

---

## Notes

- **Test coverage:** All splits must maintain 100% test coverage
- **Incremental approach:** Split one file at a time, test, commit
- **Documentation:** Update module docs after splitting
- **Claude context:** Smaller files improve Claude's ability to understand and modify code

---

## Decision Summary

| File | Decision | Priority | Effort | Risk | Benefit |
|------|----------|----------|--------|------|---------|
| `stmt/mod.rs` | **SPLIT** | High | Medium | Low | High |
| `statements.rs` | **SPLIT** | High | Medium | Low-Med | Very High |
| `io.rs` | **SPLIT** | High | Low | Very Low | High |
| `ast/stmt.rs` | **KEEP** | N/A | N/A | N/A | N/A |
| `graphics/sdl2.rs` | **DEFER** | Low | Medium-High | Medium | Low-Med |

**Recommendation:** Proceed with Phase 1 splits. They provide high value with low risk.
