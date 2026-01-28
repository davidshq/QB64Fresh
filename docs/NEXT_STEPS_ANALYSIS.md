# Next Steps Analysis
**Date:** 2026-01-28  
**Based on:** [ARCHITECTURAL_REVIEW.md](ARCHITECTURAL_REVIEW.md)

## Executive Summary

✅ **Critical blockers resolved:** Implicit variable handling fix is complete. QB64pe bootstrap passes successfully.  
**Grade restored:** A- (from temporary B+)

**Current state:** All fundamental architectural issues resolved. Remaining work focuses on:
1. Code quality and maintainability improvements
2. Performance optimizations
3. Developer experience enhancements

---

## Immediate Priority (Next Sprint)

### 1. ✅ ~~Fix Implicit Variable Handling~~ **COMPLETE** (2026-01-28)
- **Status:** Resolved
- **Impact:** Critical blocker removed, compilation errors fixed
- **Result:** QB64pe bootstrap compiles successfully

### 2. **StmtEmitter Context Refactoring** ⚠️ **HIGH PRIORITY**

**Current State:**
- ✅ Module split complete (8 focused modules: assignments, control_flow, data, def_fn, definitions, error_jump, io)
- ⚠️ Struct still has **20+ fields** in a single struct

**Fields in StmtEmitter (20 total):**
1. `label_counter: u32` - Label generation
2. `indent: usize` - Formatting
3. `loop_stack: Vec<LoopContext>` - Control flow
4. `data_label_indices: HashMap<String, usize>` - Data handling
5. `current_proc: Option<String>` - Procedure context
6. `current_func_ret_var: Option<String>` - Function context
7. `current_func_byref_strings: Vec<String>` - Function context
8. `current_func_param_names: HashSet<String>` - Function context
9. `variable_renames: HashMap<String, String>` - Variable shadowing
10. `global_var_names: HashSet<String>` - Global symbol tracking
11. `global_array_names: HashSet<String>` - Global symbol tracking
12. `shared_global_names: HashSet<String>` - Global symbol tracking
13. `global_const_names: HashSet<String>` - Global symbol tracking
14. `strig_event_counter: u32` - Event handling
15. `strig_handlers: Vec<(u32, String)>` - Event handling
16. `debug_enabled: bool` - Debug support
17. `debug_source_file: Option<String>` - Debug support
18. `no_shell: bool` - Security/feature flag
19. `emitted_labels: HashSet<String>` - Label tracking
20. `runtime_mode: RuntimeMode` - Runtime configuration

**Recommended Refactoring:**
Split into focused context structs:

```rust
pub(super) struct StmtEmitter {
    // Core formatting/control
    pub formatting: FormattingContext,
    pub control_flow: ControlFlowContext,
    
    // Procedure/function context
    pub procedure: ProcedureContext,
    
    // Global symbol tracking
    pub globals: GlobalContext,
    
    // Event handling
    pub events: EventContext,
    
    // Debug support
    pub debug: DebugContext,
    
    // Configuration
    pub config: EmitterConfig,
}

pub(super) struct FormattingContext {
    pub indent: usize,
}

pub(super) struct ControlFlowContext {
    pub loop_stack: Vec<LoopContext>,
    pub emitted_labels: HashSet<String>,
    pub label_counter: u32,
}

pub(super) struct ProcedureContext {
    pub current_proc: Option<String>,
    pub current_func_ret_var: Option<String>,
    pub current_func_byref_strings: Vec<String>,
    pub current_func_param_names: HashSet<String>,
    pub variable_renames: HashMap<String, String>,
}

pub(super) struct GlobalContext {
    pub var_names: HashSet<String>,
    pub array_names: HashSet<String>,
    pub shared_names: HashSet<String>,
    pub const_names: HashSet<String>,
}

pub(super) struct EventContext {
    pub strig_event_counter: u32,
    pub strig_handlers: Vec<(u32, String)>,
}

pub(super) struct DebugContext {
    pub enabled: bool,
    pub source_file: Option<String>,
}

pub(super) struct EmitterConfig {
    pub no_shell: bool,
    pub runtime_mode: RuntimeMode,
}
```

**Benefits:**
- ✅ Better testability (can test contexts in isolation)
- ✅ Clearer dependencies (explicit what each module needs)
- ✅ Easier to reason about state
- ✅ Better documentation (each context has clear purpose)

**Effort:** Medium (2-3 days)
- Refactoring is mechanical but requires careful testing
- All 8 modules need updates to use new context structs
- Need to ensure no functionality regressions

**Risk:** Low
- Well-defined refactoring with clear boundaries
- Module structure already in place
- Can be done incrementally

---

## Short-term Priorities (Next Month)

### 3. **LSP Incremental Parsing** ⚠️ **PERFORMANCE CRITICAL**

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

---

### 4. **Cloning Audit** ⚠️ **PERFORMANCE OPTIMIZATION**

**Current State:**
- **397 `clone()` calls** identified across 38 files
- Many may be unnecessary
- Performance impact on large programs

**Recommended Approach:**
1. Profile to identify hot paths
2. Audit clones in hot paths first
3. Use references/borrowing where possible
4. Consider `Rc`/`Arc` for shared ownership if needed

**Effort:** Medium (1 week)
- Systematic audit required
- Need to understand ownership patterns
- Testing to ensure no regressions

**Priority:** Medium - Performance improvement, but not blocking

---

### 5. **Error Recovery Tests** ⚠️ **CODE QUALITY**

**Current State:**
- Error collection works, but recovery behavior not well-tested
- Need to ensure errors don't cascade incorrectly
- Parser error recovery needs validation

**Recommended Approach:**
1. Test parser continues after errors (doesn't stop at first error)
2. Test semantic errors are collected, not just first error
3. Test error messages are helpful (not confusing cascades)

**Effort:** Low-Medium (2-3 days)
- Write test cases for error scenarios
- Validate error messages are clear

**Priority:** Medium - Improves developer experience when debugging

---

## Medium-term Priorities (Next Quarter)

### 6. **Stream Code Generation** ⚠️ **MEMORY OPTIMIZATION**

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

### 7. **Runtime Mode Abstraction Unification** ⚠️ **CODE ORGANIZATION**

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

### 8. **Test Coverage Reporting** ⚠️ **QUALITY METRICS**

**Current State:**
- No coverage metrics tracked
- Unknown which code paths are tested

**Recommended Approach:**
- Set up `cargo tarpaulin` or `cargo llvm-cov`
- Set coverage targets (e.g., 80% for core modules)
- Track coverage trends

**Effort:** Low (1 day)
- Tool setup and CI integration
- Document coverage goals

**Priority:** Low - Nice to have, but not critical

---

## Recommended Next Steps (Prioritized)

### **Option A: Code Quality Focus** (Recommended)
1. **StmtEmitter refactoring** (2-3 days)
   - High impact on maintainability
   - Low risk, well-defined scope
   - Sets foundation for future improvements

2. **Error recovery tests** (2-3 days)
   - Improves code quality
   - Validates error handling works correctly
   - Low effort, high value

3. **Cloning audit (hot paths only)** (3-4 days)
   - Focus on performance-critical paths
   - Profile first to identify bottlenecks
   - Incremental improvement

**Total:** ~1.5 weeks of focused work

---

### **Option B: Performance Focus**
1. **LSP incremental parsing** (1-2 weeks)
   - High user impact
   - Significant effort but transformative
   - Best for improving developer experience

2. **Cloning audit (hot paths)** (3-4 days)
   - Follow-up performance optimization

**Total:** ~2-3 weeks

---

### **Option C: Infrastructure Focus**
1. **Stream code generation** (1 week)
   - Memory optimization
   - Better scalability

2. **Test coverage reporting** (1 day)
   - Quality metrics

3. **StmtEmitter refactoring** (2-3 days)
   - Code organization

**Total:** ~2 weeks

---

## Decision Matrix

| Priority | Task | Impact | Effort | Risk | Recommended? |
|----------|------|--------|--------|------|--------------|
| **Immediate** | StmtEmitter refactoring | High (maintainability) | Medium (2-3 days) | Low | ✅ **YES** |
| **Short-term** | LSP incremental parsing | High (UX) | High (1-2 weeks) | Medium | ⚠️ **If UX is priority** |
| **Short-term** | Cloning audit | Medium (performance) | Medium (1 week) | Low | ✅ **Yes (hot paths)** |
| **Short-term** | Error recovery tests | Medium (quality) | Low (2-3 days) | Low | ✅ **YES** |
| **Medium-term** | Stream codegen | Medium (memory) | Medium (1 week) | Low | ⚠️ **If memory is issue** |
| **Medium-term** | Runtime mode abstraction | Low (organization) | Medium-High | Medium | ❌ **Defer (YAGNI)** |
| **Medium-term** | Test coverage | Low (metrics) | Low (1 day) | Low | ⚠️ **Nice to have** |

---

## Recommendation

**Start with Option A (Code Quality Focus):**

1. **StmtEmitter refactoring** - High value, low risk, well-defined scope
2. **Error recovery tests** - Quick win, validates important behavior
3. **Cloning audit (hot paths)** - Performance improvement where it matters most

This provides:
- ✅ Immediate maintainability improvements
- ✅ Code quality validation
- ✅ Performance improvements in critical paths
- ✅ Low risk, incremental progress
- ✅ Sets foundation for future work

**After Option A, consider:**
- LSP incremental parsing if user experience is a priority
- Stream code generation if memory usage becomes an issue
- Runtime mode abstraction only if we add more runtime modes

---

## Notes

- All critical blockers are resolved ✅
- Codebase is in excellent state (Grade A-)
- Remaining work is optimization and organization, not fundamental fixes
- Can proceed incrementally without blocking issues
