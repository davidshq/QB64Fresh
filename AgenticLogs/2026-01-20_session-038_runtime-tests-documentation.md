# Session 038: Runtime Stub Tests and Documentation Maintenance

**Date:** 2026-01-20
**Focus:** Adding runtime stub tests, implementing missing codegen, and documentation organization

---

## Summary

This session focused on verifying that File I/O, Graphics, and Sound statements correctly generate their FFI calls in the C output. We also implemented missing codegen for three functions and established documentation maintenance practices.

---

## Accomplishments

### 1. Runtime Stub Tests Added (94 tests)

Created three new test modules in `tests/integration_tests.rs`:

- **file_io_runtime_stubs** (22 tests): Verifies `KILL`, `NAME AS`, `MKDIR`, `RMDIR`, `CHDIR`, `OPEN`, `CLOSE`, `FREEFILE`, `EOF`, `LOF`, `LOC`, `SEEK` generate correct `qb_*` FFI calls

- **graphics_runtime_stubs** (43 tests): Verifies `SCREEN`, `CLS`, `COLOR`, `PSET`, `LINE`, `CIRCLE`, `PAINT`, `DRAW`, `VIEW`, `WINDOW`, and all `_MOUSE*` functions generate correct FFI calls

- **sound_runtime_stubs** (29 tests): Verifies `BEEP`, `SOUND`, `PLAY`, and all `_SND*` functions generate correct FFI calls

### 2. Missing Codegen Implemented

Three functions were missing codegen mappings:

- **`_COPYIMAGE`**: Required full pipeline (parser + semantic + codegen + runtime stub)
- **`_SNDOPENRAW`**: Needed codegen mapping + runtime stub
- **`_SNDRAWLEN`**: Needed codegen mapping + runtime stub

### 3. Documentation Maintenance Rule

Added a new section to CLAUDE.md establishing the pattern:
- Move completed `[x]` items from TODO.md to TODO-completed.md
- Move completed items from TESTING_INFRASTRUCTURE_PLAN.md to TESTING-COMPLETED.md
- Check for completed items at session end

### 4. Completed Items Archived

Moved completed testing milestones from TODO.md to TODO-completed.md:
- 80%+ line coverage achievement
- QB4.5 test cases compatibility (99.1%)
- Runtime stub tests for File I/O, Graphics, Sound

---

## Technical Details

### Test Pattern Used

The runtime stub tests follow this pattern:
```rust
#[test]
fn function_name_test() {
    let source = r#"
BASIC_CODE_HERE
"#;
    let code = compile_to_c(source).unwrap();
    assert!(code.contains("qb_expected_ffi_function("),
            "Should generate FFI call");
}
```

### Issues Encountered and Resolved

1. **Wrong FFI names**: `qb_open` should be `qb_file_open`, `qb_seek` should be `qb_file_seek`
2. **Reserved word conflict**: Variable `line` conflicts with `LINE INPUT` keyword - renamed to `textline`
3. **Mouse function naming**: Uses `qb_mouse_x` not `qb_mousex` (snake_case)
4. **Golden tests updated**: Added runtime stubs changed C output, requiring golden file update

---

## Files Modified

- `tests/integration_tests.rs` - Added 94 new tests
- `src/codegen/c_backend/expr.rs` - Added FFI mappings for 3 functions
- `src/parser/expressions.rs` - Added _COPYIMAGE parser registration
- `src/semantic/mod.rs` - Added _COPYIMAGE semantic registration
- `src/codegen/c_backend/runtime.rs` - Added 3 C runtime stubs
- `CLAUDE.md` - Added TODO maintenance rule
- `TODO.md` - Moved completed items
- `TODO-completed.md` - Received completed items

---

## Test Results

- **656 integration tests** (all passing)
- **94 new runtime stub tests** added this session
- All golden tests updated and passing

---

*Session 038 - 2026-01-20*
