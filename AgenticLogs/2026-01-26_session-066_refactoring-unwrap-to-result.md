# Session 066: Refactoring unwrap() to Result - Implementation

**Date:** 2026-01-26  
**Session:** Refactoring `writeln!().unwrap()` to `writeln_code!()` with proper error handling  
**Status:** In Progress - Phase 2-3 Complete, Runtime Files Remaining

## Objective

Replace all `writeln!().unwrap()` and `write!().unwrap()` calls in the codegen backend with `writeln_code!()` and `write_code!()` macros that return `Result` instead of panicking, addressing the code quality issue identified in `CODEBASE_REVIEW.md`.

## Implementation Approach

### Phase 1: Setup and Research ✅

1. **Research Findings:**
   - `#[macro_export]` makes macros available at crate root (used for cross-module access)
   - `#[macro_use]` on modules extends macro scope to parent (not needed with `#[macro_export]`)
   - Rust's automatic reborrowing handles `&mut String` correctly in macros
   - Existing macro pattern `&mut *{ &mut $output }` works for both `String` and `&mut String`

2. **Module Integration:**
   - Added `mod write_helpers;` to `c_backend/mod.rs`
   - Added `#[macro_export]` to both `write_code!` and `writeln_code!` macros
   - Macros now available crate-wide without explicit imports

### Phase 2: Proof of Concept ✅

**Files Completed:**
- `stmt/data.rs` - Simple file, all instances replaced
- `stmt/io.rs` - Simple file, all instances replaced
- `stmt/error_jump.rs` - All instances replaced
- `stmt/assignments.rs` - All instances replaced

**Pattern Applied:**
- Removed `use std::fmt::Write;` imports
- Replaced `writeln!(output, ...).unwrap()` with `writeln_code!(output, ...)?`
- Replaced `write!(output, ...).unwrap()` with `write_code!(output, ...)?`
- Functions already returned `Result<(), CodeGenError>`, so `?` operator worked directly

### Phase 3: Systematic Refactoring ✅ (Partial)

**Files Completed:**
- `stmt/control_flow.rs` - Updated `emit_strig_check` signature to return `Result`, updated all call sites
- `stmt/definitions.rs` - All instances replaced
- `stmt/mod.rs` - Main statement emitter, all instances replaced
- `c_backend/mod.rs` - All instances replaced
- `file_io.rs` - All instances replaced
- `analysis.rs` - All instances replaced
- `stmt/def_fn.rs` - All instances replaced

**Key Changes:**
- Function signature updates: `emit_strig_check` now returns `Result<(), CodeGenError>`
- All call sites updated to use `?` operator
- Removed unused `use std::fmt::Write;` imports from all processed files

## Current Status

### Progress Metrics

**Total Instances:**
- **Initial:** 4,126 instances
- **Replaced:** ~166 instances (4%)
- **Remaining:** 3,960 instances (96%)

**Breakdown:**
- **Runtime files:** 3,799 instances (96% of remaining)
- **Non-runtime files:** 161 instances (4% of remaining)

### Completed Files

✅ All non-runtime statement files:
- `stmt/data.rs`
- `stmt/io.rs`
- `stmt/error_jump.rs`
- `stmt/assignments.rs`
- `stmt/control_flow.rs`
- `stmt/definitions.rs`
- `stmt/mod.rs`
- `stmt/def_fn.rs`

✅ Core backend files:
- `c_backend/mod.rs`
- `file_io.rs`
- `analysis.rs`

### Remaining Work

**High Priority:**
1. Handle remaining 161 instances in non-runtime files (edge cases or missed files)
2. **Runtime files (3,799 instances)** - Require EXTRA CARE:
   - String literals with escape sequences must be preserved
   - Test after each file to ensure C code generation is correct
   - Files: `runtime/mod.rs`, `runtime/graphics.rs`, and 20+ other runtime modules

**Future Phases:**
- Phase 4: Handle ignored errors (statements.rs:1971, io.rs:59, builtins.rs)
- Phase 5: Final verification (build, tests, code generation validation)

## Technical Decisions

### Macro Export Strategy

**Chosen:** `#[macro_export]` at crate root
- **Rationale:** Simplest approach for cross-module usage
- **Result:** Macros available as `write_code!` and `writeln_code!` from anywhere in crate
- **Alternative considered:** `#[macro_use]` on module - more complex, requires explicit imports

### Reference Handling

**Chosen:** Existing macro pattern `&mut *{ &mut $output }`
- **Rationale:** Already handles both `String` and `&mut String` via automatic reborrowing
- **Result:** No changes needed to macro definition
- **Verified:** Works correctly in all test cases

### Automation Strategy

**Approach:** Hybrid - script for simple patterns, manual for complex cases
- **Rationale:** 4,126 instances is too large for fully manual approach
- **Method:** Python scripts for bulk replacements, manual verification and edge case handling
- **Safety:** Tested pattern on small files first, verified results before scaling

## Lessons Learned

1. **Research First:** Understanding Rust macro semantics and reference handling prevented the same mistakes from previous attempts
2. **Systematic Approach:** File-by-file with verification prevents cascading errors
3. **Function Signatures:** Always check if functions return `Result` before using `?` operator
4. **Scale Matters:** For large refactorings (4000+ instances), strategic automation is necessary while maintaining quality
5. **Runtime Files Need Care:** String literals with escape sequences are fragile - test after each file

## Next Steps

1. **Complete non-runtime files:** Handle remaining 161 instances
2. **Runtime files:** Start with simpler runtime modules, test string literals carefully
3. **Function signature audit:** Ensure all functions using `?` return `Result`
4. **Compilation verification:** Run `cargo check` after each batch of files
5. **Code generation tests:** Verify generated C code is correct after runtime file changes

## Files Modified

- `src/codegen/c_backend/mod.rs` - Added `mod write_helpers;`
- `src/codegen/c_backend/write_helpers.rs` - Added `#[macro_export]` to macros
- `src/codegen/c_backend/stmt/*.rs` - Multiple files refactored
- `src/codegen/c_backend/file_io.rs` - Refactored
- `src/codegen/c_backend/analysis.rs` - Refactored
- `REFACTORING_PLAN.md` - Updated with progress and research findings

## Related Documents

- `REFACTORING_PLAN.md` - Detailed implementation plan
- `AgenticLogs/2026-01-26_refactoring-failure-analysis.md` - Analysis of previous failed attempts
- `CODEBASE_REVIEW.md` - Original issue identification
