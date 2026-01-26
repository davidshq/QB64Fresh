# Continue Refactoring: Replace unwrap() with Proper Error Handling

Continue the refactoring work to replace `writeln!().unwrap()` and `write!().unwrap()` calls with `writeln_code!()` and `write_code!()` macros throughout the QB64Fresh codebase.

## Context Documents (READ THESE FIRST)

1. **`REFACTORING_PLAN.md`** - Complete refactoring plan with methodology, patterns, verification steps, and important learnings
2. **`REFACTORING_PROGRESS.md`** - Detailed progress summary showing 9 files completed (~1,042 instances converted)

## Current Status

- ✅ Phase 1: Infrastructure setup complete (`write_helpers.rs` created and integrated)
- ✅ Phase 2: Proof of concept complete (`stmt/data.rs` verified)
- ✅ Phase 3: 9 files completed:
  - `stmt/data.rs` (14 instances)
  - `stmt/error_jump.rs` (23 instances)
  - `stmt/io.rs` (7 instances - already converted)
  - `stmt/assignments.rs` (14 instances)
  - `stmt/control_flow.rs` (46 instances)
  - `stmt/definitions.rs` (61 instances)
  - `stmt/mod.rs` (236 instances)
  - `c_backend/mod.rs` (82 instances)
  - `runtime/mod.rs` (559 instances) ⚠️ **Note:** Functions in other runtime modules still return `()` and haven't been refactored yet

## Remaining Work

### High Priority Files (Runtime Modules)

**All runtime modules need refactoring. Estimated instances per file:**

1. **`runtime/graphics.rs`** - 504 instances (EXTRA CARE - string literals with escapes)
2. **`runtime/legacy.rs`** - 527 instances
3. **`runtime/keyboard.rs`** - 336 instances
4. **`runtime/strings.rs`** - 297 instances
5. **`runtime/file.rs`** - 202 instances
6. **`runtime/system.rs`** - 224 instances
7. **`runtime/debug.rs`** - 223 instances
8. **`runtime/memory.rs`** - 179 instances
9. **`runtime/io.rs`** - 185 instances
10. **`runtime/math.rs`** - 148 instances
11. **`runtime/audio.rs`** - 114 instances
12. **`runtime/timing.rs`** - 68 instances
13. **`runtime/types.rs`** - 16 instances
14. **`runtime/error.rs`** - 39 instances
15. **`runtime/arrays.rs`** - 103 instances

**Total remaining:** ~3,176 instances across 15 runtime module files

### Other Files

- `src/codegen/c_backend/file_io.rs` - 2 instances
- `src/codegen/c_backend/stmt/def_fn.rs` - 3 instances
- `src/codegen/c_backend/analysis.rs` - 6 instances

## Important Notes

### Critical Patterns to Follow

1. **Macro import:** `use crate::writeln_code;` (or `use crate::write_code;`)
2. **Replacement:** `writeln!().unwrap()` → `writeln_code!()?`
3. **Remove:** `use std::fmt::Write;` (no longer needed)
4. **Update signatures:** Functions using `?` must return `Result<(), CodeGenError>`
5. **Update call sites:** When helper function signatures change, update callers to use `?`
6. **Add `Ok(())`:** At the end of functions that now return `Result`

### Special Considerations

#### String Literals with Escapes (Runtime Modules)

- ✅ **No special handling needed** - Rust string literals with escape sequences (e.g., `"\\r\\n"`, `"\\x01"`, `"\\0"`) are preserved correctly during replacement
- ✅ **Verified in `runtime/mod.rs`** - All escape sequences work correctly

#### Local String vs &mut String

- ✅ **Local variables:** Use `writeln_code!(&mut output, ...)?` when `output` is a local `String`
- ✅ **Function parameters:** Use `writeln_code!(output, ...)?` when `output` is already `&mut String`

#### Runtime Module Dependencies

- ⚠️ **Important:** When refactoring runtime modules, functions in OTHER runtime modules may still return `()`
- ✅ **Solution:** Don't use `?` when calling unrefactored functions:
  ```rust
  // Correct (until those modules are refactored):
  types::emit_string_type(output);
  
  // Will be updated later:
  types::emit_string_type(output)?;
  ```
- ✅ **Update later:** When refactoring other runtime modules, update call sites in `runtime/mod.rs` to use `?`

#### Multi-line Replacement Patterns

For files with many instances:
1. Use `search_replace` for common patterns
2. Use `sed -i 's/\.unwrap();$/?;/g' file.rs` for end-of-line patterns
3. Use Python `re.sub` for complex multi-line patterns
4. Always verify with `grep` and `cargo check`

## Verification Checklist (Per File)

- [ ] All `writeln!().unwrap()` replaced with `writeln_code!()?`
- [ ] All `write!().unwrap()` replaced with `write_code!()?`
- [ ] `use crate::writeln_code;` (and/or `use crate::write_code;`) added
- [ ] `use std::fmt::Write;` removed
- [ ] Function signatures updated if needed (return `Result<(), CodeGenError>`)
- [ ] Call sites updated to use `?` operator (only for refactored functions)
- [ ] `Ok(())` added at end of functions that now return `Result`
- [ ] `cargo check` passes
- [ ] `cargo clippy` passes (warnings acceptable)
- [ ] `cargo fmt` applied

## Recommended Approach

1. **Start with smaller runtime modules** (e.g., `runtime/types.rs` with 16 instances) to build confidence
2. **Work systematically** - one file at a time with full verification
3. **Update `runtime/mod.rs` call sites** when you refactor modules it depends on
4. **Test string literal preservation** especially for `runtime/graphics.rs` and other modules with many escape sequences

## Key Reference Files

- **Completed examples to reference:**
  - `src/codegen/c_backend/stmt/data.rs` - Simple example
  - `src/codegen/c_backend/stmt/definitions.rs` - Multiple function signature updates
  - `src/codegen/c_backend/mod.rs` - Local String pattern (`&mut output`)
  - `src/codegen/c_backend/runtime/mod.rs` - Large file with string literals

- **Infrastructure:**
  - `src/codegen/c_backend/write_helpers.rs` - Macro definitions and helper functions

## Next Steps

1. Read `REFACTORING_PLAN.md` for complete methodology
2. Read `REFACTORING_PROGRESS.md` for detailed status
3. Pick the next file to refactor (recommend starting with `runtime/types.rs` - smallest)
4. Follow the verification checklist above
5. Update `REFACTORING_PROGRESS.md` when complete
