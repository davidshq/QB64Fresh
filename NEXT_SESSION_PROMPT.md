# Next Session Prompt - Refactoring unwrap() to Result

Continue the refactoring work to replace `writeln!().unwrap()` and `write!().unwrap()` calls with `writeln_code!()` and `write_code!()` macros throughout the QB64Fresh codebase.

## Quick Context

- **Status:** 9 files completed (~1,042 instances converted), ~3,176 instances remaining
- **Next target:** Start with `runtime/types.rs` (16 instances - smallest runtime module)
- **Reference docs:** `REFACTORING_PLAN.md`, `REFACTORING_PROGRESS.md`, `REFACTORING_SESSION_SUMMARY.md`

## Current Progress

✅ **Completed files:**
- `stmt/data.rs`, `stmt/error_jump.rs`, `stmt/io.rs`, `stmt/assignments.rs`
- `stmt/control_flow.rs`, `stmt/definitions.rs`, `stmt/mod.rs`
- `c_backend/mod.rs`, `runtime/mod.rs`

## Remaining Work

**Runtime modules (priority order - start with smallest):**
1. `runtime/types.rs` - 16 instances ⭐ **START HERE**
2. `runtime/error.rs` - 39 instances
3. `runtime/timing.rs` - 68 instances
4. `runtime/audio.rs` - 114 instances
5. `runtime/math.rs` - 148 instances
6. `runtime/memory.rs` - 179 instances
7. `runtime/io.rs` - 185 instances
8. `runtime/file.rs` - 202 instances
9. `runtime/debug.rs` - 223 instances
10. `runtime/system.rs` - 224 instances
11. `runtime/strings.rs` - 297 instances
12. `runtime/keyboard.rs` - 336 instances
13. `runtime/arrays.rs` - 103 instances
14. `runtime/legacy.rs` - 527 instances
15. `runtime/graphics.rs` - 504 instances (EXTRA CARE - string literals)

**Other files:**
- `src/codegen/c_backend/file_io.rs` - 2 instances
- `src/codegen/c_backend/stmt/def_fn.rs` - 3 instances
- `src/codegen/c_backend/analysis.rs` - 6 instances

## Critical Patterns (READ FIRST)

1. **Macro import:** `use crate::writeln_code;` (or `use crate::write_code;`)
2. **Replacement:** `writeln!().unwrap()` → `writeln_code!()?`
3. **Remove:** `use std::fmt::Write;`
4. **Update signatures:** Functions using `?` must return `Result<(), CodeGenError>`
5. **Add `Ok(())`:** At end of functions returning `Result`
6. **Update call sites:** Use `?` when calling refactored functions

## Important Notes

### Runtime Module Dependencies
⚠️ **Functions in OTHER runtime modules still return `()`** - Don't use `?` when calling them:
```rust
// Correct (until those modules are refactored):
types::emit_string_type(output);

// Will be updated later:
types::emit_string_type(output)?;
```

### Local String Pattern
- **Local variable:** `writeln_code!(&mut output, ...)?`
- **Function parameter:** `writeln_code!(output, ...)?`

### String Literals
✅ **No special handling needed** - Escape sequences (`"\\r\\n"`, `"\\x01"`, etc.) are preserved correctly.

## Verification Checklist (Per File)

- [ ] All `writeln!().unwrap()` → `writeln_code!()?`
- [ ] All `write!().unwrap()` → `write_code!()?`
- [ ] Added `use crate::writeln_code;` (and/or `use crate::write_code;`)
- [ ] Removed `use std::fmt::Write;`
- [ ] Updated function signatures (return `Result<(), CodeGenError>`)
- [ ] Updated call sites (use `?` for refactored functions only)
- [ ] Added `Ok(())` at end of functions returning `Result`
- [ ] `cargo check` passes
- [ ] `cargo clippy` passes
- [ ] `cargo fmt` applied

## Recommended Approach

1. **Start with `runtime/types.rs`** (16 instances - smallest)
2. **Follow the verification checklist** above
3. **Update `REFACTORING_PROGRESS.md`** when complete
4. **Move to next smallest file** and repeat

## Reference Files

- **Examples:** `stmt/data.rs` (simple), `runtime/mod.rs` (large file with string literals)
- **Infrastructure:** `src/codegen/c_backend/write_helpers.rs` (macro definitions)

---

**For detailed methodology, see `REFACTORING_PLAN.md`**  
**For current status, see `REFACTORING_PROGRESS.md`**  
**For session summary, see `REFACTORING_SESSION_SUMMARY.md`**
