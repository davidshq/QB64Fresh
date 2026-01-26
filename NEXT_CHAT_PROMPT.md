# Prompt for Next Chat Session

Continue the refactoring work to replace `writeln!().unwrap()` and `write!().unwrap()` calls with `writeln_code!()` and `write_code!()` macros throughout the QB64Fresh codebase.

## Context Documents

1. **`REFACTORING_PLAN.md`** - Complete refactoring plan with methodology, patterns, and verification steps
2. **`REFACTORING_PROGRESS.md`** - Detailed progress summary showing 6 files completed (~165 instances converted)

## Current Status

- ✅ Phase 1: Infrastructure setup complete (`write_helpers.rs` created and integrated)
- ✅ Phase 2: Proof of concept complete (`stmt/data.rs` verified)
- ✅ Phase 3: 6 files completed:
  - `stmt/data.rs` (14 instances)
  - `stmt/error_jump.rs` (23 instances)
  - `stmt/io.rs` (7 instances - already converted)
  - `stmt/assignments.rs` (14 instances)
  - `stmt/control_flow.rs` (46 instances)
  - `stmt/definitions.rs` (61 instances)

## Next Steps

Continue with Phase 3: Systematic file-by-file refactoring. **Next priority files:**

1. **`stmt/mod.rs`** - Main statement emitter (estimated 100+ instances)
2. **`c_backend/mod.rs`** - Backend entry point (estimated 50+ instances)

## Important Notes

- Follow the systematic approach in `REFACTORING_PLAN.md` - one file at a time with verification
- Use the patterns and learnings documented in `REFACTORING_PLAN.md` (macro pattern, import pattern, function signature updates)
- Run `cargo check`, `cargo clippy`, and `cargo fmt` after each file
- If you see compilation errors that don't match file content, run `cargo clean` first
- All completed files compile successfully - use them as reference for the pattern

## Key Patterns to Follow

1. **Macro import:** `use crate::writeln_code;` (or `use crate::write_code;`)
2. **Replacement:** `writeln!().unwrap()` → `writeln_code!()?`
3. **Remove:** `use std::fmt::Write;` (no longer needed)
4. **Update signatures:** Functions using `?` must return `Result<(), CodeGenError>`
5. **Update call sites:** When helper function signatures change, update callers to use `?`

See `REFACTORING_PROGRESS.md` for the complete verification checklist per file.
