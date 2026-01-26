# Refactoring Plan: Replace unwrap() with Proper Error Handling

**Date:** 2026-01-26  
**Status:** Phase 3 in progress - 9 files completed (~1,042 instances converted)  
**Progress:** See `REFACTORING_PROGRESS.md` for detailed status  
**Goal:** Replace `writeln!().unwrap()` with `writeln_code!()` using proper error handling  
**Remaining:** ~3,176 instances across 15 runtime module files + 3 other files

## Quick Reference: Common Gotchas (READ FIRST)

**If you see compilation errors, check these first:**

1. **"cannot find macro" error:**
   - ✅ **Fix:** Add `use crate::writeln_code;` (or `use crate::write_code;`) at top of file
   - Macros exported with `#[macro_export]` need explicit import

2. **"cannot borrow as mutable" error:**
   - ✅ **Fix:** Check macro pattern in `write_helpers.rs` - must be `$output`, NOT `&mut *{ &mut $output }`
   - Correct: `writeln_code($output, format_args!($($arg)*))`
   - Wrong: `writeln_code(&mut *{ &mut $output }, format_args!($($arg)*))`

3. **Function signature errors:**
   - ✅ **Fix:** If using `?` operator, function must return `Result<(), CodeGenError>`
   - Update function signature and all call sites

**Before starting:** Verify `write_helpers.rs` has correct macro patterns (see Phase 1, Step 1)

## Prerequisites (MUST COMPLETE BEFORE STARTING)

### 1. Understand Rust Macros and References

**Before making ANY changes:**
- [x] Read Rust Book chapter on macros: https://doc.rust-lang.org/book/ch19-06-macros.html
- [x] Understand `#[macro_export]` vs `#[macro_use]` - when to use each
- [x] Understand Rust reference semantics: `String` vs `&mut String` vs `&mut &mut String`
- [x] Understand automatic reborrowing rules

**Research Findings (2026-01-26):**

**Macro Export/Use:**
- `#[macro_export]` makes macros available at crate root for external crates
- `#[macro_use]` on a module extends macro scope to parent module (same crate)
- Modern approach (Rust 1.32+): Use `pub use macro_name;` instead of `#[macro_use]` for same-crate usage
- For internal crate usage, prefer `#[macro_use]` on module declaration OR re-export with `pub use`

**Reference Semantics:**
- `&mut String` does NOT implement `Copy` (unlike `&String`)
- Rust automatically reborrows `&mut T` when passed to functions: `&mut *reference`
- The pattern `&mut *{ &mut $output }` in macros forces reborrowing, which works for both:
  - `let mut output = String::new()` → `&mut output` → reborrows to `&mut String`
  - `fn foo(output: &mut String)` → `output` → reborrows to `&mut String`
- This means the macro in `write_helpers.rs` should work for both cases without modification

**Key Insight (VERIFIED):** The macro pattern should simply pass `$output` directly - Rust's automatic reborrowing handles `&mut String` correctly. The pattern `&mut *{ &mut $output }` causes borrow checker errors when `output` is already `&mut String`.

**Final Implementation (2026-01-26):**
```rust
#[macro_export]
macro_rules! writeln_code {
    ($output:expr, $($arg:tt)*) => {
        $crate::codegen::c_backend::write_helpers::writeln_code($output, format_args!($($arg)*))
    };
}
```
This pattern works correctly for both `String` and `&mut String` via Rust's automatic reborrowing.

### 2. Create and Test Minimal Proof of Concept

**Create a test file to verify the pattern works:**

```rust
// test_macro.rs - Standalone test
use std::fmt::Write;

// Simulate the macro and function
fn writeln_code(output: &mut String, args: std::fmt::Arguments<'_>) -> Result<(), String> {
    output.write_fmt(args).and_then(|_| output.write_char('\n'))
        .map_err(|e| format!("write error: {}", e))
}

macro_rules! writeln_code {
    ($output:expr, $($arg:tt)*) => {
        writeln_code($output, format_args!($($arg)*))
    };
}

// Test Case 1: Local String variable
fn test_local_string() -> Result<(), String> {
    let mut output = String::new();
    writeln_code!(&mut output, "test {}", 1)?;
    assert_eq!(output, "test 1\n");
    Ok(())
}

// Test Case 2: Function parameter &mut String
fn test_param(output: &mut String) -> Result<(), String> {
    writeln_code!(output, "test {}", 2)?;
    Ok(())
}

fn main() -> Result<(), String> {
    test_local_string()?;
    let mut s = String::new();
    test_param(&mut s)?;
    println!("All tests passed!");
    Ok(())
}
```

**Verify:**
- [ ] Test file compiles
- [ ] Test file runs successfully
- [ ] Understand exactly how the macro expands
- [ ] Document the exact pattern that works

### 3. Decide on Standard Pattern

**After testing, choose ONE pattern:**

**Pattern A: Always pass `&mut output` at call sites**
- Pros: Simple, consistent, macro is straightforward
- Cons: All call sites need `&mut` prefix
- Macro definition: `writeln_code!($output:expr, ...) => writeln_code($output, ...)`
- Call sites: `writeln_code!(&mut output, ...)`

**Pattern B: Macro handles both cases (CURRENT IMPLEMENTATION)**
- Pros: Call sites don't need `&mut` prefix, works with both `String` and `&mut String`
- Cons: More complex macro with reborrowing pattern
- Current macro: Uses `&mut *{ &mut $output }` which handles both cases via reborrowing

**Decision (VERIFIED 2026-01-26):** Use Pattern B with simple `$output` pattern - Rust's automatic reborrowing handles both cases correctly. Call sites can use `writeln_code!(output, ...)` directly without `&mut` prefix, and it works whether `output` is `String` or `&mut String`. The complex reborrowing pattern `&mut *{ &mut $output }` is NOT needed and causes borrow checker errors.

## Implementation Plan

### Phase 1: Setup (DO NOT SKIP)

1. **Verify write_helpers.rs exists and is correct**
   - [ ] Check that `write_code` and `writeln_code` functions exist
   - [ ] Verify they return `Result<(), CodeGenError>`
   - [ ] Verify they're properly documented
   - [ ] **CRITICAL:** Verify macro pattern is `$output` (NOT `&mut *{ &mut $output }`)
     - Correct: `$crate::codegen::c_backend::write_helpers::writeln_code($output, format_args!($($arg)*))`
     - Wrong: `$crate::codegen::c_backend::write_helpers::writeln_code(&mut *{ &mut $output }, format_args!($($arg)*))`
     - Reason: When `output` is `&mut String`, the `&mut *{ &mut $output }` pattern causes borrow checker errors

2. **Integrate write_helpers module**
   - [ ] Add `mod write_helpers;` to `c_backend/mod.rs`
   - [ ] **VERIFICATION:** Run `cargo check` - module must compile
   - [ ] Note: Macros use `#[macro_export]` so they're available at crate root

3. **Test macro in one simple file first**
   - [ ] Pick simplest file (e.g., `stmt/io.rs` or `stmt/data.rs`)
   - [ ] **CRITICAL:** Add `use crate::writeln_code;` at top (macros exported at crate root need explicit import)
   - [ ] Replace ONE `writeln!().unwrap()` call with `writeln_code!(output, ...)?`
   - [ ] **VERIFICATION:** Run `cargo check` - must pass before proceeding
   - [ ] **VERIFICATION:** Run `cargo clippy` - fix any warnings
   - [ ] **VERIFICATION:** Run `cargo fmt --check` - format code if needed
   - [ ] If "cannot find macro" error: Add `use crate::writeln_code;` (or `use crate::write_code;` for write_code!)
   - [ ] If borrow checker error: Check macro pattern in write_helpers.rs - should be `$output`, not `&mut *{ &mut $output }`

### Phase 2: Single File Proof of Concept

**Pick the SIMPLEST file first (e.g., `stmt/io.rs` or `stmt/data.rs`)**

1. **Replace unwrap() calls**
   - [ ] Find all `writeln!(output, ...).unwrap()` calls
   - [ ] Replace with `writeln_code!(output, ...)?` (no `&mut` needed - macro handles it)
   - [ ] Find all `write!(output, ...).unwrap()` calls
   - [ ] Replace with `write_code!(output, ...)?` (no `&mut` needed - macro handles it)
   - [ ] Remove `use std::fmt::Write;` if present (no longer needed)
   - [ ] Add `use crate::codegen::c_backend::write_helpers::{write_code, writeln_code};` if not present

2. **Update function signatures**
   - [ ] Check if function uses `?` operator
   - [ ] If yes, ensure function returns `Result<(), CodeGenError>`
   - [ ] Add `Ok(())` at end if needed
   - [ ] Find ALL call sites of this function
   - [ ] Update ALL call sites to use `?`

3. **Verify compilation and code quality**
   - [ ] **VERIFICATION:** Run `cargo check` - must pass
   - [ ] **VERIFICATION:** Run `cargo clippy` - fix ALL warnings before proceeding
   - [ ] **VERIFICATION:** Run `cargo fmt` - format code consistently
   - [ ] Fix ALL errors before proceeding
   - [ ] Verify no warnings about unused imports

4. **Test the output**
   - [ ] Run a simple BASIC program through the compiler
   - [ ] Verify generated C code is correct
   - [ ] Check that string literals are preserved correctly

**ONLY proceed to Phase 3 if Phase 2 is 100% complete and verified.**

## Important Learnings from Completed Work

### String Literal Handling (Runtime Modules)

**When refactoring runtime modules with string literals containing escape sequences:**

- ✅ **Escape sequences are preserved correctly** - The replacement pattern `writeln!().unwrap()` → `writeln_code!()?` does NOT affect string literal content
- ✅ **Examples that work correctly:**
  - `"\\r\\n"` → preserved as `"\\r\\n"` in C output
  - `"\\x01"` → preserved as `"\\x01"` in C output
  - `"\\0"` → preserved as `"\\0"` in C output
  - `"\\\""` → preserved as `"\\\""` in C output
- ✅ **No special handling needed** - Rust string literals are processed correctly by the macro

### Local String vs &mut String Pattern

**Important discovery from `c_backend/mod.rs` refactoring:**

- ✅ **When `output` is a local `String` variable** (e.g., `let mut output = String::new();`):
  - Use `writeln_code!(&mut output, ...)?` explicitly
  - The macro's automatic reborrowing works, but explicit `&mut` is clearer and avoids confusion
- ✅ **When `output` is a function parameter** (e.g., `fn foo(output: &mut String)`):
  - Use `writeln_code!(output, ...)?` directly
  - The parameter is already `&mut String`, so no `&mut` prefix needed
- ✅ **Pattern to follow:**
  - Local variables: `writeln_code!(&mut output, ...)?`
  - Function parameters: `writeln_code!(output, ...)?`

### Runtime Module Dependencies

**When refactoring `runtime/mod.rs` or other runtime modules:**

- ⚠️ **Other runtime modules still use `.unwrap()`** - Functions like `types::emit_string_type()`, `strings::emit_*()`, etc. still return `()` and haven't been refactored yet
- ✅ **Solution:** When calling these functions from refactored code, do NOT use `?` operator:
  ```rust
  // Correct (for now, until those modules are refactored):
  types::emit_string_type(output);
  strings::emit_string_functions(output);
  
  // Will be updated to this when those modules are refactored:
  types::emit_string_type(output)?;
  strings::emit_string_functions(output)?;
  ```
- ✅ **Update call sites later:** When refactoring other runtime modules, update `runtime/mod.rs` call sites to use `?`

### Multi-line Replacement Patterns

**For files with many instances (like `stmt/mod.rs` with 236 instances):**

- ✅ **Use iterative approach:**
  1. Start with simple `search_replace` for common patterns
  2. Use `sed` for end-of-line `.unwrap();` patterns: `sed -i 's/\.unwrap();$/?;/g' file.rs`
  3. Use Python `re.sub` for complex multi-line patterns:
     ```python
     content = re.sub(r'(\n[ \t]+)\.unwrap\(\);', r'\1?;', content)
     content = re.sub(r'\)\.unwrap\(\);', r')?;', content)
     ```
  4. Always verify with `grep` to find remaining instances
  5. Fix remaining instances manually if needed
- ✅ **Always verify:** Run `cargo check` after each batch of replacements

### Phase 3: Systematic File-by-File Refactoring

**Order of files (simplest to most complex):**
1. `stmt/data.rs` - Simple, few instances
2. `stmt/error_jump.rs` - Simple, few instances
3. `stmt/io.rs` - Simple, few instances
4. `stmt/assignments.rs` - Medium complexity
5. `stmt/control_flow.rs` - Medium complexity, watch for multi-line
6. `stmt/definitions.rs` - Medium complexity, watch for multi-line
7. `stmt/mod.rs` - Complex, many instances
8. `c_backend/mod.rs` - Complex, many instances
9. `runtime/mod.rs` - **EXTRA CARE** - string literals with escapes ✅ COMPLETED
10. `runtime/graphics.rs` - **EXTRA CARE** - string literals with escapes

**For EACH file:**
1. [ ] Replace all `writeln!().unwrap()` with `writeln_code!(output, ...)?` (macro handles `&mut` automatically)
2. [ ] Replace all `write!().unwrap()` with `write_code!(output, ...)?` (macro handles `&mut` automatically)
3. [ ] Remove `use std::fmt::Write;` if present
4. [ ] Update function signatures that use `?`
5. [ ] Update all call sites of modified functions
6. [ ] **VERIFICATION:** Run `cargo check` - MUST pass
7. [ ] **VERIFICATION:** Run `cargo clippy` - fix ALL warnings
8. [ ] **VERIFICATION:** Run `cargo fmt` - format code consistently
9. [ ] Test with a simple BASIC program
10. [ ] Verify generated C code is correct
11. [ ] **ONLY THEN** move to next file

### Phase 4: Handle Ignored Errors

**After all unwrap() replacements are done:**

1. **statements.rs:1971** - External function symbol definition
   - [ ] Replace `let _ =` with proper `match` statement
   - [ ] Only ignore duplicate external function errors
   - [ ] Document other conflicts with TODO

2. **io.rs:59** - same_line parameter
   - [ ] Add TODO comment explaining unimplemented feature
   - [ ] Or implement if feasible

3. **builtins.rs** - Builtin registration
   - [ ] Add comments explaining why errors are ignored (idempotent registration)
   - [ ] Verify this is correct behavior

### Phase 5: Final Verification

- [ ] Run `cargo build --release` - must succeed
- [ ] Run `cargo clippy` - must pass with no warnings
- [ ] Run `cargo fmt` - ensure all code is formatted consistently
- [ ] Run `cargo test` - all tests must pass
- [ ] Test code generation with multiple BASIC programs
- [ ] Verify C output is correct (no broken string literals)
- [ ] Check for any remaining `unwrap()` or `expect()` calls
- [ ] Verify no compilation warnings

## Verification Checklist (RUN AFTER EACH CHANGE TYPE)

**After making ANY change, run these commands in order:**

```bash
# 1. Check compilation
cargo check

# 2. Check linting (fix ALL warnings)
cargo clippy

# 3. Format code
cargo fmt

# 4. Verify formatting is correct
cargo fmt --check
```

**When to run verification:**
- ✅ After first macro test (Phase 1, Step 3)
- ✅ After completing each file (Phase 2 and Phase 3)
- ✅ After updating function signatures
- ✅ After introducing any new pattern
- ✅ Before moving to the next file

**If any command fails:**
- STOP immediately
- Fix the issue before proceeding
- Re-run all verification commands
- Do NOT accumulate errors

## Critical Rules (DO NOT VIOLATE)

1. **ONE file at a time** - Never modify multiple files simultaneously
2. **Verify after each file** - Run verification checklist after EVERY file
3. **Fix errors immediately** - Don't accumulate errors across files
4. **Test the pattern first** - Create test case before applying everywhere
5. **Understand before changing** - If unsure about Rust semantics, research first
6. **No bulk automation** - Manual, careful replacements only
7. **Preserve string literals** - Extra care in runtime files with escape sequences
8. **Update call sites** - When function signature changes, find ALL call sites
9. **Always verify** - Never skip the verification checklist

## Red Flags (STOP IMMEDIATELY IF YOU SEE)

- Compilation errors that you don't understand
- "cannot find macro" errors
- "cannot mutate immutable variable" errors
- Type mismatch errors related to `&mut String`
- Any errors that require guessing to fix

**If you hit a red flag:**
1. STOP making changes
2. Revert the current file
3. Research the issue
4. Understand the root cause
5. Create a test case to verify the fix
6. Only then apply the fix

## Success Criteria

- [ ] Zero `unwrap()` or `expect()` calls in codegen files
- [ ] All functions using `?` return `Result` properly
- [ ] All call sites updated correctly
- [ ] `cargo build --release` succeeds
- [ ] `cargo test` passes
- [ ] Generated C code is correct (no broken strings)
- [ ] No compilation warnings
- [ ] Code is cleaner and more maintainable

## Progress Status (2026-01-26)

**Current Status:** ✅ **Phase 3 in progress - 6 files completed successfully**

**Completed Files:**
- ✅ `stmt/data.rs` (14 instances)
- ✅ `stmt/error_jump.rs` (23 instances)
- ✅ `stmt/io.rs` (7 instances - already converted)
- ✅ `stmt/assignments.rs` (14 instances)
- ✅ `stmt/control_flow.rs` (46 instances)
- ✅ `stmt/definitions.rs` (61 instances)

**Total:** ~165 instances converted, all verified and compiling

**What Worked:**
- ✅ Systematic file-by-file approach with verification after each file
- ✅ Manual replacements with careful attention to function signatures
- ✅ Running `cargo check` after every file to catch errors early
- ✅ Updating function signatures when needed (4 functions updated)
- ✅ Updating all call sites when helper function signatures change

**Key Learnings Applied:**
- ✅ Used `#[macro_export]` for crate-wide macro availability
- ✅ Macro pattern `$output` (NOT `&mut *{ &mut $output }`) works correctly with Rust's automatic reborrowing
- ✅ Function signatures updated when using `?` operator
- ✅ Always add `Ok(())` at end of functions that now return `Result`
- ✅ Build cache issues resolved with `cargo clean`

**Next Steps:**
1. Continue with `stmt/mod.rs` (estimated 100+ instances)
2. Then `c_backend/mod.rs` (estimated 50+ instances)
3. Runtime files (extra care for string literals)
4. Handle ignored errors (Phase 4)
5. Final verification (Phase 5)

## Common Errors and Solutions

### Error 1: "cannot find macro `writeln_code` in this scope"

**Cause:** Macros exported with `#[macro_export]` are available at crate root but need explicit import.

**Solution:** Add `use crate::writeln_code;` (or `use crate::write_code;`) at the top of the file.

**Prevention:** Always add the import when first using the macro in a file.

### Error 2: "cannot borrow `output` as mutable, as it is not declared as mutable"

**Cause:** Macro pattern `&mut *{ &mut $output }` tries to reborrow `&mut String` which causes borrow checker errors.

**Solution:** Use simple pattern `$output` in macro definition - Rust's automatic reborrowing handles it correctly.

**Correct macro pattern:**
```rust
#[macro_export]
macro_rules! writeln_code {
    ($output:expr, $($arg:tt)*) => {
        $crate::codegen::c_backend::write_helpers::writeln_code($output, format_args!($($arg)*))
    };
}
```

**Wrong pattern (causes borrow errors):**
```rust
// DON'T USE THIS:
$crate::codegen::c_backend::write_helpers::writeln_code(&mut *{ &mut $output }, format_args!($($arg)*))
```

**Prevention:** Always verify macro pattern in `write_helpers.rs` uses `$output` directly, not `&mut *{ &mut $output }`.

## Notes

- This refactoring is about code quality, not functionality
- The code works with `unwrap()` - we're improving error handling
- Take time to do it right - better slow and correct than fast and broken
- When in doubt, ask for clarification rather than guessing
- **Runtime files require extra care** - test string literals after each file to ensure escape sequences are preserved
- **If rolling back:** Check macro pattern and import statements before restarting
