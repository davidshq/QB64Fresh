# Code Review - January 27, 2026

## Overview
Review of recent code changes in graphics system, debugger, and linter tools.

## Critical Issues

### 2. Mutex Poisoning Risk (Font Manager)
**File:** `runtime/src/graphics/sdl2.rs` (multiple locations)
**Severity:** Medium

Multiple `FONT_MANAGER.lock().unwrap()` calls (lines 3117, 3146, 3187, 3204, 3226, 3252) can panic if the mutex is poisoned.

**Problem:** If a thread panicked while holding the lock, subsequent `unwrap()` calls will also panic, potentially crashing the entire graphics system.

**Fix:** Use `lock().unwrap_or_else(|poisoned| poisoned.into_inner())` to recover from poisoned mutex, or handle the error gracefully.

### 3. Redundant `unwrap()` After Check
**File:** `runtime/src/graphics/sdl2.rs:1927`
**Severity:** Low (code quality)

```rust
if final_color.is_none() {
    return Ok(());
}
let final_color = final_color.unwrap(); // Redundant - we know it's Some
```

**Problem:** While safe (we check first), this is redundant. Use `if let Some(color) = final_color` pattern instead.

**Fix:** Refactor to:
```rust
let final_color = match final_color {
    Some(c) => c,
    None => return Ok(()),
};
```

## Medium Priority Issues

### 4. Unsafe Static Global State
**File:** `runtime/src/graphics/mod.rs:1021`
**Severity:** Medium

```rust
pub static mut GRAPHICS_BACKEND: Option<Box<dyn GraphicsBackend>> = None;
```

**Problem:** 
- Unsafe mutable static can cause data races in multi-threaded scenarios
- No synchronization mechanism
- Documentation says "single-threaded" but this isn't enforced

**Recommendation:** Consider using `Mutex` or `RwLock` if multi-threading is a future possibility, or add `#[cfg(not(test))]` and use thread-local storage for tests.

### 5. Error Information Loss in FFI Layer
**File:** `runtime/src/graphics_ffi.rs` (throughout)
**Severity:** Medium

All FFI functions return simple `0` (success) or `1` (error) codes, losing detailed error information.

**Problem:** 
- Debugging is difficult when errors occur
- No way to distinguish between different error types
- Error messages are lost

**Recommendation:** Consider adding an error callback mechanism or logging errors to stderr for debugging.

### 6. Array Bounds Safety
**File:** `runtime/src/graphics/sdl2.rs:2227`
**Severity:** Medium

```rust
let src_data = self.page_buffers[src_page].clone();
self.page_buffers[dst_page] = src_data;
```

**Problem:** Direct indexing without bounds check (though there's a check earlier). If the check is bypassed or the array is resized, this could panic.

**Fix:** Use `.get()` and `.get_mut()` for safer access, or ensure bounds are always validated.

### 7. Silent Error Handling
**File:** `runtime/src/graphics/sdl2.rs:2483`
**Severity:** Low

```rust
if index < 0 || index >= 256 {
    return Ok(()); // Invalid index, silently ignore
}
```

**Problem:** Invalid palette indices are silently ignored. This could hide bugs in calling code.

**Recommendation:** Consider returning an error or at least logging a warning.

## Low Priority / Code Quality Issues

### 13. Event Loss in Debugger Server
**File:** `tools/debug/src/server.rs:1002-1011`
**Severity:** Low (documented limitation)

The `handle_evaluate()` function consumes events from the channel while searching for variable values. Events consumed during evaluation won't be processed by `process_debugee_events()` later.

**Problem:** 
- Events like `Stopped`, `Terminated`, or other `VariableValue` events may be lost
- Documented as a "known limitation" but could cause issues

**Recommendation:** Consider storing consumed events in a queue for later processing, or use a different synchronization mechanism.

### 14. Potential Race Condition in Debugger Event Processing
**File:** `tools/debug/src/server.rs:1086-1201`
**Severity:** Low

The `process_debugee_events()` function collects all available events first, then processes them. If new events arrive during processing, they won't be handled until the next call.

**Note:** This is likely intentional to avoid re-entrancy issues, but could cause event ordering problems in edge cases.

### 10. Potential Integer Overflow
**File:** `runtime/src/graphics/sdl2.rs:1993-1994`
**Severity:** Low

```rust
let w = (sx1 - sx2).unsigned_abs() as i32;
let h = (sy1 - sy2).unsigned_abs() as i32;
```

**Problem:** If the difference is very large, casting back to `i32` could overflow (though unlikely in practice).

**Fix:** Use `saturating` operations or validate ranges.

## Good Practices Observed

1. ✅ **Error handling:** Most functions return `Result<(), GraphicsError>` appropriately
2. ✅ **Documentation:** Good module-level and function documentation
3. ✅ **Type safety:** Proper use of Rust types and Option/Result patterns
4. ✅ **Test coverage:** Comprehensive test suite for graphics operations
5. ✅ **Separation of concerns:** Clean separation between backend trait and implementations

## Recommendations Summary

### High Priority
1. Fix `move_mouse()` to handle `None` canvas gracefully
2. Add mutex poisoning recovery for font manager locks

### Medium Priority
3. Consider thread-safety improvements for `GRAPHICS_BACKEND` static
4. Improve error reporting in FFI layer
5. Add bounds checking for array accesses

### Low Priority
6. Refactor redundant `unwrap()` patterns
7. Implement or document icon functions
8. Add better error messages in tests

## Files Reviewed

- `runtime/src/graphics/mod.rs` - Graphics backend trait and initialization
- `runtime/src/graphics/mock.rs` - Mock backend implementation
- `runtime/src/graphics/sdl2.rs` - SDL2 backend implementation (3412 lines)
- `runtime/src/graphics_ffi.rs` - C FFI layer (2408 lines)
- `runtime/include/qb64fresh_rt.h` - C header file
- `tools/debug/src/lib.rs` - Debugger library
- `tools/debug/src/server.rs` - DAP server implementation
- `tools/debug/src/sources.rs` - Source file management
- `tools/debug/src/symbols.rs` - Debug symbol extraction
- `tools/debug/src/watch.rs` - Watch expression parsing
- `tools/lint/src/lib.rs` - Linter library
- `tools/lint/src/rules/mod.rs` - Lint rule registry

## Conclusion

The codebase is generally well-structured with good error handling patterns. The main concerns are:
1. A few potential panic points that should be made more defensive
2. Error information loss in the FFI layer
3. Thread-safety considerations for the global graphics backend

Most issues are minor and can be addressed incrementally. The code follows Rust best practices overall.
