# Session 050: Graphics Initialization and Headless Fixes

**Date:** 2026-01-24
**Focus:** Fixing graphics initialization issues for headless/container environments

## Summary

This session addressed two related issues with graphics initialization:

1. **QB64pe headless detection** - The original QB64pe compiler hangs when run with `-c` flag on headless systems (no X11 display)
2. **QB64Fresh stub infinite loops** - The inline runtime's graphics stubs could cause infinite loops in game loops

## Changes Made

### 1. QB64pe Headless Detection Fix

**File:** `QB64pe/source/qb64pe.bas` (lines 458-483)

**Problem:** The `-c` flag sets `NoIDEMode=TRUE` but leaves `ConsoleMode=FALSE`, causing `_SCREENSHOW` to be called even in command-line compilation mode. On headless systems, this hangs trying to initialize GLUT/X11.

**Solution:** Added automatic headless detection before `_SCREENSHOW`:
- Checks `ENVIRON$("DISPLAY")` on Linux/Mac
- If no display available, falls back to console mode
- Prints informative message (unless `-q` quiet mode)

```basic
IF INSTR(_OS$, "LINUX") OR INSTR(_OS$, "MAC") THEN
    IF LEN(ENVIRON$("DISPLAY")) = 0 THEN
        HeadlessMode = _TRUE
    END IF
END IF
```

### 2. QB64Fresh Inline Runtime Stub Improvements

**File:** `QB64Fresh/src/codegen/c_backend/runtime/graphics.rs`

**Problem:** `qb_gfx_poll_events()` always returned 1 ("window open"), causing infinite loops:
```basic
DO
    _DISPLAY
LOOP WHILE _SCREENEXISTS  ' Never exits!
```

**Solution:** Added frame counting with configurable limit:
- Frame counter tracks `_DISPLAY` calls
- Default limit of 1000 frames (configurable via `QB64FRESH_MAX_FRAMES` env var)
- `qb_gfx_poll_events()` and `qb_screenexists()` return "window closed" after limit
- Clear warning messages explain limitations and how to use external runtime

**New functions added:**
- `qb_screenexists()` - Was missing from stubs entirely

## Technical Details

### Frame Limiting Mechanism

```c
static int _qb_gfx_frame_count = 0;
static int _qb_gfx_max_frames = 1000;

int qb_gfx_poll_events(void) {
    if (_qb_gfx_frame_count >= _qb_gfx_max_frames) {
        return 0;  // Signal window closed
    }
    return 1;  // Window still open
}
```

### Environment Variable Override

Users can extend the frame limit for longer-running tests:
```bash
export QB64FRESH_MAX_FRAMES=5000
./my_graphics_program
```

## User Impact

| Scenario | Before | After |
|----------|--------|-------|
| QB64pe `-c` on headless | Hangs indefinitely | Falls back to console |
| QB64pe `-x` on headless | Works | Works (no change) |
| QB64Fresh stub game loop | Infinite loop | Exits after 1000 frames |

## Testing

- All 390 QB64Fresh tests pass
- Cargo check passes with no errors

## Files Modified

1. `QB64pe/source/qb64pe.bas` - Headless detection
2. `QB64Fresh/src/codegen/c_backend/runtime/graphics.rs` - Stub improvements

## Session Continuation: C Compilation Fixes

After the initial changes, execution tests revealed C compilation errors due to cross-module dependencies in the inline runtime.

### Problem 1: Undeclared `_qb_palette`

The `qb_palettecolor()` function in `system.rs` used `_qb_palette[]` before it was defined in `legacy.rs`.

**Solution:** Added forward declarations in `mod.rs`:
```c
static uint32_t _qb_palette[256];
```

### Problem 2: Undeclared `_qb_gfx_warn`

The `qb_memimage()` function in `memory.rs` called `_qb_gfx_warn()` before it was defined in `graphics.rs`.

**Solution:** Added forward declaration:
```c
static void _qb_gfx_warn(void);
```

### Problem 3: Missing Network I/O Stubs

File I/O functions in `file.rs` referenced network functions that were only declared as `extern` but never defined.

**Solution:** Added stub implementations in `system.rs`:
- `qb_net_get()` - Returns 0 bytes
- `qb_net_put()` - Returns 0 bytes
- `qb_net_get_string()` - No-op
- `qb_net_put_string()` - No-op
- `qb_net_eof()` - Returns -1 (EOF)
- `qb_net_lof()` - Returns 0
- `qb_net_close()` - No-op

### Files Modified (Continuation)

1. `src/codegen/c_backend/runtime/mod.rs` - Added `emit_forward_declarations()`
2. `src/codegen/c_backend/runtime/legacy.rs` - Removed duplicate `_qb_palette` declaration
3. `src/codegen/c_backend/runtime/system.rs` - Added network I/O stubs
4. `tests/golden/*.golden` - Updated expected C output

## Final Testing

- All 390 QB64Fresh unit tests pass
- All 27 execution tests pass
- All 10 golden tests pass
- 67 integration test failures are pre-existing (unimplemented language features)

## Related Issues

- Graphics initialization blocking issue (item 2 from task list)
- Stub functions returning 0/null causing infinite loops
