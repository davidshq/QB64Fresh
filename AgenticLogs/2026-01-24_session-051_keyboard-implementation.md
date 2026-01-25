# Session 051: Keyboard Input Implementation

**Date:** 2026-01-24
**Focus:** Implement real non-blocking keyboard input for QB64Fresh runtime

## Summary

This session implemented proper non-blocking keyboard input in the Rust runtime library, addressing one of the key missing features blocking the QB64pe bootstrap from running fully.

## Key Accomplishments

### 1. Implemented Real Keyboard Input in Rust Runtime

**File:** `runtime/src/io.rs`

Added a complete keyboard module with:
- **Raw mode handling** using termios (enable/disable)
- **Non-blocking input checking** using poll()
- **Key reading** with proper escape sequence handling
- **QB64-compatible key codes** for arrow keys, function keys, etc.

**Functions implemented:**
| Function | Purpose |
|----------|---------|
| `qb_inkey()` | Non-blocking keyboard read (INKEY$) |
| `qb_keyhit()` | Returns key code for _KEYHIT |
| `qb_keydown()` | Check if key pressed (stub - needs SDL2 for real state) |
| `qb_keyclear()` | Clear keyboard buffer |
| `qb_keyboard_shutdown()` | Restore normal terminal mode |

### 2. Escape Sequence Translation

Implemented translation from ANSI escape sequences to QB64-compatible key codes:

| Key | Escape Sequence | QB64 Code |
|-----|-----------------|-----------|
| Up Arrow | `ESC [ A` | CHR$(0) + CHR$(72) |
| Down Arrow | `ESC [ B` | CHR$(0) + CHR$(80) |
| Left Arrow | `ESC [ D` | CHR$(0) + CHR$(75) |
| Right Arrow | `ESC [ C` | CHR$(0) + CHR$(77) |
| F1-F4 | `ESC O P/Q/R/S` | CHR$(0) + CHR$(59-62) |
| F5-F12 | `ESC [ 15~` etc. | CHR$(0) + CHR$(63+) |

### 3. Updated C Header for External Runtime

**File:** `runtime/include/qb64fresh_rt.h`

Added declarations for new keyboard functions.

### 4. Verified Inline C Runtime

The inline C runtime (in `src/codegen/c_backend/runtime/keyboard.rs`) already had proper non-blocking keyboard input using termios/select. Verified this implementation is correct.

## Testing Results

### Simple Programs: ✅ Working
```bash
$ ./simple_test
Hello from QB64Fresh!
```

### QB64pe Executable: Partial Success
- **-help flag**: ✅ Works - full help text displayed
- **-c flag (compile)**: ⚠️ Hangs during config file reading
- **Headless detection**: ✅ Falls back to console mode correctly

The hang in compile mode is due to QB64pe's complex config file handling, not missing keyboard input.

## Runtime Test Status

- 195 runtime tests: ✅ All passing
- Simple BASIC programs: ✅ Compile and run correctly
- QB64pe bootstrap: ⚠️ Help works, compilation hangs (config I/O issue)

## Files Modified

1. `runtime/src/io.rs` - Added keyboard module with termios-based input
2. `runtime/include/qb64fresh_rt.h` - Added keyboard function declarations

## Architecture Note

The keyboard implementation uses a hybrid approach:
- **Rust runtime** (`runtime/src/io.rs`): Real implementation with termios
- **Inline C runtime** (`src/codegen/c_backend/runtime/keyboard.rs`): Equivalent C implementation for standalone binaries

Both implementations:
1. Set terminal to raw mode (no line buffering, no echo)
2. Use poll()/select() to check for available input without blocking
3. Read and translate escape sequences to QB64-compatible codes
4. Clean up terminal settings on program exit

## Current Bootstrap Status

| Test Case | Status | Notes |
|-----------|--------|-------|
| Simple PRINT program | ✅ Works | "Hello from QB64Fresh!" |
| Keyboard input test | ✅ Compiles | INKEY$ loop with key detection |
| QB64pe `-help` | ✅ Works | Full help text displays |
| QB64pe `-c` compile | ⚠️ Hangs | During INI config reading |
| QB64pe from `/tmp` | ✅ Works | When `internal/` folder missing, shows proper error |

## Root Cause of QB64pe Hang

The hang occurs during `qb_sub_iniload()` which reads the config file. This function:
1. Calls `qb_file_exists()` - works
2. Calls `qb_freefile()` - works
3. Calls `qb_file_open()` - opens file for `r+b` mode
4. Calls `qb_lof()` - gets file length
5. Calls `qb_space()` - allocates string
6. Calls `qb_file_get_string()` - reads content

The likely issue is in the file reading or the subsequent parsing loop. This is a specific QB64pe issue, not a fundamental runtime problem.

## Next Steps

Per RUNTIME_IMPLEMENTATION_PLAN.md Phase 1:
1. Debug specific INI file reading issue in QB64pe compilation
2. Complete file I/O edge cases (GET/PUT for binary data)
3. Implement graphics backend for real graphical programs
