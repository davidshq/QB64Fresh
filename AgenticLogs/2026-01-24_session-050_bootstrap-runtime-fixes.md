# Session 050: Bootstrap Runtime Fixes

**Date:** 2026-01-24
**Focus:** Fix runtime issues blocking QB64pe compilation to executable

## Summary

This session addressed multiple runtime stub issues that were preventing the QB64pe-generated C code from compiling successfully. The executable now compiles but still hangs at runtime - this is expected since QB64pe's IDE initialization requires more runtime features than currently implemented.

## Key Accomplishments

### 1. Fixed Graphics Frame Counter Initialization

**Problem:** `qb_gfx_screen()` was a complete no-op that didn't initialize the frame counter, causing infinite loops in game-style code.

**Solution:** Updated `system.rs:705-710` to call `_qb_gfx_warn()`, `_qb_gfx_init_max_frames()`, and reset `_qb_gfx_frame_count = 0`.

**Also fixed:** Added frame counter increment to `qb_gfx_cls()` for more robust loop termination.

### 2. Added Forward Declarations for Cross-Module Dependencies

**Problem:** `qb_gfx_screen` in system.rs referenced `_qb_gfx_frame_count` and `_qb_gfx_init_max_frames` defined in graphics.rs, but system.rs was emitted first.

**Solution:** Added forward declarations in `emit_forward_declarations()` in mod.rs.

### 3. Added Missing Runtime Constants and Variables

| Symbol | Type | Value | Purpose |
|--------|------|-------|---------|
| `_CONSOLE` | Macro | -1 | Console destination handle |
| `_INCLERRORLINE` | Variable | 0 | Error line in include files |
| `_INCLERRORFILE_str` | Variable | NULL | Include file with error |
| `_STATUSCODE` | Array[256] | 200 | HTTP status codes (workaround for legacy array access pattern) |
| `_MAPUNICODE` | Macro | _qb_unicode_map | Alias for direct array access |

### 4. Fixed STRIG Event Handling

**Problem:** Generated code referenced `_qb_strig_event_id` variable and `_qb_strig_dispatch` label that didn't exist.

**Solution:**
- Added `static uint32_t _qb_strig_event_id = 0;` in legacy.rs
- Added STRIG dispatch labels to SUB and FUNCTION definitions in definitions.rs

### 5. Fixed Duplicate Label Emission

**Problem:** Labels like `Help_CheckFinishLine:` appeared 5 times due to ambiguous parsing of `SubName: AnotherSub` patterns.

**Solution:** Added `emitted_labels` HashSet to StmtEmitter to track and skip duplicate labels.

### 6. Added _STATUSCODE to Semantic Builtins

Registered `_STATUSCODE` as a builtin function (Long → Long) for future compilations.

## Results

- **Compilation:** ✅ QB64pe C code now compiles with 0 errors
- **Executable size:** 3.6 MB
- **Runtime:** ⚠️ Hangs after "INIT: args"

## Why the Hang

QB64pe's startup initializes the IDE subsystem even in `-c` mode. The hang is likely due to:
1. Missing proper keyboard input handling (termios/polling)
2. Missing file I/O that the IDE configuration needs
3. Event loops using `_LIMIT` that don't have the graphics-based exit condition

## Files Modified

- `src/codegen/c_backend/runtime/system.rs` - Graphics screen init, MAPUNICODE alias, STATUSCODE array
- `src/codegen/c_backend/runtime/mod.rs` - Forward declarations, _CONSOLE constant
- `src/codegen/c_backend/runtime/graphics.rs` - CLS frame counter
- `src/codegen/c_backend/runtime/error.rs` - INCLERRORLINE, INCLERRORFILE variables
- `src/codegen/c_backend/runtime/legacy.rs` - STRIG event ID variable
- `src/codegen/c_backend/stmt/definitions.rs` - STRIG dispatch labels in SUB/FUNCTION
- `src/codegen/c_backend/stmt/mod.rs` - Duplicate label tracking
- `src/semantic/builtins.rs` - _STATUSCODE function registration

## Next Steps for Full Bootstrap

1. **Input handling:** Implement proper keyboard polling with termios
2. **Console I/O:** May need working INKEY$ and INPUT that don't block
3. **File discovery:** QB64pe checks for `internal/version.txt` and other config files
4. **Consider minimal runtime:** May need a "bootstrap mode" that skips IDE initialization

## Metrics

- QB64pe compilation: Preprocess 46ms → Lexer 65ms → Parser 64ms → Semantic 250ms → CodeGen 600ms → Total ~1s
- C compilation: ~30s with GCC -O0
- Bootstrap test: ✅ Passing
