# Session 005: Runtime Library Implementation

**Date:** 2026-01-16
**Focus:** Implementing the runtime library for QB64Fresh

## Summary

Successfully implemented Phase 3.5: A complete Rust runtime library that compiles to a C-compatible static library. The code generator now supports both inline and external runtime modes.

## What Was Accomplished

### New Crate: `runtime/`

Created a complete Rust runtime library (~800 lines):

| File | Lines | Purpose |
|------|-------|---------|
| `Cargo.toml` | ~25 | Crate configuration (staticlib output) |
| `src/lib.rs` | ~70 | Crate root, runtime init/shutdown |
| `src/string.rs` | ~450 | Reference-counted string type |
| `src/io.rs` | ~200 | PRINT, INPUT, console functions |
| `src/math.rs` | ~250 | Mathematical functions |
| `include/qb64fresh_rt.h` | ~160 | C header for all runtime functions |

### Key Features

1. **Reference-Counted Strings**
   - `QbString` type with refcount, length, capacity
   - `qb_string_retain()` / `qb_string_release()` for memory management
   - All string functions: concat, compare, LEFT$, RIGHT$, MID$, INSTR, UCASE$, LCASE$, LTRIM$, RTRIM$, SPACE$, CHR$, ASC, STR$, VAL

2. **I/O Functions**
   - `qb_print_int()`, `qb_print_float()`, `qb_print_string()`
   - `qb_input_string()`, `qb_input_int()`, `qb_input_float()`
   - Console: `qb_cls()`, `qb_locate()`, `qb_color()` (ANSI escape codes)

3. **Math Functions**
   - Basic: ABS, SGN, INT, FIX, CINT, CLNG
   - Trig: SIN, COS, TAN, ATN, ASIN, ACOS, SINH, COSH, TANH
   - Exp/Log: SQR, LOG, LOG10, EXP, POW
   - Random: RANDOMIZE, RND (xorshift64 algorithm)
   - Timer: TIMER, SLEEP, DELAY

4. **Dual Runtime Modes**
   - `--runtime inline` (default): Embeds minimal runtime in generated C
   - `--runtime external`: Uses `#include "qb64fresh_rt.h"` and links against `libqb64fresh_rt.a`

### Build Artifacts

```
target/release/libqb64fresh_rt.a   # ~20MB static library
runtime/include/qb64fresh_rt.h      # C header file
```

## Test Results

- **104 tests passing** (83 compiler + 21 runtime)
- All unit tests for string operations, math functions, etc.
- End-to-end test with both runtime modes

## Usage Examples

### Inline Runtime (default)
```bash
cargo run -- program.bas --emit-c
gcc program.c -o program -lm
./program
```

### External Runtime
```bash
cargo run -- program.bas --emit-c --runtime external
gcc -I runtime/include program.c -L target/release -lqb64fresh_rt -o program -lm -lpthread -ldl
./program
```

## Design Decisions

### 1. Rust with C ABI

Chose Rust for the runtime (with `extern "C"`) because:
- Memory safety in runtime code
- Access to Rust ecosystem for future graphics/audio
- Single language for compiler + runtime
- Easy to test with Rust's built-in test framework

### 2. Reference Counting for Strings

BASIC strings are dynamic and can be freely copied. Reference counting allows:
- Efficient "copy" (just increment refcount)
- Automatic deallocation when refcount hits 0
- Compatible with C code (just pointers)

Future improvement: Copy-on-write for mutations.

### 3. Inline vs External Modes

- **Inline**: Self-contained .c files, no dependencies, great for simple programs
- **External**: Smaller generated code, full runtime features, required for graphics/audio

## Files Created

- `runtime/Cargo.toml`
- `runtime/src/lib.rs`
- `runtime/src/string.rs`
- `runtime/src/io.rs`
- `runtime/src/math.rs`
- `runtime/include/qb64fresh_rt.h`

## Files Modified

- `Cargo.toml` (added workspace)
- `src/codegen/mod.rs` (export RuntimeMode)
- `src/codegen/c_backend.rs` (RuntimeMode support)
- `src/lib.rs` (prelude update)
- `src/main.rs` (--runtime flag)

## Known Limitations / Future Work

1. **No graphics yet** - SDL2 integration planned
2. **No audio yet** - rodio integration planned
3. **No file I/O** - OPEN, CLOSE, INPUT#, PRINT# not implemented
4. **String memory leaks** - Generated code doesn't call `qb_string_release()` yet
5. **Large static lib** - Consider dynamic linking or LTO for production

## Next Steps (For Future Sessions)

- Phase 3.6: LSP Server
- Add graphics runtime (SDL2)
- Add file I/O functions
- Fix generated code to properly release strings
- Consider garbage collection for strings

## Milestone Summary

**QB64Fresh now has a complete compilation pipeline with proper runtime:**
```
Source (.bas) → Lexer → Parser → AST → Semantic Analysis → Typed IR → C Backend → .c file
                                                                            ↓
                                                          gcc + libqb64fresh_rt.a → executable
```
