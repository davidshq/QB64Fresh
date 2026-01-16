# Session 004: Code Generation Implementation

**Date:** 2026-01-16
**Focus:** Implementing code generation - C backend for the typed IR

## Summary

Successfully implemented Phase 3.4 of the project plan: the code generation module that transforms typed IR into C source code. **Major milestone achieved: QB64Fresh can now compile BASIC code to working executables!**

## What Was Accomplished

### New Module: `src/codegen/`

Created 3 new files (~1,300 lines total):

| File | Lines | Purpose |
|------|-------|---------|
| `mod.rs` | ~100 | `CodeGenerator` trait, `GeneratedOutput`, module exports |
| `error.rs` | ~90 | `CodeGenError` types with spans and context |
| `c_backend.rs` | ~1,100 | Full C code generation from typed IR |

### Key Features Implemented

1. **CodeGenerator Trait Interface**
   - Backend-agnostic design allows future LLVM/Cranelift implementations
   - Returns `GeneratedOutput` with code, format metadata, and file extension

2. **C Backend Complete Coverage**
   - All expression types: literals, variables, binary/unary ops, function calls, arrays, conversions
   - All statement types: assignment, PRINT, INPUT, IF/ELSEIF/ELSE, SELECT CASE, FOR, WHILE, DO LOOP, GOTO, GOSUB, EXIT, END
   - SUB/FUNCTION definitions with parameter handling
   - DIM declarations (variables and arrays)
   - CONST declarations
   - Labels and comments

3. **Inline Runtime Library**
   - String type (`qb_string`) with dynamic allocation
   - String operations: new, free, concat, compare
   - PRINT functions for int, float, string
   - INPUT functions for all types
   - Built-in function stubs (LEN, CHR$, ASC)

4. **CLI Integration**
   - New `--emit-c` flag generates C code
   - New `--typed-ir` flag shows semantic analysis output
   - Output file defaults to input.c or uses `-o` flag

## Design Decisions

### 1. Type Mapping

| BASIC Type | C Type |
|------------|--------|
| INTEGER | int16_t |
| LONG | int32_t |
| _INTEGER64 | int64_t |
| SINGLE | float |
| DOUBLE | double |
| STRING | qb_string* |

### 2. Inline Runtime Library

Rather than requiring a separate runtime library file, the C backend embeds the minimal runtime directly in each generated file. This makes the output self-contained and easy to compile:

```bash
gcc output.c -o output -lm
```

### 3. BASIC Identifier Handling

BASIC allows characters like `$`, `%` in variable names. These are converted to valid C identifiers:
- `name$` → `name_str`
- `count%` → `count_int`

## Test Results

- **83 tests passing** (13 new codegen tests + 70 existing)
- All existing lexer, parser, and semantic tests still pass
- No compilation warnings

## End-to-End Test

Successfully compiled and ran a BASIC program:

**Input (simple.bas):**
```basic
PRINT "Hello, World!"
DIM x AS LONG
x = 42
PRINT "x = "; x
FOR i = 1 TO 3
    PRINT "Count: "; i
NEXT i
IF x > 10 THEN
    PRINT "x is greater than 10"
END IF
END
```

**Output:**
```
Hello, World!
x = 42
Count: 1
Count: 2
Count: 3
x is greater than 10
```

## Files Modified

- `src/lib.rs` - Added `pub mod codegen;` and prelude exports
- `src/main.rs` - Full pipeline integration with new flags

## Files Created

- `src/codegen/mod.rs`
- `src/codegen/error.rs`
- `src/codegen/c_backend.rs`
- `examples/simple.bas` (test program)

## Known Limitations / Future Work

1. **Memory management** - Generated code doesn't free strings (needs GC or manual tracking)
2. **Type conversion strictness** - Semantic analyzer rejects some valid BASIC narrowing conversions
3. **GOSUB/RETURN** - Currently treated as function calls (needs proper continuation support)
4. **Power operator (^)** - Needs special handling to call pow()

## Next Steps (For Future Sessions)

- Phase 3.5: Runtime Library (external, more complete)
- Phase 3.6: LSP Server
- Fix semantic analyzer to allow BASIC-style implicit narrowing conversions
- Add more built-in functions to runtime

## Milestone Summary

**QB64Fresh now has a complete compilation pipeline:**
```
Source (.bas) → Lexer → Parser → AST → Semantic Analysis → Typed IR → C Backend → .c file
```

The generated C code compiles with standard gcc/clang and produces working executables.
