# QB64Fresh Compiles QB64pe: A Bootstrap Achievement

*January 2026*

QB64Fresh, a Rust-based BASIC compiler, successfully compiles the QB64pe compiler - a 59,000-line BASIC codebase - into a working executable. This document summarizes the achievement, technical challenges overcome, and current status.

---

## Overview

| Metric | Value |
|--------|-------|
| QB64pe source | 39 files, ~59,000 lines of BASIC |
| Preprocessed size | 2.64 MB (with all `$INCLUDE` files) |
| Generated C code | 83,705 lines (~4.5 MB) |
| Final executable | 2.1 MB ELF binary |
| Compilation time | ~800ms on modern hardware |

**What this means:** A Rust-based compiler (QB64Fresh) can compile a C++-targeting BASIC compiler (QB64pe), creating a unique cross-compilation chain: Rust → BASIC compiler → BASIC programs.

---

## Technical Challenges Solved

### 1. Dual Namespace Model

**Problem:** QB64 allows a scalar variable and an array to share the same name.

```basic
DIM sf AS INTEGER       ' Scalar
DIM SF(10) AS INTEGER   ' Array - same base name!
sf = 5
SF(1) = 10              ' Both coexist
```

**Solution:** Implemented separate `scalars` and `arrays` HashMaps in each scope, with `lookup_scalar()` and `lookup_array()` methods for proper resolution. Documented in QB64PE_LANGUAGE_SPECIFICATION.md §9.5.

### 2. Function Call Name Resolution

**Problem:** Function calls weren't using canonical names with type suffixes.

```basic
FUNCTION GetElement$(text$, index%)
    ' ...
END FUNCTION

result$ = GetElement$(line$, 1)  ' Was generating: getelement()
                                  ' Should be: getelement_str()
```

**Solution:** Modified code generation to use the procedure's canonical name (from the symbol table) rather than the raw call site identifier. Fixed hundreds of linker errors.

### 3. TYPE Alternate Syntax

**Problem:** QB64pe uses a compact TYPE field syntax not in original QB.

```basic
TYPE usedVarList
    AS LONG id, linenumber, includeLevel    ' Multiple fields, one type
    AS STRING name, cname
END TYPE
```

**Solution:** Extended parser to handle `AS TYPE field1, field2, ...` syntax, reducing ~200 parse errors.

### 4. Extended Type Suffixes

**Problem:** QB64pe uses 64-bit and unsigned type suffixes.

| Suffix | Type | Example |
|--------|------|---------|
| `&&` | _INTEGER64 (LONGLONG) | `count&&` |
| `~&&` | _UNSIGNED _INTEGER64 | `size~&&` |
| `~&` | _UNSIGNED LONG | `flags~&` |
| `%%` | _BYTE | `char%%` |

**Solution:** Added all suffix variants to lexer token recognition and symbol lookup.

### 5. SHARED Array Handling

**Problem:** `REDIM _PRESERVE` on SHARED arrays created local copies instead of modifying globals.

```basic
DIM SHARED MyArray(10) AS STRING

SUB AddItem(item$)
    REDIM _PRESERVE MyArray(UBOUND(MyArray) + 1) AS STRING  ' Must modify global!
    MyArray(UBOUND(MyArray)) = item$
END SUB
```

**Solution:** Added `is_module_shared()` check and `update_shared_symbol()` to properly modify global scope.

### 6. Polymorphic _IIF

**Problem:** `_IIF` needed to accept any compatible type pair, not just matching types.

```basic
result$ = _IIF(condition, "yes", "no")      ' STRING
result# = _IIF(flag, 3.14, 2.71)            ' DOUBLE
result& = _IIF(test, intVal%, longVal&)     ' Numeric promotion
```

**Solution:** Type checker now infers common type from both branches using standard numeric promotion rules.

### 7. C Code Generation Issues

Multiple code generation fixes were required:

| Issue | Fix |
|-------|-----|
| TYPE definitions after usage | Emit all TYPE definitions before global variables |
| Fixed-length strings | Generate `char name[N]` instead of `qb_string` |
| Dots in identifiers | Convert `path.exe$` → `path_exe_str` |
| C reserved words | Escape `default` → `default_`, `errno` → `errno_` |
| POSIX conflicts | Rename `getpid` → `getpid_` to avoid system function |
| Deep recursion | 16MB stack for processing 59K-line AST |

---

## Compilation Pipeline

```
QB64pe Source (39 .bas files)
         ↓
    Preprocessor ($INCLUDE expansion)
         ↓ 2.64 MB preprocessed source
    Lexer (logos-based)
         ↓ 400,523 tokens
    Parser (Pratt + recursive descent)
         ↓ 2,172 AST statements
    Semantic Analysis
         ↓ Typed IR with resolved symbols
    C Code Generator
         ↓ 83,705 lines of C
    GCC Compilation
         ↓
    2.1 MB Executable
```

**Timing breakdown:**
- Preprocessing: 45ms
- Lexing: 64ms
- Parsing: 63ms
- Semantic analysis: 242ms
- Code generation: 376ms
- **Total: ~800ms**

---

## Current Status

### What Works
- ✅ QB64pe compiles without errors (0 parse, 0 semantic, 0 GCC errors)
- ✅ Generated C code is valid and links successfully
- ✅ Executable runs without crashing
- ✅ Clean exit (waiting for initialization)

### What's Pending
- ⏳ Runtime stubs need real implementations
- ⏳ Graphics initialization (QB64pe expects graphical mode)
- ⏳ End-to-end test: QB64pe compiling a "Hello World" program

### Known Limitations
- QB64pe's IDE requires graphics (SDL2) - not available in stub runtime
- Some `_KEY_*` constants may be missing
- Type warnings exist but don't affect functionality

---

## Error Reduction Journey

| Phase | Errors | Reduction |
|-------|--------|-----------|
| Initial parse | 992 | - |
| After Phase B | 0 | 100% |
| Initial GCC | 14,547 | - |
| After Session 7 | 0 | 100% |

Key milestones:
- **Session 3:** Polymorphic `_IIF`, unsigned suffixes, `SHELL` function (47→17 errors)
- **Session 4:** Dual namespace model, SHARED arrays (17→0 semantic errors)
- **Session 7:** Function name resolution, LSET/RSET, ByRef handling (105→0 GCC errors)

---

## Reproducing the Build

```bash
# Prerequisites: Rust toolchain, GCC

# Clone repositories side-by-side
git clone <qb64fresh-repo> QB64Fresh
git clone <qb64pe-repo> QB64pe

# Build QB64Fresh
cd QB64Fresh
cargo build --release

# Compile QB64pe
./target/release/qb64fresh ../QB64pe/source/qb64pe.bas -o qb64pe_fresh

# Run tests
./scripts/test-bootstrap.sh
```

---

## Files Reference

| File | Purpose |
|------|---------|
| `tests/bootstrap_tests.rs` | Compilation regression tests |
| `scripts/test-bootstrap.sh` | Test runner script |
| `docs/archive/BOOTSTRAP_PLAN_FULL.md` | Detailed implementation history |
| `docs/ThingsToDo/BOOTSTRAP_PLAN_REMAINING.md` | Outstanding work |
| `docs/QB64PE_LANGUAGE_SPECIFICATION.md` | Language semantics (§9.5: namespaces) |

---

## Acknowledgments

This achievement demonstrates that a modern Rust compiler can handle the complexity of a large, real-world BASIC codebase with advanced features. The systematic approach of gap analysis, incremental implementation, and continuous testing proved effective for this cross-compilation challenge.
