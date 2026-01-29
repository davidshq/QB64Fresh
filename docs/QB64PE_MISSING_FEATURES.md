# Missing Language Features in QB64Fresh (Used by QB64pe)

**Last Updated:** 2026-01-29  
**Purpose:** Document language features that QB64pe uses but are not yet implemented or fully supported in QB64Fresh. This helps prioritize development and understand compatibility gaps.

**Note:** This document is updated to reflect the current codebase status as of 2026-01-29. Many features previously marked as "not implemented" have since been added.

**Related Documents:**
- [BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md](BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md) — Items that are tricky to implement
- [PARTIAL_IMPLEMENTATIONS.md](ThingsToDo/PARTIAL_IMPLEMENTATIONS.md) — Partially implemented features
- [QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md](QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md) — Behavioral differences
- [QB64PE_MISSING_FEATURES_USER_INTERACTION.md](QB64PE_MISSING_FEATURES_USER_INTERACTION.md) — Features requiring user decisions/interaction

---

## Summary

When compiling QB64pe's main source file (`source/qb64pe.bas`, ~24,700 lines), QB64Fresh successfully:
- ✅ Parses the entire file without errors
- ✅ Generates C code that compiles
- ✅ Links with the runtime library

However, the resulting executable hangs (100% CPU, ~4GB memory) because QB64pe uses many advanced features that are either:
1. **Not implemented** — Missing from QB64Fresh
2. **Partially implemented** — Parsed but not fully codegen'd
3. **Implemented differently** — Works but may have behavioral differences

This document catalogs the missing features based on analysis of `qb64pe.bas` and related source files.

---

## 1. Preprocessor Directives / Metacommands

### 1.1 Fully Missing Metacommands

| Metacommand | Usage in QB64pe | Status | Notes |
|-------------|-----------------|--------|-------|
| `$COLOR:0` | Used for syntax highlighting | ✅ Parsed | Generates comment in codegen (runtime behavior may differ) |
| `$COLOR:32` | Used for syntax highlighting | ✅ Parsed | Generates comment in codegen (runtime behavior may differ) |
| `$ASSERTS` | Debug assertions | ✅ Implemented | Emits C code to enable assertions |
| `$ASSERTS:CONSOLE` | Console assertions | ✅ Implemented | Emits C code to enable console assertions |
| `$NOPREFIX` | Deprecated feature flag | ✅ Parsed | Generates comment in codegen |
| `$USELIBRARY:'author/library'` | Library loading system | ✅ Implemented | Parsed, library files included during preprocessing (see [QB64PE_MISSING_FEATURES_USER_INTERACTION.md](QB64PE_MISSING_FEATURES_USER_INTERACTION.md)) |
| `$INCLUDEONCE` | Prevent duplicate includes | ✅ Implemented | Tracks included files and skips duplicates |
| `$DYNAMIC` | Dynamic array allocation | ✅ Parsed | Generates comment in codegen (currently all arrays are dynamic) |
| `$STATIC` | Static array allocation | ✅ Parsed | Generates comment in codegen (static arrays not yet supported) |
| `$EMBED:'filename'` | Embed binary files | ✅ Implemented | Parsed, files collected, codegen emits embedded data as C arrays (see [QB64PE_MISSING_FEATURES_USER_INTERACTION.md](QB64PE_MISSING_FEATURES_USER_INTERACTION.md)) |
| `$MIDISOUNDFONT:'file.sf2'` | MIDI soundfont | ⚠️ Parsed but not implemented | Deprecated in favor of `_MIDISOUNDBANK` |
| `$UNSTABLE:feature` | Unstable feature flag | ⚠️ Parsed but not implemented | Feature gating |
| `$FORMAT` | Code formatting directive | ⚠️ Parsed but not implemented | Formatting control |

### 1.2 Partially Implemented Metacommands

| Metacommand | QB64Fresh Status | QB64pe Usage | Gap |
|-------------|------------------|--------------|-----|
| `$VERSIONINFO:key=value` | ✅ **FULLY IMPLEMENTED** | Multiple keys (CompanyName, FileDescription, etc.) | ✅ Generates `.rc`, `manifest.h`, `.manifest` files |
| `$EXEICON:'filename'` | ✅ **FULLY IMPLEMENTED** | Icon file path | ✅ Generates `.rc` file with icon entry |
| `$CONSOLE` | ✅ Parsed | Console window support | ⚠️ Runtime behavior may differ |
| `$CONSOLE:ONLY` | ✅ Parsed | Console-only mode | ⚠️ Runtime behavior may differ |
| `$SCREENHIDE` | ✅ Parsed | Hide graphics window | ⚠️ Runtime behavior may differ |
| `$DEBUG` | ✅ Parsed | Debug mode | ⚠️ Debug info emission not implemented |
| `$ERROR message` | ✅ Parsed | Compile-time error | ⚠️ May not halt compilation correctly |

### 1.3 Conditional Compilation

| Feature | QB64Fresh Status | QB64pe Usage | Gap |
|---------|------------------|--------------|-----|
| `$IF condition THEN` | ✅ Implemented | Extensive use (93 matches) | ⚠️ Condition evaluation may differ |
| `$ELSEIF condition THEN` | ✅ Implemented | Used in conditionals | ✅ Works |
| `$ELSE` | ✅ Implemented | Used in conditionals | ✅ Works |
| `$END IF` / `$ENDIF` | ✅ Implemented | Used in conditionals | ✅ Works |
| `$LET variable = value` | ✅ Implemented | Compile-time variables | ⚠️ Variable scope/visibility may differ |
| `$CHECKING:ON/OFF` | ✅ Implemented | Type checking control | ⚠️ May not fully disable checks |

**Note:** QB64pe uses `$IF` extensively for platform detection (WINDOWS, LINUX, MAC, 32BIT, 64BIT, etc.) and feature flags. The condition evaluation in QB64Fresh may not match QB64pe's logic exactly.

---

## 2. Control Flow Features

### 2.1 GOTO and Labels

| Feature | QB64pe Usage | QB64Fresh Status | Gap |
|---------|--------------|-------------------|-----|
| `GOTO label` | **1,687 matches** | ✅ Implemented | ✅ Works |
| `GOSUB label` | Used extensively | ✅ Implemented | ✅ Works |
| `RETURN` (from GOSUB) | Used with GOSUB | ✅ Implemented | ✅ Works |
| Line number labels | 0 matches (QB64pe uses named labels) | ⚠️ Partially supported | Line numbers not used in QB64pe |
| Named labels | 17 matches | ✅ Implemented | ✅ Works |

**Note:** QB64pe uses GOTO/GOSUB extensively (1,687 matches), but all are to named labels, not line numbers. QB64Fresh supports this.

### 2.2 ON Statements

| Feature | QB64pe Usage | QB64Fresh Status | Gap |
|---------|--------------|-------------------|-----|
| `ON ... GOTO` | 93 matches | ✅ Implemented | ✅ Fully implemented (parser, AST, codegen with switch statements) |
| `ON ... GOSUB` | Used | ✅ Implemented | ✅ Fully implemented (parser, AST, codegen with switch statements) |
| `ON ERROR GOTO label` | 11 matches | ✅ Implemented | ✅ Works (including `_NEWHANDLER`) |
| `ON ERROR GOTO 0` | Used | ✅ Implemented | ✅ Works |
| `ON KEY ... GOTO` | Used | ✅ Implemented | ✅ Fully implemented (parser, AST, codegen, runtime) |
| `ON TIMER ... GOTO` | Used | ✅ Implemented | ✅ Fully implemented (parser, AST, codegen, runtime) |
| `ON UEVENT ... GOTO` | Used | ✅ Implemented | ✅ Fully implemented (parser, AST, codegen, runtime) |
| `ON STRIG ... GOTO` | Used | ✅ Implemented | ✅ Fully implemented (parser, AST, codegen, runtime) |
| `ON COM ... GOTO` | Used | ⚠️ Codegen only | Parser, AST, and codegen implemented; runtime functions not implemented |
| `ON PEN ... GOTO` | Used | ⚠️ Codegen only | Parser, AST, and codegen implemented; runtime functions not implemented |

**Note:** QB64pe uses `ON ERROR GOTO` extensively (11 matches) for error handling. QB64Fresh supports this, including the `_NEWHANDLER` extension.

### 2.3 Error Handling

| Feature | QB64pe Usage | QB64Fresh Status | Gap |
|---------|--------------|-------------------|-----|
| `ON ERROR GOTO label` | 11 matches | ✅ Implemented | ✅ Works |
| `RESUME` | Used | ⚠️ Partially implemented | May not match QB64pe behavior exactly |
| `RESUME NEXT` | Used | ⚠️ Partially implemented | May not match QB64pe behavior exactly |
| `RESUME label` | Used | ⚠️ Partially implemented | May not match QB64pe behavior exactly |
| `ERL` | 60 matches | ⚠️ Partially implemented | Error line number |
| `ERR` / `ERROR` | Used | ⚠️ Partially implemented | Error code |
| `_ERRORLINE` | Used | ✅ Implemented | QB64 extension (returns error line number) |
| `_ERRORMESSAGE$` | Used | ✅ Implemented | QB64 extension (returns error message string) |

---

## 3. Type System Features

### 3.1 DEFTYPE Statements

| Feature | QB64pe Usage | QB64Fresh Status | Gap |
|---------|--------------|-------------------|-----|
| `DEFLNG A-Z` | 3 matches | ✅ Implemented | Default type for variables without suffix |
| `DEFINT A-Z` | Used | ✅ Implemented | Default type for INTEGER |
| `DEFSNG A-Z` | Used | ✅ Implemented | Default type for SINGLE |
| `DEFDBL A-Z` | Used | ✅ Implemented | Default type for DOUBLE |
| `DEFSTR A-Z` | Used | ✅ Implemented | Default type for STRING |
| `DEFLNG A-M` | Range syntax | ✅ Implemented | Type for specific letter ranges |

**Note:** QB64pe uses `DEFLNG A-Z` extensively, meaning all variables without type suffixes default to LONG. QB64Fresh now fully implements DEFTYPE, so this should work correctly.

### 3.2 Extended Types

| Type | QB64pe Usage | QB64Fresh Status | Gap |
|------|--------------|-------------------|-----|
| `_BYTE` | 106 matches | ✅ Implemented | ✅ Works |
| `_UNSIGNED _BYTE` | Used | ✅ Implemented | ✅ Works |
| `_INTEGER64` | Used | ✅ Implemented | ✅ Works |
| `_UNSIGNED _INTEGER64` | Used | ✅ Implemented | ✅ Works |
| `_OFFSET` | Used | ✅ Implemented | ✅ Works |
| `_UNSIGNED _OFFSET` | Used | ✅ Implemented | ✅ Works |
| `_BIT` | Used | ✅ Implemented | ✅ Works |
| `_UNSIGNED _BIT` | Used | ✅ Implemented | ✅ Works |
| `_FLOAT` | Used | ✅ Implemented | ✅ Works |

**Note:** All extended QB64 types are supported in QB64Fresh.

---

## 4. Array Features

| Feature | QB64pe Usage | QB64Fresh Status | Gap |
|---------|--------------|-------------------|-----|
| Dynamic arrays | Used extensively | ✅ Implemented | ✅ Works |
| `REDIM` | Used | ✅ Implemented | ✅ Works |
| `REDIM PRESERVE` | Used | ✅ Implemented | ✅ Works |
| `REDIM SHARED` | Used | ✅ Implemented | ✅ Works |
| `OPTION BASE 0/1` | Used | ✅ Implemented | ✅ Works |
| Multi-dimensional arrays | Used | ✅ Implemented | ✅ Works |
| `LBOUND` / `UBOUND` | Used | ✅ Implemented | Array bounds tracking with runtime functions |
| Static arrays (`$STATIC`) | Not used in QB64pe | ✅ Implemented | ✅ `$STATIC` directive parsed, static arrays emit fixed-size C arrays |

---

## 5. String Features

| Feature | QB64pe Usage | QB64Fresh Status | Gap |
|---------|--------------|-------------------|-----|
| Dynamic strings | Used extensively | ✅ Implemented | ✅ Works |
| Fixed-length strings (`STRING * N`) | Used | ✅ Implemented | ✅ Works |
| String concatenation (`+` or `&`) | Used extensively | ✅ Implemented | ✅ Works |
| `MID$` assignment | Used | ✅ Implemented | ✅ Works |
| `ASC` assignment | Used | ✅ Implemented | ✅ Works |
| String functions | Used extensively | ✅ Mostly implemented | Some edge cases may differ |

**Note:** String handling is well-implemented in QB64Fresh, including the complex temp pool system for managing string lifetimes.

---

## 6. Built-in Functions and Statements

### 6.1 Graphics Functions

QB64pe uses many graphics functions that may not be fully implemented in QB64Fresh. See [GRAPHICS.md](GRAPHICS.md) for details.

### 6.2 System Functions

| Function | QB64pe Usage | QB64Fresh Status | Gap |
|----------|--------------|-------------------|-----|
| `_OS$` | Used for platform detection | ✅ Implemented | Platform string (returns [PLATFORM][BITS] format) |
| `_DIREXISTS` | Used | ✅ Implemented | Directory existence check |
| `_SCREENSHOW` | Used | ✅ Implemented | Show graphics window |
| `TIMER(0.001)` | Used | ⚠️ May not match precision | High-precision timer |
| `INSTR` | Used extensively | ✅ Implemented | ✅ Works |
| `CHR$` / `ASC` | Used extensively | ✅ Implemented | ✅ Works |
| `MKI$` / `CVI` | Used | ✅ Implemented | Binary string conversion (MKI$, MKL$, MKS$, MKD$, CVI, CVL, CVS, CVD) |
| `Version$` | Used | ✅ Implemented | Compiler version string (returns "QB64Fresh 0.1.0") |

### 6.3 File I/O

| Feature | QB64pe Usage | QB64Fresh Status | Gap |
|---------|--------------|-------------------|-----|
| `OPEN` | Used extensively | ✅ Implemented | ✅ Works |
| `CLOSE` | Used | ✅ Implemented | ✅ Works |
| `GET` / `PUT` | Used | ✅ Implemented | ✅ Works |
| `SEEK` | Used | ✅ Implemented | ✅ Works |
| `FIELD` | Used | ✅ Implemented | Random access file fields |
| `LSET` / `RSET` | Used | ✅ Implemented | Field buffer alignment |

---

## 7. Procedure Features

| Feature | QB64pe Usage | QB64Fresh Status | Gap |
|---------|--------------|-------------------|-----|
| `SUB` / `FUNCTION` | Used extensively | ✅ Implemented | ✅ Works |
| `CALL` | Used | ✅ Implemented | ✅ Works |
| `BYVAL` parameters | Used | ✅ Implemented | ✅ Works |
| `BYREF` parameters | Used extensively | ✅ Implemented | ✅ Works (including scalar fix) |
| `STATIC` variables | Used | ✅ Implemented | ✅ Works |
| `SHARED` variables | Used | ✅ Implemented | ✅ Works |
| Recursive procedures | Used | ✅ Implemented | ✅ Works |
| `EXIT SUB` / `EXIT FUNCTION` | Used | ✅ Implemented | ✅ Works |
| `RETURN` (from FUNCTION) | Used | ✅ Implemented | ✅ Works |

---

## 8. Runtime Behavior Differences

Even when features are "implemented," QB64Fresh may behave differently from QB64pe:

1. **String memory management** — QB64Fresh uses a temp pool system; QB64pe may use different allocation
2. **Error handling** — ON ERROR / RESUME behavior may differ in edge cases
3. **Graphics** — SDL2-based vs QB64pe's OpenGL/freeglut
4. **Audio** — Rodio-based vs QB64pe's miniaudio
5. **Random number generation** — Different PRNG algorithms
6. **Timer precision** — Platform-dependent differences

See [QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md](QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md) for details.

---

## 9. Why QB64pe Hangs

Based on the analysis, the most likely causes for QB64pe hanging when compiled with QB64Fresh:

1. ~~**Missing `DEFLNG A-Z`**~~ — ✅ **FIXED** — QB64pe assumes all variables without suffixes are LONG
2. ~~**Missing `_OS$` and platform detection**~~ — ✅ **FIXED** — QB64pe uses `$IF` blocks based on `_OS$`
3. ~~**Missing `Version$`**~~ — ✅ **FIXED** — Used in initialization
4. ~~**Event trapping runtime not implemented**~~ — ✅ **FIXED** — `ON KEY`, `ON TIMER`, `ON UEVENT`, `ON STRIG` fully implemented with runtime support
5. ~~**Graphics initialization**~~ — ✅ **IMPLEMENTED** — Graphics initialization via `init_graphics()` function in runtime
6. ~~**File I/O for internal files**~~ — ✅ **IMPLEMENTED** — `resolve_file_path()` handles `internal/` directory relative to executable or CWD
7. **Missing `ON COM` and `ON PEN` runtime** — Parser, AST, and codegen implemented; runtime functions not implemented

---

## 10. Priority for Implementation

To make QB64pe compile and run, prioritize:

### High Priority (Blocks Execution)
1. ~~**`DEFLNG A-Z` and DEFTYPE**~~ — ✅ **FIXED** — Critical for variable type inference
2. ~~**`_OS$` built-in**~~ — ✅ **FIXED** — Required for platform detection in `$IF` blocks
3. ~~**`Version$` built-in**~~ — ✅ **FIXED** — Used in initialization
4. ~~**`$USELIBRARY` implementation**~~ — ✅ **IMPLEMENTED** — Library system with AtTop/AfterMain/AtBottom inclusion (see [QB64PE_MISSING_FEATURES_USER_INTERACTION.md](QB64PE_MISSING_FEATURES_USER_INTERACTION.md))
5. ~~**Graphics initialization**~~ — ✅ **IMPLEMENTED** — Graphics initialization via `init_graphics()` function in runtime

### Medium Priority (Causes Incorrect Behavior)
1. ~~**Event trapping runtime (`ON KEY`, `ON TIMER`, `ON UEVENT`, `ON STRIG`)**~~ — ✅ **IMPLEMENTED** — Fully implemented with runtime support (see [QB64PE_MISSING_FEATURES_USER_INTERACTION.md](QB64PE_MISSING_FEATURES_USER_INTERACTION.md))
2. ~~**`$INCLUDEONCE`**~~ — ✅ **FIXED** — Prevents duplicate includes
3. ~~**`$EMBED`**~~ — ✅ **IMPLEMENTED** — Embedded files collected and emitted as C arrays (see [QB64PE_MISSING_FEATURES_USER_INTERACTION.md](QB64PE_MISSING_FEATURES_USER_INTERACTION.md))
4. ~~**`_DIREXISTS` and file system functions**~~ — ✅ **FIXED** — Used for internal folder checks
5. **`ON COM` and `ON PEN` runtime** — Parser, AST, and codegen implemented; runtime functions (`qb_on_com`, `qb_on_pen`) not implemented

### Low Priority (Nice to Have)
1. ~~**`$COLOR` directives**~~ — ✅ **PARSED** — Generates comments in codegen (runtime behavior may differ)
2. ~~**`$ASSERTS`**~~ — ✅ **IMPLEMENTED** — Emits C code to enable assertions
3. ~~**`$VERSIONINFO` / `$EXEICON`**~~ — ✅ **IMPLEMENTED** — Windows resource generation (generates `.rc`, `manifest.h`, `.manifest` files)
4. ~~**`$NOPREFIX`**~~ — ✅ **PARSED** — Generates comment in codegen

---

## 11. Testing Strategy

To verify QB64pe compatibility:

1. **Start with simpler QB64pe programs** — Don't try to compile the full IDE first
2. **Test individual features** — Create minimal test cases for each missing feature
3. **Compare behavior** — Run same program in QB64pe and QB64Fresh, compare output
4. **Incremental compilation** — Compile QB64pe modules one at a time
5. **Runtime debugging** — Use debugger to see where QB64pe hangs

---

## 12. References

- QB64pe source: `/home/dave/repos/qb64contain/QB64pe/source/qb64pe.bas`
- QB64Fresh parser: `src/parser/directives.rs`
- QB64Fresh semantic: `src/semantic/`
- QB64Fresh codegen: `src/codegen/c_backend/`
- Problematic items: [BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md](BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md)
