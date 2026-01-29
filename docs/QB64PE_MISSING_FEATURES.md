# Missing Language Features in QB64Fresh (Used by QB64pe)

**Last Updated:** 2026-01-28  
**Purpose:** Document language features that QB64pe uses but are not yet implemented or fully supported in QB64Fresh. This helps prioritize development and understand compatibility gaps.

**Related Documents:**
- [BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md](BASIC_TO_C_PROBLEMATIC_LANGUAGE_ITEMS.md) — Items that are tricky to implement
- [PARTIAL_IMPLEMENTATIONS.md](ThingsToDo/PARTIAL_IMPLEMENTATIONS.md) — Partially implemented features
- [QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md](QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md) — Behavioral differences

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
| `$COLOR:0` | Used for syntax highlighting | ❌ Not implemented | Color mode for IDE output |
| `$COLOR:32` | Used for syntax highlighting | ❌ Not implemented | Color mode for IDE output |
| `$ASSERTS` | Debug assertions | ❌ Not implemented | Enable assertion checking |
| `$ASSERTS:CONSOLE` | Console assertions | ❌ Not implemented | Assertions to console |
| `$NOPREFIX` | Deprecated feature flag | ❌ Not implemented | Legacy compatibility |
| `$USELIBRARY:'author/library'` | Library loading system | ⚠️ Parsed but not implemented | Complex library system with AtTop/AfterMain files |
| `$INCLUDEONCE` | Prevent duplicate includes | ⚠️ Parsed but not implemented | Needs tracking of included files |
| `$DYNAMIC` | Dynamic array allocation | ⚠️ Parsed but not implemented | Currently all arrays are dynamic |
| `$STATIC` | Static array allocation | ⚠️ Parsed but not implemented | Static arrays not supported |
| `$EMBED:'filename'` | Embed binary files | ⚠️ Parsed but not implemented | Requires binary embedding in C output |
| `$MIDISOUNDFONT:'file.sf2'` | MIDI soundfont | ⚠️ Parsed but not implemented | Deprecated in favor of `_MIDISOUNDBANK` |
| `$UNSTABLE:feature` | Unstable feature flag | ⚠️ Parsed but not implemented | Feature gating |
| `$FORMAT` | Code formatting directive | ⚠️ Parsed but not implemented | Formatting control |

### 1.2 Partially Implemented Metacommands

| Metacommand | QB64Fresh Status | QB64pe Usage | Gap |
|-------------|------------------|--------------|-----|
| `$VERSIONINFO:key=value` | ✅ Parsed | Multiple keys (CompanyName, FileDescription, etc.) | ⚠️ Not emitted to C (Windows resource file generation) |
| `$EXEICON:'filename'` | ✅ Parsed | Icon file path | ⚠️ Not emitted to C (Windows resource file generation) |
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
| `ON ... GOTO` | 93 matches | ⚠️ Partially implemented | May not handle all cases |
| `ON ... GOSUB` | Used | ⚠️ Partially implemented | May not handle all cases |
| `ON ERROR GOTO label` | 11 matches | ✅ Implemented | ✅ Works (including `_NEWHANDLER`) |
| `ON ERROR GOTO 0` | Used | ✅ Implemented | ✅ Works |
| `ON KEY ... GOTO` | Used | ❌ Not implemented | Event trapping |
| `ON TIMER ... GOTO` | Used | ❌ Not implemented | Timer events |
| `ON UEVENT ... GOTO` | Used | ❌ Not implemented | User events |
| `ON COM ... GOTO` | Used | ❌ Not implemented | Serial port events |
| `ON STRIG ... GOTO` | Used | ❌ Not implemented | Joystick events |
| `ON PEN ... GOTO` | Used | ❌ Not implemented | Light pen events |

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
| `_ERRORLINE` | Used | ❌ Not implemented | QB64 extension |
| `_ERRORMESSAGE$` | Used | ❌ Not implemented | QB64 extension |

---

## 3. Type System Features

### 3.1 DEFTYPE Statements

| Feature | QB64pe Usage | QB64Fresh Status | Gap |
|---------|--------------|-------------------|-----|
| `DEFLNG A-Z` | 3 matches | ❌ Not implemented | Default type for variables without suffix |
| `DEFINT A-Z` | Used | ❌ Not implemented | Default type for INTEGER |
| `DEFSNG A-Z` | Used | ❌ Not implemented | Default type for SINGLE |
| `DEFDBL A-Z` | Used | ❌ Not implemented | Default type for DOUBLE |
| `DEFSTR A-Z` | Used | ❌ Not implemented | Default type for STRING |
| `DEFLNG A-M` | Range syntax | ❌ Not implemented | Type for specific letter ranges |

**Note:** QB64pe uses `DEFLNG A-Z` extensively, meaning all variables without type suffixes default to LONG. QB64Fresh does not implement DEFTYPE, so variables must have explicit types or `AS` clauses.

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
| `LBOUND` / `UBOUND` | Used | ⚠️ Partially implemented | Inline runtime may not track bounds correctly |
| Static arrays (`$STATIC`) | Not used in QB64pe | ❌ Not implemented | Static arrays not supported |

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
| `_OS$` | Used for platform detection | ❌ Not implemented | Platform string |
| `_DIREXISTS` | Used | ⚠️ May not be implemented | Directory existence check |
| `_SCREENSHOW` | Used | ⚠️ May not be implemented | Show graphics window |
| `TIMER(0.001)` | Used | ⚠️ May not match precision | High-precision timer |
| `INSTR` | Used extensively | ✅ Implemented | ✅ Works |
| `CHR$` / `ASC` | Used extensively | ✅ Implemented | ✅ Works |
| `MKI$` / `CVI` | Used | ⚠️ May not be implemented | Binary string conversion |
| `Version$` | Used | ❌ Not implemented | Compiler version string |

### 6.3 File I/O

| Feature | QB64pe Usage | QB64Fresh Status | Gap |
|---------|--------------|-------------------|-----|
| `OPEN` | Used extensively | ✅ Implemented | ✅ Works |
| `CLOSE` | Used | ✅ Implemented | ✅ Works |
| `GET` / `PUT` | Used | ✅ Implemented | ✅ Works |
| `SEEK` | Used | ✅ Implemented | ✅ Works |
| `FIELD` | Used | ⚠️ May not be fully implemented | Random access file fields |
| `LSET` / `RSET` | Used | ⚠️ May not be fully implemented | Field buffer alignment |

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

1. **Missing `DEFLNG A-Z`** — QB64pe assumes all variables without suffixes are LONG; QB64Fresh may treat them differently, causing type mismatches
2. **Missing `_OS$` and platform detection** — QB64pe uses `$IF` blocks based on `_OS$`; if this isn't set correctly, wrong code paths may execute
3. **Missing `Version$`** — Used in initialization; undefined value may cause issues
4. **Event trapping not implemented** — `ON KEY`, `ON TIMER`, etc. may be called but not work, causing hangs
5. **Graphics initialization** — QB64pe's IDE requires graphics; if initialization fails silently, it may hang
6. **File I/O for internal files** — QB64pe reads from `internal/` directory; if paths are wrong, it may hang waiting for files

---

## 10. Priority for Implementation

To make QB64pe compile and run, prioritize:

### High Priority (Blocks Execution)
1. **`DEFLNG A-Z` and DEFTYPE** — Critical for variable type inference
2. **`_OS$` built-in** — Required for platform detection in `$IF` blocks
3. **`Version$` built-in** — Used in initialization
4. **`$USELIBRARY` implementation** — QB64pe uses library system
5. **Graphics initialization fixes** — IDE requires graphics window

### Medium Priority (Causes Incorrect Behavior)
1. **Event trapping (`ON KEY`, `ON TIMER`, etc.)** — May cause hangs if called
2. **`$INCLUDEONCE`** — Prevents duplicate includes
3. **`$EMBED`** — Used for embedding resources
4. **`_DIREXISTS` and file system functions** — Used for internal folder checks

### Low Priority (Nice to Have)
1. **`$COLOR` directives** — IDE syntax highlighting
2. **`$ASSERTS`** — Debug features
3. **`$VERSIONINFO` / `$EXEICON`** — Windows resource generation
4. **`$NOPREFIX`** — Deprecated feature

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
