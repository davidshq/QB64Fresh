# Plan: Compiling QB64pe Using QB64Fresh

*Created: 2026-01-20*
*Updated: 2026-01-22*

This document outlines the strategy for compiling the QB64pe compiler using QB64Fresh, achieving a form of cross-compilation where a Rust-based BASIC compiler builds a C++-targeting BASIC compiler.

---

## Executive Summary

**Goal:** Use QB64Fresh to compile `qb64pe.bas` (the QB64pe compiler source) into a working executable.

**Challenge:** QB64pe is a ~59,000 line BASIC codebase (39 files) that uses many advanced QB64-specific features. QB64Fresh must support all features used by QB64pe to successfully compile it.

**Approach:** Systematic gap analysis, incremental feature implementation, and progressive testing.

**Current Status (2026-01-22):** **PHASE C IN PROGRESS!** Code generation validation underway. GCC errors reduced from 14,547 → 169 (**98.8% reduction**). Session 4 fixes include:
- Fixed BYREF parameter passing in user-defined function calls (added `params` to IR)
- Added C standard library names to reserved word list (`isalpha`, `isdigit`, etc.)
- Fixed built-in constant BYREF handling (use temp vars for `_TRUE`, `_FALSE`)
- Added keyboard constants `_KEY_*` (F1-F12, arrows, modifiers)
- Added function variants: `_INSTRREV`, `_MESSAGEBOX`, `_LOADFONT`, `_WIDTH`, `_HEIGHT`
- Fixed array subscript type casting (always cast to int64_t)

**Ready for Phase C Session 5: Fix remaining 169 GCC errors - major issues:**
- Implicit variable declarations from BYREF function outputs (~80 errors)
- Type/assignment issues (~8 errors)

---

## 0. Initial Gap Analysis Results (COMPLETED)

### Test Performed
```bash
cd QB64pe/source && qb64fresh qb64pe.bas 2>&1 | sort | uniq -c | sort -rn
```

### Preprocessor Status: ✅ WORKING
- Windows path separators (`\`) now converted to `/` automatically
- Non-UTF-8 files (CP437/Windows-1252) handled via lossy conversion
- Source expands from 1.07MB → 2.47MB with all includes

### Parse Errors Summary (992 total)

| Error | Count | Root Cause |
|-------|-------|------------|
| `invalid expression` | 373 | Various syntax not recognized |
| `unexpected token Case` | 116 | Colon-separated CASE statements? |
| `expected FUNCTION name, found Newline` | 63 | Unknown - needs investigation |
| `expected THEN, found Identifier` | 58 | Likely complex IF conditions |
| `unexpected token Else` | 56 | Single-line IF parsing edge cases |
| `unexpected token StringLiteral` | 35 | Unknown pattern |
| `unexpected token Error` | 21 | ERROR keyword context issues |
| `unexpected token ElseIf` | 21 | ELSEIF parsing issues |
| `expected ), found Error` | 21 | ERROR in expressions |
| `unexpected token Next` | 20 | FOR/NEXT parsing |
| `unexpected token Loop` | 20 | DO/LOOP parsing |
| `expected SUB name, found Newline` | 20 | Unknown |
| `unexpected token Percent` | 19 | `%` type suffix issues |
| `expected ), found Ampersand` | 19 | `&&` (LONGLONG) suffix not supported |

### Critical Missing Features Identified

#### 1. TYPE Definition Alternate Syntax (HIGH PRIORITY)
QB64pe uses:
```basic
TYPE usedVarList
    AS LONG id, linenumber, includeLevel    ' ← Not supported
    AS _BYTE used, watch, isarray
    AS STRING name, cname
END TYPE
```

QB64Fresh only supports the standard syntax:
```basic
TYPE usedVarList
    id AS LONG
    linenumber AS LONG
    ' etc.
END TYPE
```

**Impact:** Affects all TYPE definitions in QB64pe (multiple files)

#### 2. Extended Type Suffixes (MEDIUM PRIORITY)
Not supported:
- `&&` - LONGLONG (64-bit signed integer)
- `~&&` - Unsigned LONGLONG
- `~&` - Unsigned LONG
- `%%` - _BYTE shorthand

Examples from QB64pe:
```basic
bitmask(i&&) = i2&&           ' LONGLONG variables
constval~&&                    ' Unsigned LONGLONG
```

#### 3. Already Fixed (This Session)
- ✅ Windows path separators in `$INCLUDE`
- ✅ Non-UTF-8 source file encoding
- ✅ `FieldAssignment` code generation (unrelated compile fix)

---

## 0.5 Phase B Implementation Progress (2026-01-21) ✅ MAJOR MILESTONE

### Features Implemented

| Feature | Status | Impact |
|---------|--------|--------|
| TYPE alternate syntax (`AS LONG x, y, z`) | ✅ Done | ~200 errors fixed |
| Extended type suffixes (`&&`, `%%`, `~&&`, etc.) | ✅ Done | Proper LONGLONG parsing |
| `_ORELSE` / `_ANDALSO` operators | ✅ Done | ~58 errors fixed |
| Keywords as field names (`.name`, `.type`) | ✅ Done | ~13 errors fixed |
| `_OFFSET` and `_BIT` in type specs | ✅ Done | Type parsing complete |
| `$VERSIONINFO` with `#` suffix | ✅ Done | Minor fix |
| Line numbers in error messages | ✅ Done | Easier debugging |
| `$CONSOLE` lexer workaround | ✅ Done | Parser handles Error tokens |
| `DIM AS type var1, var2` syntax | ✅ Done | Type-first declarations |
| `REDIM` with scalar variables | ✅ Done | No parentheses required |
| Keywords as TYPE member names | ✅ Done | `.name`, `.type` fields |
| `SHELL _HIDE _DONTWAIT` syntax | ✅ Done | Combined options |
| `_CONSOLE`/`_DEST` as expressions | ✅ Done | Function/statement duality |
| Type suffix tokenization (`i2&&`) | ✅ Done | Fix multi-char suffixes |
| Improved parser error recovery | ✅ Done | Reduced cascading |

### Current Error Count

```
Semantic errors (with $INCLUDE): 0 (was 992) - 100% reduction ✅ COMPLETE!
Parse errors: 0 - Parsing is complete!
```

**Session 3 Fixes (47 → 17 errors):**
- Polymorphic `_IIF` handling - accepts any compatible type pair
- Unsigned type suffix lookup (`~%`, `~&`, `~%%`, `~&&`) in procedure names
- `SHELL` function registration (was treated as array access)
- `_STR_CRLF`, `_STR_LF`, `_STR_CR`, `_STR_EMPTY` string constants
- `_CHR_HT` (horizontal tab) character constant
- `_ASC_*` numeric constants for ASCII codes
- Built-in constant scope visibility from inside functions
- Function return base name aliasing (`FUNCTION foo$` allows `foo = value`)
- Multiple DIM for same array in same scope (valid QB64 pattern)
- Array/scalar namespace separation in assignments (`sf` vs `SF()`)
- `STRING * N` to `STRING` coercion for OPEN, MID$, ASC, CHAIN statements

**Session 4 Fixes (17 → 0 errors):**
- **Dual namespace model implementation** - Separate storage for scalars and arrays
  - `Scope` struct now has separate `scalars` and `arrays` HashMaps
  - `lookup_scalar()` for simple variable access
  - `lookup_array()` for array element access with parentheses
  - Updated `check_function_call` to use `lookup_array` first for `name(args)` syntax
  - Updated `check_array_assignment` to use `lookup_array`
  - Updated `check_array_field_assignment` to use `lookup_array`
- `_MESSAGEBOX` signature corrected to 5 parameters (all STRING except last LONG)
- See QB64_LANGUAGE_SPECIFICATION.md section 9.5 for dual namespace documentation
- **REDIM _PRESERVE on SHARED arrays** - Fixed to update global scope instead of creating local copies
  - `REDIM _PRESERVE UserDefine(...)` inside a SUB now correctly uses the SHARED array type
  - Added `is_module_shared()` and `update_shared_symbol()` to SymbolTable
- **_OPENHOST signature fix** - Changed from `(port: LONG)` to `(connection_string: STRING)`

### Next Steps for Phase B Completion

1. ✅ ~~Add line numbers to error messages for better debugging~~
2. ✅ ~~Improve parser error recovery to reduce cascading~~
3. ✅ ~~DEFTYPE processing before procedure declarations~~
4. ✅ ~~Fix _NEWHANDLER parsing for ON ERROR GOTO~~
5. ✅ ~~Polymorphic _IIF handling~~
6. ✅ ~~STRING * N to STRING coercion~~
7. ✅ ~~Built-in constant visibility from functions~~
8. ✅ ~~Function return base name aliasing~~
9. ✅ ~~Target: reduce to <50 errors~~ **ACHIEVED: 17 errors!**
10. **Optional:** Full array/scalar namespace separation (complex refactor)

---

## 1. Understanding the Target

### QB64pe Source Structure

```
QB64pe/source/
├── qb64pe.bas           # Main compiler (24,246 lines)
├── global/              # Version, settings, constants
├── ide/                 # IDE components
├── subs_functions/      # Core compiler functions
├── utilities/           # Helper modules
└── emit/                # Code generation
```

**Total:** 39 BASIC files, ~58,821 lines of code

### Key QB64pe Features Used

Based on analysis of `qb64pe.bas`:

| Feature Category | Examples | QB64Fresh Status |
|-----------------|----------|------------------|
| QB64 Extensions | `_DIREXISTS`, `_OS$`, `_LIMIT`, `_SCREENSHOW`, `TIMER(accuracy)` | ⚠️ Partial |

---

## 2. Gap Analysis Required

### Phase 2.1: Feature Inventory (Priority: Critical)

Create a comprehensive list of every language feature used in QB64pe:

```bash
# Suggested analysis approach:
# 1. Extract all keywords/functions from QB64pe source
# 2. Cross-reference against QB64Fresh's supported features
# 3. Identify gaps and prioritize by usage frequency
```

**Deliverable:** Feature compatibility matrix showing:
- ✅ Fully supported in QB64Fresh
- ⚠️ Partially supported (works but edge cases may fail)
- ❌ Not yet implemented

### Phase 2.2: Metacommand Support (Priority: High)

QB64pe uses these metacommands that need verification:

| Metacommand | Purpose | QB64Fresh Status |
|-------------|---------|------------------|
| `$EXEICON` | Windows icon | ⚠️ Stubbed (Windows-specific) |

## 3. Implementation Phases

### Phase C: Code Generation Validation (2-4 sessions) - IN PROGRESS

**Objective:** Ensure generated C code is correct

**Session 1 Progress (2026-01-21):**
- [x] Generate C code for QB64pe source (3.9MB, 76K lines generated successfully)
- [x] Review generated code for correctness - identified 14,547 initial GCC errors
- [x] Fix code generation issues discovered:
  - [x] TYPE definitions emitted before global variables that use them
  - [x] Fixed-length STRING * N generates proper `char name[N]` syntax
  - [x] Struct initialization uses `{0}` instead of `= 0`
  - [x] Dots in variable names converted to underscores (`path.exe$` → `path_exe_str`)
  - [x] Tilde in identifiers converted (`constval~&` → `constval_u_lng`)
  - [x] C reserved words escaped (`default` → `default_`)
  - [x] `_IIF` polymorphic handling - uses `qb_iif_str` for string return types
  - [x] QB64 built-in constants added (`_TRUE`, `_FALSE`, `_EQUAL`, `_LESS`, `_GREATER`)
  - [x] Function argument variants (`qb_mid2`, `qb_instr2`, `qb_command_n`)
  - [x] Implicit local variable collection for function bodies

**Session 2 Progress (2026-01-22):**
- [x] Fixed duplicate variable declarations (two-pass implicit local collector)
- [x] Fixed `qb_asc` two-argument variant (303 errors fixed)
- [x] Fixed `qb_timer` with accuracy parameter (34 errors fixed)
- [x] Fixed type/variable name collision with `qbt_` prefix (152 errors fixed)
- [x] Fixed fixed-length string in struct field assignments (144 errors fixed)
- [x] Fixed fixed-length string array declarations (`char name[N]` syntax)
- [x] Fixed scalar fixed-length string assignments (use `strncpy`)
- [x] Fixed byref parameter passing (added `&` for non-lvalue args with temps)
- [x] Fixed REDIM SHARED global array declarations
- [x] Fixed CONST definitions as global constants

**Session 3 Progress (2026-01-22):**
- [x] Fixed `END 1` and `SYSTEM 1` exit code parsing (was generating line number labels)
- [x] Fixed static string initializers (use NULL, not qb_string_new())
- [x] Fixed string CONST initialization (moved to main())
- [x] Fixed duplicate labels (procedure-prefixed line number labels)
- [x] Fixed label vs SUB call disambiguation (`label: x = 1` vs `Sub1: Sub2`)

**Session 4 Progress (2026-01-22):**
- [x] Fixed BYREF parameter passing in user-defined function calls (added `params` to `FunctionCall` IR)
- [x] Added C standard library names to reserved word list (`isalpha`, `isdigit`, `malloc`, etc.)
- [x] Fixed built-in constant BYREF handling (use temp vars for `_TRUE`, `_FALSE`, etc.)
- [x] Added missing `_KEY_*` keyboard constants (F1-F12, arrows, modifiers)
- [x] Added `_EXIT`, `_DEFAULTCOLOR`, `_BACKGROUNDCOLOR` as built-in functions
- [x] Used C compound literals for BYREF non-lvalue expressions in function calls
- [x] Added function variants: `_INSTRREV`, `_MESSAGEBOX`, `_LOADFONT`, `_WIDTH`, `_HEIGHT`
- [x] Added dialog function variants: `_SAVEFILEDIALOG$`, `_OPENFILEDIALOG$`, `_SELECTFOLDERDIALOG$`
- [x] Fixed array subscript type casting (always cast indices to int64_t)

**Current Status:** **169 GCC errors remaining** (down from 14,547 = **98.8% reduction!**)

**Remaining Issues Analysis:**
1. **Implicit BYREF variable declarations** (~80 errors) - Variables like `hashresflags`, `hashresref`, `ideprogname_str` created implicitly through BYREF function calls
   - Root cause: Semantic analyzer doesn't create variables when passed to BYREF params
   - Fix: Detect BYREF outputs in call analysis, auto-declare needed variables
2. **Type/assignment issues** (~8 errors):
   - `subscripted value is neither array nor pointer` (2) - Non-array being subscripted
   - `incompatible types when assigning` (2) - qb_string assignment issues
   - `assignment to expression with array type` (2) - Fixed-length string field assignments
   - `expected identifier or '(' before '['` (2) - Syntax issues

**Tasks:**
1. [x] Generate C code for QB64pe source
2. [x] Review generated code for correctness
3. [~] Fix code generation issues discovered (98.0% complete)
4. [x] Ensure proper handling of:
   - Large string concatenations ✓ (working)
   - Complex nested expressions ✓ (working)
   - Multi-file `$INCLUDE` structure ✓ (working)

**Success Criteria:** Clean C code generation with no internal errors

### Phase D: Compilation & Testing (2-4 sessions)

**Objective:** Build and test the compiled QB64pe

**Tasks:**
1. [ ] Compile generated C code with gcc/clang
2. [ ] Link against QB64Fresh runtime
3. [ ] Address any link errors (missing functions, etc.)
4. [ ] Run basic functionality tests
5. [ ] Compare output with original QB64pe behavior

**Success Criteria:** QB64pe compiled by QB64Fresh can compile simple BASIC programs

### Phase E: Validation & Documentation (1-2 sessions)

**Objective:** Verify correctness and document the achievement

**Tasks:**
1. [ ] Create test suite for QB64Fresh-compiled QB64pe
2. [ ] Document any behavioral differences
3. [ ] Write migration/compatibility notes
4. [ ] Update project documentation

**Success Criteria:** Documented, reproducible process for QB64pe compilation

---

## 4. Technical Considerations

### 4.1 Runtime Compatibility

QB64pe's internal runtime differs from QB64Fresh's:
- QB64pe generates C++ code
- QB64Fresh generates C code with its own runtime

**Implication:** The QB64Fresh-compiled QB64pe will use QB64Fresh's runtime, not QB64pe's original runtime. This means:
- Some internal behaviors may differ
- Performance characteristics may vary
- Platform support follows QB64Fresh's capabilities

### 4.2 IDE Component

QB64pe includes an integrated IDE (`ide/` directory). Options:
1. **Exclude IDE:** Compile compiler-only portion (`-c` mode)
2. **Include IDE:** Requires graphics support (SDL2)

**Recommendation:** Start with compiler-only mode to reduce complexity.

### 4.3 Self-Hosting Implications

Once QB64Fresh can compile QB64pe:
- QB64Fresh-compiled QB64pe can compile BASIC programs
- This creates an interesting toolchain: Rust → BASIC compiler → BASIC programs
- Could eventually lead to QB64Fresh compiling itself (if rewritten in BASIC)

---

## 5. Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Many unsupported features | Medium | High | Prioritize by usage, implement incrementally |
| Subtle semantic differences | High | Medium | Comprehensive testing, comparison with original |
| Performance issues | Medium | Low | Profile and optimize after correctness |
| Windows-specific features | High | Low | Stub out or provide alternatives |
| Large codebase stress | Medium | Medium | Test incrementally, optimize memory usage |

---

## 6. Success Metrics

### Milestone 3: Code Generation Success
- [x] C code generated for entire QB64pe (4.3MB, ~100K lines)
- [x] No internal compiler errors
- [~] Generated code compiles with C compiler (169 GCC errors remaining, 98.8% fixed)

### Milestone 4: Functional Success
- [ ] QB64Fresh-compiled QB64pe runs
- [ ] Can compile simple "Hello World" BASIC program
- [ ] Produces working executable

### Milestone 5: Validation Success
- [ ] QB64Fresh-compiled QB64pe passes subset of QB64pe's test suite
- [ ] Behavior matches original QB64pe for core functionality
- [ ] Process is documented and reproducible

---

## 7. Immediate Next Steps

### Current Actions

1. **Begin Phase C: Code Generation Validation**
   - Generate C code for QB64pe source using `--emit-c`
   - Review generated code for correctness
   - Test compilation with gcc/clang

---

## 8. Estimated Timeline

| Phase | Sessions | Status | Notes |
|-------|----------|--------|-------|
| C: Code Gen | 2-4 | **In Progress** | 98.8% GCC errors fixed (Session 4) |
| D: Testing | 2-4 | Pending | Build and validate |
| E: Documentation | 1-2 | Pending | Write up results |

**Progress:** Phases A, B complete. Phase C in progress (4 sessions, 169 errors remaining).

---

## Appendix A: QB64pe File Inventory

```
qb64pe.bas                              24,246 lines (main)
global/version.bas                       version info
global/settings.bas                      compiler settings
global/constants.bas                     constant definitions
ide/ide_global.bas                       IDE globals
ide/ide_methods.bas                      IDE functions
ide/config/cfg_global.bas                IDE config
ide/config/cfg_methods.bas               IDE config functions
utilities/ini-manager/ini.bi             INI declarations
utilities/ini-manager/ini.bm             INI implementation
utilities/s-buffer/simplebuffer.bi       Buffer declarations
utilities/s-buffer/simplebuffer.bm       Buffer implementation
utilities/const_eval.bi                  Const eval declarations
utilities/const_eval.bas                 Const eval implementation
utilities/give_error.bi                  Error handling decl
utilities/give_error.bas                 Error handling impl
utilities/statevars.bi                   State variable decl
utilities/statevars.bas                  State variable impl
utilities/type.bi                        Type handling decl
utilities/type.bas                       Type handling impl
utilities/hash.bi                        Hash table decl
utilities/hash.bas                       Hash table impl
utilities/strings.bas                    String utilities
utilities/file.bas                       File utilities
utilities/build.bas                      Build utilities
utilities/elements.bas                   Element utilities
utilities/format.bas                     Formatting utilities
utilities/terminal.bas                   Terminal utilities
emit/logging.bas                         Logging functions
subs_functions/subs_functions.bas        Core functions
subs_functions/extensions/opengl/*.bas   OpenGL support
```

---

## Appendix B: Key Differences Between QB64pe and QB64Fresh

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| Implementation language | QBasic/C++ | Rust |
| Target code | C++ | C |
| Graphics backend | OpenGL | SDL2 |
| Audio backend | Custom | Rodio |
| IDE | Built-in | External (LSP) |
| Memory model | Custom cmem | Sandboxed cmem |
| OpenGL support | ~300 commands | Via DECLARE LIBRARY |

---

## Appendix C: QB64Fresh Codebase Structure

The QB64Fresh compiler is a Rust workspace with the following key components:

### Compiler Pipeline (`src/`) - ~25,851 lines

```
src/
├── main.rs              # CLI entry point
├── lib.rs               # Library crate root
├── preprocessor.rs      # $INCLUDE directive handling
├── lexer/               # Tokenization (logos-based)
│   ├── mod.rs           # Lexer implementation
│   └── token.rs         # Token types and type suffixes
├── parser/              # ~11,350 lines
│   ├── mod.rs           # Parser entry, error recovery
│   ├── tokens.rs        # Token navigation utilities
│   ├── expressions.rs   # Pratt parser for expressions
│   ├── statements.rs    # Core statement parsing
│   ├── control_flow.rs  # IF/FOR/WHILE/DO/SELECT
│   ├── procedures.rs    # SUB/FUNCTION/TYPE
│   ├── graphics.rs      # Graphics statements
│   ├── audio.rs         # Audio statements
│   ├── file_io.rs       # File I/O parsing
│   ├── system.rs        # System commands
│   ├── directives.rs    # Preprocessor directives
│   └── error.rs         # Parse error types
├── ast/                 # ~2,646 lines
│   ├── mod.rs           # Span, Program types
│   ├── expr.rs          # Expression AST nodes
│   └── stmt.rs          # Statement AST nodes (~100 variants)
├── semantic/            # ~14,440 lines
│   ├── mod.rs           # Analysis entry, built-ins
│   ├── types.rs         # BasicType enum, inference
│   ├── symbols.rs       # Symbol table, scopes
│   ├── typed_ir.rs      # Type-checked IR output
│   ├── error.rs         # Semantic error types
│   └── checker/         # Type checking submodule
│       ├── mod.rs
│       ├── expressions.rs
│       ├── statements.rs
│       ├── control_flow.rs
│       ├── assignments.rs
│       ├── definitions.rs
│       └── const_eval.rs
├── codegen/             # ~11,728 lines
│   ├── mod.rs           # CodeGenerator trait
│   ├── error.rs         # Codegen error types
│   └── c_backend/       # C code generator
│       ├── mod.rs       # Backend entry
│       ├── expr.rs      # Expression codegen
│       ├── stmt.rs      # Statement codegen
│       ├── file_io.rs   # File I/O helpers
│       ├── types.rs     # BASIC ↔ C type mapping
│       ├── runtime.rs   # Inline C runtime (~4,141 lines)
│       ├── analysis.rs  # DATA/label pre-pass
│       └── const_fold.rs
└── lsp/                 # ~2,105 lines
    ├── mod.rs           # LSP server implementation
    └── main.rs          # qb64fresh-lsp binary
```

### Runtime Library (`runtime/`) - ~11,678 lines

```
runtime/
├── lib.rs               # Crate root, initialization
├── string.rs            # Reference-counted strings
├── io.rs                # PRINT, INPUT, console
├── math.rs              # Mathematical functions
├── graphics_ffi.rs      # C FFI for graphics
├── audio_ffi.rs         # C FFI for audio
├── dialogs.rs           # File dialogs
├── joystick.rs          # Gamepad input
├── graphics/            # Pluggable graphics
│   ├── mod.rs           # GraphicsBackend trait
│   ├── sdl2.rs          # SDL2 implementation
│   ├── mock.rs          # Testing mock
│   ├── font.rs          # Font rendering
│   └── error.rs
├── audio/               # Pluggable audio
│   ├── mod.rs           # AudioBackend trait
│   ├── rodio_backend.rs # Rodio implementation
│   ├── mock.rs          # Testing mock
│   └── error.rs
└── include/
    └── qb64fresh_rt.h   # C header for FFI
```

### Tools (`tools/`)

- **fix_encoding** - CP437/Latin1 → UTF-8 converter
- **qb64fresh-fmt** - Code formatter
- **qb64fresh-lint** - Code linter

### Test Suite (`tests/`)

- **937+ tests**, 81.63% coverage
- **99.1% QB4.5 compatibility** (114/115 test files)
- Integration, golden, property-based, and compatibility tests

### Compilation Pipeline

```
Source (.bas)
    ↓
Preprocessor ($INCLUDE)
    ↓
Lexer (logos) → Tokens
    ↓
Parser (Pratt + Recursive Descent) → AST
    ↓
Semantic Analysis → Typed IR
    ↓
Code Generation → C Code
    ↓
C Compiler (gcc/clang) → Executable
    ↓
Runtime Library (linked)
```

---

*This plan will be updated as analysis progresses and more specific requirements are discovered.*
