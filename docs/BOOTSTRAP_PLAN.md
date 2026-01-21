# Plan: Compiling QB64pe Using QB64Fresh

*Created: 2026-01-20*
*Updated: 2026-01-21 (Phase B implementation in progress - 47% error reduction)*

This document outlines the strategy for compiling the QB64pe compiler using QB64Fresh, achieving a form of cross-compilation where a Rust-based BASIC compiler builds a C++-targeting BASIC compiler.

---

## Executive Summary

**Goal:** Use QB64Fresh to compile `qb64pe.bas` (the QB64pe compiler source) into a working executable.

**Challenge:** QB64pe is a ~59,000 line BASIC codebase (39 files) that uses many advanced QB64-specific features. QB64Fresh must support all features used by QB64pe to successfully compile it.

**Approach:** Systematic gap analysis, incremental feature implementation, and progressive testing.

**Current Status (2026-01-21):** Phase B implementation in progress. Parse errors reduced from 992 → ~520 (47% reduction). Key syntax features implemented.

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

## 0.5 Phase B Implementation Progress (2026-01-21)

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
Parse errors (with $INCLUDE): 162 (was 992) - 84% reduction
Parse errors (main file only): 27 (was ~150) - 82% reduction
```

### Remaining Parse Error Categories

| Error | Count | Analysis |
|-------|-------|----------|
| `invalid expression` | 69 | Cascade from earlier errors |
| `unexpected token Case` | 38 | SELECT CASE block issues |
| `unexpected token Else` | 15 | Cascade |
| `expected FUNCTION name` | 12 | Procedure parsing |
| `invalid statement: unexpected token Dot` | 7 | Field access syntax |
| `expected FOR, found Comma` | 5 | FOR loop with multiple vars? |

**Key Finding:** Individual include files parse successfully! The errors are cascading effects from earlier parse failures causing the parser to lose synchronization.

### Next Steps

1. ✅ ~~Add line numbers to error messages for better debugging~~
2. ✅ ~~Improve parser error recovery to reduce cascading~~
3. Investigate remaining root causes (162 errors)
4. Target: reduce to <100 errors

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
| Metacommands | `$CONSOLE`, `$SCREENHIDE`, `$EXEICON`, `$VERSIONINFO`, `$INCLUDE`, `$DYNAMIC` | Partial |
| String Operations | Complex string manipulation, `INSTR`, `MID$`, concatenation | ✅ Implemented |
| Arrays | `REDIM`, multi-dimensional, dynamic sizing | ✅ Implemented |
| File I/O | Sequential, random, binary file access | ✅ Implemented |
| Type System | `DEFLNG`, `DEFSNG`, user-defined types | Partial |
| QB64 Extensions | `_DIREXISTS`, `_OS$`, `_LIMIT`, `_SCREENSHOW`, `TIMER(accuracy)` | Partial |
| Control Flow | `IF/THEN/ELSE`, `SELECT CASE`, `FOR/NEXT`, `DO/LOOP`, `GOTO` | ✅ Implemented |
| Procedures | `SUB`, `FUNCTION`, `SHARED` variables | ✅ Implemented |

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
| `$INCLUDE` | File inclusion | ✅ Implemented |
| `$DYNAMIC` | Dynamic arrays | ✅ Implemented |
| `$CONSOLE` | Console window | ⚠️ Needs verification |
| `$SCREENHIDE` | Hide graphics window | ⚠️ Needs verification |
| `$EXEICON` | Windows icon | ❌ Windows-specific, can stub |
| `$VERSIONINFO` | Version metadata | ❌ Windows-specific, can stub |

### Phase 2.3: QB64 Extension Functions (Priority: High)

Critical `_` prefixed functions used by QB64pe:

| Function | Purpose | QB64Fresh Status |
|----------|---------|------------------|
| `_DIREXISTS` | Directory check | ⚠️ Needs verification |
| `_FILEEXISTS` | File check | ⚠️ Needs verification |
| `_OS$` | Operating system string | ⚠️ Needs verification |
| `_LIMIT` | Frame rate limiter | ✅ Implemented |
| `_SCREENSHOW` / `_SCREENHIDE` | Window visibility | ⚠️ Needs verification |
| `_BYTE` / `_INTEGER64` etc. | Extended types | ✅ Implemented |
| `TIMER(accuracy)` | High-precision timer | ⚠️ Needs verification |

---

## 3. Implementation Phases

### Phase A: Analysis & Baseline (1-2 sessions)

**Objective:** Establish current compatibility baseline

**Tasks:**
1. [ ] Run QB64pe source through QB64Fresh lexer/parser
2. [ ] Collect all parse errors and categorize
3. [ ] Run semantic analysis on parseable portions
4. [ ] Document all unsupported features with usage counts
5. [ ] Create prioritized implementation backlog

**Success Criteria:** Complete feature gap document with prioritization

### Phase B: Critical Feature Implementation (4-8 sessions)

**Objective:** Implement features blocking QB64pe compilation

**Confirmed gaps to address (from analysis):**

1. [ ] **TYPE alternate syntax** (HIGH - 1-2 sessions)
   - Support `AS TYPE field1, field2, ...` syntax
   - Affects: parser/statements.rs TYPE parsing

2. [ ] **Extended type suffixes** (MEDIUM - 1 session)
   - `&&` for LONGLONG (_INTEGER64)
   - `~&&` for unsigned LONGLONG
   - `~&` for unsigned LONG
   - `%%` for _BYTE
   - Affects: lexer/token.rs, parser type suffix handling

3. [ ] **Investigate remaining 900+ errors** (1-2 sessions)
   - Many may cascade from the TYPE syntax issue
   - Group by root cause

4. [ ] **Console-mode support** (1 session)
   - `$CONSOLE` metacommand
   - `$SCREENHIDE` behavior

5. [ ] **Remaining QB64 extensions** (2-3 sessions)
   - Verify `_ERRORMESSAGE$`, `_ERRORLINE`, `_INCLERRORLINE`
   - Verify `_TOSTR$` function
   - Other `_` functions used by QB64pe

**Success Criteria:** QB64pe source parses and analyzes without errors

### Phase C: Code Generation Validation (2-4 sessions)

**Objective:** Ensure generated C code is correct

**Tasks:**
1. [ ] Generate C code for QB64pe source
2. [ ] Review generated code for correctness
3. [ ] Fix code generation issues discovered
4. [ ] Ensure proper handling of:
   - Large string concatenations
   - Complex nested expressions
   - Multi-file `$INCLUDE` structure

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

### Milestone 1: Parse Success
- [ ] All 39 QB64pe files parse without errors
- [ ] All tokens recognized
- [ ] AST generated for entire codebase

### Milestone 2: Semantic Analysis Success
- [ ] Type checking passes
- [ ] All symbols resolved
- [ ] No semantic errors

### Milestone 3: Code Generation Success
- [ ] C code generated for entire QB64pe
- [ ] No internal compiler errors
- [ ] Generated code compiles with C compiler

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

~~1. Run initial parse test~~ ✅ DONE (2026-01-20)
~~2. Capture all errors~~ ✅ DONE - 992 parse errors identified

**Next actions:**

1. **Implement TYPE alternate syntax** (Highest impact)
   ```basic
   ' Support this QB64 syntax:
   TYPE Foo
       AS LONG x, y, z     ' Multiple fields, type first
   END TYPE
   ```
   - Location: `src/parser/statements.rs` around TYPE parsing
   - Test: Create test case with QB64-style TYPE

2. **Add extended type suffixes**
   - `&&` → _INTEGER64
   - `~&&` → _UNSIGNED _INTEGER64
   - Location: `src/lexer/token.rs` identifier/suffix parsing

3. **Re-run analysis after fixes** to measure progress:
   ```bash
   cd QB64pe/source && qb64fresh qb64pe.bas 2>&1 | sort | uniq -c | sort -rn
   ```

4. **Track progress** - Target: reduce from 992 to <100 errors

---

## 8. Estimated Timeline

| Phase | Sessions | Focus |
|-------|----------|-------|
| A: Analysis | 1-2 | Gap identification |
| B: Implementation | 4-8 | Missing features |
| C: Code Gen | 2-4 | C output correctness |
| D: Testing | 2-4 | Build and validate |
| E: Documentation | 1-2 | Write up results |

**Total estimate:** 10-20 development sessions

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

*This plan will be updated as analysis progresses and more specific requirements are discovered.*
