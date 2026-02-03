# Commits on 2026-01-22 — Detailed

This document lists every commit made on January 22, 2026, in chronological order, with full commit messages and file-change summaries. These commits represent the start of Phase C (code generation validation) for QB64pe bootstrap.

---

## Branch application status

**Applied to this branch:** **All 13 commits** have been applied: `e7bbd59` through `503155b`. Nothing was omitted. Commit #13 (503155b) applied with minor whitespace warnings (squelched by `git apply`).

---

## 1. `e7bbd59` — Phase C code generation validation (97.8% error reduction) — **APPLIED**

**Subject:** feat(codegen): Phase C code generation validation - 97.8% error reduction

**What changed:**

Major improvements to C code generation targeting QB64pe bootstrap compilation. Reduced GCC compilation errors from 14,547 to 325 (97.8% reduction).

**Key fixes:**
- Two-pass implicit variable collection to avoid duplicate declarations
- Fixed-length string handling with strncpy() instead of direct assignment
- Type/variable name collision fix by prefixing UDT names with qbt_
- Byref parameter passing with temp variables for non-lvalue expressions
- REDIM SHARED global array declarations
- CONST definitions as global constants
- Added qb_asc2() and qb_timer_n() runtime function variants
- Pointer-to-array syntax for fixed-length string byref parameters

**Files modified:**
- src/codegen/c_backend/stmt.rs: Two-pass locals, byref handling, fixed strings
- src/codegen/c_backend/analysis.rs: Type prefixing, REDIM SHARED, CONST
- src/codegen/c_backend/types.rs: qbt_ prefix for user-defined types
- src/codegen/c_backend/expr.rs: ASC/TIMER variants, FixedString cast handling
- src/codegen/c_backend/runtime.rs: qb_asc2, qb_timer_n functions
- src/semantic/typed_ir.rs: Added is_array and params to Call statement
- src/semantic/checker/*: Populate new TypedParameter fields

**Remaining:** ~325 errors (static initializers, duplicate labels, function variants)

**Stats:** 13 files changed, 899 insertions(+), 98 deletions(-)

---

## 2. `d8f974b` — Phase C Session 3: duplicate labels and static strings — **APPLIED**

**Subject:** feat(codegen): Phase C Session 3 - fix duplicate labels and static strings

**What changed:**

Fixed 29 more GCC errors (325 → 296), continuing code generation validation.

**Key fixes:**
- END with exit code: `END 1` now correctly generates `exit(1)` instead of being parsed as `END` + line number label
- SYSTEM with exit code: `SYSTEM 1` now correctly generates `exit(1)`
- Static string initializers: Use NULL for static local string variables (qb_string_new() is not valid in static initializers)
- String CONST initialization: Moved to main() since function calls can't be used as global initializers in C
- Duplicate label fix: Line number labels now prefixed with procedure name to ensure uniqueness per function
- Label vs SUB call disambiguation: Improved heuristic to distinguish `label: x = 1` (label) from `Sub1: Sub2` (two calls)

**Files modified:**
- src/ast/stmt.rs: Added exit_code to End and System variants
- src/parser/statements.rs: Parse optional exit code for END/SYSTEM, improved label detection heuristic
- src/semantic/checker/statements.rs: Handle new End/System variants
- src/semantic/typed_ir.rs: Added exit_code to End/System in typed IR
- src/codegen/c_backend/stmt.rs: Generate exit() with code, NULL for static strings, procedure-prefixed labels
- src/codegen/c_backend/analysis.rs: Collect string const inits separately
- src/codegen/c_backend/mod.rs: Emit string const inits at start of main()

**Remaining:** ~296 GCC errors (function signatures, undeclared variables, missing constants)

**Stats:** 8 files changed, 183 insertions(+), 59 deletions(-)

---

## 3. `cafedb7` — Phase C Session 4: BYREF function calls and keyboard constants — **APPLIED**

**Subject:** feat(codegen): Phase C Session 4 - BYREF function calls and keyboard constants

**What changed:**

Major improvements to reduce GCC errors from 296 to 169 (98.8% reduction from original 14,547).

**BYREF parameter passing:**
- Added params field to FunctionCall TypedIR for BYREF detection
- Semantic checker now populates parameter info from procedure definitions
- Code generator adds & prefix for BYREF arguments
- Use C compound literals &(type){expr} for non-lvalue expressions
- Detect built-in constants (_TRUE, _FALSE, etc.) that are C macros

**Keyboard constants:**
- Added _KEY_* constants for F1–F12, arrows, Insert, Delete, etc.
- Registered in both semantic analyzer and runtime.rs C macros

**Function variants:**
- _INSTRREV with 3 arguments
- _MESSAGEBOX with 1–4 arguments
- _LOADFONT with 2–4 arguments
- _WIDTH/_HEIGHT without arguments
- Dialog functions (_SAVEFILEDIALOG, _OPENFILEDIALOG, _SELECTFOLDERDIALOG)

**Other:**
- Array subscript safety: Cast all array indices to int64_t in both expr.rs and stmt.rs
- C reserved word escaping: Added ctype.h, stdlib.h, string.h, stdio.h, math.h function names

**Stats:** 10 files changed, 395 insertions(+), 40 deletions(-)

---

## 4. `ef4a7a9` — Phase C Session 5: ASCII/CHR constants and array parameters — **APPLIED**

**Subject:** feat(codegen): Phase C Session 5 - ASCII/CHR constants and array parameters

**What changed:**

Progress: 169 → 142 GCC errors (99.0% reduction from original 14,547).

**New constants:**
- Added 70+ _ASC_* ASCII value constants (NUL=0 through DEL=127)
- Added 70+ _CHR_* character string constants (qb_string* macros)
- Added _KEY_LAPPLE (100310) and _KEY_RAPPLE (100309) for macOS
- Added _FONT macro for zero-argument function call

**Error handling fixes:**
- Fixed ON ERROR GOTO _LASTHANDLER — disable error handler (QB64 scoping)
- Fixed global error labels in subroutines — disabled cross-function goto

**Array parameter fixes:**
- Arrays remain as pointers in byref copies, not dereferenced to scalars
- Fixed array of fixed-length string parameter syntax: char (*arr_ref)[N]

**SWAP statement fix:**
- Fixed-length strings use strcpy in a block instead of direct assignment

**Remaining issue:** Variable name suffix mismatch (~140 errors) — variables declared as "DIM x AS STRING" but used as "x$", producing "x_str" vs "x" mismatch in generated C code.

**Stats:** 3 files changed, 277 insertions(+), 33 deletions(-)

---

## 5. `80f0a69` — Phase C Session 6: variable name suffix mismatch — **APPLIED**

**Subject:** feat(semantic): Phase C Session 6 - fix variable name suffix mismatch

**What changed:**

Add suffix fallback logic to symbol table lookups. When a variable is declared as `DIM x AS STRING` but referenced as `x$`, the symbol table now finds the base declaration by stripping the suffix and checking type compatibility.

**Changes:**
- symbols.rs: Add suffix_matches_type() helper and fallback logic to lookup_symbol, lookup_scalar, lookup_array, lookup_global_symbol
- expressions.rs: Use symbol.name.clone() in check_identifier instead of raw reference name
- assignments.rs: Return resolved symbol names from all assignment handlers (check_assignment, check_array_assignment, etc.)
- statements.rs: Fix FileGet/FilePut InputTarget::Variable name resolution
- stmt.rs: Remove experimental pass 3 expression variable collector (caused regression from 101 to 172 errors)

**Progress:** GCC errors 142 → 105 (26% reduction this session, 99.3% total)

**Stats:** 7 files changed, 542 insertions(+), 254 deletions(-)

---

## 6. `310c344` — Phase C Session 7: implicit variable collection for ByRef args — **APPLIED**

**Subject:** feat(codegen): Phase C Session 7 - implicit variable collection for ByRef args

**What changed:**

Reduces GCC errors from 105 to 51 (51% reduction this session, 99.6% total).

**Semantic checker fixes:**
- LSET/RSET: Resolve variable names through symbol lookup before codegen
- Array access: Use symbol.name instead of raw input name for consistency

**Code generator fixes:**
- Add targeted ByRef argument variable collection for implicit declarations
- Only declare variables when passed as ByRef function args (avoids declaring global/shared arrays as local scalars)
- Add SHARED variable handling to prevent duplicate local declarations
- Properly handle FixedString type in implicit variable declarations

**Remaining:** 51 errors are mostly variables not in ByRef contexts.

**Stats:** 4 files changed, 314 insertions(+), 42 deletions(-)

---

## 7. `060d82f` — Phase C Session 7 continuation: implicit locals for main() — **APPLIED**

**Subject:** feat(codegen): Phase C Session 7 continuation - implicit locals for main()

**What changed:**

Reduce GCC errors from 51 to 31 (39% reduction, 99.79% total from initial 14,547).

**Runtime additions:**
- Add qb_lbound/qb_ubound stub functions for array bounds
- Add qb_lbound2/qb_ubound2 for 2-argument versions with dimension
- Add qb_font_get() for _FONT pseudo-variable (zero-arg read)

**Code generation:**
- Add LBOUND/UBOUND special handling for 2-arg variants in expr.rs
- Add FileGet/FileLineInput/Input/LineInput target variable declaration
- Add Call statement ByRef parameter variable declaration
- Add main() implicit local variable collection (previously only SUB/FUNCTION)
- Fix const declaration parsing in global variable name extraction

**Remaining:** 31 errors are in harder categories: LEN(dummy_*) pattern, read-only variable references, and macro expansion edge cases.

**Stats:** 5 files changed, 245 insertions(+), 57 deletions(-)

---

## 8. `c5b3cd0` — Macro collisions and LEN() type sizing — **APPLIED**

**Subject:** Fix macro collisions and LEN() type sizing for QB64pe bootstrap

**What changed:**

- Exclude _TRUE/_FALSE from local variable declarations (macro collision)
- Exclude string constant macros (_CHR_QUOTE, _STR_*, etc.) from declarations
- Add dummy_* global variables for LEN() type sizing pattern
- Implement type-specific LEN handling: qb_len_str for strings, sizeof for numerics
- Add exclusion lists to both collect_implicit_locals and collect_globals

**Progress:** GCC errors 31 → 18 (42% reduction this session, 99.88% total). Remaining errors are read-only variable references in original QB64pe code.

**Stats:** 5 files changed, 101 insertions(+), 15 deletions(-)

---

## 9. `bdeb0ba` — Suffix fallback for constant lookups — **APPLIED**

**Subject:** Add suffix fallback for constant lookups in symbol table

**What changed:**

When looking up a constant reference without a type suffix (e.g., `idecpnum`), also check for suffixed variants (e.g., `idecpnum&`) in both local and global scope. This handles the common BASIC pattern:

```basic
CONST idecpnum& = 27    ' Defined with suffix
IF x > idecpnum THEN    ' Referenced without suffix
```

The lookup is constrained to constants only to avoid incorrectly matching distinct variables (since in BASIC, x$ and x% are different variables but constants are unique by base name).

**Progress:** GCC errors 18 → 16 (2 fixed, idecpnum lookup now works)

**Stats:** 1 file changed, 35 insertions(+)

---

## 10. `c6cd917` — Variable scoping and declaration issues (bootstrap 15 → 0 errors) — **APPLIED**

**Subject:** Fix variable scoping and declaration issues for QB64PE bootstrap

**What changed:**

- Hoist scalar DIM declarations to function scope (BASIC function scope vs C block scope) while keeping array DIMs inline for runtime allocation
- Add collect_shared_vars to detect variables used in SHARED statements that need implicit global declarations
- Skip built-in constants (_EQUAL, _GREATER, _LESS, _KEY_*, etc.) in implicit local collection to avoid redeclaration errors
- Add collect_vars_from_expr to detect read-only variables in expressions that still need declaration in C
- Filter system functions (getpid, etc.) in DECLARE LIBRARY extern declarations to avoid conflicts with C library headers
- Pass global_var_names to StmtEmitter so SUB/FUNCTION emitters can properly exclude globals from implicit local collection
- Add handling for Color, Print, Call, SelectCase, ArrayAssignment, and FieldAssignment in collect_stmt_byref for ByRef argument detection

**Result:** These changes bring QB64PE bootstrap compilation from 15 errors to 0.

**Stats:** 3 files changed, 434 insertions(+), 44 deletions(-)

---

## 11. `7d35e62` — Extract RESERVED_IDENTIFIERS constant — **APPLIED**

**Subject:** refactor(codegen): extract RESERVED_IDENTIFIERS constant

**What changed:**

Move the duplicated list of built-in constants and runtime variables (that should never be redeclared) to a shared constant in types.rs.

- Add RESERVED_IDENTIFIERS const array with all reserved names
- Add add_reserved_identifiers() helper function
- Update analysis.rs and stmt.rs to use the shared helper
- Add C_BACKEND_REFACTORING.md documenting remaining tech debt

This removes ~45 lines of duplicated code and ensures the reserved identifier list stays in sync across both global and local variable collection.

**Stats:** 4 files changed, 302 insertions(+), 48 deletions(-)

---

## 12. `322d9dc` — Extract declare_scalar_var and declare_array_var helpers — **APPLIED**

**Subject:** refactor(codegen): extract declare_scalar_var and declare_array_var helpers

**What changed:**

Add two helper functions to types.rs that consolidate the duplicated variable declaration patterns:

- **declare_scalar_var:** handles String (NULL), FixedString (char[N]), UserDefined ({0}), and other types with default_init
- **declare_array_var:** handles array pointers with proper FixedString support (char (*name)[N])

These helpers are now used in:
- analysis.rs: collect_globals, collect_redim_shared, collect_shared_vars, collect_implicit_vars_from_stmt, collect_vars_from_expr
- stmt.rs: collect_dims, collect_implicits, collect_byref_vars, declare_input_target

Net reduction of ~95 lines of duplicated code while maintaining identical behavior (verified with QB64PE bootstrap compilation).

**Stats:** 3 files changed, 119 insertions(+), 214 deletions(-)

---

## 13. `503155b` — Extract implicit_vars module and fix function call names — **APPLIED**

**Subject:** refactor(codegen): extract implicit_vars module and fix function call names

**What changed:**

- Extract nested functions from stmt.rs into new implicit_vars.rs module for improved testability and maintainability
- Move infer_type_from_suffix from analysis.rs to types.rs where it belongs with other type-related utilities
- **Fix critical bug in expressions.rs:** function calls now use the procedure's canonical name (with type suffix) instead of the caller's name, ensuring correct C function names are generated (e.g., qb_getelement_str instead of qb_getelement)

The function call fix is essential for QB64PE bootstrap — without it, generated C code references non-existent functions.

**Note:** This commit also included a large docs/repo reorganization (ARCHITECTURE.md, DEVELOPMENT.md, docs/ layout, STUB_FUNCTIONS, TESTING.md, BOOTSTRAP_PLAN, codebase review docs, etc.).

**Stats:** 36 files changed, 7116 insertions(+), 4694 deletions(-)

---

## Summary

| Order | Hash     | Focus                                      | GCC errors (after) |
|-------|----------|--------------------------------------------|--------------------|
| 1     | e7bbd59  | Phase C validation, two-pass locals, byref | 14,547 → 325       |
| 2     | d8f974b  | END/SYSTEM exit code, static strings, labels | 325 → 296        |
| 3     | cafedb7  | BYREF params, keyboard constants           | 296 → 169          |
| 4     | ef4a7a9  | ASCII/CHR constants, array params, SWAP    | 169 → 142          |
| 5     | 80f0a69  | Variable name suffix mismatch (semantic)   | 142 → 105          |
| 6     | 310c344  | ByRef implicit var collection              | 105 → 51           |
| 7     | 060d82f  | main() implicit locals, LBOUND/UBOUND      | 51 → 31            |
| 8     | c5b3cd0  | Macro collisions, LEN() type sizing        | 31 → 18            |
| 9     | bdeb0ba  | Constant suffix fallback                   | 18 → 16            |
| 10    | c6cd917  | Scoping, SHARED, collect_shared_vars       | 15 → 0             |
| 11    | 7d35e62  | RESERVED_IDENTIFIERS constant               | (refactor)          |
| 12    | 322d9dc  | declare_scalar_var / declare_array_var     | (refactor)          |
| 13    | 503155b  | implicit_vars module, function call names  | (refactor + fix)    |

By end of Jan 22, QB64pe bootstrap compilation reached **0 GCC errors** (commit c6cd917); the final three commits were refactors and the critical function-call-name fix.
