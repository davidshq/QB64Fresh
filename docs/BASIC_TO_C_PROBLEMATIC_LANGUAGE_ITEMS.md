# BASIC Language Items Problematic for Rust/C

**Last Updated:** 2026-01-28 (updated to reflect BYREF scalar fix and ON ERROR _NEWHANDLER implementation)  
**Purpose:** Catalog language items in BASIC that are tricky when compiling to C (and when implementing the compiler in Rust). Includes items that have already caused bugs and items that may still cause problems.

**See also:**
- [REGRESSION_TEST_COVERAGE.md](REGRESSION_TEST_COVERAGE.md) — bugs fixed and test coverage
- [KEY_LEARNINGS.md](KEY_LEARNINGS.md) — compiler implementation gotchas
- [QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md](QB64pe/QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md) — semantic/behavioral differences
- [adrs/ADR-0008-c-interoperability.md](adrs/ADR-0008-c-interoperability.md) — C interop type mapping

---

## Overview

QB64 BASIC has many features that do not map directly to C or Rust:

1. **String model** — dynamic vs fixed-length, reference semantics, temp lifetimes
2. **Type system** — suffixes, DEFTYPE, numeric/string coercion, UDTs
3. **Parameter passing** — BYREF vs BYVAL, writeback, C interop
4. **Syntax ambiguity** — same syntax for array access and function calls
5. **Array semantics** — OPTION BASE, bounds, multi-dimensional indexing
6. **Control flow** — GOTO scope, ON ERROR, line numbers

The sections below list **items that have caught us** (caused real bugs or required special handling) and **items that may still be problematic** (known limitations or areas to watch).

---

## 1. Strings

### 1.1 Items That Have Caught Us

| Item | Why It's Problematic | What We Did |
|------|----------------------|-------------|
| **QbString vs qb_string type name** | Runtime header uses `QbString*`; codegen once emitted `qb_string*`. C compiler sees incompatible pointer types. | Standardized on `QbString*` in all emitted code to match `runtime/include/qb64fresh_rt.h`. |
| **String double-wrapping** | Passing an already-wrapped `qb_str_from_c(...)` result into BYREF/BYVAL or built-ins caused double wrap → 807 C errors. | Added `unwrap_qb_str_from_c()`; avoid wrapping already-wrapped strings at call sites, BYREF/BYVAL, and MID$ assignment. |
| **String temp pool and cleanup** | Temps from expressions (e.g. `qb_string_concat`) were never freed → 42+ GB growth. | Temp pool with `qbs_tmp_register`/`qbs_cleanup`; scoped cleanup in loops and per-statement in main/procedures. |
| **Temp pool overflow** | When pool was full, overflow wasn't tracked → unbounded allocation. | Overflow tracking; `qbs_tmp_base_get()` returns packed bases; cleanup handles both pool and overflow. |
| **Fixed-length string in MID$ assignment** | MID$ on fixed-length string/array used wrong C type (pointer to ref-counted string vs `char[]`) → stack corruption. | Detect `FixedString` and use manual `strlen`/`strncpy` instead of `qb_mid_assign`. |
| **SELECT CASE with strings** | Case comparison assumed numeric or didn't wrap fixed-length strings for `qb_string_compare`. | Pass test expression type through; use `qb_string_compare` for equality/ranges/relational; wrap fixed-length for compare. |
| **C APIs expecting `const char*`** | Built-ins like `qb_shell`, `qb_gfx_printstring` take `const char*`; we pass `QbString*`. | Use `qb_string_data()` (or equivalent) at call sites so C sees `const char*`. |
| **qbt_ParseNum* vs qb_string*** | Runtime parse/format APIs use a different internal type; mixing caused type errors. | Align runtime API and codegen to use the correct type in generated C. |

### 1.2 Items That May Still Be Problematic

| Item | Risk | Notes |
|------|------|--------|
| **BYREF STRING to DECLARE LIBRARY** | C has no direct “BASIC string by reference”; passing as output param is awkward. | ADR-0008: BYREF STRING not directly supported for C interop; use _OFFSET or document workaround. |
| **Very long concatenations** | Many temps in one expression could stress temp pool or code size. | Current design holds; watch for edge cases in huge expressions. |
| **String literal lifetime vs C** | BASIC string literals become C string constants; must not be freed. | Static/const usage in runtime; ensure no free of literals. |

---

## 2. Types and Type System

### 2.1 Items That Have Caught Us

| Item | Why It's Problematic | What We Did |
|------|----------------------|-------------|
| **Binary/unary operators on incompatible types** | BASIC allows some coercions; we reject e.g. STRING + number or `-"hello"`. | Semantic errors: `InvalidBinaryOp`, `InvalidUnaryOp` with clear messages. |
| **_IIF true/false type mismatch** | Both branches must be same type (both string or both numeric). | `IifTypeMismatch` semantic error. |
| **Argument type mismatch** | Procedure calls must match parameter types. | `ArgumentTypeMismatch` with position and expected/found types. |
| **Fixed-length vs dynamic string** | `STRING * N` is `char buf[N]` in C; dynamic is `QbString*`. Assignments and comparisons must not mix them incorrectly. | Type checker distinguishes; codegen uses `strncpy` for fixed-length, ref-counted ops for dynamic. |
| **Type name consistency in emitted C** | Typedef ordering and naming (e.g. `qb_string` vs `QbString`) caused “incomplete type” or “incompatible pointer” errors. | TypeRegistry and dependency-ordered emission; single canonical name in header and codegen. |
| **Function signature vs codegen** | Semantic analyzer and codegen could disagree on parameter/return types. | TypeRegistry and shared type mapping so signatures match emitted C. |

### 2.2 Items That May Still Be Problematic

| Item | Risk | Notes |
|------|------|--------|
| **Numeric coercion (e.g. INTEGER + LONG)** | C rules differ from BASIC (promotion, overflow). | We rely on C semantics; document where behavior may differ from QB64pe. |
| **DEFTYPE scope** | DEFLNG A–Z etc. affect default types; must apply before procedure parameters. | Two-pass semantic; DEFTYPE processed before procedures. Edge cases may remain. |
| **Type suffix normalization** | `count&` vs `count_lng` internally; suffix fallback for constants. | Implemented; watch for lookup bugs when suffix differs. |
| **UDT with fixed-length string fields** | Layout and assignment (strncpy, no free) must match runtime. | Handled; regression tests for UDT string fields. |

---

## 3. Parameter Passing (BYREF / BYVAL)

### 3.1 Items That Have Caught Us

| Item | Why It's Problematic | What We Did |
|------|----------------------|-------------|
| **BYREF string writeback** | Changes inside SUB/FUNCTION must be visible to caller. | Explicit writeback of BYREF string parameters before return/EXIT. |
| **BYVAL string at call site** | Must pass one wrapped value, not double-wrap. | Unwrap when argument is already `qb_str_from_c(...)`; single wrap for C string. |
| **BYREF and temp strings** | Passing temp string by reference required correct lifetime and writeback. | Temp pool + writeback so caller’s variable receives the result. |
| **BYREF scalar (integer/float)** | Codegen initially used local *copies* (`int32_t x = *x_ref;`). Modifications inside the procedure were **not** written back. | Fixed: use pointer alias in body (`int32_t* n = n_ref`) and dereference on use (`*n = value`). Tracked via `current_func_byref_scalar_names` in expr/assignments/control_flow. Integration and execution tests verify write-through. |

### 3.2 Items That May Still Be Problematic

| Item | Risk | Notes |
|------|------|--------|
| **BYREF with complex expressions** | Passing an expression by reference (if ever allowed) would need temp and writeback. | Typically BASIC passes variables; document if we support expression-byref. |
| **DECLARE LIBRARY BYVAL vs BYREF** | C expects BYVAL for most params; BYREF for output. | ADR-0008: BYVAL default for C; BYREF STRING not directly supported. |

---

## 4. Arrays

### 4.1 Items That Have Caught Us

| Item | Why It's Problematic | What We Did |
|------|----------------------|-------------|
| **Array access vs function call** | Same syntax `name(args)`. Semantic must decide: array or function. | No separate ArrayElement in AST; use `FunctionCall` and resolve via symbol table (array vs procedure). |
| **Array variable rename in codegen** | Array base name may be renamed (e.g. shadowing); access must use renamed name. | Apply `variable_renames` in `emit_array_access` and `emit_array_field_assignment`. |
| **Array dimension and OPTION BASE** | C arrays are 0-based; BASIC can be OPTION BASE 1. | Symbol table stores lower bound; codegen emits index calculation (e.g. `i - lower`). |
| **REDIM SHARED** | Shared arrays must refer to global, not local. | REDIM codegen handles SHARED and global symbol. |
| **Array scoping (main vs procedure)** | Main uses global arrays; procedures use local unless SHARED. | Codegen uses correct scope so globals vs locals are emitted correctly. |
| **Local array shadowing parameter** | e.g. `DIM args(5) AS ParseNum` inside a sub with parameter `args AS STRING`. | Codegen must not confuse parameter and local array (name + type). |

### 4.2 Items That May Still Be Problematic

| Item | Risk | Notes |
|------|------|--------|
| **LBOUND/UBOUND in inline runtime** | Inline runtime stubs don’t track bounds per array. | “Proper implementation would track array bounds”; external runtime may differ. |
| **Very large or many dimensions** | C stack/heap and our size computation must be consistent. | No known bugs; watch for overflow in index/size calculations. |
| **OPTION BASE per file/module** | If we ever support multiple modules, OPTION BASE scope could be subtle. | Currently single global OPTION BASE. |

---

## 5. Control Flow and Scoping

### 5.1 Items That Have Caught Us

| Item | Why It's Problematic | What We Did |
|------|----------------------|-------------|
| **Duplicate labels** | Same label emitted multiple times → C duplicate symbol. | Track `emitted_labels` in StmtEmitter and skip re-emitting. |
| **Forward declarations** | Cross-module or cross-procedure references (e.g. `qb_gfx_screen`) need declarations before use. | `emit_forward_declarations()` in codegen. |
| **EXIT/CONTINUE outside loop** | Invalid use must be rejected. | Semantic errors: `ExitOutsideLoop`, `ContinueOutsideLoop`. |
| **RETURN outside procedure** | RETURN only valid inside FUNCTION/SUB. | `ReturnOutsideContext` semantic error. |
| **FOR/NEXT variable mismatch** | NEXT must match the controlling FOR variable. | `ForNextMismatch` semantic error. |
| **ON ERROR GOTO _NEWHANDLER label** | QB64 extension: `_NEWHANDLER` modifier pushes a new error handler scope. Parser must combine `_NEWHANDLER` with following label as one statement. | Parser treats `_NEWHANDLER` + label as one statement; codegen strips prefix and uses the label. Parser unit test, bootstrap test, and integration test verify. Documented in error_jump.rs and PARTIAL_IMPLEMENTATIONS. |

### 5.2 Items That May Still Be Problematic

| Item | Risk | Notes |
|------|------|--------|
| **Cross-function GOTO** | BASIC allows GOTO label in another procedure; we do not. | Documented restriction; can give clearer errors. |
| **ON ERROR GOTO 0** | Special case: disables handler. | Parser/semantic handle line number 0 as “disable”. |
| **Resume behavior** | RESUME, RESUME NEXT, RESUME line — subtle order and state. | Implemented; edge cases may remain vs QB64pe. |

---

## 6. C Interoperability (DECLARE LIBRARY)

### 6.1 Items That Have Caught Us

| Item | Why It's Problematic | What We Did |
|------|----------------------|-------------|
| **STRING to C** | C expects `const char*` or `char*`; we have `QbString*`. | BYVAL STRING → temporary null-terminated copy as `const char*` at call. |
| **Return type qb_string* vs QbString*** | Header uses `QbString*`; codegen must match. | Use same type name as in runtime header everywhere. |
| **ALIAS for C name** | BASIC name may differ from C symbol. | DECLARE LIBRARY supports ALIAS for external name. |

### 6.2 Items That May Still Be Problematic

| Item | Risk | Notes |
|------|------|--------|
| **BYREF STRING to C** | No direct mapping; C often uses `char* buf, size_t len` or similar. | Document as unsupported; suggest _OFFSET or wrapper. |
| **Complex struct/union in headers** | Nested structs, unions, bitfields — header parser may not support. | ADR-0008: limitations listed; avoid or hand-declare. |
| **Platform-specific macros** | `#ifdef WIN32` etc. in headers. | Parser predefines platform macros; complex expressions may be wrong. |
| **Function-like macros** | `#define FOO(x) ...` not imported. | Skipped by design; use real C functions. |

---

## 7. Lexer and Parser

### 7.1 Items That Have Caught Us

| Item | Why It's Problematic | What We Did |
|------|----------------------|-------------|
| **$CONSOLE** | Lexer tokenized as Error with “$CONSOLE” text. | Parser workaround: treat Error token containing `"$CONSOLE"` as directive. |
| **SUB call with parenthesized args** | `SubName (a), (b)` looked like function call. | Parse SUB args uniformly as expressions; don’t special-case `(`. |
| **Function/array in comparison** | `x = arr(1) = 5` or `ASC("A") = 65` — parser thought assignment. | Early check: only treat as array assignment if token after identifier is `(`. |
| **STATIC AS type var1, var2** | Type-first syntax for STATIC not supported. | Added type-first STATIC parsing to match DIM. |
| **Single-line IF-THEN-ELSE with colons** | Colon-separated statements after ELSE. | Parser handles ELSE terminator and colons. |
| **DATA with operator-like tokens** | DATA values can look like operators. | DATA parsing accepts operator tokens as values. |
| **Empty array dimensions** | DIM x() or similar. | Parser allows empty dimensions where valid. |

### 7.2 Items That May Still Be Problematic

| Item | Risk | Notes |
|------|------|--------|
| **Reserved words as identifiers** | Some contexts allow e.g. “default” as name. | Mangle to C-safe name (e.g. `default_`); ensure no clash with C keywords. |
| **Line numbers vs labels** | GOTO 100 vs GOTO label — both exist. | Parser/semantic distinguish; codegen emits correct target. |
| **Colon and line continuation** | Line break vs statement separator. | Rules documented; edge cases in rare formatting. |

---

## 8. Identifiers and Name Mangling

### 8.1 Items That Have Caught Us

| Item | Why It's Problematic | What We Did |
|------|----------------------|-------------|
| **Type suffixes in C names** | `count&` → unique C name; suffix is part of identifier in BASIC. | Canonical names: e.g. `count_lng`; suffix fallback for lookups. |
| **Dots in names** | `path.exe` — C doesn’t allow `.` in identifiers. | Replace with `_` e.g. `path_exe`. |
| **Reserved C/BASIC words** | `default`, `switch`, etc. | Mangling (e.g. `default_`) so generated C is valid. |
| **Function return name** | FUNCTION Foo$ → assignment to `Foo$`; name must match. | Canonical name used consistently in codegen. |

### 8.2 Items That May Still Be Problematic

| Item | Risk | Notes |
|------|------|--------|
| **Unicode or non-ASCII identifiers** | QB64 allows some; C is ASCII. | May need encoding or restriction; document. |
| **Name length** | C limits identifier length. | Unlikely for normal BASIC; could truncate or hash in theory. |

---

## 9. Constants and Literals

### 9.1 Items That Have Caught Us

| Item | Why It's Problematic | What We Did |
|------|----------------------|-------------|
| **_CHR_*, _STR_* constants** | Used in source but not in symbol table. | Registered ~60 _CHR_* / _STR_* built-in constants. |
| **Constant in DIM bounds** | Array bounds can be constant expressions. | Const-eval for bounds; semantic checks. |
| **Assignment to CONST** | Rejection required. | `AssignmentToConst` semantic error. |

### 9.2 Items That May Still Be Problematic

| Item | Risk | Notes |
|------|------|--------|
| **Floating-point literal rounding** | BASIC vs C literals; different precisions. | Use C literals; document if we ever normalize. |
| **Hex/octal literals** | Range and sign (e.g. &HFFFFFFFF). | Match QB64 semantics where possible. |

---

## 10. Built-ins and Runtime

### 10.1 Items That Have Caught Us

| Item | Why It's Problematic | What We Did |
|------|----------------------|-------------|
| **_SHELLHIDE as function** | Used as function returning LONG; was only registered as statement. | Registered as built-in function returning LONG. |
| **_STATUSCODE** | Array[256] HTTP status; legacy pattern. | Stub returning 200; document. |
| **ParseNum / VAL and string type** | Some built-ins take or return internal parse type; mixing with QbString caused errors. | Align types in runtime API and codegen. |

### 10.2 Items That May Still Be Problematic

| Item | Risk | Notes |
|------|------|--------|
| **RND / RANDOMIZE** | Different PRNG than QB64pe → different sequence. | Document; acceptable for compatibility level. |
| **TIMER accuracy** | Parameter and platform differences. | Implemented; document platform behavior. |
| **SHELL return value** | Exit code handling; platform differences. | Document. |

---

## Summary Table

| Category | Caught Us (resolved or documented) | May Still Be Problematic |
|----------|-------------------------------------|---------------------------|
| Strings | 8 items (types, double-wrap, temp pool, fixed-length, SELECT CASE, C API) | 3 (BYREF to C, long concat, literal lifetime) |
| Types | 6 items (operators, _IIF, args, fixed-length, typedefs, signatures) | 4 (coercion, DEFTYPE, suffix, UDT) |
| BYREF/BYVAL | 4 items (string writeback, BYVAL unwrap, temp, BYREF scalar fix) | 1 (DECLARE LIBRARY) |
| Arrays | 6 items (array vs call, rename, OPTION BASE, REDIM SHARED, scope, shadowing) | 3 (LBOUND/UBOUND inline, huge arrays, OPTION BASE scope) |
| Control flow | 6 items (labels, forwards, EXIT/CONTINUE/RETURN, FOR/NEXT, ON ERROR _NEWHANDLER) | 3 (cross-proc GOTO, ON ERROR 0, Resume) |
| C interop | 3 items (STRING→C, return type, ALIAS) | 4 (BYREF STRING, structs, macros, platform) |
| Parser/Lexer | 7 items ($CONSOLE, SUB args, comparison, STATIC, IF-THEN-ELSE, DATA, empty dims) | 3 (reserved words, line numbers, colons) |
| Identifiers | 4 items (suffix, dots, reserved, function name) | 2 (Unicode, length) |
| Constants | 3 items (_CHR_/_STR_, DIM bounds, CONST assign) | 2 (float literal, hex/octal) |
| Built-ins | 3 items (_SHELLHIDE, _STATUSCODE, ParseNum) | 3 (RND, TIMER, SHELL) |

**Highest-impact remaining:** Any new code paths that pass strings (double-wrap, fixed-length, C API types), and BYREF with complex expressions if ever supported.
