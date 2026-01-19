# Session 023: QB45 Compatibility Fixes

**Date:** 2026-01-19
**Focus:** Running QB45 test programs through the compiler and fixing errors

## Summary

Ran the QB45/QBasic test suite (143 .bas files) through the QB64Fresh compiler and fixed several compatibility issues. Improved pass rate from ~39% (56 files) to ~45% (64 files) of files compiling successfully through multiple fix iterations.

## Issues Fixed

### 1. Optional Function Parameters
Added support for optional parameters in built-in functions:

- **MID$(string, start [, length])** - Third parameter is now optional
- **RND([seed])** - Seed parameter is now optional
- **INSTR([start,] string, substring)** - Start position is now optional

Implementation: Added `is_optional` field to `ParameterInfo` struct and `required_param_count()` method to `ProcedureEntry`. Updated argument validation to use min/max ranges.

### 2. Missing PEEK Function
Added `PEEK(address)` built-in function for reading memory (stubbed to return 0 for safety, like other DOS-era memory functions).

### 3. LINE INPUT Separator
LINE INPUT now accepts either semicolon OR comma after the prompt:
```basic
LINE INPUT "prompt"; var$   ' Original supported syntax
LINE INPUT "prompt", var$   ' Now also supported (common in old QB code)
```

### 4. DATA Statement Unquoted Strings
Fixed parsing of unquoted strings in DATA statements. BASIC allows DATA values like:
```basic
DATA o3e-o2b-ge-   ' PLAY music notation - now parsed correctly
```

The parser now collects all tokens until comma/newline for unquoted identifier values.

### 5. STRING$ Type Flexibility
Changed STRING$(n, char) second parameter to accept either:
- Integer (ASCII code): `STRING$(10, 65)` -> "AAAAAAAAAA"
- String (single char): `STRING$(10, "A")` -> "AAAAAAAAAA"

### 6. Variable Array Bounds (DIM with runtime expressions)
Fixed `DIM array(1 TO variable)` to allow non-constant array bounds. QB/BASIC allows runtime-evaluated bounds. The semantic analyzer now uses placeholder values when constant evaluation fails, allowing runtime-sized arrays.

### 7. DIM SHARED Module-Level Visibility
Fixed `DIM SHARED` variables declared at module level to be visible in SUB/FUNCTION procedures. Added `module_shared_vars` HashSet to track module-level shared variables and updated symbol lookup to check this set when in procedure scope.

```rust
// In symbols.rs
module_shared_vars: HashSet<String>,

pub fn add_module_shared_var(&mut self, name: String) {
    self.module_shared_vars.insert(strip_suffix(&name).to_uppercase());
}

// In lookup_symbol - for procedure scopes
if self.module_shared_vars.contains(&name_upper) {
    return self.scopes.get(&ScopeId::GLOBAL)?.symbols.get(&name_upper);
}
```

### 8. MID$ as LValue (Substring Assignment)
Added support for `MID$(str$, pos, len) = value$` for in-place string modification. This required changes across the entire pipeline:

- **Parser**: New `parse_mid_statement()` function detects MID$ at statement start
- **AST**: New `StatementKind::MidAssignment` variant
- **Semantic**: New `check_mid_assignment()` validates types (string target, numeric indices, string value)
- **Typed IR**: New `TypedStatementKind::MidAssignment` variant
- **Codegen**: Generates calls to `qb_mid_assign()` runtime function

```c
// Generated C code for: MID$(a$, 7, 5) = "BASIC"
qb_mid_assign(&a_str, 7LL, 5LL, qb_string_new("BASIC"));
```

## Known Remaining Issues

### QB64-Specific Extensions
Many test files use QB64-only features (`_SNDPLAYFILE`, `_UNSIGNED`, etc.) which are not yet implemented. These are expected failures for QB4.5 compatibility testing.

### UTF-8 Encoding
~5 files have non-UTF-8 characters (old DOS code pages) and fail to load.

## Test Results

| Metric | Initial | After Round 1 | Final |
|--------|---------|---------------|-------|
| Passing files | ~56 | 57 | 64 |
| Failing files | ~87 | 86 | 79 |
| Total | 143 | 143 | 143 |

## Files Modified

### Round 1 (Optional Parameters, PEEK, LINE INPUT, DATA)
- `src/semantic/symbols.rs` - Added `is_optional` to ParameterInfo, `required_param_count()` method
- `src/semantic/mod.rs` - Updated MID$, RND, INSTR, STRING$ registrations; added PEEK
- `src/semantic/error.rs` - Changed ArgumentCountMismatch to use min/max range
- `src/semantic/checker/expressions.rs` - Updated argument count validation
- `src/semantic/checker/control_flow.rs` - Updated argument count validation
- `src/parser/statements.rs` - Fixed LINE INPUT, DATA unquoted strings
- `src/codegen/c_backend/expr.rs` - Added PEEK function mapping
- `src/codegen/c_backend/runtime.rs` - Added qb_peek() implementation

### Round 2 (Variable Array Bounds, DIM SHARED, MID$ LValue)
- `src/semantic/symbols.rs` - Added `module_shared_vars` HashSet, `add_module_shared_var()` method, updated `lookup_symbol()`
- `src/semantic/checker/definitions.rs` - Allow non-constant array bounds, track module-level SHARED vars
- `src/semantic/checker/statements.rs` - Handle MidAssignment, updated array dimension evaluation
- `src/semantic/checker/assignments.rs` - Added `check_mid_assignment()` function
- `src/semantic/typed_ir.rs` - Added `TypedStatementKind::MidAssignment` variant
- `src/ast/stmt.rs` - Added `StatementKind::MidAssignment` variant
- `src/parser/statements.rs` - Added `parse_mid_statement()`, detect MID$ in array assignment context
- `src/codegen/c_backend/stmt.rs` - Added MidAssignment codegen handler
- `src/codegen/c_backend/runtime.rs` - Added `qb_mid_assign()` runtime function
