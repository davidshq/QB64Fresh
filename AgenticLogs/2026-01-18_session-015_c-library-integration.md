# Session 015: Phase 5 - C Library Integration

**Date:** 2026-01-18
**Focus:** DECLARE LIBRARY / DECLARE DYNAMIC LIBRARY

## Session Goals

Implement C library integration allowing BASIC programs to call external C functions:
- `DECLARE LIBRARY "name"` - Static library linking
- `DECLARE DYNAMIC LIBRARY "name"` - Runtime library loading
- Function/Sub declarations with BYVAL and ALIAS support
- Type marshalling between BASIC and C types

## Implementation Progress

| Feature | Lexer | AST | Parser | Semantic | Codegen | Status |
|---------|-------|-----|--------|----------|---------|--------|
| DECLARE LIBRARY | ✅ | ✅ | ✅ | ✅ | ✅ | Complete |
| DECLARE DYNAMIC LIBRARY | ✅ | ✅ | ✅ | ✅ | ✅ | Parsed (runtime deferred) |
| END DECLARE | ✅ | ✅ | ✅ | ✅ | ✅ | Complete |
| BYVAL parameter | ✅ | ✅ | ✅ | ✅ | ✅ | Complete |
| ALIAS clause | ✅ | ✅ | ✅ | ✅ | ✅ | Complete |

## Technical Design

### Syntax

```basic
DECLARE [DYNAMIC] LIBRARY ["library_name"]
    FUNCTION name[suffix]([BYVAL] param AS type, ...) [ALIAS "c_name"]
    SUB name([BYVAL] param AS type, ...) [ALIAS "c_name"]
END DECLARE
```

### AST Representation

```rust
enum StatementKind {
    DeclareLibrary {
        library_name: Option<String>,
        is_dynamic: bool,
        declarations: Vec<ExternalDeclaration>,
    }
}

struct ExternalDeclaration {
    name: String,
    alias: Option<String>,
    params: Vec<ExternalParam>,
    return_type: Option<BasicType>,
    is_function: bool,
}

struct ExternalParam {
    name: String,
    typ: BasicType,
    is_byval: bool,
}
```

### Code Generation Strategy

**Static Library:**
- Emit `extern` declarations for C functions
- Add library to linker flags

**Dynamic Library:**
- Generate function pointer types
- Emit runtime `dlopen`/`LoadLibrary` code (deferred)
- Emit `dlsym`/`GetProcAddress` for each function (deferred)

## Session Notes

### Files Modified

1. **src/lexer/token.rs** - Added `Library`, `Dynamic`, `Alias` tokens
2. **src/ast/stmt.rs** - Added `DeclareLibrary` statement kind, `ExternalDeclaration`, `ExternalParam` structs
3. **src/parser/statements.rs** - Added `parse_declare_library`, `parse_external_declaration`, `parse_external_param_list` functions
4. **src/semantic/symbols.rs** - Added `SymbolKind::ExternalFunction` variant
5. **src/semantic/typed_ir.rs** - Added `TypedStatementKind::DeclareLibrary`, `TypedExternalDeclaration`, `TypedExternalParam`
6. **src/semantic/checker/statements.rs** - Added semantic analysis for DECLARE LIBRARY
7. **src/codegen/c_backend/stmt.rs** - Added code generation for extern declarations

### Key Design Decisions

1. **Symbol Table Registration**: External functions are registered in the symbol table with `SymbolKind::ExternalFunction` storing the C name, parameter types, and return type
2. **Type Suffix Support**: Function names with type suffixes (e.g., `add_values&`) automatically infer return type
3. **ALIAS Mapping**: When ALIAS is specified, the C name is stored separately from the BASIC name
4. **Dynamic Library Deferred**: Runtime loading (`dlopen`/`LoadLibrary`) is parsed but code generation deferred for future implementation

### Example Created

`examples/declare_library_demo.bas` - Demonstrates:
- Declaring C standard library functions (`isdigit`, `toupper`, `abs`)
- Using ALIAS for name mapping
- BYVAL parameter passing

## Outstanding Work

- Runtime loading for DECLARE DYNAMIC LIBRARY (`dlopen`/`LoadLibrary`)
- Automatic header parsing
- Complex type marshalling (structs, unions)
- Callback support (C calling BASIC functions)

## Codebase Review (Continued Session)

After C Library Integration was committed, a comprehensive codebase review was performed.

### Review Summary

Created `docs/ThingsToDo/2026-01-18_codebase-review.md` with findings:

**Critical Issues (Document/Accept):**
- SHELL command injection risk (expected BASIC behavior)
- Path traversal in file operations (expected BASIC behavior)

**High Priority:**
- 1,024 `unwrap()` calls (up from 945) - needs incremental conversion
- 12 `panic!()` calls in non-panic contexts
- Test coverage gaps - major modules like `stmt.rs` have no dedicated tests

**Medium Priority:**
- 200+ excessive `.clone()` calls
- Inconsistent error handling patterns
- Missing module documentation

**Architecture Health:**
- Clean pipeline separation (lexer → parser → semantic → codegen)
- Well-designed trait abstractions for backends
- stmt.rs files growing large (1000-2500 lines) - consider splitting

### Progress Assessment

| Phase | Status |
|-------|--------|
| Phase 1 | Complete |
| Phase 2 | Complete |
| Phase 3 | In Progress (framework done) |
| Phase 4 | In Progress (framework done) |
| Phase 5 | In Progress (C Library ✅, Networking ✅) |

## Documentation Improvements (Continued Session)

Based on the codebase review findings, addressed the "Missing Documentation" item.

### Review Findings

**Assessment of existing documentation:**
- `src/codegen/c_backend/` - Already well-documented (mod.rs, expr.rs, types.rs, analysis.rs, runtime.rs all have thorough module-level docs)
- `src/semantic/checker/` - Already well-documented (mod.rs has comprehensive module docs, sub-modules have section docs)
- Runtime FFI functions - Most already had `# Safety` sections

### Changes Made

Added missing `# Safety` documentation to 4 FFI functions:
1. `runtime/src/graphics_ffi.rs`:
   - `qb_gfx_draw` - Added safety note for `commands` parameter
   - `qb_gfx_loadimage` - Added safety note for `filename` parameter
   - `qb_gfx_printstring` - Added safety note for `text` parameter
2. `runtime/src/audio_ffi.rs`:
   - `qb_sndopen` - Added safety note for `filename` parameter

### Conclusion

The codebase review's "Medium Priority: Missing Documentation" finding was largely addressed already. The existing documentation is thorough and follows Rust conventions. Only 4 FFI functions needed safety documentation additions.
