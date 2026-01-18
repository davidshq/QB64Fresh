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
