# ADR-0008: C Interoperability (DECLARE LIBRARY)

## Status

**Accepted** - January 18, 2026

## Context

QB64 extends QBasic with the ability to call C library functions directly via `DECLARE LIBRARY`. This enables:
- System API calls (Windows API, POSIX functions)
- Integration with third-party C libraries
- High-performance routines written in C
- Access to platform-specific features

QB64Fresh needs this capability to be a viable QB64 replacement.

Key considerations:
- Type mapping between BASIC and C types
- Calling conventions (cdecl vs stdcall)
- String handling (BASIC strings vs C strings)
- Pointer support for advanced use cases
- Safety considerations

## Decision

**We implement DECLARE LIBRARY as a first-class language feature with direct C code generation**.

### Syntax Support

```basic
' Static library linking
DECLARE LIBRARY "mylib"
    FUNCTION add_numbers& (BYVAL a AS LONG, BYVAL b AS LONG)
    SUB print_message (msg AS STRING)
END DECLARE

' Dynamic library (loaded at runtime)
DECLARE DYNAMIC LIBRARY "plugin"
    FUNCTION get_version$ ()
END DECLARE

' Header-only (system headers, no library linking)
DECLARE LIBRARY
    FUNCTION getenv$ (BYVAL name AS STRING)
END DECLARE

' Custom alias for C function name
DECLARE LIBRARY "math"
    FUNCTION square_root# ALIAS "sqrt" (BYVAL x AS DOUBLE)
END DECLARE
```

### AST Representation

```rust
pub enum StatementKind {
    DeclareLibrary {
        /// Library name/path (without extension). None for header-only.
        library_name: Option<String>,
        /// Whether this is a dynamic library.
        is_dynamic: bool,
        /// External function/sub declarations.
        declarations: Vec<ExternalDeclaration>,
    },
}

pub struct ExternalDeclaration {
    pub name: String,           // BASIC name
    pub alias: Option<String>,  // C name (if different)
    pub params: Vec<ExternalParam>,
    pub return_type: Option<TypeSpec>,
    pub is_function: bool,
}

pub struct ExternalParam {
    pub name: String,
    pub type_spec: TypeSpec,
    pub is_byval: bool,  // Required for most C interop
}
```

### Type Mapping

| BASIC Type | C Type | Notes |
|------------|--------|-------|
| `INTEGER` | `int16_t` | 16-bit signed |
| `LONG` | `int32_t` | 32-bit signed |
| `_INTEGER64` | `int64_t` | 64-bit signed |
| `SINGLE` | `float` | 32-bit float |
| `DOUBLE` | `double` | 64-bit float |
| `STRING` | `char*` | Null-terminated (BYVAL) |
| `_UNSIGNED INTEGER` | `uint16_t` | 16-bit unsigned |
| `_UNSIGNED LONG` | `uint32_t` | 32-bit unsigned |
| `_OFFSET` | `void*` | Pointer/handle |

### Code Generation

DECLARE LIBRARY generates:

1. **Function prototypes** at the top of C output:
```c
// From: DECLARE LIBRARY "mylib"
extern int32_t add_numbers(int32_t a, int32_t b);
extern void print_message(const char* msg);
```

2. **Linker directives** for library linking:
```c
// Compile with: -lmylib
```

3. **Call sites** that use the external functions directly:
```c
int32_t result = add_numbers(10, 20);
print_message("Hello from C!");
```

### BYVAL vs BYREF

- **BYVAL** (default for C interop): Pass value directly
- **BYREF** (default BASIC): Pass pointer to value

Most C functions expect BYVAL. BYREF is useful for:
- Output parameters
- Modifying caller's variables
- Passing arrays

### String Handling

BASIC strings are converted to C strings for external calls:
- BYVAL STRING → `const char*` (temporary null-terminated copy)
- BYREF STRING → Not directly supported (use _OFFSET)

### Rationale

1. **Direct C generation**: Natural fit since we emit C code anyway
2. **Syntax matches QB64**: Compatibility with existing programs
3. **BYVAL explicit**: Safer than implicit value/reference confusion
4. **ALIAS support**: Handle name mangling, C++ compatibility
5. **Dynamic libraries**: Runtime plugin support

### Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| **libffi at runtime** | Complex, another dependency |
| **Only static linking** | Limits use cases (plugins) |
| **Custom FFI syntax** | Incompatible with QB64 programs |
| **No C interop** | Severely limits usefulness |

## Consequences

### Positive

- Full compatibility with QB64 DECLARE LIBRARY syntax
- Direct system API access
- Third-party library integration
- No runtime FFI overhead (compile-time binding)
- Enables advanced graphics/audio extensions

### Negative

- Unsafe by nature (calling into C code)
- Type mismatches cause undefined behavior
- Platform-specific code may not be portable
- Dynamic libraries add deployment complexity
- No automatic memory management for C allocations

### Safety Considerations

1. **No runtime type checking**: BASIC types must match C exactly
2. **Memory management**: C allocations must be freed by C
3. **String lifetime**: Temporary C strings invalid after call
4. **Pointer arithmetic**: _OFFSET enables unsafe operations

### Implementation Status

| Component | Status |
|-----------|--------|
| Parser (DECLARE LIBRARY) | Complete |
| AST representation | Complete |
| Semantic analysis | Complete |
| Symbol table integration | Complete |
| C code generation | Complete |
| Static library support | Complete |
| Dynamic library support | Complete |
| ALIAS support | Complete |
| BYVAL/BYREF | Complete |
| _OFFSET type | Partial |

### Example Usage

```basic
' Call C standard library
DECLARE LIBRARY
    FUNCTION strlen& (BYVAL s AS STRING)
    FUNCTION atoi& (BYVAL s AS STRING)
    SUB qsort (base AS _OFFSET, BYVAL nmemb AS _UNSIGNED LONG, BYVAL size AS _UNSIGNED LONG, compar AS _OFFSET)
END DECLARE

DIM s AS STRING
s = "Hello, World!"
PRINT "Length:"; strlen(s)

DIM num AS STRING
num = "42"
PRINT "Number:"; atoi(num)
```

### Files

- `src/ast/stmt.rs` - DeclareLibrary, ExternalDeclaration AST nodes
- `src/parser/statements.rs` - DECLARE LIBRARY parsing
- `src/semantic/symbols.rs` - External function symbol handling
- `src/codegen/c_backend/stmt.rs` - C prototype/directive generation
