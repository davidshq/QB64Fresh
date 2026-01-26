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

### Automatic Header Parsing

QB64Fresh includes a comprehensive C header parser that can automatically extract declarations when using `DECLARE LIBRARY "header.h"` syntax. This feature requires the `header-parsing` feature flag.

```bash
cargo build --features header-parsing
```

```basic
' Automatically parse and import functions from a C header
DECLARE LIBRARY "mylib.h"
    ' Functions are auto-extracted from the header
END DECLARE
```

**Supported constructs:**
- Function declarations with full type mapping
- `#define` constants (integer, hex, float, string literals)
- `#ifdef`/`#ifndef`/`#if`/`#elif`/`#else`/`#endif` conditional compilation
- `struct` and `typedef struct` definitions
- Array members (e.g., `char name[64]`)
- Basic C types and stdint types

**Platform-Aware Parsing:**

The parser pre-defines platform macros to handle platform-specific code:

| Platform | Defined Macros |
|----------|---------------|
| Windows | `WIN32`, `_WIN32`, `__WIN32__`, `_MSC_VER` |
| Linux | `__linux__`, `__unix__`, `__GNUC__`, `linux`, `unix` |
| macOS | `__APPLE__`, `__MACH__`, `__unix__`, `__GNUC__` |

**Limitations:**
- Function-like macros (`#define FOO(x)`) are skipped
- Complex `#if` expressions may not evaluate correctly
- Nested structs and unions inside structs are not supported
- Bit fields are not supported

**Programmatic API:**

```rust
use qb64fresh::header_parser::{parse_header_full, Platform, HeaderParseResult};

let header = r#"
    #define VERSION 100
    #ifdef WIN32
    int win_only_func();
    #endif
    struct Point { int x; int y; };
    int cross_platform_func();
"#;

let result: HeaderParseResult = parse_header_full(header, Some(Platform::Linux));
// result.constants: VERSION = 100
// result.structs: Point with members x, y
// result.functions: cross_platform_func (win_only_func excluded on Linux)
```

The header parser maps C types to BASIC types:

| C Type | BASIC Type | Notes |
|--------|------------|-------|
| `int`, `int32_t`, `long` | `LONG` | 32-bit signed |
| `short`, `int16_t` | `INTEGER` | 16-bit signed |
| `char`, `int8_t` | `_BYTE` | 8-bit signed |
| `long long`, `int64_t` | `_INTEGER64` | 64-bit signed |
| `float` | `SINGLE` | 32-bit float |
| `double` | `DOUBLE` | 64-bit float |
| `char*`, `const char*` | `STRING` | String pointer |
| `void*`, other pointers | `_OFFSET` | Generic pointer |
| `char[N]` | `STRING * N` | Fixed-length string |
| `unsigned` variants | `_UNSIGNED` variants | Unsigned types |

**Struct to TYPE Conversion:**

Parsed structs can be automatically converted to QB64 TYPE definitions:

```c
// C header
struct Player {
    char name[64];
    int score;
    float x, y;
};
```

```basic
' Generated QB64 TYPE
TYPE Player
    name AS STRING * 64
    score AS LONG
    x AS SINGLE
    y AS SINGLE
END TYPE
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

For SHELL, file operations, path handling, and the no-sandbox execution model, see [ADR-0015](ADR-0015-no-sandbox-execution-model.md) and [SECURITY_MODEL.md](../SECURITY_MODEL.md).

### Implementation Status

| Component | Status |
|-----------|--------|
| Parser (DECLARE LIBRARY) | ✅ Complete |
| AST representation | ✅ Complete |
| Semantic analysis | ✅ Complete |
| Symbol table integration | ✅ Complete |
| C code generation | ✅ Complete |
| Static library support | ✅ Complete |
| Dynamic library support | ✅ Complete |
| ALIAS support | ✅ Complete |
| BYVAL/BYREF | ✅ Complete |
| _OFFSET type | ✅ Complete |
| **Memory Functions** | |
| VARPTR(variable) | ✅ Complete |
| VARPTR$(variable) | ✅ Complete |
| VARSEG(variable) | ✅ Complete |
| SADD(string$) | ✅ Complete |
| **_MEM Support** | |
| _MEM type in parameters | ✅ Complete |
| _MEM(variable) function | ✅ Complete |
| _MEMGET AS type clause | ✅ Complete |
| _MEMPUT AS type clause | ✅ Complete |
| **Callback Functions** | |
| _PROCPTR(procedureName) | ✅ Complete |
| FUNCTION callback signatures | ✅ Complete |
| SUB callback signatures | ✅ Complete |
| **Header Parsing** | |
| Function declarations | ✅ Complete |
| `#define` constants | ✅ Complete |
| `#ifdef`/`#ifndef`/`#endif` | ✅ Complete |
| `#if`/`#elif`/`#else` | ✅ Complete |
| `defined()` expressions | ✅ Complete |
| struct definitions | ✅ Complete |
| typedef struct | ✅ Complete |
| Array members | ✅ Complete |
| Platform detection | ✅ Complete |

### Memory Address Functions

```basic
' VARPTR - Get address of variable as LONG
DIM x AS INTEGER
addr& = VARPTR(x)

' VARPTR$ - Get address as binary string (4 bytes)
DIM y AS LONG
addrStr$ = VARPTR$(y)

' VARSEG - Returns 0 (flat memory model, no segmentation)
seg% = VARSEG(x)  ' Always 0

' SADD - Get address of string data
DIM s AS STRING
s = "Hello"
strAddr& = SADD(s)
```

### _MEM Type Support

```basic
' _MEM as parameter type in DECLARE LIBRARY
DECLARE LIBRARY
    SUB process_memory (BYVAL mem AS _MEM)
END DECLARE

' _MEM(variable) - Get _MEM block for variable's memory
DIM arr(100) AS INTEGER
DIM m AS _MEM
m = _MEM(arr())

' _MEMGET/_MEMPUT with AS type clause
DIM mem AS _MEM
x = _MEMGET(mem, offset, AS INTEGER)
_MEMPUT mem, offset, value AS DOUBLE
```

### Callback Functions (_PROCPTR)

```basic
' Define callback procedure
SUB MyCallback (BYVAL x AS LONG)
    PRINT "Callback received:"; x
END SUB

' Get procedure pointer for C library
DECLARE LIBRARY "somelib"
    SUB register_callback (BYVAL cb AS _OFFSET)
END DECLARE

register_callback(_PROCPTR(MyCallback))
```

The compiler generates proper C function signatures:
- FUNCTION callbacks return the appropriate C type
- SUB callbacks return `void`
- BYVAL/BYREF parameters are handled correctly in the wrapper

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
- `src/header_parser/mod.rs` - C header parser entry point
- `src/header_parser/lexer.rs` - C header tokenizer
- `src/header_parser/parser.rs` - C function declaration parser

**API Reference:** See [docs/reference/HEADER_PARSER_API.md](../reference/HEADER_PARSER_API.md) for complete programmatic API documentation.
