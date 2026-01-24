# Header Parser API Reference

The QB64Fresh header parser extracts declarations from C header files for automatic `DECLARE LIBRARY` support. This document describes the programmatic Rust API.

## Feature Flag

The header parser requires the `header-parsing` feature flag:

```bash
cargo build --features header-parsing
cargo test --features header-parsing
```

## Quick Start

```rust
use qb64fresh::header_parser::{parse_header_full, Platform};

let header = r#"
    #define VERSION 100
    struct Point { int x; int y; };
    int add(int a, int b);
"#;

let result = parse_header_full(header, Some(Platform::Linux));

println!("Constants: {:?}", result.constants);
println!("Structs: {:?}", result.structs);
println!("Functions: {:?}", result.functions);
```

---

## API Overview

### Main Entry Point

```rust
/// Parse a C header file and extract all declarations.
pub fn parse_header_full(
    header_content: &str,
    platform: Option<Platform>
) -> HeaderParseResult
```

**Parameters:**
- `header_content` - The C header file contents as a string
- `platform` - Target platform for conditional compilation. If `None`, uses `Platform::current()`

**Returns:** A `HeaderParseResult` containing extracted functions, constants, and structs.

### Legacy API (Deprecated)

```rust
#[deprecated(since = "0.2.0", note = "Use parse_header_full() instead")]
pub fn parse_header(header_content: &str) -> Vec<CFunction>
```

The legacy API only extracts functions and ignores preprocessor directives.

---

## Data Structures

### HeaderParseResult

```rust
/// Complete result of parsing a C header file.
pub struct HeaderParseResult {
    /// Function declarations extracted from the header.
    pub functions: Vec<CFunction>,
    /// Constant definitions from `#define` directives.
    pub constants: Vec<CConstant>,
    /// Struct/typedef struct definitions.
    pub structs: Vec<CStruct>,
}
```

### Platform

```rust
/// Target platform for conditional compilation.
pub enum Platform {
    /// Windows (defines WIN32, _WIN32, __WIN32__, _MSC_VER)
    Windows,
    /// Linux (defines __linux__, __unix__, __GNUC__, linux, unix)
    Linux,
    /// macOS (defines __APPLE__, __MACH__, __unix__, __GNUC__)
    MacOS,
}

impl Platform {
    /// Get the current platform at runtime.
    pub fn current() -> Self;

    /// Get the predefined macros for this platform.
    pub fn predefined_macros(&self) -> &'static [&'static str];
}
```

### CFunction

```rust
/// A function extracted from a C header.
pub struct CFunction {
    /// The function name.
    pub name: String,
    /// Return type converted to BASIC.
    pub return_type: BasicType,
    /// Parameter types converted to BASIC.
    pub params: Vec<CParam>,
}

/// A parameter from a C function declaration.
pub struct CParam {
    /// Parameter name (if present, otherwise empty).
    pub name: String,
    /// Type converted to BASIC.
    pub typ: BasicType,
}
```

### CConstant

```rust
/// A constant defined via `#define`.
pub struct CConstant {
    /// The constant name.
    pub name: String,
    /// The constant value.
    pub value: ConstantValue,
}

/// The value of a `#define` constant.
pub enum ConstantValue {
    /// Integer value (decimal, hex, octal, or binary).
    Integer(i64),
    /// Floating-point value.
    Float(f64),
    /// String literal.
    String(String),
    /// Expression that couldn't be evaluated (stored as string).
    Expression(String),
}

impl ConstantValue {
    /// Convert this constant value to a QB64 BASIC type.
    pub fn to_basic_type(&self) -> BasicType;
}
```

### CStruct

```rust
/// A C struct definition.
pub struct CStruct {
    /// The struct name.
    pub name: String,
    /// The struct members in declaration order.
    pub members: Vec<CStructMember>,
}

impl CStruct {
    /// Generate QB64 TYPE declaration for this struct.
    pub fn to_qb64_type(&self) -> String;
}

/// A member of a C struct.
pub struct CStructMember {
    /// The member name.
    pub name: String,
    /// The member type.
    pub typ: BasicType,
    /// Array size if this is a fixed-size array (e.g., `char name[64]`).
    pub array_size: Option<usize>,
}

impl CStructMember {
    /// Get the QB64 type string for this member.
    pub fn to_qb64_type_str(&self) -> String;
}
```

---

## Helper Functions

### c_type_to_basic

```rust
/// Map a C type to a BASIC type.
pub fn c_type_to_basic(c_type: &str, is_pointer: bool) -> BasicType
```

**Examples:**
```rust
c_type_to_basic("int", false)           // BasicType::Long
c_type_to_basic("char", true)           // BasicType::String
c_type_to_basic("double", false)        // BasicType::Double
c_type_to_basic("void", true)           // BasicType::Offset
c_type_to_basic("unsigned int", false)  // BasicType::UnsignedLong
```

### parse_constant_value

```rust
/// Parse a constant value string into a ConstantValue.
pub fn parse_constant_value(value: &str) -> ConstantValue
```

**Examples:**
```rust
parse_constant_value("123")       // ConstantValue::Integer(123)
parse_constant_value("-42")       // ConstantValue::Integer(-42)
parse_constant_value("0xFF")      // ConstantValue::Integer(255)
parse_constant_value("0b1010")    // ConstantValue::Integer(10)
parse_constant_value("3.14")      // ConstantValue::Float(3.14)
parse_constant_value("\"hello\"") // ConstantValue::String("hello")
parse_constant_value("'A'")       // ConstantValue::Integer(65)
parse_constant_value("(1 << 8)")  // ConstantValue::Expression("(1 << 8)")
```

---

## Type Mapping Reference

### C to BasicType Mapping

| C Type | BasicType | Notes |
|--------|-----------|-------|
| `int`, `int32_t`, `long` | `Long` | 32-bit signed |
| `short`, `int16_t` | `Integer` | 16-bit signed |
| `char`, `int8_t`, `signed char` | `Byte` | 8-bit signed |
| `long long`, `int64_t` | `Integer64` | 64-bit signed |
| `unsigned int`, `uint32_t`, `unsigned long` | `UnsignedLong` | 32-bit unsigned |
| `unsigned short`, `uint16_t` | `UnsignedInteger` | 16-bit unsigned |
| `unsigned char`, `uint8_t` | `UnsignedByte` | 8-bit unsigned |
| `unsigned long long`, `uint64_t` | `UnsignedInteger64` | 64-bit unsigned |
| `float` | `Single` | 32-bit float |
| `double` | `Double` | 64-bit float |
| `long double` | `Float` | Extended precision |
| `void` | `Void` | No return value |
| `size_t`, `intptr_t`, `uintptr_t` | `Offset` | Pointer-sized |
| `char*`, `const char*` | `String` | String pointer |
| Other pointers | `Offset` | Generic pointer |

### Array Member Mapping

| C Member | QB64 Type |
|----------|-----------|
| `char name[64]` | `STRING * 64` |
| `int values[10]` | `LONG ' ARRAY SIZE 10` |
| `float data[100]` | `SINGLE ' ARRAY SIZE 100` |

---

## Conditional Compilation

### Supported Directives

| Directive | Support |
|-----------|---------|
| `#ifdef NAME` | Full |
| `#ifndef NAME` | Full |
| `#if expression` | Partial (see below) |
| `#elif expression` | Partial |
| `#else` | Full |
| `#endif` | Full |
| `#define NAME value` | Full |
| `#define NAME(args) ...` | Skipped (function macro) |

### Supported `#if` Expressions

The parser evaluates simple conditions:

```c
#if defined(WIN32)          // ✅ Supported
#if defined WIN32           // ✅ Supported
#if !defined(__linux__)     // ✅ Supported
#if defined(A) || defined(B) // ✅ Supported
#if defined(A) && defined(B) // ✅ Supported
#if (defined(A))            // ✅ Supported
#if MACRO_NAME              // ✅ Checks if defined
#if 1                       // ❌ Not evaluated (defaults to false)
#if VERSION > 100           // ❌ Not evaluated (defaults to false)
```

### Nesting Support

Nested conditionals are fully supported:

```c
#ifdef __unix__
    // Always included on Linux/macOS
    #ifdef __linux__
        // Only on Linux
    #endif
    #ifdef __APPLE__
        // Only on macOS
    #endif
#endif
```

---

## Examples

### Extracting Constants

```rust
use qb64fresh::header_parser::{parse_header_full, Platform, ConstantValue};

let header = r#"
    #define VERSION_MAJOR 1
    #define VERSION_MINOR 2
    #define VERSION_PATCH 3
    #define PI 3.14159265358979
    #define APP_NAME "MyApp"
    #define MAX(a, b) ((a) > (b) ? (a) : (b))  // Skipped
"#;

let result = parse_header_full(header, Some(Platform::Linux));

for c in &result.constants {
    match &c.value {
        ConstantValue::Integer(n) => println!("{} = {}", c.name, n),
        ConstantValue::Float(f) => println!("{} = {}", c.name, f),
        ConstantValue::String(s) => println!("{} = \"{}\"", c.name, s),
        ConstantValue::Expression(e) => println!("{} = <expr: {}>", c.name, e),
    }
}
// Output:
// VERSION_MAJOR = 1
// VERSION_MINOR = 2
// VERSION_PATCH = 3
// PI = 3.14159265358979
// APP_NAME = "MyApp"
```

### Platform-Specific Functions

```rust
use qb64fresh::header_parser::{parse_header_full, Platform};

let header = r#"
    #ifdef WIN32
    HANDLE CreateFileW(LPCWSTR lpFileName, ...);
    #else
    int open(const char* pathname, int flags);
    #endif

    void common_function(void);
"#;

// Parse for Windows
let win = parse_header_full(header, Some(Platform::Windows));
println!("Windows functions:");
for f in &win.functions {
    println!("  {}", f.name);
}
// Output: CreateFileW, common_function

// Parse for Linux
let linux = parse_header_full(header, Some(Platform::Linux));
println!("Linux functions:");
for f in &linux.functions {
    println!("  {}", f.name);
}
// Output: open, common_function
```

### Generating QB64 TYPE Definitions

```rust
use qb64fresh::header_parser::{parse_header_full, Platform};

let header = r#"
    typedef struct {
        char name[32];
        int score;
        float position[3];
    } Player;

    struct GameState {
        Player players[4];
        int current_level;
        double elapsed_time;
    };
"#;

let result = parse_header_full(header, Some(Platform::Linux));

for s in &result.structs {
    println!("{}\n", s.to_qb64_type());
}
// Output:
// TYPE Player
//     name AS STRING * 32
//     score AS LONG
//     position AS SINGLE ' ARRAY SIZE 3
// END TYPE
//
// TYPE GameState
//     players AS LONG ' ARRAY SIZE 4  (Note: nested struct arrays not fully supported)
//     current_level AS LONG
//     elapsed_time AS DOUBLE
// END TYPE
```

---

## Limitations

1. **Function-like macros** are detected and skipped but not expanded
2. **Complex `#if` expressions** (arithmetic, comparisons) default to false
3. **Nested structs** are not supported
4. **Unions** are skipped
5. **Bit fields** are not supported
6. **Inline functions** are treated as regular functions
7. **Variadic functions** (`...`) may not parse correctly
8. **C++ constructs** (templates, namespaces, classes) are not supported

---

## Files

| File | Description |
|------|-------------|
| `src/header_parser/mod.rs` | Public API and data structures |
| `src/header_parser/lexer.rs` | C tokenizer with preprocessor support |
| `src/header_parser/parser.rs` | Function/struct/constant extraction |
