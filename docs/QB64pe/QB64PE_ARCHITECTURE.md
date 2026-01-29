# QB64 Phoenix Edition - Architecture Analysis & Documentation

This document provides a comprehensive analysis and reference overview of the QB64 Phoenix Edition (QB64pe) architecture. QB64pe is a self-hosted BASIC compiler that translates QBasic/QuickBASIC-compatible source code into C++ code, which is then compiled to native executables.

**Note:** This document serves both as a reference and analysis. QB64pe is READ-ONLY in this workspace - we analyze it to inform QB64Fresh development.

---

## Executive Summary

QB64-PE is a complete BASIC compiler and IDE system written primarily in QB64 itself. It maintains QB4.5/QBasic compatibility while adding modern extensions and compiling to native executables for Windows, Linux, and macOS.

**Key Metrics:**
- **Main compiler:** ~24,000 lines of QB64 code (`source/qb64pe.bas`)
- **IDE:** ~970,000 lines of QB64 code (`source/ide/`)
- **Runtime library:** ~31,000 lines of C++ code (`internal/c/libqb.cpp` + modular components)
- **Built-in functions:** ~123,000 lines of QB64 code (`source/subs_functions/subs_functions.bas`)
- **Total codebase:** Over 1 million lines

---

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        QB64-PE System                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐ │
│  │    IDE      │───▶│  Compiler   │───▶│   C++ Toolchain     │ │
│  │ (optional)  │    │ (qb64pe.bas)│    │  (GCC/MinGW/Clang)  │ │
│  │             │    │             │    │                     │ │
│  │ ~970K lines │    │ ~24K lines │    │  External compiler  │ │
│  └─────────────┘    └──────┬──────┘    └──────────┬──────────┘ │
│                            │                      │             │
│                            ▼                      ▼             │
│                     ┌─────────────┐        ┌───────────┐       │
│                     │ Generated   │───────▶│  Native   │       │
│                     │ C++ Code    │        │ Executable│       │
│                     │ (internal/  │        │           │       │
│                     │  temp/*.cpp)│        │           │       │
│                     └─────────────┘        └───────────┘       │
│                            │                      │             │
│                            ▼                      │             │
│                     ┌─────────────┐               │             │
│                     │  Runtime    │───────────────┘             │
│                     │  Library    │                             │
│                     │ (libqb.cpp) │                             │
│                     │ + modules   │                             │
│                     └─────────────┘                             │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Directory Structure

```
QB64pe/
├── source/                    # QB64 source code (compiler + IDE)
│   ├── qb64pe.bas            # Main compiler (~24K lines)
│   ├── global/               # Global definitions
│   │   ├── version.bas       # Version information
│   │   ├── constants.bas     # Global constants
│   │   └── settings.bas      # Configuration settings
│   ├── subs_functions/       # Built-in function definitions
│   │   ├── subs_functions.bas # ~123K lines of function mappings
│   │   └── extensions/        # Extension modules (OpenGL, etc.)
│   ├── ide/                  # IDE components (~970K lines)
│   │   ├── ide_global.bas    # IDE global variables
│   │   ├── ide_methods.bas   # Main IDE functionality (~862KB)
│   │   ├── ide_export.bas    # Export functions
│   │   └── ide_converters.bas # Format converters
│   └── utilities/            # Utility modules
│       ├── const_eval.bas    # Constant evaluation
│       ├── hash.bas          # Hash table implementation
│       ├── type.bas          # Type utilities
│       └── ...
├── internal/                 # Internal compiler files
│   ├── c/                    # C++ runtime library
│   │   ├── libqb.cpp         # Main runtime (~31K lines)
│   │   ├── libqb/            # Modular runtime components
│   │   │   ├── include/      # Header files
│   │   │   └── src/          # Source files
│   │   ├── parts/            # External dependencies
│   │   │   ├── audio/        # Audio libraries
│   │   │   ├── video/        # Video/image handling
│   │   │   ├── core/         # Core utilities
│   │   │   ├── gui/          # GUI components
│   │   │   ├── network/      # Networking
│   │   │   ├── input/        # Input handling
│   │   │   └── os/           # OS-specific code
│   │   └── qbx*.cpp          # Generated C++ files (temp)
│   ├── source/               # Internal source data
│   └── support/              # Support files
├── tests/                    # Test suite
│   ├── compile_tests/        # Compilation tests by feature
│   └── qbasic_testcases/     # Classic QBasic programs
└── .ci/                      # Build scripts
    ├── bootstrap.sh          # Bootstrap compilation
    ├── compile.sh            # Compilation script
    └── ...
```

---

## Compiler Architecture

### Main Compiler (`source/qb64pe.bas`)

The compiler is a single monolithic file (~24,000 lines) written in QB64 itself. It operates as a self-hosted compiler.

#### Compilation Pipeline

```
┌──────────────┐
│ BASIC Source │
│   (.bas)     │
└──────┬───────┘
       │
       ▼
┌──────────────────────────────────────────────────────────────┐
│                    QB64 COMPILER                             │
│                  (qb64pe.bas)                                │
├──────────────────────────────────────────────────────────────┤
│  1. PREPROCESSOR                                              │
│     ├─ $INCLUDE processing                                    │
│     ├─ $IF/$ELSE/$END IF conditional compilation             │
│     ├─ $LET constant definitions                              │
│     ├─ $CONSOLE, $RESIZE, $SCREENHIDE metacommands           │
│     └─ $EMBED file embedding                                 │
├──────────────────────────────────────────────────────────────┤
│  2. LEXICAL ANALYSIS                                         │
│     ├─ Line reading and normalization                        │
│     ├─ Element extraction (getelement$ function)             │
│     ├─ String/comment handling                               │
│     ├─ Line continuation (underscore)                        │
│     └─ Token recognition                                     │
├──────────────────────────────────────────────────────────────┤
│  3. PREPASS                                                  │
│     ├─ TYPE definition collection                             │
│     ├─ SUB/FUNCTION declaration collection                   │
│     ├─ Forward reference resolution                           │
│     └─ Symbol table initialization                           │
├──────────────────────────────────────────────────────────────┤
│  4. PARSING & SEMANTIC ANALYSIS                              │
│     ├─ Statement recognition                                  │
│     ├─ Expression parsing (evaluate$, evaluatetotyp$)        │
│     ├─ Type checking                                         │
│     ├─ Symbol table management (hash-based)                  │
│     ├─ Scope handling                                        │
│     └─ Error collection                                      │
├──────────────────────────────────────────────────────────────┤
│  5. CODE GENERATION                                          │
│     ├─ Direct C++ emission (no AST)                          │
│     ├─ Variable declarations                                  │
│     ├─ Function/Sub translations                             │
│     ├─ Runtime library calls                                 │
│     └─ Multiple output buffers (MainTxtBuf, DataTxtBuf, etc.)│
└──────────────────────────────────────────────────────────────┘
       │
       ▼
┌──────────────┐
│ Generated    │
│ C++ Code     │
│ (internal/   │
│  temp/qbx*.cpp)│
└──────┬───────┘
       │
       ▼
┌──────────────────────────────────────────────────────────────┐
│                    C++ COMPILER                               │
│              (GCC / MinGW / Clang)                             │
├──────────────────────────────────────────────────────────────┤
│  • Compiles generated code                                    │
│  • Links with libqb (runtime library)                         │
│  • Links with platform libraries (OpenGL, ALSA, etc.)        │
│  • Produces native executable                                 │
└──────────────────────────────────────────────────────────────┘
       │
       ▼
┌──────────────┐
│   Native     │
│  Executable  │
└──────────────┘
```

#### Key Design Characteristics

1. **Self-Hosted:** Written in QB64 itself, compiled by a previous version
2. **Single-Pass with Recompile Loops:** Uses multiple passes to resolve forward references
3. **Line-by-Line Processing:** Processes source one line at a time
4. **Direct Code Generation:** Generates C++ code directly without an intermediate AST
5. **Hash-Based Symbol Table:** Uses hash tables for efficient symbol lookup
6. **Multiple Output Buffers:** Separates main code, data, declarations, etc.

#### Internal Structure

The compiler is organized around these key components:

**Global State:**
- `ids()` - Symbol table array (variables, arrays, subs, functions)
- `id` - Current identifier being processed
- `subfunc$` - Current SUB/FUNCTION name (empty for main module)
- `linenumber` - Current source line being compiled
- `MainTxtBuf`, `DataTxtBuf`, etc. - C++ code output buffers

**Key Functions:**
- `getelement$()` - Extracts tokens/elements from source lines
- `evaluate$()` - Expression evaluation
- `evaluatetotyp$()` - Expression evaluation with type conversion
- `fixoperationorder$()` - Operator precedence handling
- `WriteBufLine()` - Writes to C++ output buffers
- `HashFind()`, `HashAdd()`, `HashRemove()` - Symbol table operations

**Processing Flow:**
1. Preprocessor handles `$INCLUDE`, `$IF`, `$LET` directives
2. Line-by-line processing at `compileline:` label
3. Element extraction via `getelement$()`
4. Statement recognition and parsing
5. Expression evaluation and type checking
6. C++ code emission to buffers
7. Final C++ file generation

---

## Symbol Table System

### Hash Table Implementation

QB64pe uses a hash table for efficient symbol lookup:

```basic
HashAdd name$, flags, index    ' Add symbol to table
HashFind name$, flags, index   ' Find symbol in table
HashRemove name$               ' Remove symbol from table
```

### Symbol Flags

- `HASHFLAG_TYPE` - Type definition
- `HASHFLAG_RESERVED` - Reserved keyword
- `HASHFLAG_OPERATOR` - Operator symbol
- `HASHFLAG_CUSTOMSYNTAX` - Custom syntax handler

### Symbol Structure (`idstruct`)

Each symbol in `ids()` array contains:
- Name (BASIC identifier)
- Type information
- Scope information
- C++ name mapping
- Array dimensions (if applicable)
- Parameter list (for SUB/FUNCTION)
- Return type (for FUNCTION)

---

## Type System

### Native Data Types

| QB64 Type | C++ Equivalent | Size | Notes |
|-----------|----------------|------|-------|
| `_BIT` | `int8` (packed) | 1 bit | Packed bit field |
| `_BYTE` | `int8` | 1 byte | Signed byte |
| `INTEGER` | `int16` | 2 bytes | Standard integer |
| `LONG` | `int32` | 4 bytes | Long integer |
| `_INTEGER64` | `int64` | 8 bytes | 64-bit integer |
| `_OFFSET` | `ptrszint` | pointer-sized | Pointer type |
| `SINGLE` | `float` | 4 bytes | Single precision float |
| `DOUBLE` | `double` | 8 bytes | Double precision float |
| `_FLOAT` | `long double` | 10+ bytes | Extended precision |
| `STRING` | `qbs*` | variable | Dynamic string |
| `STRING * n` | `char[n]` | fixed | Fixed-length string |
| `_UNSIGNED` variants | `unsigned` versions | same | Unsigned integer types |

### Type Suffixes

BASIC identifiers can have type suffixes:
- `$` - String
- `%` - Integer
- `&` - Long
- `!` - Single
- `#` - Double

### User-Defined Types (UDT)

```basic
TYPE MyType
    field1 AS INTEGER
    field2 AS STRING * 20
    field3 AS MyOtherType
END TYPE
```

UDTs are collected during prepass and generate C++ struct definitions.

---

## Built-in Functions System

### Function Registration (`source/subs_functions/subs_functions.bas`)

The built-in functions file (~123,000 lines) defines all standard BASIC functions and QB64 extensions.

**Registration Pattern:**
```basic
clearid
id.n = "FunctionName"        ' BASIC name
id.subfunc = 1               ' 1=function, 2=subroutine
id.callname = "c_func"       ' C++ function to call
id.args = N                  ' Number of arguments
id.arg = MKL$(type1) + ...   ' Argument types (packed)
id.ret = returntype          ' Return type
id.hr_syntax = "..."         ' Human-readable syntax
regid
```

### Function Categories

1. **String Functions:** `LEFT$`, `RIGHT$`, `MID$`, `CHR$`, `ASC`, `LEN`, etc.
2. **Math Functions:** `SIN`, `COS`, `TAN`, `SQR`, `LOG`, `EXP`, `RND`, etc.
3. **File I/O:** `OPEN`, `CLOSE`, `INPUT#`, `PRINT#`, `GET`, `PUT`, `SEEK`, etc.
4. **Graphics:** `_NEWIMAGE`, `PSET`, `LINE`, `CIRCLE`, `PAINT`, `_PUTIMAGE`, etc.
5. **Audio:** `_SNDOPEN`, `_SNDPLAY`, `BEEP`, `SOUND`, etc.
6. **System:** `TIMER`, `DATE$`, `TIME$`, `SHELL`, `_OS$`, etc.
7. **Memory:** `_MEM`, `PEEK`, `POKE`, `VARPTR`, `_OFFSET`, etc.
8. **QB64 Extensions:** `_MEM*`, `_OFFSET`, `_GL*`, `_SND*`, etc.

---

## Runtime Library Architecture

### Main Runtime (`internal/c/libqb.cpp`)

The runtime library (~31,000 lines) provides all standard library functions callable from generated C++ code.

### Modular Components (`internal/c/libqb/src/`)

| File | Purpose | Lines |
|------|---------|-------|
| `graphics.cpp` | Graphics rendering, screen modes | ~105K |
| `shell.cpp` | SHELL command, process execution | ~54K |
| `filesystem.cpp` | File operations | ~36K |
| `gfs.cpp` | General file system | ~33K |
| `qbs.cpp` | QB string handling (qbs type) | ~19K |
| `http.cpp` | HTTP/networking | ~13K |
| `mem.cpp` | Memory operations | ~11K |
| `error_handle.cpp` | Error handling | ~11K |
| `datetime.cpp` | Date/time functions | ~9K |
| `string_functions.cpp` | String manipulation | Various |
| `threading.cpp` | Threading support | Various |

### Core Data Types

**QB String (`qbs`):**
```cpp
struct qbs {
    uint32 len;        // String length
    uint8 *chr;        // Character data
    uint32 fix;        // Fixed-length flag
    // ... reference counting, etc.
};
```

**QB List (`qblist`):**
Dynamic array structure for QB64 arrays.

### External Dependencies (`internal/c/parts/`)

- **audio/** - Audio playback (miniaudio)
- **video/** - Video/image handling (STB libraries)
- **core/** - Core utilities
- **gui/** - GUI components (FreeGLUT)
- **network/** - Networking libraries
- **input/** - Input handling (gamepad, keyboard, mouse)
- **os/** - OS-specific code (Windows, Linux, macOS)

### Platform Support

- **Windows:** MinGW compiler, Win32 APIs
- **Linux:** GCC compiler, X11, ALSA
- **macOS:** Clang compiler, Cocoa APIs

---

## IDE Architecture

### IDE Components (`source/ide/`)

The IDE is a massive component (~970,000 lines) written in QB64 itself.

**Files:**
- `ide_global.bas` (~64KB) - Global variables and state
- `ide_methods.bas` (~862KB) - Main IDE functionality
- `ide_export.bas` (~34KB) - Export functions
- `ide_converters.bas` (~6KB) - Format converters

**Features:**
- Syntax highlighting
- Code completion
- Integrated debugger
- Project management
- Export functionality
- Wiki integration

**Note:** QB64Fresh does NOT reproduce the IDE. Instead, we provide:
- Language Server Protocol (LSP) implementation
- VSCode extension
- Other editor integrations

---

## Build System

### Bootstrap Process

1. **Initial Bootstrap:** Uses a pre-compiled `qb64pe_bootstrap` executable
2. **Self-Compilation:** Compiles `qb64pe.bas` to generate `qb64pe` executable
3. **IDE Compilation:** IDE components compiled into main executable

### Build Scripts (`.ci/`)

- `bootstrap.sh` / `bootstrap.bat` - Bootstrap compilation
- `compile.sh` / `compile.bat` - Standard compilation
- `make-dist.sh` - Distribution packaging

### Compilation Process

1. QB64 compiler (`qb64pe.bas`) generates C++ code to `internal/temp/qbx*.cpp`
2. Makefile/CMake compiles generated C++ with runtime library
3. Links with platform-specific libraries (OpenGL, ALSA, etc.)
4. Produces native executable

### Platform-Specific Builds

**Windows:**
- Uses MinGW compiler
- Links against Windows libraries
- Produces `.exe` files

**Linux:**
- Uses GCC compiler
- Links against X11, ALSA, OpenGL
- Produces executable binaries

**macOS:**
- Uses Clang compiler
- Links against Cocoa, OpenGL
- Produces `.app` bundles or executables

---

## Code Generation Strategy

### Direct C++ Emission

QB64pe generates C++ code directly without an intermediate AST:

1. **Statement Recognition:** Identifies statement type
2. **Expression Evaluation:** Evaluates expressions inline
3. **Code Emission:** Writes C++ code directly to buffers
4. **Type Conversion:** Handles type conversions during emission

### Output Buffers

Multiple buffers for different code sections:
- `MainTxtBuf` - Main program code
- `DataTxtBuf` - DATA statement values
- `DeclareTxtBuf` - Variable declarations
- `SubFuncTxtBuf` - SUB/FUNCTION definitions
- `TypeTxtBuf` - TYPE definitions

### Generated Code Structure

```cpp
// Includes
#include "libqb.h"
#include "common.h"

// Type definitions
struct MyType { ... };

// Global variables
int32 myVar;
qbs* myString;

// Main function
int32 main(int32 argc, int8** argv) {
    // Generated code
    ...
    return 0;
}

// SUB/FUNCTION definitions
void SUB_MySub(int32 param) { ... }
int32 FUNCTION_MyFunc(int32 param) { ... }
```

---

## Preprocessor System

### Metacommands

QB64pe supports various metacommands (preprocessor directives):

**Conditional Compilation:**
```basic
$IF DEFINED(WINDOWS) THEN
    ' Windows-specific code
$ELSE
    ' Other platforms
$END IF
```

**Includes:**
```basic
$INCLUDE:'global\constants.bas'
```

**Constants:**
```basic
$LET MYCONST = 42
```

**Console Mode:**
```basic
$CONSOLE
```

**Screen Control:**
```basic
$SCREENHIDE
$RESIZE
```

**File Embedding:**
```basic
$EMBED:'data.bin'
```

### Predefined Constants

- `WINDOWS`, `WIN` - Windows platform
- `LINUX` - Linux platform
- `MAC`, `MACOSX` - macOS platform
- `32BIT`, `64BIT` - Architecture
- `VERSION` - Version string
- `_QB64PE_` - QB64-PE identifier
- `_ARM_` - ARM architecture

---

## Error Handling

### Error Collection

The compiler collects errors during compilation rather than stopping at the first error:

1. **Error Storage:** Errors stored in arrays with line numbers
2. **Error Reporting:** Errors displayed after compilation attempt
3. **Error Recovery:** Attempts to continue parsing after errors

### Error Types

- Syntax errors
- Type mismatch errors
- Undefined variable/function errors
- Duplicate definition errors
- Scope errors

---

## Test Suite

### Test Organization (`tests/`)

**Compile Tests (`tests/compile_tests/`):**
- Organized by feature (audio, graphics, filesystem, etc.)
- Tests specific language features
- Validates compilation success

**QBasic Testcases (`tests/qbasic_testcases/`):**
- Classic QBasic/QB4.5 programs
- Compatibility validation
- Organized by contributor (misc, pete, thebob, etc.)

**Format Tests (`tests/format_tests/`):**
- Code formatting validation
- Style consistency checks

---

## Key Design Observations

### Strengths

1. **Complete QB4.5/QBasic Compatibility** - Runs vintage BASIC programs
2. **Extensive QB64 Extensions** - Modern features (`_MEM`, `_SNDPLAY`, etc.)
3. **Cross-Platform** - Windows, Linux, macOS support
4. **Self-Contained** - Ships with its own C++ compiler on Windows
5. **Large Test Suite** - Comprehensive compatibility testing
6. **Mature Codebase** - Well-tested, stable implementation

### Weaknesses / Areas for Improvement

1. **Monolithic Compiler** - Single 24K line file is hard to maintain
2. **No Proper AST** - Direct code generation without intermediate representation
3. **Massive IDE** - 862KB of IDE code tightly coupled with compiler
4. **Large Runtime** - libqb.cpp is 31K lines, could be more modular
5. **No LSP Support** - Modern editor integration is missing
6. **Limited Error Recovery** - Parser tends to give up on first error
7. **Build Time** - C++ compilation adds significant overhead
8. **GOTO-Heavy Code** - Compiler uses lots of GOTO (legacy style)
9. **Memory Usage** - Can consume 25GB+ memory on large programs

### QB64-Specific Extensions

These extensions are important for compatibility:

- `_MEM` operations (direct memory access)
- `_SNDOPEN`, `_SNDPLAY` (audio)
- `_LOADIMAGE`, `_PUTIMAGE` (graphics)
- `_NEWIMAGE`, screen modes
- `_OFFSET` type
- `$INCLUDE`, `$IF` preprocessor
- OpenGL integration (`_GL*` commands)
- Console support (`$CONSOLE`)
- File embedding (`$EMBED`)

---

## Comparison with QB64Fresh

| Aspect | QB64pe | QB64Fresh |
|--------|--------|-----------|
| **Language** | QB64 (self-hosted) | Rust |
| **Code Generation** | Direct C++ | C (via trait abstraction) |
| **AST** | None (direct emission) | Full AST → Typed IR |
| **Parser** | Line-by-line, GOTO-heavy | Recursive descent, Pratt parsing |
| **IDE** | Built-in (~970K lines) | LSP-based (external editors) |
| **Runtime** | C++ (~31K lines) | Rust (~19K lines, C FFI) |
| **Modularity** | Monolithic compiler | Modular phases |
| **Error Recovery** | Limited | Multiple errors collected |
| **Testing** | Large test suite | Unit + integration + golden tests |
| **Memory Safety** | Manual management | Rust ownership system |

For intentional behavioral differences and migration from QB64pe, see `docs/INTENTIONAL_DIFFERENCES_FROM_QB64PE.md` and `docs/QB64pe/QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md`.

---

## Recommendations for QB64Fresh

Based on this analysis, here are key recommendations for the QB64Fresh implementation:

### 1. Separate Concerns Clearly

**Architecture:**
```
Source → Lexer → Parser → AST → Semantic Analysis → Typed IR → CodeGen → Output
 ↑
 Backend trait interface
```

Each phase should be:
- Independently testable
- Single responsibility
- Well-documented

### 2. Build a Proper AST

**Benefits:**
- Enables better error messages with precise source locations
- Supports IDE features (go-to-definition, refactoring, symbol navigation)
- Makes optimization possible (constant folding, dead code elimination)
- Enables incremental parsing for LSP

**Implementation:** QB64Fresh already implements this - maintain and extend the AST structure.

### 3. Design for LSP from the Start

**Requirements:**
- Parser should support incremental parsing (parse only changed regions)
- Symbol table should support queries (find all references, go-to-definition)
- Error recovery is essential (don't stop at first error)
- Source location tracking throughout pipeline

**Status:** QB64Fresh has LSP implementation - ensure it leverages the AST effectively.

### 4. Modular Runtime

**Approach:**
- Separate concerns (graphics, audio, file I/O, networking)
- Allow dead code elimination (only link what's used)
- Consider Rust or modern C++ for safety
- Trait-based backends enable testing without hardware dependencies

**Status:** QB64Fresh uses trait-based graphics/audio backends - good foundation.

### 5. Modern Code Generation Options

**Current:** C intermediate (proven, simple, portable)

**Future Considerations:**
- Consider LLVM for optimization (if performance becomes critical)
- Or continue with C as intermediate (simpler, proven)
- Support WebAssembly for browser deployment? (future consideration)

**Recommendation:** Stick with C intermediate for now (YAGNI), but keep backend trait abstraction for future flexibility.

### 6. Preserve Compatibility

**Strategy:**
- Use QB64-PE test suite for validation (`tests/qbasic_testcases/`)
- Document any intentional deviations
- Focus on `qb45com/` directory first (core QB4.5 compatibility)
- Programs using `_` prefixed commands are QB64 extensions (may not be in initial scope)

**Status:** QB64Fresh achieves 99.1% compatibility with QB45 test suite.

### 7. Testing Strategy

**Approach:**
- Unit tests for each compiler phase
- Integration tests for end-to-end compilation
- Golden tests for regression detection
- Compatibility tests using QB64pe's test suite

**Status:** QB64Fresh has comprehensive test infrastructure.

### 8. Error Handling

**Best Practices:**
- Collect multiple errors per compilation (don't stop at first)
- Include source locations in all diagnostics
- Provide actionable error messages
- Support error recovery in parser

**Status:** QB64Fresh collects multiple errors with source spans.

---

## References

- **QB64pe Repository:** https://github.com/QB64-Phoenix-Edition/QB64pe
- **QB64pe Wiki:** https://qb64phoenix.com/qb64wiki
- **QB64pe Forum:** https://qb64phoenix.com/forum
- **QB64Fresh Architecture:** See `docs/ARCHITECTURE.md`
- **QB64Fresh intentional differences:** See `docs/INTENTIONAL_DIFFERENCES_FROM_QB64PE.md` and `docs/adrs/ADR-0016-intentional-behavioral-differences.md`
- **Migrating from QB64pe:** See `docs/QB64pe/QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md`

---

---

## Document History

- **2026-01-16:** Initial architecture analysis created (`QB64PE_ARCHITECTURE_ANALYSIS.md`)
- **2026-01-25:** Comprehensive architecture documentation created (`QB64PE_ARCHITECTURE.md`)
- **2026-01-27:** Documents merged into unified analysis and reference document
- **2026-01-28:** References updated (QB64Fresh runtime size, intentional differences and migration docs); document history extended

*This document combines the original analysis and comprehensive documentation into a single reference.*
