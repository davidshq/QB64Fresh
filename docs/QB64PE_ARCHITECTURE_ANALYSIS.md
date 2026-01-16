# QB64 Phoenix Edition - Architecture Analysis

This document analyzes the architecture of QB64-PE to inform the QB64Fresh rewrite.

---

## Executive Summary

QB64-PE is a self-hosted BASIC compiler that translates QBasic/QuickBASIC-compatible source code into C++ code, which is then compiled to native executables using a C++ toolchain (GCC/MinGW). It includes a built-in IDE written in QB64 itself.

**Key Metrics:**
- Main compiler: ~24,000 lines of QB64 code
- IDE: ~862,000 lines of QB64 code (huge!)
- Runtime library: ~31,000 lines of C++ code
- Built-in functions definition: ~123,000 lines of QB64 code

---

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        QB64-PE System                           │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐ │
│  │    IDE      │───▶│  Compiler   │───▶│   C++ Toolchain     │ │
│  │ (optional)  │    │ (qb64pe.bas)│    │  (GCC/MinGW/Clang)  │ │
│  └─────────────┘    └──────┬──────┘    └──────────┬──────────┘ │
│                            │                      │             │
│                            ▼                      ▼             │
│                     ┌─────────────┐        ┌───────────┐       │
│                     │ Generated   │───────▶│  Native   │       │
│                     │ C++ Code    │        │ Executable│       │
│                     └─────────────┘        └───────────┘       │
│                            │                      │             │
│                            ▼                      │             │
│                     ┌─────────────┐               │             │
│                     │  Runtime    │───────────────┘             │
│                     │  Library    │                             │
│                     │ (libqb.cpp) │                             │
│                     └─────────────┘                             │
└─────────────────────────────────────────────────────────────────┘
```

---

## Component Breakdown

### 1. Compiler (`source/qb64pe.bas`)

**Location:** `source/qb64pe.bas` (~24,000 lines)

**Responsibilities:**
- Lexical analysis (tokenization)
- Parsing BASIC source code
- Semantic analysis
- C++ code generation
- Preprocessor directives ($IF, $ELSE, $INCLUDE, etc.)
- Error reporting

**Key Characteristics:**
- **Self-hosted**: Written in QB64 itself
- **Single-pass with recompile loops**: Uses multiple passes to resolve forward references
- **Line-by-line processing**: Processes source one line at a time
- **Direct C++ emission**: Generates C++ code directly (no intermediate representation)

**Internal Structure:**
```
qb64pe.bas
├── Global definitions and constants
├── Preprocessor ($IF, $ELSE, $LET, $INCLUDE, etc.)
├── Line processing loop
│   ├── Element extraction (getelement$)
│   ├── Statement recognition
│   ├── Expression parsing
│   └── C++ code emission
├── Type system management
├── Symbol table (hash-based)
└── Error handling
```

**Included Files:**
- `global/version.bas` - Version information
- `global/constants.bas` - Global constants
- `global/settings.bas` - Configuration settings
- `subs_functions/subs_functions.bas` - Built-in function definitions
- `ide/ide_*.bas` - IDE components (optional)
- `utilities/*.bi` - Utility functions

### 2. Built-in Functions (`source/subs_functions/subs_functions.bas`)

**Location:** `source/subs_functions/subs_functions.bas` (~123,000 lines)

**Purpose:** Defines the signatures and mappings for all built-in BASIC functions and subroutines.

**Structure:**
```basic
clearid
id.n = "FunctionName"     ' BASIC name
id.subfunc = 1            ' 1=function, 2=subroutine
id.callname = "c_func"    ' C++ function to call
id.args = N               ' Number of arguments
id.arg = MKL$(type1) + ...  ' Argument types
id.ret = returntype       ' Return type
id.hr_syntax = "..."      ' Human-readable syntax
regid
```

**Categories of built-ins:**
- String functions (LEFT$, RIGHT$, MID$, etc.)
- Math functions (SIN, COS, SQR, etc.)
- File I/O (OPEN, CLOSE, INPUT, PRINT#, etc.)
- Graphics (_NEWIMAGE, PSET, LINE, CIRCLE, etc.)
- Audio (_SNDOPEN, _SNDPLAY, etc.)
- System functions (TIMER, DATE$, TIME$, etc.)
- QB64-specific extensions (_MEM*, _OFFSET, etc.)

### 3. IDE (`source/ide/`)

**Location:** `source/ide/ide_*.bas` (~970,000 lines total)

**Files:**
- `ide_global.bas` (~64KB) - Global variables and state
- `ide_methods.bas` (~862KB) - Main IDE functionality
- `ide_export.bas` (~34KB) - Export functions
- `ide_converters.bas` (~6KB) - Format converters

**Note for QB64Fresh:** We are NOT reproducing the IDE. Instead, we'll provide:
- Language Server Protocol (LSP) implementation
- VSCode extension
- Potentially other editor integrations

### 4. Runtime Library (`internal/c/`)

**Main file:** `internal/c/libqb.cpp` (~31,000 lines)

**Modular components in `internal/c/libqb/src/`:**
| File | Lines | Purpose |
|------|-------|---------|
| graphics.cpp | ~105K | Graphics rendering, screen modes |
| shell.cpp | ~54K | SHELL command, process execution |
| filesystem.cpp | ~36K | File operations |
| gfs.cpp | ~33K | General file system |
| qbs.cpp | ~19K | QB string handling |
| http.cpp | ~13K | HTTP/networking |
| mem.cpp | ~11K | Memory operations |
| datetime.cpp | ~9K | Date/time functions |
| error_handle.cpp | ~11K | Error handling |

**Dependencies (in `internal/c/parts/`):**
- `audio/` - Audio playback (miniaudio)
- `video/` - Video/image handling
- `core/` - Core utilities
- `gui/` - GUI components (GLUT/freeglut)
- `network/` - Networking
- `input/` - Input handling
- `os/` - OS-specific code

**External Libraries:**
- FreeGLUT - OpenGL windowing
- STB libraries - Image loading
- miniaudio - Audio playback
- FreeType - Font rendering
- zlib - Compression
- Various platform-specific APIs

### 5. Build System

**Main file:** `Makefile`

**Process:**
1. QB64 compiler generates C++ code to `internal/temp/`
2. Makefile compiles generated C++ with runtime library
3. Links with platform-specific libraries
4. Produces native executable

**Platform support:**
- Windows (MinGW)
- Linux (GCC)
- macOS (Clang)

---

## Compilation Pipeline

```
┌──────────────┐
│ BASIC Source │
│   (.bas)     │
└──────┬───────┘
       │
       ▼
┌──────────────────────────────────────────────────────────────┐
│                    QB64 COMPILER                              │
├──────────────────────────────────────────────────────────────┤
│  1. PREPROCESSOR                                              │
│     ├─ $INCLUDE processing                                    │
│     ├─ $IF/$ELSE/$END IF conditional compilation             │
│     ├─ $LET constant definitions                              │
│     └─ Metacommand handling ($CONSOLE, $RESIZE, etc.)        │
├──────────────────────────────────────────────────────────────┤
│  2. LEXICAL ANALYSIS                                          │
│     ├─ Line reading                                           │
│     ├─ Element extraction (getelement$)                       │
│     ├─ String/comment handling                                │
│     └─ Line continuation                                      │
├──────────────────────────────────────────────────────────────┤
│  3. PARSING & SEMANTIC ANALYSIS                               │
│     ├─ Statement recognition                                  │
│     ├─ Expression parsing                                     │
│     ├─ Type checking                                          │
│     ├─ Symbol table management (hash table)                   │
│     └─ Scope handling                                         │
├──────────────────────────────────────────────────────────────┤
│  4. CODE GENERATION                                           │
│     ├─ Direct C++ emission                                    │
│     ├─ Variable declarations                                  │
│     ├─ Function/Sub translations                              │
│     └─ Runtime library calls                                  │
└──────────────────────────────────────────────────────────────┘
       │
       ▼
┌──────────────┐
│ Generated    │
│ C++ Code     │
│ (internal/   │
│  temp/*.cpp) │
└──────┬───────┘
       │
       ▼
┌──────────────────────────────────────────────────────────────┐
│                    C++ COMPILER                               │
│              (GCC / MinGW / Clang)                            │
├──────────────────────────────────────────────────────────────┤
│  • Compiles generated code                                    │
│  • Links with libqb (runtime library)                         │
│  • Links with platform libraries                              │
└──────────────────────────────────────────────────────────────┘
       │
       ▼
┌──────────────┐
│   Native     │
│  Executable  │
└──────────────┘
```

---

## Type System

QB64 supports these data types:

| QB64 Type | C++ Equivalent | Size |
|-----------|----------------|------|
| _BIT | int8 (packed) | 1 bit |
| _BYTE | int8 | 1 byte |
| INTEGER | int16 | 2 bytes |
| LONG | int32 | 4 bytes |
| _INTEGER64 | int64 | 8 bytes |
| _OFFSET | ptrdiff_t | pointer-sized |
| SINGLE | float | 4 bytes |
| DOUBLE | double | 8 bytes |
| _FLOAT | long double | 10+ bytes |
| STRING | qbs* | variable |
| STRING * n | char[n] | fixed |
| _UNSIGNED variants | unsigned versions | same |

**User-Defined Types (UDT):**
```basic
TYPE MyType
    field1 AS INTEGER
    field2 AS STRING * 20
END TYPE
```

---

## Symbol Table

Uses a hash table for efficient symbol lookup:

```basic
HashAdd name$, flags, index
HashFind name$, flags, index
HashRemove name$
```

**Flags:**
- HASHFLAG_TYPE
- HASHFLAG_RESERVED
- HASHFLAG_OPERATOR
- HASHFLAG_CUSTOMSYNTAX

---

## Key Design Observations

### Strengths
1. **Complete QB4.5/QBasic compatibility** - Runs vintage BASIC programs
2. **Extensive QB64 extensions** - Modern features (_MEM, _SNDPLAY, etc.)
3. **Cross-platform** - Windows, Linux, macOS support
4. **Self-contained** - Ships with its own C++ compiler on Windows
5. **Large test suite** - Comprehensive compatibility testing

### Weaknesses / Areas for Improvement
1. **Monolithic compiler** - Single 24K line file is hard to maintain
2. **No proper AST** - Direct code generation without intermediate representation
3. **Massive IDE** - 862KB of IDE code tightly coupled with compiler
4. **Large runtime** - libqb.cpp is 31K lines, could be more modular
5. **No LSP support** - Modern editor integration is missing
6. **Limited error recovery** - Parser tends to give up on first error
7. **Build time** - C++ compilation adds significant overhead
8. **GOTO-heavy code** - Compiler uses lots of GOTO (legacy style)

### QB64-Specific Extensions to Preserve
- `_MEM` operations (direct memory access)
- `_SNDOPEN`, `_SNDPLAY` (audio)
- `_LOADIMAGE`, `_PUTIMAGE` (graphics)
- `_NEWIMAGE`, screen modes
- `_OFFSET` type
- `$INCLUDE`, `$IF` preprocessor
- OpenGL integration
- Console support

---

## Test Suite Structure

Location: `tests/`

```
tests/
├── compile_tests/       # Compilation tests by feature
│   ├── basic/          # Basic functionality
│   ├── audio_*/        # Audio features
│   ├── cast/           # Type casting
│   ├── const/          # Constants
│   ├── data/           # DATA statements
│   ├── filesystem/     # File operations
│   ├── font/           # Font handling
│   ├── image/          # Image operations
│   ├── mem/            # Memory operations
│   ├── types/          # Type definitions
│   └── ...
├── qbasic_testcases/   # Classic QBasic programs
│   ├── misc/           # Miscellaneous programs
│   ├── pete/           # Pete's collection
│   ├── thebob/         # TheBob's collection
│   └── ...
└── format_tests/       # Code formatting tests
```

---

## Recommendations for QB64Fresh

Based on this analysis:

1. **Separate concerns clearly:**
   - Lexer → Parser → AST → Semantic Analysis → Code Generation
   - Each phase should be independent and testable

2. **Build a proper AST:**
   - Enables better error messages
   - Supports IDE features (go-to-definition, refactoring)
   - Makes optimization possible

3. **Design for LSP from the start:**
   - Parser should support incremental parsing
   - Symbol table should support queries
   - Error recovery is essential

4. **Modular runtime:**
   - Separate concerns (graphics, audio, file I/O)
   - Allow dead code elimination
   - Consider Rust or modern C++ for safety

5. **Modern code generation options:**
   - Consider LLVM for optimization
   - Or continue with C as intermediate (simpler)
   - Support WebAssembly for browser deployment?

6. **Preserve compatibility:**
   - Use QB64-PE test suite for validation
   - Document any intentional deviations

---

*Document generated: 2026-01-16*
*Source analyzed: QB64 Phoenix Edition (QB64pe)*
