# QB64Fresh Handbook

A comprehensive guide to using QB64Fresh. For a step-by-step **tutorial and getting started** (install, first program, compile, run), see [GETTING_STARTED.md](GETTING_STARTED.md). This handbook covers language basics, graphics, audio, file I/O, C library integration, and advanced topics.

**Last Updated:** 2026-01-31

---

## Table of Contents

1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Language Basics](#language-basics)
4. [C Library Integration](#c-library-integration)
5. [Graphics Programming](#graphics-programming) (incl. [OpenGL](#opengl-optional))
6. [Audio Programming](#audio-programming)
7. [File I/O](#file-io)
8. [Error Handling](#error-handling)
9. [Advanced Topics](#advanced-topics)
10. [Troubleshooting](#troubleshooting)

---

## Introduction

QB64Fresh is a ground-up rewrite of QB64, a compiler that transforms QuickBASIC code into native executables. It maintains compatibility with classic QBasic while adding modern features.

### Why QB64Fresh?

- **Modern Tooling**: LSP-based editor support instead of a custom IDE
- **Clean Codebase**: Educational, well-documented Rust implementation
- **Fast Compilation**: Compiles large programs in under a second
- **Cross-Platform**: Windows, Linux, and macOS support

### Quick Example

```basic
' hello.bas - A simple QB64Fresh program
PRINT "Hello, World!"

DIM name AS STRING
INPUT "What is your name? ", name
PRINT "Nice to meet you, "; name; "!"
```

Compile and run:
```bash
cargo run --release -- hello.bas --emit-c   # writes hello.c
gcc hello.c -o hello -lm
./hello
```
Use a memory limit when running the compiler (e.g. `ulimit -v 4194304` or `./run_limited.sh`); see [GETTING_STARTED.md](GETTING_STARTED.md) and [MEMORY_LIMITS.md](MEMORY_LIMITS.md).

---

## Getting Started

### Installation

1. **Install Rust** (1.70 or later):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. **Clone and build QB64Fresh**:
   ```bash
   git clone <repository-url>
   cd QB64Fresh
   cargo build --release
   ```

3. **Install a C compiler** (GCC or Clang)

### Compiling Your First Program

See [GETTING_STARTED.md](GETTING_STARTED.md) for the full walkthrough, including memory limits and the C link step.

```bash
# Compile BASIC to C (use memory limit: ulimit -v 4194304, or ./run_limited.sh)
cargo run --release -- myprogram.bas --emit-c   # writes myprogram.c

# Build the executable (inline runtime, default — graphics are stubs)
gcc myprogram.c -o myprogram -lm
./myprogram

# For full graphics/audio: use --runtime external and link libqb64fresh_rt (see GRAPHICS.md)
# Inspect pipeline stages
cargo run -- myprogram.bas --tokens     # Tokenization
cargo run -- myprogram.bas --ast        # Parse tree
cargo run -- myprogram.bas --typed-ir   # Typed IR
cargo run -- myprogram.bas --emit-c     # C code
```

---

## Language Basics

QB64Fresh supports the full QuickBASIC language plus QB64 extensions.

### Variables and Types

```basic
' Implicit declaration (default SINGLE)
x = 3.14

' Explicit declaration
DIM count AS INTEGER        ' 16-bit signed
DIM total AS LONG           ' 32-bit signed
DIM big AS _INTEGER64       ' 64-bit signed
DIM ratio AS SINGLE         ' 32-bit float
DIM precise AS DOUBLE       ' 64-bit float
DIM name AS STRING          ' Variable-length string
DIM buffer AS STRING * 80   ' Fixed-length string

' Type suffixes (alternative syntax)
count% = 42                 ' INTEGER
total& = 1000000            ' LONG
ratio! = 2.5                ' SINGLE
precise# = 3.14159265358979 ' DOUBLE
name$ = "Hello"             ' STRING
```

### Arrays

```basic
' Static arrays
DIM scores(10) AS INTEGER           ' 0 to 10 (11 elements)
DIM matrix(3, 3) AS DOUBLE          ' 2D array
DIM grid(1 TO 100, 1 TO 100) AS LONG ' Custom bounds

' Dynamic arrays
REDIM names$(100)                   ' Can be resized
REDIM PRESERVE names$(200)          ' Keep existing data
```

### Control Flow

```basic
' IF/THEN/ELSE
IF score >= 90 THEN
    PRINT "A"
ELSEIF score >= 80 THEN
    PRINT "B"
ELSE
    PRINT "C"
END IF

' SELECT CASE
SELECT CASE grade$
    CASE "A", "B"
        PRINT "Excellent!"
    CASE "C"
        PRINT "Acceptable"
    CASE ELSE
        PRINT "Needs improvement"
END SELECT

' FOR/NEXT
FOR i = 1 TO 10
    PRINT i
NEXT i

' DO/LOOP
DO WHILE running
    ProcessInput
LOOP

DO
    x = x + 1
LOOP UNTIL x > 100
```

### Procedures

```basic
' Subroutines
SUB DrawBox (x AS INTEGER, y AS INTEGER, w AS INTEGER, h AS INTEGER)
    LINE (x, y)-(x + w, y + h), 15, B
END SUB

' Functions
FUNCTION Factorial& (n AS INTEGER)
    IF n <= 1 THEN
        Factorial& = 1
    ELSE
        Factorial& = n * Factorial&(n - 1)
    END IF
END FUNCTION

' Call them
DrawBox 10, 10, 100, 50
PRINT Factorial&(5)  ' Prints 120
```

### User-Defined Types

```basic
TYPE Player
    name AS STRING * 32
    x AS SINGLE
    y AS SINGLE
    health AS INTEGER
    score AS LONG
END TYPE

DIM hero AS Player
hero.name = "Bob"
hero.x = 100
hero.y = 200
hero.health = 100
hero.score = 0
```

---

## C Library Integration

QB64Fresh provides powerful C interoperability through `DECLARE LIBRARY`.

### Basic Usage

```basic
' Call standard C library functions
DECLARE LIBRARY
    FUNCTION abs& (BYVAL n AS LONG)
    FUNCTION atoi& (s AS STRING)
    SUB exit (BYVAL code AS LONG)
END DECLARE

PRINT abs&(-42)        ' 42
PRINT atoi&("123")     ' 123
```

### Custom Libraries

```basic
' Use your own C library
DECLARE LIBRARY "mylib"
    FUNCTION mylib_init& ()
    SUB mylib_process (data AS STRING)
END DECLARE
```

### Static Libraries

```basic
' Link against a static library
DECLARE STATIC LIBRARY "libcrypto"
    FUNCTION SHA256$ (data AS STRING)
END DECLARE
```

### C Header Parsing (Advanced)

QB64Fresh can automatically parse C header files to extract function declarations, constants, and struct definitions. This feature requires the `header-parsing` feature flag.

#### Enabling Header Parsing

Build with the feature enabled:
```bash
cargo build --features header-parsing
```

**Note:** The header parser is available as a library feature. See [HEADER_PARSER_API.md](reference/HEADER_PARSER_API.md) for complete documentation.

#### Programmatic Header Parsing

```rust
use qb64fresh::header_parser::{parse_header_full, Platform};

let header = r#"
    #define VERSION 100
    #define MAX_PLAYERS 8

    #ifdef WIN32
    int win32_init(void);
    #endif

    struct Player {
        char name[64];
        int score;
        float x, y;
    };

    int game_init(void);
    void game_update(float dt);
    int game_get_score(int player_id);
"#;

// Parse for Linux platform
let result = parse_header_full(header, Some(Platform::Linux));

// Access extracted declarations
for constant in &result.constants {
    println!("Constant: {} = {:?}", constant.name, constant.value);
}
// Output:
//   Constant: VERSION = Integer(100)
//   Constant: MAX_PLAYERS = Integer(8)

for func in &result.functions {
    println!("Function: {} -> {:?}", func.name, func.return_type);
}
// Output:
//   Function: game_init -> Long
//   Function: game_update -> Void
//   Function: game_get_score -> Long
// Note: win32_init is NOT included (inside #ifdef WIN32)

for s in &result.structs {
    println!("Struct: {}", s.name);
    println!("{}", s.to_qb64_type());
}
// Output:
//   Struct: Player
//   TYPE Player
//       name AS STRING * 64
//       score AS LONG
//       x AS SINGLE
//       y AS SINGLE
//   END TYPE
```

#### Supported Header Features

| Feature | Support | Example |
|---------|---------|---------|
| `#define` constants | ✅ Full | `#define MAX 100` |
| Hex/octal/binary | ✅ Full | `#define MASK 0xFF` |
| Float constants | ✅ Full | `#define PI 3.14159` |
| String constants | ✅ Full | `#define NAME "app"` |
| Function macros | ⚠️ Skipped | `#define MAX(a,b) ...` |
| `#ifdef`/`#ifndef` | ✅ Full | Platform-specific code |
| `#if defined()` | ✅ Full | Complex conditions |
| `#elif`/`#else` | ✅ Full | Conditional branches |
| `struct` | ✅ Full | `struct Point { ... }` |
| `typedef struct` | ✅ Full | `typedef struct { } Name;` |
| Array members | ✅ Full | `char name[64]` |
| Pointer members | ✅ Full | `void* data` |
| Nested structs | ❌ No | Not supported |
| Bit fields | ❌ No | Not supported |

#### Platform Macros

The parser pre-defines these macros based on the target platform:

| Platform | Defined Macros |
|----------|---------------|
| Windows | `WIN32`, `_WIN32`, `__WIN32__`, `_MSC_VER` |
| Linux | `__linux__`, `__unix__`, `__GNUC__`, `linux`, `unix` |
| macOS | `__APPLE__`, `__MACH__`, `__unix__`, `__GNUC__` |

#### Type Mapping

C types are automatically mapped to QB64 types:

| C Type | QB64 Type | Notes |
|--------|-----------|-------|
| `int`, `int32_t`, `long` | `LONG` | 32-bit signed |
| `short`, `int16_t` | `INTEGER` | 16-bit signed |
| `char`, `int8_t` | `_BYTE` | 8-bit signed |
| `long long`, `int64_t` | `_INTEGER64` | 64-bit signed |
| `unsigned int`, `uint32_t` | `_UNSIGNED LONG` | 32-bit unsigned |
| `float` | `SINGLE` | 32-bit float |
| `double` | `DOUBLE` | 64-bit float |
| `char*`, `const char*` | `STRING` | String pointer |
| `void*`, other pointers | `_OFFSET` | Generic pointer |
| `char[N]` | `STRING * N` | Fixed-length string |
| `size_t`, `intptr_t` | `_OFFSET` | Pointer-sized |

---

## Graphics Programming

QB64Fresh supports classic QBasic graphics plus modern QB64 extensions. With the **inline** runtime (default), graphics calls are stubs (no-ops with frame limiting); for real windows and drawing, use the **external** runtime and link against `libqb64fresh_rt`. See [GRAPHICS.md](GRAPHICS.md) for architecture and build steps.

### Screen Modes

```basic
' Classic modes
SCREEN 12    ' 640x480, 16 colors
SCREEN 13    ' 320x200, 256 colors

' QB64 extended modes
SCREEN _NEWIMAGE(800, 600, 32)  ' 32-bit color
```

### Drawing

```basic
SCREEN 12

' Points and lines
PSET (100, 100), 14           ' Yellow point
LINE (0, 0)-(639, 479), 15    ' White diagonal

' Shapes
LINE (10, 10)-(100, 100), 12, B   ' Red box outline
LINE (120, 10)-(210, 100), 10, BF ' Green filled box
CIRCLE (320, 240), 100, 9         ' Blue circle

' Fill
PAINT (320, 240), 4, 9       ' Fill circle with red

' Text
LOCATE 1, 1
PRINT "Hello Graphics!"
```

### Images

```basic
DIM img AS LONG
img = _LOADIMAGE("sprite.png", 32)

_PUTIMAGE (100, 100), img          ' Draw at position
_PUTIMAGE (0, 0)-(199, 199), img   ' Stretch to fit
_FREEIMAGE img                      ' Release memory
```

### Double Buffering

```basic
SCREEN _NEWIMAGE(800, 600, 32)
_SCREENMOVE _MIDDLE

DO
    CLS
    ' Draw your frame
    DrawGame
    _DISPLAY  ' Show the frame
    _LIMIT 60 ' Cap at 60 FPS
LOOP UNTIL _KEYHIT = 27  ' ESC to exit
```

### OpenGL (Optional)

QB64Fresh supports QB64pe-style OpenGL (`_GL*`, `_GLU*`, `SUB _GL`) as an optional feature. Use `SUB _GL` and `_GL*` commands; the compiler enables OpenGL by use. The runtime must be built with the `opengl` feature for GL rendering. See [OPENGL.md](OPENGL.md) for enabling and building.

---

## Audio Programming

### Classic Sounds

```basic
BEEP                    ' System beep
SOUND 440, 18          ' 440 Hz for ~1 second
PLAY "O4 C D E F G A B > C"  ' Musical notes
```

### Modern Audio (QB64)

```basic
' Load and play a sound file
DIM snd AS LONG
snd = _SNDOPEN("music.ogg")

_SNDPLAY snd           ' Play once
_SNDLOOP snd           ' Loop continuously
_SNDVOL snd, 0.5       ' Set volume to 50%
_SNDPAUSE snd          ' Pause playback
_SNDSTOP snd           ' Stop playback
_SNDCLOSE snd          ' Release handle
```

---

## File I/O

### Sequential Files

```basic
' Writing
OPEN "data.txt" FOR OUTPUT AS #1
PRINT #1, "Hello, World!"
PRINT #1, 42; 3.14
CLOSE #1

' Reading
OPEN "data.txt" FOR INPUT AS #1
LINE INPUT #1, text$
INPUT #1, num%, dec!
CLOSE #1

' Appending
OPEN "log.txt" FOR APPEND AS #1
PRINT #1, TIME$; " - Event occurred"
CLOSE #1
```

### Random Access Files

```basic
TYPE Record
    id AS LONG
    name AS STRING * 30
    value AS SINGLE
END TYPE

DIM r AS Record

' Writing
OPEN "data.dat" FOR RANDOM AS #1 LEN = LEN(r)
r.id = 1: r.name = "Item One": r.value = 99.95
PUT #1, 1, r
CLOSE #1

' Reading
OPEN "data.dat" FOR RANDOM AS #1 LEN = LEN(r)
GET #1, 1, r
PRINT r.id, r.name, r.value
CLOSE #1
```

### Binary Files

```basic
DIM buffer AS STRING * 1024

OPEN "file.bin" FOR BINARY AS #1
GET #1, , buffer      ' Read 1024 bytes
SEEK #1, 100          ' Move to byte 100
PUT #1, , buffer      ' Write 1024 bytes
CLOSE #1
```

---

## Error Handling

### ON ERROR GOTO

```basic
ON ERROR GOTO ErrorHandler

OPEN "nonexistent.txt" FOR INPUT AS #1
' This will trigger the error handler

END

ErrorHandler:
PRINT "Error"; ERR; "at line"; ERL
SELECT CASE ERR
    CASE 53
        PRINT "File not found!"
    CASE 75
        PRINT "Path/File access error!"
    CASE ELSE
        PRINT "Unknown error occurred"
END SELECT
RESUME NEXT  ' Continue after the error
```

### ERROR Statement

```basic
' Trigger a custom error
ERROR 100  ' User-defined error

' Error codes
' 5  - Illegal function call
' 6  - Overflow
' 9  - Subscript out of range
' 11 - Division by zero
' 53 - File not found
' 62 - Input past end of file
```

---

## Advanced Topics

### Memory Operations

```basic
' Direct memory access
DIM m AS _MEM
m = _MEM(variable)
value = _MEMGET(m, m.OFFSET, LONG)
_MEMPUT m, m.OFFSET, newvalue AS LONG
_MEMFREE m
```

### Bit Operations

```basic
' Bit manipulation
result = a AND b      ' Bitwise AND
result = a OR b       ' Bitwise OR
result = a XOR b      ' Bitwise XOR
result = NOT a        ' Bitwise NOT

' QB64 bit functions
bit = _READBIT(value, position)
value = _SETBIT(value, position)
value = _RESETBIT(value, position)
value = _TOGGLEBIT(value, position)
```

### Date and Time

```basic
PRINT DATE$           ' Current date (MM-DD-YYYY)
PRINT TIME$           ' Current time (HH:MM:SS)
PRINT TIMER           ' Seconds since midnight

' QB64 extended
PRINT _DATE$          ' ISO format (YYYY-MM-DD)
PRINT _ENVIRONMENTVARIABLE$("HOME")
```

---

## Troubleshooting

### Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| "Type mismatch" | Wrong variable type | Check type suffixes and declarations |
| "Subscript out of range" | Array bounds exceeded | Verify array dimensions |
| "File not found" | Path incorrect | Use absolute path or check working directory |
| "Illegal function call" | Invalid parameter | Check function documentation |

### Debug Output

```bash
# See the generated C code to debug issues
cargo run -- myprogram.bas --emit-c

# Enable verbose compiler output
cargo run -- myprogram.bas --verbose
```

### Getting Help

- [DOCS-README.md](DOCS-README.md) — Documentation index and quick navigation
- [QB64Fresh_LANGUAGE_REFERENCE.md](QB64Fresh_LANGUAGE_REFERENCE.md) — Supported statements and functions
- [QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md](QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md) — Differences from QB64pe
- [QB64 Wiki](https://qb64phoenix.com/qb64wiki/) — Language reference
- Report issues at the project repository

---

## Appendix: Feature Flag Reference

QB64Fresh uses Cargo feature flags to enable optional functionality:

| Feature | Description | Default |
|---------|-------------|---------|
| `header-parsing` | C header file parsing for DECLARE LIBRARY | Off |
| `graphics-sdl2` | SDL2 graphics backend (runtime) | On |
| `opengl` | OpenGL support in runtime (`_GL*`, `SUB _GL`) | Off |

Enable features when building the **compiler** or **runtime** as needed:
```bash
cargo build --features header-parsing
cargo test --features header-parsing
# Runtime with full graphics + OpenGL:
cd runtime && cargo build --release --features "graphics-sdl2,opengl"
```
