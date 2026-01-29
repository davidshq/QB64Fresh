# Migration Guide: QB64 to QB64Fresh

This guide helps QB64 users transition to QB64Fresh, a modern rewrite of QB64 built in Rust.

---

## Quick Start

**Good news:** Most QB64 programs work with minimal or no changes. QB64Fresh achieves **99.1% compatibility** (114/115 files) with QB45/QBasic test suites.

**Ultimate validation:** QB64Fresh successfully compiles the **QB64pe compiler itself** - a 59,000-line BASIC codebase across 39 files - into a working 2.1MB executable. See [ARCHITECTURE.md](../ARCHITECTURE.md#bootstrap-achievement) for a summary, or [docs/archive/BOOTSTRAP_PLAN_FULL.md](../archive/BOOTSTRAP_PLAN_FULL.md) for detailed implementation history.

```bash
# Try compiling your program
cargo run --release -- your_program.bas --emit-c

# If it compiles, generate the executable
gcc output.c -o your_program -L./target/release -lqb64fresh_rt -lSDL2 -lm
```

If you get errors, this guide will help you resolve them.

---

## What is QB64Fresh?

QB64Fresh is a **ground-up rewrite** of QB64 (not a fork). Key differences:

| Aspect | QB64 | QB64Fresh |
|--------|------|-----------|
| **Implementation** | C++ + QB64 (self-hosted) | Rust |
| **IDE** | Built-in (862K lines) | None - uses LSP for editor integration |
| **Editor Support** | QB64 IDE only | VSCode, Vim, Emacs, any LSP client |
| **Graphics Backend** | SDL2 (hardcoded) | Trait-based (SDL2 default, pluggable) |
| **Audio Backend** | SDL2_mixer | Rodio (Rust crate) |
| **Codebase Size** | ~24K compiler + 862K IDE | ~40K lines total (modular) |

### Why Migrate?

- **Modern workflow**: Use your favorite editor with full language support via LSP
- **Cross-platform parity**: All platforms treated equally (not Windows-first)
- **Clean architecture**: Easier to understand, extend, and contribute to
- **Educational value**: Well-documented Rust codebase following compiler best practices

---

## Installation

### Prerequisites

| Tool | Version | Install |
|------|---------|---------|
| Rust | 1.70+ | [rustup.rs](https://rustup.rs/) |
| GCC or Clang | Any recent | Package manager |
| SDL2 dev libraries | 2.0+ | `apt install libsdl2-dev` (Linux) |

### Build QB64Fresh

```bash
# Clone the repository
git clone <repository-url>
cd QB64Fresh

# Build compiler and runtime
cargo build --release

# Verify installation
cargo run --release -- examples/hello.bas --tokens
```

### Memory Limits (Important)

**QB64Fresh and QB64pe can consume 25GB+ memory during compilation.** Always use memory limits to prevent system crashes:

```bash
# Set 16GB virtual memory limit (recommended)
ulimit -v 16777216

# Then run compilation
cargo run --release -- your_program.bas --emit-c
```

Or use a one-liner:
```bash
bash -c 'ulimit -v 16777216 && cargo run --release -- your_program.bas --emit-c'
```

See [docs/MEMORY_LIMITS.md](../MEMORY_LIMITS.md) for details.

### Editor Setup (VSCode)

1. Copy `vscode-qb64fresh/` to your VSCode extensions folder
2. Reload VSCode
3. Open any `.bas` file to get syntax highlighting and diagnostics

---

## What Works Unchanged

These QB64 features work identically in QB64Fresh:

### Core Language (100% Compatible)

- All statements: `PRINT`, `INPUT`, `LET`, `DIM`, `REDIM`, `CONST`, `SHARED`
- Control flow: `IF/THEN/ELSE`, `FOR/NEXT`, `WHILE/WEND`, `DO/LOOP`, `SELECT CASE`
- Procedures: `SUB`, `FUNCTION`, `GOSUB/RETURN`, recursion
- Data types: `INTEGER`, `LONG`, `SINGLE`, `DOUBLE`, `STRING`, `_INTEGER64`, etc.
- Arrays: Multi-dimensional, `REDIM`, `PRESERVE`, `LBOUND`, `UBOUND`
- User-defined types: `TYPE/END TYPE`
- String functions: `MID$`, `LEFT$`, `RIGHT$`, `LEN`, `INSTR`, `CHR$`, `ASC`, etc.
- Math functions: `SIN`, `COS`, `TAN`, `LOG`, `EXP`, `SQR`, `INT`, `ABS`, etc.
- File I/O: `OPEN`, `CLOSE`, `GET`, `PUT`, `SEEK`, `LINE INPUT`, `INPUT #`, etc.

### QB64 Extensions (Working)

- Graphics: `SCREEN`, `PSET`, `LINE`, `CIRCLE`, `PAINT`, `GET`, `PUT`
- Color: `COLOR`, `PALETTE`, `_RGB`, `_RGBA`
- Audio: `BEEP`, `SOUND`, `PLAY`, `_SNDOPEN`, `_SNDPLAY`, `_SNDSTOP`
- Input: `_MOUSEINPUT`, `_MOUSEX`, `_MOUSEY`, `_KEYHIT`, `_KEYDOWN`
- System: `SHELL`, `KILL`, `NAME`, `MKDIR`, `CHDIR`
- Memory: `DEF SEG`, `VARSEG`, `VARPTR`, `PEEK`, `POKE`
- Binary: `BLOAD`, `BSAVE`
- Error handling: `ON ERROR GOTO`
- C interop: `DECLARE LIBRARY`
- Networking: `_OPENHOST`, `_OPENCONNECTION`, `_OPENCLIENT`, `_CONNECTED`

---

## Changes Required

### 1. No Built-in IDE

**QB64 workflow:**
```
Open QB64 IDE → Edit code → Press F5 → Run
```

**QB64Fresh workflow:**
```bash
# Terminal-based compilation
qb64fresh program.bas --emit-c
gcc output.c -o program -lqb64fresh_rt -lSDL2 -lm
./program

# Or use VSCode with the QB64Fresh extension for IDE-like experience
```

### 2. Fixed-Length Strings in UDTs

QB64 implicitly converts and pads strings when assigning to `STRING * n` fields. QB64Fresh requires explicit padding.

```basic
' QB64 (works):
TYPE Person
    Name AS STRING * 20
END TYPE
DIM p AS Person
p.Name = "John"        ' Implicitly pads to 20 characters

' QB64Fresh (workaround):
p.Name = LEFT$("John" + SPACE$(20), 20)  ' Explicit padding
```

**Note:** This is an intentional difference for type safety. See [INTENTIONAL_DIFFERENCES_FROM_QB64PE.md](../INTENTIONAL_DIFFERENCES_FROM_QB64PE.md) for details.

### 3. OpenGL Commands Not Supported

QB64's 300+ `_GL*` commands (raw OpenGL access) are **intentionally excluded**. QB64Fresh uses a trait-based graphics abstraction for portability.

```basic
' QB64 (works):
_GLBEGIN _GL_TRIANGLES
_GLVERTEX2F 0, 0
' ...

' QB64Fresh alternative:
' Use standard graphics commands or _MAPTRIANGLE for textured rendering
LINE (0, 0)-(100, 100), _RGB(255, 0, 0)
```

**Why?** Raw OpenGL ties code to a specific graphics API. QB64Fresh's abstraction allows future WebAssembly, Vulkan, or native API backends.

If you need OpenGL, you can still use `DECLARE LIBRARY` to call OpenGL functions directly.

### 4. Windows-Specific Features

These Windows-only features are not supported:

| Feature | QB64Fresh Alternative |
|---------|----------------------|
| `_SCREENPRINT` | `SHELL "echo text"` |
| `_SCREENCLICK` | Not available |
| `_SCREENIMAGE` | Not available |
| `_WINDOWHANDLE` | Not available |
| `_CONSOLETITLE` | Not available |

### 5. CALL ABSOLUTE

`CALL ABSOLUTE` compiles but emits a warning. It cannot execute machine code in modern flat memory models.

```basic
' Compiles but warns:
CALL ABSOLUTE(addr%)   ' Warning: flat memory model, no-op
```

Legacy code using `CALL ABSOLUTE` for mouse drivers should use `_MOUSEINPUT` instead.

### 6. Cross-Procedure GOTO

QB64pe allows `GOTO` to labels in other procedures (creating a forward reference in the current scope). QB64Fresh requires labels to be in the same procedure.

```basic
' QB64pe (works):
SUB Proc1
    GOTO Label2  ' Label2 is in Proc2, but QB64pe creates it here
END SUB

SUB Proc2
Label2:
    PRINT "Hello"
END SUB

' QB64Fresh (error):
' Error: undefined label 'Label2' - label must be in current procedure
```

**Rationale:** This stricter behavior catches typos and prevents scope confusion. See [INTENTIONAL_DIFFERENCES_FROM_QB64PE.md](../INTENTIONAL_DIFFERENCES_FROM_QB64PE.md) for details.

### 7. Error Reporting

QB64pe typically stops at the first error. QB64Fresh collects and reports multiple errors per compilation.

```basic
' Program with multiple errors:
DIM x AS INTEGER
x = "hello"     ' Error 1: type mismatch
y = undefined   ' Error 2: undefined variable
z$ = 42         ' Error 3: type mismatch

' QB64pe: Reports only Error 1, stops
' QB64Fresh: Reports all three errors at once
```

**Rationale:** Better debugging experience - see all issues at once rather than fixing one at a time.

---

## Compilation Workflow

### Debug Your Code

QB64Fresh provides detailed diagnostics at each stage:

```bash
# Lexer output (tokenization)
qb64fresh program.bas --tokens

# Parser output (AST)
qb64fresh program.bas --ast

# Semantic analysis output (typed IR)
qb64fresh program.bas --typed-ir

# Full compilation to C
qb64fresh program.bas --emit-c
```

### Build the Executable

```bash
# Option 1: Inline runtime (self-contained C)
# Graphics/audio are stubs; frame limiting prevents infinite loops
ulimit -v 16777216  # Set memory limit first
qb64fresh program.bas --emit-c --runtime=inline
gcc output.c -o program -lSDL2 -lm

# Option 2: External runtime (links against library, full graphics/audio)
ulimit -v 16777216  # Set memory limit first
qb64fresh program.bas --emit-c --runtime=external
gcc output.c -o program -I runtime/include -L./target/release -lqb64fresh_rt -lSDL2 -lm
```

**Runtime Modes:**
- **Inline:** Self-contained C with embedded runtime. Graphics/audio are stubs (no-op). Use for CI, headless testing, or when you don't need graphics.
- **External:** Links against `libqb64fresh_rt` for full graphics/audio support via SDL2 and Rodio.

**Important:** Never edit generated `.c` files directly. If compilation fails, fix the code generator, not the output. See [ADR-0017](../adrs/ADR-0017-generated-code-is-ephemeral.md) for details.

---

## Common Migration Scenarios

### Scenario 1: Simple Console Program

**Usually works unchanged:**

```basic
' hello.bas
PRINT "Hello, World!"
INPUT "Your name: ", name$
PRINT "Hello, "; name$
```

```bash
qb64fresh hello.bas --emit-c
gcc output.c -o hello -lm
./hello
```

### Scenario 2: Graphics Program

**Usually works unchanged:**

```basic
' graphics.bas
SCREEN 12
LINE (0, 0)-(639, 479), 14
CIRCLE (320, 240), 100, 15
PAINT (320, 240), 9, 15
SLEEP
```

```bash
qb64fresh graphics.bas --emit-c
gcc output.c -o graphics -lSDL2 -lm
./graphics
```

### Scenario 3: Game with Mouse Input

**Usually works unchanged:**

```basic
' game.bas
SCREEN 13
DO
    WHILE _MOUSEINPUT: WEND
    mx = _MOUSEX
    my = _MOUSEY
    PSET (mx, my), 15
    _LIMIT 60
LOOP UNTIL _KEYHIT = 27
```

### Scenario 4: Program Using DECLARE LIBRARY

**Works, with same library:**

```basic
' clib.bas
DECLARE LIBRARY
    FUNCTION strlen& (s AS STRING)
END DECLARE

PRINT strlen("Hello")  ' Prints 5
```

### Scenario 5: Program Using Raw OpenGL

**Requires modification:**

```basic
' QB64 OpenGL code - won't work
_GLBEGIN _GL_TRIANGLES
' ...
_GLEND

' QB64Fresh alternative - use standard graphics
' For simple 2D: use LINE, CIRCLE, PSET, PUT
' For textured 2D: use _MAPTRIANGLE
' For full 3D: call OpenGL via DECLARE LIBRARY
```

### Scenario 6: Program Using Networking

**Works unchanged:**

```basic
' server.bas
serverHandle = _OPENHOST(12345)
IF serverHandle = 0 THEN
    PRINT "Failed to start server"
    END
END IF

' Wait for client connection (non-blocking)
DO
    clientHandle = _OPENCONNECTION(serverHandle)
    IF clientHandle <> 0 THEN
        PRINT "Client connected!"
        ' Use clientHandle for communication
        ' Network handles are negative numbers
    END IF
    _LIMIT 60  ' Prevent CPU spinning
LOOP

' client.bas
clientHandle = _OPENCLIENT("TCP/IP:12345:localhost")
IF clientHandle = 0 THEN
    PRINT "Failed to connect"
    END
END IF

IF _CONNECTED(clientHandle) THEN
    PRINT "Connected to server!"
END IF
```

**Note:** Network handles are negative numbers (or 0 on failure). Use `<> 0` to check for success, not `> 0`.

---

## Feature Comparison Table

| Feature | QB64 | QB64Fresh | Notes |
|---------|:----:|:---------:|-------|
| QBasic compatibility | ✅ | ✅ | 99.1% (114/115 files) |
| QB64 extensions | ✅ | ✅ | Most implemented |
| Built-in IDE | ✅ | ❌ | Uses LSP instead |
| LSP support | ❌ | ✅ | VSCode, Vim, etc. |
| OpenGL (_GL* commands) | ✅ | ❌ | Intentional - use DECLARE LIBRARY |
| Windows automation | ✅ | ❌ | Platform-specific |
| Cross-platform | ⚠️ | ✅ | Equal support all platforms |
| DECLARE LIBRARY | ✅ | ✅ | Full C interop |
| _THREAD | ✅ | ⚠️ | Planned |
| Joystick | ✅ | ✅ | Full support in external runtime (STICK, STRIG, _DEVICES, _AXIS, _BUTTON) |
| Networking | ✅ | ✅ | Full TCP/IP support (_OPENHOST, _OPENCLIENT, _CONNECTED) |

---

## Getting Help

### Error Messages

QB64Fresh provides detailed error messages with source locations:

```
error[E0042]: type mismatch
  --> program.bas:15:5
   |
15 |     player.Name = "John"
   |     ^^^^^^^^^^^^^^^^^^^^ expected STRING * 20, found STRING
   |
   = help: use LEFT$(value + SPACE$(20), 20) to pad the string
```

### Debugging Compilation Issues

1. **Parser errors**: Check syntax against QB64 reference
2. **Semantic errors**: Usually type mismatches - check variable declarations
3. **Codegen errors**: Rare - report as a bug

### Resources

- [QB64Fresh TODO.md](../TODO.md) - Known issues and roadmap
- [INTENTIONAL_DIFFERENCES_FROM_QB64PE.md](../INTENTIONAL_DIFFERENCES_FROM_QB64PE.md) - Deliberate behavioral differences
- [QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md](QB64PE_TO_QB64Fresh_BEHAVIORAL_DIFFERENCES.md) - All behavioral differences (architectural and intentional)
- [PARTIAL_IMPLEMENTATIONS.md](../ThingsToDo/PARTIAL_IMPLEMENTATIONS.md) - Implementation status by feature
- [MEMORY_LIMITS.md](../MEMORY_LIMITS.md) - Memory limit requirements and usage
- [DEVELOPMENT.md](../DEVELOPMENT.md) - Contributing guide
- [QB64-PE Wiki](https://qb64phoenix.com/qb64wiki/) - Language reference (mostly compatible)

---

## FAQ

### Q: Why rewrite QB64 instead of improving it?

QB64's codebase has grown organically over many years, making it difficult to maintain and extend. QB64Fresh starts fresh with modern architecture, making it easier to:
- Add new features
- Fix bugs
- Understand the code
- Contribute

### Q: Will my QB64 programs run faster/slower?

Similar performance. Both compile to C/C++ and use SDL2 for graphics. Minor differences may exist in specific operations due to runtime implementation differences.

### Q: Can I use QB64Fresh with my existing workflow?

Yes, but differently:
- **QB64**: Edit in QB64 IDE → F5 to run
- **QB64Fresh**: Edit in any editor → Compile in terminal → Run

The VSCode extension provides IDE-like features (syntax highlighting, error checking, etc.) via LSP.

### Q: What about my programs that use _GL* commands?

Options:
1. Use standard graphics commands if possible
2. Use `DECLARE LIBRARY` to call OpenGL directly
3. Continue using QB64 for those specific programs

### Q: Is QB64Fresh stable enough for production?

For most programs, yes. The 99.1% compatibility rate (114/115 test files) covers the vast majority of QB64 code. QB64Fresh successfully compiles the 59K-line QB64pe compiler itself (bootstrap validation). Check the [Known Issues](#changes-required) section and [INTENTIONAL_DIFFERENCES_FROM_QB64PE.md](../INTENTIONAL_DIFFERENCES_FROM_QB64PE.md) for potential edge cases.

### Q: Why does QB64Fresh behave differently in some cases?

Some differences are **intentional** for safety, clarity, or portability:
- Stricter GOTO scope (prevents bugs from typos)
- Multiple error reporting (better debugging)
- Different PRNG (implementation choice)
- Excluded OpenGL commands (portability)

See [INTENTIONAL_DIFFERENCES_FROM_QB64PE.md](../INTENTIONAL_DIFFERENCES_FROM_QB64PE.md) for the complete list and rationale.

---

## Migration Checklist

- [ ] Install Rust and build QB64Fresh
- [ ] **Set up memory limits** (see [MEMORY_LIMITS.md](../MEMORY_LIMITS.md))
- [ ] Test compile your program with `--typed-ir` flag
- [ ] Address any type errors (usually `STRING * n` issues)
- [ ] Check for cross-procedure GOTO (must be in same procedure)
- [ ] Replace any `_GL*` commands if used
- [ ] Replace Windows-specific features if used
- [ ] Review [INTENTIONAL_DIFFERENCES_FROM_QB64PE.md](../INTENTIONAL_DIFFERENCES_FROM_QB64PE.md) for behavioral differences
- [ ] Set up your preferred editor with LSP (optional but recommended)
- [ ] Compile to C and build executable (with memory limits)
- [ ] Test the compiled program

---

*Last updated: 2026-01-28*