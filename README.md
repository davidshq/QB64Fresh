# QB64Fresh

**VIBE CODED: Use with caution.**

A modern, ground-up rewrite of QB64 — a QuickBASIC compatible compiler that produces native executables.

## Status

🚧 **Active Development** — Core compiler pipeline complete, approaching feature completeness.

| Component | Status |
|-----------|--------|
| Lexer | ✅ Complete (~1,846 lines) |
| Parser | ✅ Complete (~11,350 lines) |
| Semantic Analysis | ✅ Complete (~14,440 lines) |
| Code Generation | ✅ Complete (~11,728 lines) |
| Runtime | ✅ Complete (~11,678 lines) |
| LSP Server | ✅ Complete (~2,105 lines) |
| **QB4.5 Compatibility** | **99.1%** (114/115 test files) |

## Quick Start

```bash
# Build the compiler
cargo build --release

# Compile a BASIC program
cargo run --release -- examples/hello.bas

# Run with debug output
cargo run -- examples/hello.bas --tokens   # Show tokenization
cargo run -- examples/hello.bas --ast      # Show parse tree
cargo run -- examples/hello.bas --typed-ir # Show typed IR
cargo run -- examples/hello.bas --emit-c   # Show generated C code
```

## Goals

- ✅ QBasic/QuickBASIC compatibility
- ✅ QB64 extension support (240+ built-in functions)
- ✅ LSP for modern editor integration
- ✅ Clean, educational codebase
- 🔄 Full documentation (in progress)

## Tech Stack

- **Language:** Rust 1.70+
- **Code Generation:** C intermediate (GCC/Clang)
- **Graphics:** SDL2 (via Rust bindings)
- **Audio:** Rodio (pure Rust)
- **Lexer:** logos crate
- **Error Reporting:** ariadne crate
- **LSP:** tower-lsp crate

## Architecture

```
Source (.bas) → Lexer → Parser → Semantic → CodeGen → C → GCC → Executable
                                    ↓
                              Runtime Library (SDL2, Rodio, I/O)
```

See [ARCHITECTURE.md](ARCHITECTURE.md) for detailed documentation.

## Features

### Implemented
- Full QB4.5 language support (variables, arrays, control flow, procedures)
- User-defined types (TYPE/END TYPE)
- File I/O (OPEN, PRINT#, INPUT#, GET, PUT, SEEK)
- Graphics (SCREEN, LINE, CIRCLE, PAINT, PSET, _PUTIMAGE)
- Audio (BEEP, SOUND, PLAY, _SNDOPEN, _SNDPLAY)
- Mouse input (_MOUSEX, _MOUSEY, _MOUSEBUTTON)
- Keyboard input (INKEY$, _KEYHIT, _KEYDOWN)
- Networking (_OPENHOST, _OPENCLIENT, _CONNECTED)
- Error handling (ON ERROR GOTO, RESUME)
- 240+ built-in functions

### Not Yet Implemented
- Alpha blending
- Hardware acceleration
- Multi-threading (_THREAD)
- OpenGL commands (intentionally excluded — use DECLARE LIBRARY for raw GL)

## Docs

- [DEVELOPMENT.md](DEVELOPMENT.md) — Setup & contributing
- [ARCHITECTURE.md](ARCHITECTURE.md) — Compiler pipeline design
- [Migration Guide](docs/MIGRATION_GUIDE.md) — For QB64 users switching to QB64Fresh
- [Language Spec](docs/QB64_LANGUAGE_SPECIFICATION.md) — Language reference
- [Examples](examples/) — Example programs
- [AgenticLogs/](AgenticLogs/) — Development history

## VSCode Extension

A companion VSCode extension is available in [vscode-qb64fresh/](../vscode-qb64fresh/):
- Syntax highlighting
- Real-time diagnostics
- Go-to-definition
- Hover information
- Code completion

## Testing

```bash
# Run all tests
cargo test

# Run QB4.5 compatibility tests
cargo test --test qb45_compat

# Run with coverage
cargo tarpaulin --out Html
```

**Test Coverage:** 81.63% (937+ tests)

## License

MIT
