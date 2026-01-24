# QB64Fresh

**VIBE CODED: Use with caution.**

A modern, ground-up rewrite of QB64 — a QuickBASIC compatible compiler that produces native executables.

## Status

🚧 **Active Development** — Core compiler pipeline complete, approaching feature completeness.

| Component | Status |
|-----------|--------|
| Lexer | ✅ Complete (~1,850 lines, logos-based) |
| Parser | ✅ Complete (~11,450 lines) |
| Semantic Analysis | ✅ Complete (~15,800 lines) |
| Code Generation | ✅ Complete (~14,750 lines) |
| Runtime | ✅ Complete (~11,700 lines Rust + inline C) |
| LSP Server | ✅ Complete (~2,200 lines) |
| **Test Suite** | **1,500+ tests** (850+ unit, 10 golden, 19 fuzz) |
| **QB4.5 Compatibility** | **99.1%** (114/115 test files) |
| **QB64pe Bootstrap** | ✅ Compiles 59K-line compiler |

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

## Bootstrap Achievement

QB64Fresh can compile **QB64pe itself** — a 59,000-line BASIC compiler — into a working executable:

| Metric | Value |
|--------|-------|
| Source | 39 files, ~59K lines |
| Generated C | 83,705 lines |
| Executable | 2.1 MB |
| Compile time | ~800ms |

This demonstrates real-world compatibility with large, complex BASIC codebases. See [BOOTSTRAP_ACHIEVEMENT.md](docs/BOOTSTRAP_ACHIEVEMENT.md) for technical details.

## Goals

- ✅ QBasic/QuickBASIC compatibility
- ✅ QB64 extension support (240+ built-in functions)
- ✅ LSP for modern editor integration
- ✅ Clean, educational codebase
- ✅ Compiles QB64pe compiler (bootstrap validation)
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
- [Bootstrap Achievement](docs/BOOTSTRAP_ACHIEVEMENT.md) — Compiling QB64pe with QB64Fresh
- [Behavioral Differences](docs/BEHAVIORAL_DIFFERENCES.md) — QB64Fresh vs QB64pe semantics
- [Language Spec](docs/QB64PE_LANGUAGE_SPECIFICATION.md) — QB64PE language reference
- [Examples](examples/) — Example programs
- [AgenticLogs/](AgenticLogs/) — Development history

## VSCode Extension

A full-featured VSCode extension is available in [vscode-qb64fresh/](../vscode-qb64fresh/):

| Feature | Description |
|---------|-------------|
| Syntax highlighting | Keywords, strings, numbers, comments |
| Real-time diagnostics | Errors and warnings as you type |
| Go-to-definition | Jump to variable, function, SUB definitions |
| Find references | Find all usages of a symbol |
| Rename symbol | Rename variables/functions across files (F2) |
| Hover information | View types and documentation |
| Code completion | Keywords, 240+ built-in functions, user symbols |
| Signature help | Parameter hints for 50+ functions |
| Document symbols | Outline view (Ctrl+Shift+O) |
| Workspace symbols | Search symbols across project (Ctrl+T) |
| Build & Run | F5 to compile and run, Ctrl+Shift+B to build |
| Code formatting | qb64fresh-fmt integration with style presets |
| Code linting | qb64fresh-lint with quick fix code actions |

Build the tools: `cargo build -p qb64fresh-fmt -p qb64fresh-lint`

## Testing

```bash
# Run all tests
cargo test

# Run QB4.5 compatibility tests
cargo test --test qb45_compat

# Run QB64pe bootstrap tests
cargo test --test bootstrap_tests

# Run with coverage
cargo tarpaulin --out Html
```

**Test Coverage:** 81.63% (1,500+ tests including unit, integration, golden, and fuzz tests)

## License

MIT
