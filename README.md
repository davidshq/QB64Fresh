# QB64Fresh

A modern, ground-up rewrite of QB64 — a QuickBASIC compatible compiler that produces native executables.

## Status

🚧 **Early Development**

## Quick Start

```bash
cargo build
cargo run -- examples/hello.bas --tokens
```

## Goals

- QBasic/QuickBASIC compatibility
- QB64 extension support
- LSP for modern editor integration
- Clean, educational codebase

## Tech Stack

- **Language:** Rust
- **Code Generation:** C intermediate
- **Runtime:** Rust (sdl2, rodio)

## Docs

- [DEVELOPMENT.md](DEVELOPMENT.md) — Setup & contributing
- [AgenticLogs/](AgenticLogs/) — Development history

## License

MIT
