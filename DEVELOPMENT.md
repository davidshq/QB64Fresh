# QB64Fresh Development Guide

Welcome to QB64Fresh! This guide will help you set up your development environment and get started contributing.

## What is QB64Fresh?

QB64Fresh is a ground-up rewrite of QB64, a modern BASIC compiler that compiles QBasic/QuickBASIC-compatible code to native executables. We're building it in Rust with a focus on:

- Clean, educational code
- Modern tooling (LSP instead of built-in IDE)
- Extensible architecture

For project history and decision rationale, see `AgenticLogs/`.

---

## Prerequisites

### Required

| Tool | Minimum Version | Check Command |
|------|-----------------|---------------|
| Rust | 1.70+ | `rustc --version` |
| Cargo | 1.70+ | `cargo --version` |
| GCC/Clang | Any recent | `gcc --version` |

**Install Rust** (if needed):
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Recommended Rust Components

These should be installed by default, but verify:
```bash
rustup component add clippy rustfmt rust-src
```

---

## Development Tools

### Cargo Tools (Recommended)

```bash
# File watcher with TUI - run tests/check on save
cargo install bacon

# Faster test runner with better output
cargo install cargo-nextest

# Security vulnerability scanning
cargo install cargo-audit
```

### VS Code Extensions (Recommended)

If using VS Code, install these extensions:

```bash
# Essential
code --install-extension rust-lang.rust-analyzer

# Highly Recommended
code --install-extension tamasfe.even-better-toml
code --install-extension fill-labs.dependi
code --install-extension vadimcn.vscode-lldb
```

| Extension | Purpose |
|-----------|---------|
| **rust-analyzer** | Rust language support (completion, errors, navigation) |
| **Even Better TOML** | Cargo.toml syntax highlighting and validation |
| **Dependi** | Dependency version management and updates |
| **CodeLLDB** | Debugging support for Rust |

---

## Getting Started

### Clone and Build

```bash
# Clone the repository
git clone <repository-url>
cd QB64Fresh

# Build the project
cargo build

# Run tests
cargo test
# Or with nextest (faster, better output):
cargo nextest run

# Run clippy lints
cargo clippy

# Format code
cargo fmt
```

### Development Workflow

We recommend using **bacon** for continuous feedback while developing:

```bash
# Watch for changes and run checks
bacon

# Watch and run tests
bacon test

# Watch and run clippy
bacon clippy
```

Bacon provides a TUI that shows compilation errors, warnings, and test results as you save files.

---

## Project Structure

```
QB64Fresh/
├── src/                    # Source code
│   ├── lib.rs             # Library root
│   ├── main.rs            # CLI entry point
│   ├── lexer/             # Tokenization
│   ├── parser/            # AST construction
│   ├── ast/               # AST type definitions
│   ├── semantic/          # Type checking, symbol resolution
│   ├── ir/                # Typed intermediate representation
│   ├── codegen/           # Code generation backends
│   │   ├── mod.rs         # CodeGenerator trait
│   │   └── c/             # C backend implementation
│   ├── runtime/           # Runtime library interface
│   └── lsp/               # Language server implementation
├── tests/                  # Integration tests
├── docs/                   # Documentation
│   └── QB64PE_ARCHITECTURE_ANALYSIS.md
├── AgenticLogs/           # Development history and decisions
│   └── IndividualProblems/ # Detailed troubleshooting docs
├── CLAUDE.md              # AI assistant configuration
├── PROJECT_PLAN.md        # Project roadmap and decisions
└── Cargo.toml             # Rust package manifest
```

---

## Coding Standards

### Rust Style

- Run `cargo fmt` before committing
- Run `cargo clippy` and address warnings
- Follow [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)

### Documentation

- All public items must have doc comments
- Use `///` for item docs, `//!` for module docs
- Include examples in doc comments where helpful:

```rust
/// Parses a BASIC expression into an AST node.
///
/// # Example
///
/// ```
/// let expr = parse_expression("1 + 2 * 3")?;
/// ```
///
/// # Errors
///
/// Returns `ParseError` if the input is not a valid expression.
pub fn parse_expression(input: &str) -> Result<Expr, ParseError> {
    // ...
}
```

### Error Handling

- Use `Result<T, E>` for fallible operations
- Create descriptive error types with source locations
- Errors should help users fix their BASIC code

### Naming Conventions

```rust
// Types: PascalCase, domain-specific names
struct BinaryExpr { ... }
enum Statement { ... }
struct SourceLocation { ... }

// Functions: snake_case, verb phrases
fn parse_expression(...) -> Result<Expr, ParseError>
fn emit_c_code(...) -> String
fn resolve_symbol(...) -> Option<Symbol>

// Modules: snake_case, noun phrases
mod lexer;
mod semantic_analysis;
mod c_backend;
```

---

## Testing

### Running Tests

```bash
# Standard cargo test
cargo test

# With nextest (recommended - faster, better output)
cargo nextest run

# Run specific test
cargo nextest run test_name

# Run tests in specific module
cargo nextest run lexer::
```

### Writing Tests

- Place unit tests in the same file as the code, in a `#[cfg(test)]` module
- Place integration tests in `tests/` directory
- Name test functions descriptively: `test_parse_if_statement_with_else`

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_recognizes_keywords() {
        let tokens = lex("IF THEN ELSE");
        assert_eq!(tokens[0].kind, TokenKind::If);
        assert_eq!(tokens[1].kind, TokenKind::Then);
        assert_eq!(tokens[2].kind, TokenKind::Else);
    }
}
```

### Compatibility Testing

We aim for compatibility with QB64-PE. Test cases from `../QB64pe/tests/` can be used for validation.

---

## Architecture Overview

QB64Fresh follows a traditional compiler pipeline:

```
Source (.bas)
    │
    ▼
┌─────────┐
│  Lexer  │  Converts source text to tokens
└────┬────┘
     │
     ▼
┌─────────┐
│ Parser  │  Builds Abstract Syntax Tree (AST)
└────┬────┘
     │
     ▼
┌──────────┐
│ Semantic │  Type checking, symbol resolution
│ Analysis │
└────┬─────┘
     │
     ▼
┌─────────┐
│   IR    │  Typed intermediate representation
└────┬────┘
     │
     ▼
┌─────────┐
│ CodeGen │  Generates target code (C, LLVM, etc.)
└────┬────┘
     │
     ▼
C Source → C Compiler → Native Executable
```

### Key Design Decisions

1. **Trait-based backends**: `CodeGenerator` trait allows multiple backends
2. **Rust runtime**: Using Rust crates (sdl2, rodio) instead of porting C++
3. **LSP-first**: No built-in IDE; provide LSP for editor integration

See `PROJECT_PLAN.md` for full decision rationale.

---

## Debugging

### VS Code Debugging

1. Install the CodeLLDB extension
2. Create `.vscode/launch.json`:

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "lldb",
            "request": "launch",
            "name": "Debug QB64Fresh",
            "cargo": {
                "args": ["build", "--bin=qb64fresh", "--package=qb64fresh"],
                "filter": {
                    "name": "qb64fresh",
                    "kind": "bin"
                }
            },
            "args": ["path/to/test.bas"],
            "cwd": "${workspaceFolder}"
        },
        {
            "type": "lldb",
            "request": "launch",
            "name": "Debug Unit Tests",
            "cargo": {
                "args": ["test", "--no-run", "--lib"],
                "filter": {
                    "kind": "lib"
                }
            },
            "cwd": "${workspaceFolder}"
        }
    ]
}
```

3. Set breakpoints and press F5

### Logging

Use the `log` crate for debug output:

```rust
use log::{debug, info, warn, error};

debug!("Parsing expression: {:?}", tokens);
info!("Compilation complete");
warn!("Deprecated syntax used at line {}", line);
error!("Failed to resolve symbol: {}", name);
```

Enable with `RUST_LOG=debug cargo run`.

---

## Contributing

### Before Submitting

1. Run `cargo fmt`
2. Run `cargo clippy` and fix warnings
3. Run `cargo test` (or `cargo nextest run`)
4. Run `cargo audit` for security check
5. Update documentation if needed

### Commit Messages

Follow conventional commits:
```
type(scope): brief description

Longer explanation if needed.
```

Types: `feat`, `fix`, `docs`, `refactor`, `test`, `chore`

Examples:
```
feat(lexer): add support for hexadecimal literals
fix(parser): handle empty ELSE blocks correctly
docs(readme): update build instructions
```

---

## Resources

### QB64/QBasic References
- [QB64-PE Documentation](https://qb64phoenix.com/qb64wiki/)
- [QBasic/QuickBASIC Reference](https://www.qbasic.net/)
- Original QB64-PE source: `../QB64pe/`

### Rust Resources
- [The Rust Book](https://doc.rust-lang.org/book/)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)

### Compiler Resources
- [Crafting Interpreters](https://craftinginterpreters.com/)
- [Writing An Interpreter In Go](https://interpreterbook.com/)

---

## Getting Help

- Check `AgenticLogs/` for past decisions and problem solutions
- Check `AgenticLogs/IndividualProblems/` for detailed troubleshooting
- Open an issue for bugs or questions

---

*Last updated: 2026-01-16*
