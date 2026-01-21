# QB64Fresh Architecture

This document describes the high-level architecture of QB64Fresh, a BASIC compiler written in Rust.

## Compiler Pipeline

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              QB64Fresh Pipeline                              │
└─────────────────────────────────────────────────────────────────────────────┘

  Source Code (.bas)
        │
        ▼
┌───────────────┐
│ Preprocessor  │  Handles $INCLUDE, $IF, $CHECKING directives
│(preprocessor) │  Expands includes, evaluates conditional compilation
└───────┬───────┘
        │ Preprocessed source
        ▼
┌───────────────┐
│    Lexer      │  Tokenizes source into a stream of tokens
│  (src/lexer)  │  Uses `logos` crate for fast lexical analysis (~1,846 lines)
└───────┬───────┘
        │ Vec<Token>
        ▼
┌───────────────┐
│    Parser     │  Builds Abstract Syntax Tree from tokens
│ (src/parser)  │  Pratt parsing for expressions, recursive descent for statements
│               │  12 specialized modules (~11,350 lines)
└───────┬───────┘
        │ Program (AST)
        ▼
┌───────────────┐
│   Semantic    │  Type checking, symbol resolution, validation
│ (src/semantic)│  Two-pass analysis, constant evaluation (~14,440 lines)
└───────┬───────┘
        │ TypedProgram (IR)
        ▼
┌───────────────┐
│   CodeGen     │  Generates target code via backend trait
│ (src/codegen) │  C backend with inline runtime (~11,728 lines)
└───────┬───────┘
        │ Generated C code
        ▼
┌───────────────┐
│  C Compiler   │  External: gcc/clang compiles to executable
│  (external)   │  Links against qb64fresh_runtime library
└───────┬───────┘
        │
        ▼
┌───────────────┐
│   Runtime     │  Graphics (SDL2), Audio (Rodio), I/O, Strings
│   (runtime/)  │  Rust library with C FFI (~11,678 lines)
└───────┬───────┘
        │
        ▼
    Executable
```

## Module Organization

```
src/
├── lib.rs                # Library root, public API exports
├── main.rs               # CLI entry point (--tokens, --ast, --typed-ir, --emit-c)
├── preprocessor.rs       # $INCLUDE directive processing
│
├── lexer/                # Phase 1: Lexical Analysis (~1,846 lines)
│   ├── mod.rs            # Lexer struct, iterator interface
│   └── token.rs          # TokenKind enum (logos-generated)
│
├── ast/                  # AST Type Definitions (~2,646 lines)
│   ├── mod.rs            # Span, Program types
│   ├── expr.rs           # Expression AST nodes
│   └── stmt.rs           # Statement AST nodes (comprehensive coverage)
│
├── parser/               # Phase 2: Syntax Analysis (~11,350 lines) ✓
│   ├── mod.rs            # Parser entry point, recursive descent
│   ├── tokens.rs         # Token navigation utilities (peek, advance, match)
│   ├── expressions.rs    # Pratt parser with full operator precedence
│   ├── statements.rs     # Core statement parsing (largest file)
│   ├── control_flow.rs   # IF/FOR/WHILE/DO/SELECT parsing
│   ├── procedures.rs     # SUB/FUNCTION/TYPE definitions
│   ├── directives.rs     # Preprocessor directives ($IF, $CHECKING)
│   ├── audio.rs          # Audio: BEEP, SOUND, PLAY, _SNDxxx
│   ├── graphics.rs       # Graphics: SCREEN, CLS, PSET, LINE, CIRCLE, etc.
│   ├── file_io.rs        # File I/O: OPEN, CLOSE, GET, PUT, SEEK
│   ├── system.rs         # System: KILL, NAME, MKDIR, SHELL, mouse, clipboard
│   └── error.rs          # ParseError types with spans
│
├── semantic/             # Phase 3: Semantic Analysis (~14,440 lines) ✓
│   ├── mod.rs            # Analysis entry point, built-in registration
│   ├── symbols.rs        # Symbol table with scope management
│   ├── types.rs          # BasicType enum, type inference
│   ├── typed_ir.rs       # TypedProgram, TypedExpr, TypedStatement
│   ├── error.rs          # Semantic error types with spans
│   └── checker/          # Type Checker Submodule
│       ├── mod.rs        # Checker entry point
│       ├── expressions.rs # Expression type checking
│       ├── statements.rs  # Statement type checking
│       ├── control_flow.rs # Control flow validation
│       ├── assignments.rs  # Assignment validation
│       ├── definitions.rs  # Definition handling
│       └── const_eval.rs   # Constant expression evaluation
│
├── codegen/              # Phase 4: Code Generation (~11,728 lines) ✓
│   ├── mod.rs            # CodeGenerator trait, GeneratedOutput
│   ├── error.rs          # CodeGenError types
│   └── c_backend/        # C Code Generator Submodule
│       ├── mod.rs        # Backend entry point
│       ├── expr.rs       # Expression code generation
│       ├── stmt.rs       # Statement code generation
│       ├── types.rs      # Type mapping (BASIC → C)
│       ├── runtime.rs    # Inline C runtime library (~4,141 lines)
│       ├── analysis.rs   # DATA/label collection pre-pass
│       ├── const_fold.rs # Constant folding optimization
│       └── file_io.rs    # File I/O code generation
│
└── lsp/                  # Language Server Protocol ✓
    ├── mod.rs            # LSP server implementation (tower-lsp)
    └── main.rs           # qb64fresh-lsp binary entry

runtime/                  # Runtime Library (workspace member) (~11,678 lines) ✓
├── src/
│   ├── lib.rs            # Crate root, init/shutdown
│   ├── string.rs         # Reference-counted dynamic strings (~1,572 lines)
│   ├── io.rs             # PRINT, INPUT, console operations (~1,379 lines)
│   ├── math.rs           # Mathematical functions (~641 lines)
│   ├── graphics_ffi.rs   # C FFI layer for graphics (~1,597 lines)
│   ├── audio_ffi.rs      # C FFI layer for audio (~439 lines)
│   ├── graphics/         # Graphics Backend System (~3,998 lines)
│   │   ├── mod.rs        # GraphicsBackend trait definition
│   │   ├── sdl2.rs       # SDL2 implementation (primary)
│   │   ├── font.rs       # Font rendering
│   │   ├── mock.rs       # Mock backend for testing
│   │   └── error.rs      # Graphics error types
│   └── audio/            # Audio Backend System (~1,304 lines)
│       ├── mod.rs        # AudioBackend trait definition
│       ├── rodio_backend.rs  # Rodio implementation (primary)
│       ├── mock.rs       # Mock backend for testing
│       └── error.rs      # Audio error types
└── include/
    └── qb64fresh_rt.h    # C header for FFI
```

## Key Components

### Lexer (`src/lexer/`)

The lexer converts source text into tokens using the `logos` crate.

**Design choices:**
- Case-insensitive keywords via `ignore(ascii_case)`
- Preserves original text in each token (for error messages, identifiers)
- Tracks byte spans for source mapping
- Skips horizontal whitespace, but newlines are significant tokens (BASIC is line-oriented)

**Key types:**
```rust
pub struct Token {
    pub kind: TokenKind,      // What type of token
    pub span: Range<usize>,   // Where in source
    pub text: String,         // Original text
}

pub enum TokenKind {
    // Keywords: If, Then, Print, For, ...
    // Operators: Plus, Minus, Equals, ...
    // Literals: IntegerLiteral, StringLiteral, ...
    // Punctuation: LeftParen, Comma, ...
}
```

### Parser (`src/parser/`)

The parser builds an AST from tokens using two techniques:

1. **Pratt Parsing** for expressions - handles operator precedence elegantly
2. **Recursive Descent** for statements - straightforward and readable

**BASIC Operator Precedence (highest to lowest):**
```
^           Exponentiation (right-associative)
- NOT       Unary negation, logical NOT
* / \ MOD   Multiplicative
+ -         Additive
= <> < > <= >=  Comparison
AND         Logical AND
OR XOR      Logical OR
EQV IMP     Equivalence, Implication
```

**Error Recovery:**
- Collects multiple errors instead of stopping at first
- Synchronizes at statement boundaries (newlines, keywords)
- Returns `Result<T, ()>` internally; actual errors in `self.errors`

### AST (`src/ast/`)

The AST represents the syntactic structure of the program.

**Design choices:**
- **Owned nodes** - No lifetimes; AST can outlive source text
- **Spans everywhere** - Every node knows its source location
- **Separate Expr/Statement** - Reflects BASIC's distinction

**Expression kinds:**
```rust
pub enum ExprKind {
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Identifier(String),
    Binary { left, op, right },
    Unary { op, operand },
    FunctionCall { name, args },
    Grouped(Box<Expr>),
}
```

**Statement kinds:**
```rust
pub enum StatementKind {
    Print { values, newline },
    Let { name, value },
    Dim { name, dimensions, type_spec, shared },
    If { condition, then_branch, elseif_branches, else_branch },
    For { variable, start, end, step, body },
    While { condition, body },
    DoLoop { pre_condition, body, post_condition },
    SelectCase { test_expr, cases, case_else },
    SubDefinition { name, params, body, is_static },
    FunctionDefinition { name, params, return_type, body, is_static },
    // ... and more
}
```

### Semantic Analysis (`src/semantic/`)

Handles:
- **Symbol Resolution** - Build symbol tables, resolve references, SHARED variables
- **Type Checking** - Verify type compatibility, infer types from suffixes
- **Validation** - Check for undefined labels, duplicate definitions, EXIT context
- **Two-Pass Analysis** - Pass 1 collects declarations, Pass 2 type checks
- **Built-in Functions** - 30+ standard functions registered (LEN, CHR$, SIN, etc.)

**Key types:**
```rust
pub struct SemanticAnalyzer { ... }
pub struct SymbolTable { ... }
pub struct TypeChecker { ... }
pub enum BasicType { Integer, Long, Single, Double, String, ... }
pub struct TypedProgram { statements: Vec<TypedStatement> }
```

### Code Generation (`src/codegen/`)

Uses a trait-based design for backend flexibility:

```rust
pub trait CodeGenerator {
    fn generate(&self, program: &TypedProgram) -> Result<GeneratedOutput, CodeGenError>;
}
```

**C Backend (implemented, ~11,728 lines across 8 files):**
- Proven approach (QB64pe uses C++)
- Refactored into specialized submodules:
  - `expr.rs` - Expression generation with type coercion (~1,079 lines)
  - `stmt.rs` - Statement generation (control flow, I/O, procedures) (~3,690 lines)
  - `types.rs` - BASIC to C type mapping
  - `runtime.rs` - Inline C runtime (~4,141 lines)
  - `analysis.rs` - Pre-pass for DATA statements and labels
  - `const_fold.rs` - Constant folding optimization (~855 lines)
  - `file_io.rs` - File I/O code generation (~569 lines)
- Two runtime modes: `inline` (self-contained) and `external` (library-linked)
- Handles all statements: assignments, control flow, procedures
- Type conversions, string operations, built-in functions
- Portable across platforms
- Delegates optimization to C compiler

**Future backends (possible):**
- LLVM via `inkwell`
- Cranelift for JIT
- Direct x86/ARM

### Runtime Library (`runtime/`)

The runtime provides standard library functions callable from generated C code.

**Core Modules:**
- `string.rs` - Reference-counted dynamic strings with copy-on-write
- `io.rs` - PRINT, INPUT, console operations with formatting
- `math.rs` - Mathematical functions (SIN, COS, RND, etc.)

**Graphics Backend System (`runtime/src/graphics/`):**

Uses a trait-based abstraction for graphics operations:

```rust
pub trait GraphicsBackend {
    fn init(&mut self, width: u32, height: u32) -> Result<(), GraphicsError>;
    fn set_pixel(&mut self, x: i32, y: i32, color: u32);
    fn draw_line(&mut self, x1: i32, y1: i32, x2: i32, y2: i32, color: u32);
    fn present(&mut self);
    // ... more operations
}
```

- **SDL2 Backend** (`sdl2.rs`) - Primary implementation using SDL2 library
- **Mock Backend** (`mock.rs`) - For testing without graphics hardware
- **Font Rendering** (`font.rs`) - Text drawing support
- **C FFI** (`graphics_ffi.rs`) - Exposes backend to generated C code

**Audio Backend System (`runtime/src/audio/`):**

Uses a trait-based abstraction for audio operations:

```rust
pub trait AudioBackend {
    fn init(&mut self) -> Result<(), AudioError>;
    fn beep(&mut self, frequency: f32, duration: f32);
    fn play_sound(&mut self, handle: u32);
    fn load_sound(&mut self, path: &str) -> Result<u32, AudioError>;
    // ... more operations
}
```

- **Rodio Backend** (`rodio_backend.rs`) - Primary implementation using Rodio crate
- **Mock Backend** (`mock.rs`) - For testing without audio hardware
- **C FFI** (`audio_ffi.rs`) - Exposes backend to generated C code

## Data Flow

```
Source: "PRINT 1 + 2"
            │
            ▼
Tokens: [PRINT] [1] [+] [2] [Newline]
            │
            ▼
AST:    Statement::Print {
            values: [
                PrintItem {
                    expr: Expr::Binary {
                        left: Expr::IntegerLiteral(1),
                        op: Add,
                        right: Expr::IntegerLiteral(2)
                    }
                }
            ],
            newline: true
        }
            │
            ▼ (after semantic analysis)
IR:     TypedStatement::Print {
            values: [TypedExpr { kind: Add, type: Integer, ... }],
            ...
        }
            │
            ▼ (after code generation)
C:      printf("%d\n", (1 + 2));
```

## Extension Points

### Adding New Syntax

1. Add token(s) to `src/lexer/token.rs` (use logos derive macros)
2. Add AST node(s) to `src/ast/expr.rs` or `src/ast/stmt.rs`
3. Add parser logic to appropriate module:
   - `parser/expressions.rs` - New operators or expression forms
   - `parser/statements.rs` - New statement types
   - `parser/control_flow.rs` - New control structures
   - `parser/graphics.rs` - Graphics commands
   - `parser/audio.rs` - Audio commands
   - `parser/file_io.rs` - File operations
   - `parser/system.rs` - System/OS operations
4. Add typed IR node to `src/semantic/typed_ir.rs`
5. Add type checking to `src/semantic/checker/`
6. Add code generation to `src/codegen/c_backend/stmt.rs` or `expr.rs`

### Adding a New Code Generation Backend

1. Create new module under `src/codegen/`
2. Implement the `CodeGenerator` trait
3. Handle all `TypedStatement` and `TypedExpr` variants
4. Register in CLI options in `src/main.rs`

### Adding a New Graphics Backend

1. Create new module under `runtime/src/graphics/`
2. Implement the `GraphicsBackend` trait
3. Add feature flag to `runtime/Cargo.toml`
4. Wire up in `runtime/src/graphics/mod.rs`

### Adding a New Audio Backend

1. Create new module under `runtime/src/audio/`
2. Implement the `AudioBackend` trait
3. Add feature flag to `runtime/Cargo.toml`
4. Wire up in `runtime/src/audio/mod.rs`

## Testing Strategy

- **Unit tests** - Each module has `#[cfg(test)]` tests
- **Integration tests** - Parse and compile real BASIC programs
- **Golden tests** - Compare output against known-good files
- **Fixture tests** - Success cases with expected output, error cases with expected errors
- **Property-based tests** - Using `proptest` for fuzzing inputs
- **Compatibility tests** - QBasic 4.5 compatibility suite

**Test Directory Structure:**
```
tests/
├── common/mod.rs          # Shared test utilities
├── compatibility.rs       # Compatibility test suite
├── golden_tests.rs        # Golden file testing
├── integration_tests.rs   # Integration tests
├── proptest_tests.rs      # Property-based tests
├── qb45_compat.rs         # QBasic 4.5 compatibility
├── fixtures/
│   ├── success/           # Valid programs with expected output
│   │   ├── *.bas          # BASIC source files
│   │   └── *.output       # Expected stdout
│   └── error/             # Error cases with expected messages
│       ├── *.bas          # Invalid programs
│       └── *.err          # Expected error output
└── golden/
    ├── *.bas              # Source files
    └── *.golden           # Expected compiler output
```

Run tests:
```bash
cargo test                # All tests
cargo test parser         # Parser tests only
cargo test --doc          # Doc tests only
cargo test golden         # Golden file tests
cargo test --features graphics-sdl2  # With graphics
```

## Dependencies

### Compiler Crate

| Crate | Purpose |
|-------|---------|
| `logos` 0.14 | Lexer generation with derive macros |
| `ariadne` 0.4 | Beautiful error diagnostics with spans |
| `clap` 4 | CLI argument parsing |
| `thiserror` 1.0 | Error type derivation |
| `log` + `env_logger` | Logging infrastructure |
| `tower-lsp` 0.20 | Language Server Protocol support |
| `tokio` 1 | Async runtime for LSP |
| `serde` + `serde_json` | JSON serialization |

### Runtime Crate

| Crate | Purpose |
|-------|---------|
| `libc` 0.2 | C library bindings for FFI |
| `sdl2` 0.37 | Graphics backend (optional, feature-gated) |
| `image` 0.25 | Image loading for sprites (optional) |
| `rodio` 0.19 | Audio backend (optional, feature-gated) |

### Feature Flags

```toml
[features]
default = ["graphics-sdl2", "audio-rodio"]
graphics-sdl2 = ["sdl2", "image"]
audio-rodio = ["rodio"]
```

### Dev Dependencies

| Crate | Purpose |
|-------|---------|
| `pretty_assertions` 1.4 | Enhanced test diff output |
| `tempfile` 3 | Temporary files for tests |
| `proptest` 1.5 | Property-based testing |
| `criterion` 0.5 | Benchmarking |

## Build Configuration

- **Workspace** - Two crates: `qb64fresh` (compiler) and `qb64fresh_runtime`
- **Pre-commit hooks** - `cargo-husky` runs fmt/clippy
- **CI/CD** - GitHub Actions in `.github/workflows/`
- **Benchmarks** - Criterion benchmarks in `benches/`
- **Fuzzing** - Infrastructure in `fuzz/`

## Related Documents

- [CLAUDE.md](CLAUDE.md) - AI assistant configuration
- [DEVELOPMENT.md](DEVELOPMENT.md) - Developer onboarding
- [GRAPHICS_ARCHITECTURE.md](GRAPHICS_ARCHITECTURE.md) - Graphics system design
- [docs/PARSER_PLAN.md](docs/PARSER_PLAN.md) - Parser implementation details
- [docs/QB64_SYNTAX_REFERENCE.md](docs/QB64_SYNTAX_REFERENCE.md) - Language syntax reference
- [docs/QB64_LANGUAGE_SPECIFICATION.md](docs/QB64_LANGUAGE_SPECIFICATION.md) - Complete language reference

### Architecture Decision Records (ADRs)

| ADR | Decision |
|-----|----------|
| ADR-0001 | Implementation language: Rust |
| ADR-0002 | Code generation backend: C with trait abstraction |
| ADR-0003 | Runtime library: Hybrid Rust + established crates |
| ADR-0004 | Build system tooling |
| ADR-0005 | Testing framework |
| ADR-0006 | Graphics system: Trait-based with SDL2 |
| ADR-0007 | Audio system: Trait-based with Rodio |
| ADR-0008 | C interoperability |

## Code Statistics Summary

| Component | Lines | Status |
|-----------|-------|--------|
| Lexer | ~1,846 | ✓ Complete |
| AST | ~2,646 | ✓ Complete |
| Parser | ~11,350 | ✓ Complete |
| Semantic Analysis | ~14,440 | ✓ Complete |
| Code Generation | ~11,728 | ✓ Complete |
| LSP | ~2,105 | ✓ Complete |
| Runtime Library | ~11,678 | ✓ Complete |
| **Compiler (src/)** | **~45,753** | |
| **Runtime** | **~11,678** | |
| **Total** | **~57,431** | |
