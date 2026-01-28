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
│  (src/lexer)  │  Uses `logos` crate for fast lexical analysis (~1,850 lines)
└───────┬───────┘
        │ Vec<Token>
        ▼
┌───────────────┐
│    Parser     │  Builds Abstract Syntax Tree from tokens
│ (src/parser)  │  Pratt parsing for expressions, recursive descent for statements
│               │  12 specialized modules (~11,450 lines)
└───────┬───────┘
        │ Program (AST)
        ▼
┌───────────────┐
│   Semantic    │  Type checking, symbol resolution, validation
│ (src/semantic)│  Two-pass analysis, constant evaluation (~15,800 lines)
└───────┬───────┘
        │ TypedProgram (IR)
        ▼
┌───────────────┐
│   CodeGen     │  Generates target code via backend trait
│ (src/codegen) │  C backend with inline runtime (~14,750 lines)
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
│   (runtime/)  │  Rust library with C FFI (~11,700 lines)
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
├── lexer/                # Phase 1: Lexical Analysis (~1,847 lines)
│   ├── mod.rs            # Lexer struct, iterator interface
│   └── token.rs          # TokenKind enum (logos-generated)
│
├── ast/                  # AST Type Definitions (~2,651 lines)
│   ├── mod.rs            # Span, Program types
│   ├── expr.rs           # Expression AST nodes
│   └── stmt.rs           # Statement AST nodes (comprehensive coverage)
│
├── parser/               # Phase 2: Syntax Analysis (~11,861 lines) ✓
│   ├── mod.rs            # Parser entry point, recursive descent
│   ├── tokens.rs         # Token navigation utilities (peek, advance, match)
│   ├── expressions.rs    # Pratt parser with full operator precedence
│   ├── statements/       # Statement parsing submodules
│   │   ├── mod.rs        # Statement dispatcher
│   │   ├── assignments.rs # Variable assignments, MID$, ASC statements
│   │   ├── print_input.rs # PRINT, INPUT, LINE INPUT statements
│   │   ├── declare.rs    # DECLARE SUB/FUNCTION/LIBRARY statements
│   │   ├── data_dims.rs  # DIM, REDIM, DATA, CONST statements
│   │   └── control_etc.rs # Control flow helpers and miscellaneous statements
│   ├── control_flow.rs   # IF/FOR/WHILE/DO/SELECT parsing
│   ├── procedures.rs     # SUB/FUNCTION/TYPE definitions
│   ├── directives.rs     # Preprocessor directives ($IF, $CHECKING)
│   ├── audio.rs          # Audio: BEEP, SOUND, PLAY, _SNDxxx
│   ├── graphics.rs       # Graphics: SCREEN, CLS, PSET, LINE, CIRCLE, etc.
│   ├── file_io.rs        # File I/O: OPEN, CLOSE, GET, PUT, SEEK
│   ├── system.rs         # System: KILL, NAME, MKDIR, SHELL, mouse, clipboard
│   ├── tests.rs          # Parser test utilities
│   └── error.rs          # ParseError types with spans
│
├── semantic/             # Phase 3: Semantic Analysis (~15,153 lines) ✓
│   ├── mod.rs            # Analysis entry point, built-in registration
│   ├── builtins.rs       # Built-in function and constant registration
│   ├── collect.rs        # Declaration collection (Pass 1)
│   ├── symbols.rs        # Symbol table with scope management
│   ├── types.rs          # BasicType enum, type inference
│   ├── typed_ir.rs       # TypedProgram, TypedExpr, TypedStatement
│   ├── error.rs          # Semantic error types with spans
│   └── checker/          # Type Checker Submodule
│       ├── mod.rs        # Checker entry point
│       ├── expressions.rs # Expression type checking
│       ├── statements.rs  # Statement type checking dispatcher
│       ├── statements/    # Statement type checking submodules
│       │   ├── audio.rs   # Audio statement type checking
│       │   ├── graphics.rs # Graphics statement type checking
│       │   ├── data.rs    # DATA/READ/RESTORE type checking
│       │   ├── error_flow.rs # Error handling and computed control flow
│       │   └── io.rs      # File I/O statement type checking
│       ├── control_flow.rs # Control flow validation
│       ├── assignments.rs  # Assignment validation
│       ├── definitions.rs  # Definition handling
│       └── const_eval.rs   # Constant expression evaluation
│
├── codegen/              # Phase 4: Code Generation (~19,800 lines) ✓
│   ├── mod.rs            # CodeGenerator trait, GeneratedOutput
│   ├── error.rs          # CodeGenError types
│   └── c_backend/        # C Code Generator Submodule (~19,800 lines)
│       ├── mod.rs        # Backend entry point (~631 lines)
│       ├── expr.rs       # Expression code generation
│       ├── types.rs      # Type mapping (BASIC → C)
│       ├── analysis.rs   # DATA/label collection pre-pass
│       ├── const_fold.rs # Constant folding optimization
│       ├── file_io.rs    # File I/O code generation
│       ├── implicit_vars.rs # Implicit variable handling
│       ├── stmt/         # Statement code generation (~5,195 lines)
│       │   ├── mod.rs    # Core StmtEmitter and dispatcher
│       │   ├── assignments.rs # Assignment statements
│       │   ├── control_flow.rs # IF, FOR, WHILE, DO, SELECT CASE
│       │   ├── data.rs   # DATA/READ/RESTORE handling
│       │   ├── def_fn.rs # DEF FN single-line and multi-line functions
│       │   ├── definitions.rs # DIM, REDIM, SUB/FUNCTION, DECLARE LIBRARY
│       │   ├── error_jump.rs # Error handling (ON ERROR) and computed jumps
│       │   └── io.rs     # PRINT and INPUT helpers
│       └── runtime/      # Inline C runtime library (~8,748 lines)
│           ├── mod.rs   # Runtime module root
│           ├── types.rs  # String type definition and type size helpers
│           ├── strings.rs # Core string operations (LEFT$, MID$, etc.)
│           ├── io.rs    # PRINT and INPUT functions
│           ├── math.rs  # Mathematical functions and type conversions
│           ├── file.rs  # File I/O operations (OPEN, CLOSE, GET, PUT)
│           ├── keyboard.rs # Keyboard input functions (INKEY$, _KEYHIT, etc.)
│           ├── memory.rs # Memory functions (PEEK, POKE, VARPTR)
│           ├── timing.rs # Timer and timing functions (TIMER, SLEEP, DATE$, TIME$)
│           ├── arrays.rs # Array operations (LBOUND, UBOUND, REDIM)
│           ├── audio.rs # Audio functions (_SNDPLAY, BEEP, SOUND)
│           ├── graphics.rs # Graphics stubs (SCREEN, LINE, CIRCLE, etc.)
│           ├── legacy.rs # Legacy DOS functions (DEF SEG, OUT, INP)
│           ├── system.rs # System stubs (filesystem, shell, console)
│           ├── error.rs # Error handling functions
│           └── debug.rs  # Debug runtime support (breakpoints, stepping, IPC)
│
├── header_parser/        # C Header Parser (optional feature)
│   ├── mod.rs            # Module root
│   ├── lexer.rs          # C token lexer
│   └── parser.rs         # C declaration parser
│
└── lsp/                  # Language Server Protocol ✓
    ├── mod.rs            # LSP server implementation (tower-lsp)
    ├── position.rs       # Position/offset conversion utilities
    ├── signatures.rs    # Built-in function signature helpers
    ├── tests.rs         # LSP test utilities
    └── main.rs          # qb64fresh-lsp binary entry

tools/                    # Development Tools
├── fix_encoding.rs       # DOS encoding converter
├── fmt/                  # Code formatter
│   └── src/              # Formatter implementation
└── lint/                 # Code linter
    └── src/              # Linter implementation

runtime/                  # Runtime Library (workspace member) (~9,026 lines) ✓
├── src/
│   ├── lib.rs            # Crate root, init/shutdown
│   ├── string.rs         # Reference-counted dynamic strings
│   ├── io.rs             # PRINT, INPUT, console operations
│   ├── math.rs           # Mathematical functions
│   ├── graphics_ffi.rs   # C FFI layer for graphics
│   ├── font_ffi.rs       # Font rendering FFI layer
│   ├── font_manager.rs   # Font management and caching
│   ├── audio_ffi.rs      # C FFI layer for audio
│   ├── dialogs.rs        # Native file dialogs
│   ├── joystick.rs       # Joystick input support
│   ├── graphics/         # Graphics Backend System
│   │   ├── mod.rs        # GraphicsBackend trait definition
│   │   ├── sdl2.rs       # SDL2 implementation (primary)
│   │   ├── font.rs       # Font rendering
│   │   ├── mock.rs       # Mock backend for testing
│   │   └── error.rs      # Graphics error types
│   └── audio/            # Audio Backend System
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

**Module Organization:**
- Core parsing logic in `mod.rs` and `tokens.rs`
- Expression parsing in `expressions.rs` (Pratt parser)
- Statement parsing organized into `statements/` subdirectory:
  - `assignments.rs` - Variable assignments, MID$, ASC
  - `print_input.rs` - PRINT, INPUT, LINE INPUT
  - `declare.rs` - DECLARE statements
  - `data_dims.rs` - DIM, REDIM, DATA, CONST
  - `control_etc.rs` - Miscellaneous control flow helpers
- Specialized modules for domain-specific statements:
  - `control_flow.rs` - IF/FOR/WHILE/DO/SELECT
  - `procedures.rs` - SUB/FUNCTION/TYPE definitions
  - `graphics.rs` - Graphics statements
  - `audio.rs` - Audio statements
  - `file_io.rs` - File I/O statements
  - `system.rs` - System/OS statements

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
- **Two-Pass Analysis** - Pass 1 collects declarations (`collect.rs`), Pass 2 type checks
- **Built-in Functions** - 30+ standard functions registered in `builtins.rs` (LEN, CHR$, SIN, etc.)

**Module Organization:**
- `mod.rs` - Main analyzer entry point
- `builtins.rs` - Built-in function and constant registration
- `collect.rs` - Declaration collection (Pass 1: SUB/FUNCTION/labels)
- `checker/` - Type checking (Pass 2):
  - `statements.rs` - Statement dispatcher
  - `statements/` - Specialized statement type checking:
    - `audio.rs` - Audio statements
    - `graphics.rs` - Graphics statements
    - `data.rs` - DATA/READ/RESTORE
    - `error_flow.rs` - Error handling and computed control flow
    - `io.rs` - File I/O statements

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

**C Backend (implemented, ~19,800 lines across multiple modules):**
- Proven approach (QB64pe uses C++)
- Refactored into specialized submodules for maintainability:
  - **Core modules:**
    - `mod.rs` - Backend entry point and coordination (~631 lines)
    - `expr.rs` - Expression generation with type coercion
    - `types.rs` - BASIC to C type mapping
    - `analysis.rs` - Pre-pass for DATA statements and labels
    - `const_fold.rs` - Constant folding optimization
    - `file_io.rs` - File I/O code generation
    - `implicit_vars.rs` - Implicit variable handling
  - **Statement generation (`stmt/` subdirectory, ~5,195 lines):**
    - `mod.rs` - Core `StmtEmitter` struct and main dispatcher
    - `assignments.rs` - Assignment statement helpers
    - `control_flow.rs` - IF, FOR, WHILE, DO, SELECT CASE
    - `data.rs` - DATA/READ/RESTORE handling
    - `def_fn.rs` - DEF FN single-line and multi-line functions
    - `definitions.rs` - DIM, REDIM, SUB/FUNCTION definitions, DECLARE LIBRARY
    - `error_jump.rs` - Error handling (ON ERROR) and computed jumps
    - `io.rs` - PRINT and INPUT helpers
  - **Runtime library (`runtime/` subdirectory, ~8,748 lines):**
    - `mod.rs` - Runtime module root
    - `types.rs` - String type definition and type size helpers
    - `strings.rs` - Core string operations (LEFT$, MID$, etc.)
    - `io.rs` - PRINT and INPUT functions
    - `math.rs` - Mathematical functions and type conversions
    - `file.rs` - File I/O operations (OPEN, CLOSE, GET, PUT)
    - `keyboard.rs` - Keyboard input functions (INKEY$, _KEYHIT, etc.)
    - `memory.rs` - Memory functions (PEEK, POKE, VARPTR)
    - `timing.rs` - Timer and timing functions (TIMER, SLEEP, DATE$, TIME$)
    - `arrays.rs` - Array operations (LBOUND, UBOUND, REDIM)
    - `audio.rs` - Audio functions (_SNDPLAY, BEEP, SOUND)
    - `graphics.rs` - Graphics stubs (SCREEN, LINE, CIRCLE, etc.)
    - `legacy.rs` - Legacy DOS functions (DEF SEG, OUT, INP)
    - `system.rs` - System stubs (filesystem, shell, console)
    - `error.rs` - Error handling functions
    - `debug.rs` - Debug runtime support (breakpoints, stepping, IPC)
- Two runtime modes: `inline` (self-contained) and `external` (library-linked)
- Debug mode support: breakpoints, stepping, debugger integration
- Handles all statements: assignments, control flow, procedures
- Type conversions, string operations, built-in functions
- Portable across platforms
- Delegates optimization to C compiler

**Statement Emitter Architecture (`stmt/` subdirectory):**

The `StmtEmitter` struct accumulates state during C code generation for statements. The module has been split into focused submodules for maintainability:

- **`mod.rs`** - Core `StmtEmitter` struct and main `emit_stmt()` dispatcher
- **`assignments.rs`** - Assignment statements (LET, MID$, array assignments)
- **`control_flow.rs`** - Control flow (IF, FOR, WHILE, DO, SELECT CASE)
- **`data.rs`** - DATA/READ/RESTORE handling
- **`def_fn.rs`** - DEF FN single-line and multi-line functions
- **`definitions.rs`** - DIM, REDIM, SUB/FUNCTION definitions, DECLARE LIBRARY
- **`error_jump.rs`** - Error handling (ON ERROR) and computed jumps (ON...GOTO/GOSUB)
- **`io.rs`** - PRINT and INPUT helpers

**Current State:**
- ✅ Module split complete - Logic is well-organized across modules
- ⚠️ Struct refactoring pending - `StmtEmitter` still has 20+ fields

**Recommended Refactoring:**
Split `StmtEmitter` into focused context structs:
- `LabelContext` - Label generation and tracking
- `FormattingContext` - Indentation and formatting
- `ControlFlowContext` - Loop stack for EXIT statements
- `DataContext` - DATA label indices
- `ProcedureContext` - Current procedure, function return variables, parameter tracking
- `GlobalContext` - Global symbol tracking (variables, arrays, constants)
- `EventContext` - STRIG event handling
- `DebugContext` - Debug mode settings
- `ConfigContext` - Runtime mode and configuration flags

This would improve:
- Explicit dependencies (each function takes only needed contexts)
- Better testability (can test individual contexts in isolation)
- Clearer intent (function signatures show what state is accessed)
- Easier maintenance (changes to one context don't affect others)

**Write Helpers:**
The `write_helpers.rs` module provides error-handling wrappers for `write!`/`writeln!` macros, ensuring consistent error handling throughout codegen. See [docs/reference/CODEGEN_WRITE_HELPERS.md](../reference/CODEGEN_WRITE_HELPERS.md) for details.

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

## Binary Architecture

### Dual Binary Architecture

The project builds two binaries:
- **`qb64fresh`** - Compiler CLI (lexer → parser → semantic → codegen)
- **`qb64fresh-lsp`** - Language server for IDE integration (stdio JSON-RPC)

### Runtime Modes

Code generation supports two modes via `--runtime` flag:
- **`inline`** (default) - Self-contained C with embedded runtime functions
- **`external`** - Links against `libqb64fresh_rt` static library

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

This section describes how to extend QB64Fresh with new features. For development workflow and coding standards, see [DEVELOPMENT.md](DEVELOPMENT.md).

### Adding New Syntax

1. Add token(s) to `src/lexer/token.rs` (use logos derive macros)
2. Add AST node(s) to `src/ast/expr.rs` or `src/ast/stmt.rs`
3. Add parser logic to appropriate module:
   - `parser/expressions.rs` - New operators or expression forms
   - `parser/statements/` - New statement types:
     - `assignments.rs` - Assignment variants
     - `print_input.rs` - I/O statements
     - `declare.rs` - DECLARE statements
     - `data_dims.rs` - DIM/REDIM/DATA/CONST
     - `control_etc.rs` - Miscellaneous statements
   - `parser/control_flow.rs` - New control structures
   - `parser/graphics.rs` - Graphics commands
   - `parser/audio.rs` - Audio commands
   - `parser/file_io.rs` - File operations
   - `parser/system.rs` - System/OS operations
4. Add typed IR node to `src/semantic/typed_ir.rs`
5. Add type checking to `src/semantic/checker/`:
   - `statements.rs` - For statement dispatcher updates
   - `statements/` - For specialized statement type checking:
     - `audio.rs` - Audio statements
     - `graphics.rs` - Graphics statements
     - `data.rs` - DATA/READ/RESTORE
     - `error_flow.rs` - Error handling
     - `io.rs` - File I/O
   - `expressions.rs` - For expression type checking
   - `assignments.rs` - For assignment validation
   - `definitions.rs` - For definition handling
6. Add code generation to appropriate module:
   - `src/codegen/c_backend/expr.rs` - For new expression forms
   - `src/codegen/c_backend/stmt/` - For new statements:
     - `assignments.rs` - Assignment variants
     - `control_flow.rs` - Control structures
     - `io.rs` - I/O statements
     - `definitions.rs` - Declaration statements
     - `data.rs` - DATA/READ/RESTORE
     - `error_jump.rs` - Error handling
     - `def_fn.rs` - DEF FN functions
     - `mod.rs` - Core statement dispatcher
7. Update runtime library if new runtime functions are needed:
   - `src/codegen/c_backend/runtime/` - For inline runtime functions
   - `runtime/src/` - For external runtime library functions

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

For comprehensive testing documentation including how to run tests, write tests, and test organization, see [TESTING.md](TESTING.md).

**Overview:**
- **Unit tests** - Each module has `#[cfg(test)]` tests
- **Integration tests** - Parse and compile real BASIC programs
- **Golden tests** - Compare output against known-good files
- **Fixture tests** - Success cases with expected output, error cases with expected errors
- **Property-based tests** - Using `proptest` for fuzzing inputs
- **Compatibility tests** - QBasic 4.5 compatibility suite

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
| `sdl2` 0.38 | Graphics backend (optional, feature-gated) |
| `image` 0.25 | Image loading for sprites (optional) |
| `rodio` 0.21 | Audio backend (optional, feature-gated) |
| `rfd` 0.17 | Native file dialogs (optional, feature-gated) |

### Feature Flags

```toml
[features]
default = ["graphics-sdl2", "audio-rodio", "dialogs"]
graphics-sdl2 = ["sdl2", "image"]
graphics-sdl2-ttf = ["sdl2/ttf"]  # Requires SDL2_ttf library
graphics-mock = []  # For testing without display
audio-rodio = ["rodio"]
audio-mock = []  # For testing without audio
dialogs = ["rfd"]  # Native file dialogs
```

### Dev Dependencies

| Crate | Purpose |
|-------|---------|
| `pretty_assertions` 1.4 | Enhanced test diff output |
| `tempfile` 3 | Temporary files for tests |
| `proptest` 1.5 | Property-based testing |
| `criterion` 0.5 | Benchmarking |

## Build Configuration

- **Workspace** - Multiple crates: `qb64fresh` (compiler), `qb64fresh-runtime`, `tools`, `tools/fmt`, `tools/lint`
- **Pre-commit hooks** - `cargo-husky` runs fmt/clippy
- **CI/CD** - GitHub Actions in `.github/workflows/`
- **Benchmarks** - Criterion benchmarks in `benches/`
- **Fuzzing** - Infrastructure in `fuzz/`
- **Tools** - Code formatter and linter in `tools/` directory

## Related Documents

- [CLAUDE.md](CLAUDE.md) - AI assistant configuration
- [DEVELOPMENT.md](DEVELOPMENT.md) - Developer onboarding
- [TESTING.md](TESTING.md) - Comprehensive testing guide
- [docs/PARSER_PLAN.md](docs/PARSER_PLAN.md) - Parser implementation details
- [docs/QB64_SYNTAX_REFERENCE.md](docs/QB64_SYNTAX_REFERENCE.md) - Language syntax reference
- [docs/QB64PE_LANGUAGE_SPECIFICATION.md](docs/QB64PE_LANGUAGE_SPECIFICATION.md) - QB64PE language reference
- [docs/reference/HEADER_PARSER_API.md](reference/HEADER_PARSER_API.md) - C header parser API reference
- [docs/GRAPHICS.md](GRAPHICS.md) - Graphics system architecture, implementation, and usage guide
- [docs/INFORM_FUNCTIONALITY.md](docs/INFORM_FUNCTIONALITY.md) - InForm WYSIWYG UI designer and GUI engine (external to QB64pe)
- [docs/INFORM_EXPERT_DISCUSSION.md](docs/INFORM_EXPERT_DISCUSSION.md) - Expert discussion: enhanced InForm (usefulness, direction, classic vs. modern, AI)
- [SECURITY_MODEL.md](SECURITY_MODEL.md) - SHELL and file operation security (command injection, path traversal, no sandbox)

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
| ADR-0009 | LSP architecture |
| ADR-0010 | Parser modularization |
| ADR-0011 | Error handling |
| ADR-0012 | Preprocessor architecture |
| ADR-0013 | Debugger architecture (DAP, tools/debug) |
| ADR-0014 | Scope and intentionally excluded features |
| ADR-0015 | No-sandbox execution model (SHELL, file ops) |

## Design Decisions Summary

Key architectural decisions made during development:

### Graphics and Audio Backends

- **Graphics backend**: Trait-based abstraction with SDL2 as default, mock for testing
  - See [ADR-0006](../adrs/ADR-0006-graphics-system.md) for details
- **Sound backend**: Trait-based abstraction with rodio as default, mock for testing
  - See [ADR-0007](../adrs/ADR-0007-audio-system.md) for details

### Memory Model

- **PEEK/POKE**: Use sandboxed conventional memory (cmem) - a 1MB heap buffer emulating DOS memory model, matching QB64pe's approach. This allows legacy programs to do pointer arithmetic tricks safely without accessing real system memory.
- **_MEM operations**: Fully integrated with VARPTR compatibility - `_MEM` functions accept any pointer via `qb_mem_of()`, enabling `_MEM(VARPTR(variable))` patterns to work correctly.

For detailed architecture decision records, see [docs/adrs/](../adrs/README.md).

## Code Statistics Summary

| Component | Lines | Status |
|-----------|-------|--------|
| Lexer | ~1,850 | ✓ Complete (logos-based) |
| AST | ~2,651 | ✓ Complete |
| Parser | ~11,450 | ✓ Complete |
| Semantic Analysis | ~15,800 | ✓ Complete |
| Code Generation | ~19,800 | ✓ Complete (refactored into submodules) |
| LSP | ~2,200 | ✓ Complete |
| Runtime Library | ~9,026 | ✓ Complete (Rust + inline C) |
| Formatter (fmt) | ~1,500 | ✓ Complete |
| Linter (lint) | ~1,200 | ✓ Complete |
| **Test Suite** | **1,500+** | Unit, integration, golden, fuzz |

## Bootstrap Achievement

QB64Fresh successfully compiles the **QB64pe compiler itself** - a 59,000-line BASIC codebase across 39 files - into a working 2.1MB executable. This demonstrates QB64Fresh's capability to handle large, real-world BASIC programs.

**Metrics:**
- QB64pe source: 39 files, ~59,000 lines of BASIC
- Preprocessed size: 2.64 MB (with all `$INCLUDE` files)
- Generated C code: 83,705 lines (~4.5 MB)
- Final executable: 2.1 MB ELF binary
- Compilation time: ~800ms on modern hardware

**Key Technical Challenges Solved:**
1. **Dual Namespace Model** - Separate storage for scalars and arrays with same base name
2. **Function Call Name Resolution** - Using canonical names with type suffixes
3. **TYPE Alternate Syntax** - Extended parser for `AS TYPE field1, field2, ...` syntax
4. **Extended Type Suffixes** - Support for `&&`, `~&&`, `~&`, `%%` suffixes
5. **SHARED Array Handling** - Proper global scope modification for `REDIM _PRESERVE`
6. **Polymorphic _IIF** - Type inference from both branches using numeric promotion
7. **C Code Generation** - Fixed TYPE ordering, identifier escaping, stack size handling

**Status:** QB64pe compiles without errors (0 parse, 0 semantic, 0 GCC errors) and runs successfully. Runtime features (file I/O, keyboard input) are complete. The generated executable runs and displays help (`-h` works). Full execution testing (compiling BASIC programs with bootstrapped QB64pe) is in progress.

**Important:** QB64pe is compiled with `RuntimeMode::External` because it requires graphics support for its GUI. The runtime library must be built with `--features graphics-sdl2` and linked with SDL2 libraries.

**Runtime Features Completed:**
- ✅ File I/O operations (OPEN, CLOSE, PRINT#, INPUT#, GET, PUT, SEEK)
- ✅ Keyboard input (INKEY$, _KEYHIT) - Unix and Windows support
- ✅ String operations (validated through integration tests)
- ✅ Array operations (validated through integration tests)
- ✅ Command-line argument parsing (verified via `-h` flag)
- ✅ Error handling (ON ERROR GOTO, RESUME)

See [docs/QB64PE_COMPILATION_PLAN.md](QB64PE_COMPILATION_PLAN.md) for detailed implementation status.

For detailed implementation history and technical challenges, see [docs/archive/BOOTSTRAP_PLAN_FULL.md](archive/BOOTSTRAP_PLAN_FULL.md).

---

*Last updated: 2026-01-26*
