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
│    Lexer      │  Tokenizes source into a stream of tokens
│  (src/lexer)  │  Uses `logos` crate for fast lexical analysis
└───────┬───────┘
        │ Vec<Token>
        ▼
┌───────────────┐
│    Parser     │  Builds Abstract Syntax Tree from tokens
│ (src/parser)  │  Pratt parsing for expressions, recursive descent for statements
└───────┬───────┘
        │ Program (AST)
        ▼
┌───────────────┐
│   Semantic    │  Type checking, symbol resolution, validation
│ (src/semantic)│  Produces TypedProgram IR
└───────┬───────┘
        │ TypedProgram (IR)
        ▼
┌───────────────┐
│   CodeGen     │  Generates target code via backend trait
│ (src/codegen) │  C backend implemented (~1375 lines)
└───────┬───────┘
        │ Generated C code
        ▼
┌───────────────┐
│  C Compiler   │  External: gcc/clang compiles to executable
│  (external)   │
└───────┬───────┘
        │
        ▼
    Executable
```

## Module Organization

```
src/
├── lib.rs              # Library root, public API
├── main.rs             # CLI entry point
│
├── lexer/              # Phase 1: Lexical Analysis
│   ├── mod.rs          # Lexer struct, iterator interface
│   └── token.rs        # TokenKind enum (logos-generated)
│
├── ast/                # AST Type Definitions
│   ├── mod.rs          # Span, Program
│   ├── expr.rs         # Expression nodes
│   └── stmt.rs         # Statement nodes
│
├── parser/             # Phase 2: Syntax Analysis
│   ├── mod.rs          # Parser implementation
│   └── error.rs        # ParseError types
│
├── semantic/           # Phase 3: Semantic Analysis [COMPLETE]
│   ├── mod.rs          # Analysis entry point, built-in registration
│   ├── symbols.rs      # Symbol table with scope management
│   ├── types.rs        # BasicType enum, type inference
│   ├── checker.rs      # Type checker for expressions/statements
│   ├── typed_ir.rs     # TypedProgram, TypedExpr, TypedStatement
│   └── error.rs        # Semantic error types
│
├── codegen/            # Phase 4: Code Generation ✓
│   ├── mod.rs          # CodeGenerator trait, GeneratedOutput
│   ├── error.rs        # CodeGenError types
│   └── c_backend.rs    # C code generator (~1375 lines)
│
├── lsp/                # Language Server Protocol ✓
│   ├── mod.rs          # LSP server implementation
│   └── main.rs         # qb64fresh-lsp binary entry
│
├── lib.rs              # Library crate root
└── main.rs             # CLI binary entry

runtime/                # Runtime Library (workspace member) ✓
├── src/
│   ├── lib.rs          # Crate root, init/shutdown
│   ├── string.rs       # Reference-counted strings (~730 lines)
│   ├── io.rs           # PRINT, INPUT, console (~305 lines)
│   └── math.rs         # Math functions (~390 lines)
└── include/
    └── qb64fresh_rt.h  # C header for FFI
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

**C Backend (implemented):**
- Proven approach (QB64pe uses C++)
- ~1375 lines in `c_backend.rs`
- Two runtime modes: `inline` (self-contained) and `external` (library-linked)
- Handles all statements: assignments, control flow, procedures
- Type conversions, string operations, built-in functions
- Portable across platforms
- Delegates optimization to C compiler

**Future backends (possible):**
- LLVM via `inkwell`
- Cranelift for JIT
- Direct x86/ARM

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

1. Add token(s) to `src/lexer/token.rs`
2. Add AST node(s) to `src/ast/expr.rs` or `src/ast/stmt.rs`
3. Add parser logic to `src/parser/mod.rs`
4. Add semantic rules to `src/semantic/` [when implemented]
5. Add codegen rules to `src/codegen/` [when implemented]

### Adding a New Backend

1. Implement the `CodeGenerator` trait
2. Handle all IR node types
3. Register in CLI options

## Testing Strategy

- **Unit tests** - Each module has `#[cfg(test)]` tests
- **Integration tests** - Parse real BASIC programs
- **Compatibility tests** - Use QB64pe test suite (`tests/qbasic_testcases/`)

Run tests:
```bash
cargo test          # All tests
cargo test parser   # Parser tests only
cargo test --doc    # Doc tests only
```

## Dependencies

| Crate | Purpose |
|-------|---------|
| `logos` | Lexer generation |
| `clap` | CLI argument parsing |
| `thiserror` | Error type derivation |
| `env_logger` | Logging |

## Related Documents

- [CLAUDE.md](CLAUDE.md) - AI assistant configuration
- [DEVELOPMENT.md](DEVELOPMENT.md) - Developer onboarding
- [PROJECT_PLAN.md](PROJECT_PLAN.md) - Roadmap and milestones
- [docs/PARSER_PLAN.md](docs/PARSER_PLAN.md) - Parser implementation details
- [docs/QB64_SYNTAX_REFERENCE.md](docs/QB64_SYNTAX_REFERENCE.md) - Language syntax reference
