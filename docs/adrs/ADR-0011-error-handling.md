# ADR-0011: Error Handling and Diagnostics Strategy

## Status

**Accepted** - January 20, 2026

## Context

Compiler error messages are critical for user experience. A compiler that says "syntax error" is far less useful than one that says "expected `)` after function arguments at line 42, column 15."

Key considerations:
- Errors must include source locations (line, column, span)
- Errors should be human-readable with helpful context
- Errors must integrate with LSP for real-time IDE feedback
- Different compiler phases produce different error types
- The `ariadne` crate is available for pretty terminal output

## Decision

**We chose a hierarchical error type system with source spans tracked through all compiler phases, integrated with LSP diagnostics**.

### Error Type Hierarchy

```
                    ┌─────────────────┐
                    │ CompilationError│  (Future: unified error)
                    └────────┬────────┘
           ┌─────────────────┼─────────────────┐
           ▼                 ▼                 ▼
    ┌────────────┐    ┌──────────────┐   ┌──────────────┐
    │ ParseError │    │SemanticError │   │ CodeGenError │
    └────────────┘    └──────────────┘   └──────────────┘
```

### ParseError (`src/parser/error.rs`)

```rust
#[derive(Debug, Clone, thiserror::Error)]
pub enum ParseError {
    #[error("expected {expected}, found {found}")]
    UnexpectedToken {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("unexpected end of file while parsing {context}")]
    UnexpectedEof { context: String },

    #[error("invalid {what}: {details}")]
    Invalid {
        what: String,
        details: String,
        span: Span,
    },
    // ... more variants
}

impl ParseError {
    /// Returns the source span if available.
    pub fn span(&self) -> Option<&Span> { ... }
}
```

### SemanticError (`src/semantic/error.rs`)

```rust
#[derive(Debug, Clone, thiserror::Error)]
pub enum SemanticError {
    // Variable/Symbol Errors
    #[error("undefined variable `{name}`")]
    UndefinedVariable { name: String, span: Span },

    #[error("variable `{name}` already defined")]
    DuplicateVariable { name: String, span: Span },

    // Type Errors
    #[error("type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },

    // ... 50+ more variants covering all semantic checks
}
```

### Span Type (`src/ast/mod.rs`)

All errors reference source positions via `Span`:

```rust
/// A range in the source code (byte offsets).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,  // Byte offset of start
    pub end: usize,    // Byte offset of end (exclusive)
}
```

### Error Flow

```
Source Code
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ Lexer                                                       │
│ - Tokens include spans from logos                           │
│ - Invalid tokens preserved with Error variant               │
└────────────────────┬────────────────────────────────────────┘
                     │ Vec<Token> (each with span)
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Parser                                                      │
│ - Propagates spans from tokens to AST nodes                 │
│ - Collects Vec<ParseError> (continues after errors)         │
└────────────────────┬────────────────────────────────────────┘
                     │ (AST, Vec<ParseError>)
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ Semantic Analyzer                                           │
│ - Uses AST spans for error locations                        │
│ - Collects Vec<SemanticError>                               │
└────────────────────┬────────────────────────────────────────┘
                     │ (TypedIR, Vec<SemanticError>)
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ LSP Server                                                  │
│ - Converts spans to LSP positions (UTF-16)                  │
│ - Publishes as Diagnostic notifications                     │
└─────────────────────────────────────────────────────────────┘
```

### Error Recovery

The parser uses **panic-mode recovery** to continue after errors:

```rust
fn parse_statement(&mut self) -> Result<Statement, ()> {
    match self.try_parse_statement() {
        Ok(stmt) => Ok(stmt),
        Err(()) => {
            // Skip to next line and try again
            self.skip_to_end_of_line();
            Err(())
        }
    }
}
```

This allows reporting multiple errors per compilation.

### LSP Integration

Errors are converted to LSP `Diagnostic` objects:

```rust
fn get_diagnostics(&self, source: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for err in &parse_errors {
        if let Some(span) = err.span() {
            diagnostics.push(Diagnostic {
                range: span_to_range(source, span.start, span.end),
                severity: Some(DiagnosticSeverity::ERROR),
                message: err.to_string(),
                ..Default::default()
            });
        }
    }

    diagnostics
}
```

### Ariadne Integration (CLI)

The `ariadne` crate is used for rich CLI diagnostics in `src/error_formatting.rs`:

- **Parse errors**: Formatted with source context, labels, and optional "did you mean" suggestions (e.g. keyword typos)
- **Semantic errors**: Formatted with span highlighting and suggestion notes (e.g. undefined variable → similar names)

CLI (`qb64fresh`) uses `error_formatting::format_parse_errors` and `format_semantic_errors` when emitting diagnostics. LSP continues to convert errors to LSP `Diagnostic` objects for the IDE.

### Rationale

1. **thiserror derive**: Clean, idiomatic Rust error definitions
2. **Span in every error**: Enables precise source highlighting
3. **Separate error types per phase**: Type safety, appropriate detail level
4. **Error collection (not early exit)**: Report all errors at once
5. **LSP-first design**: IDE experience is primary concern

### Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| **Single error type** | Loses phase-specific detail |
| **anyhow/eyre** | Too generic, loses structured information |
| **Early exit on first error** | Poor UX, users fix one error at a time |
| **String errors** | No structured access to span, severity |

## Consequences

### Positive

- Every error includes source location
- Multiple errors reported per compilation
- Clean LSP integration
- Type-safe error handling
- Easy to add new error variants

### Negative

- Span tracking adds complexity to parser
- Error types grow as language features added
- No error codes yet (QB64pe uses numbered errors)

### Implementation Status

| Component | Status |
|-----------|--------|
| ParseError with spans | Complete |
| SemanticError with spans | Complete |
| CodeGenError | Complete |
| LSP diagnostic conversion | Complete |
| Error recovery in parser | Complete |
| ariadne CLI output | Complete (`src/error_formatting.rs`) |
| Suggestions ("did you mean") | Complete (parse: keyword typo; semantic: `suggestions` in checker) |
| Error codes | Not started |

### Files

- `src/parser/error.rs` - Parse error types
- `src/semantic/error.rs` - Semantic error types
- `src/semantic/suggestions.rs` - "Did you mean" for undefined variable/label/procedure
- `src/codegen/error.rs` - Code generation errors
- `src/error_formatting.rs` - ariadne-based CLI diagnostics
- `src/ast/mod.rs` - Span type definition
- `src/lsp/mod.rs` - Error-to-diagnostic conversion

### Future Work

1. **Error codes**: Numbered errors for documentation/searchability
2. **Related information**: Show related locations (e.g., "first defined here")
3. **Warning levels**: Currently all errors, no warnings
