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

### Ariadne Integration (Planned)

The `ariadne` crate is a dependency for future CLI pretty-printing:

```rust
// Future: Pretty terminal output
use ariadne::{Report, ReportKind, Source, Label};

Report::build(ReportKind::Error, filename, span.start)
    .with_message("type mismatch")
    .with_label(Label::new((filename, span.start..span.end))
        .with_message(format!("expected {}, found {}", expected, found)))
    .finish()
    .print((filename, Source::from(source)));
```

Currently, errors flow directly to LSP. CLI error output uses simple formatting.

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
- ariadne not yet used for CLI (future work)
- No error codes yet (QB64pe uses numbered errors)

### Implementation Status

| Component | Status |
|-----------|--------|
| ParseError with spans | Complete |
| SemanticError with spans | Complete |
| CodeGenError | Complete |
| LSP diagnostic conversion | Complete |
| Error recovery in parser | Complete |
| ariadne CLI output | Planned |
| Error codes | Not started |

### Files

- `src/parser/error.rs` - Parse error types
- `src/semantic/error.rs` - Semantic error types
- `src/codegen/error.rs` - Code generation errors
- `src/ast/mod.rs` - Span type definition
- `src/lsp/mod.rs` - Error-to-diagnostic conversion

### Future Work

1. **ariadne CLI output**: Pretty terminal errors with source context
2. **Error codes**: Numbered errors for documentation/searchability
3. **Related information**: Show related locations (e.g., "first defined here")
4. **Suggestions**: "Did you mean..." for typos
5. **Warning levels**: Currently all errors, no warnings
