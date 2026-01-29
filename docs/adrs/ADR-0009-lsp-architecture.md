# ADR-0009: Language Server Protocol Architecture

## Status

**Accepted** - January 20, 2026

## Context

Modern IDE support is essential for developer productivity. QB64pe has a built-in IDE which is tightly coupled to the compiler. This approach has limitations:

- Difficult to integrate with external editors (VSCode, Neovim, etc.)
- IDE maintenance burden falls on compiler developers
- Cannot leverage existing editor ecosystems

Key considerations:
- Must provide real-time diagnostics as users type
- Should support modern features (hover, go-to-definition, completion)
- Need to decouple IDE support from the compiler itself
- Must work with multiple editors via a standard protocol

## Decision

**We chose to implement a separate LSP (Language Server Protocol) binary using `tower-lsp`**.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Editor (VSCode, Neovim, Emacs, etc.)                        │
│ - vscode-qb64fresh extension                                │
└────────────────────┬────────────────────────────────────────┘
                     │ JSON-RPC over stdio
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ qb64fresh-lsp binary                                        │
│ - Maintains document state cache                            │
│ - Handles LSP lifecycle (initialize, shutdown)              │
│ - Converts byte spans to LSP positions                      │
└────────────────────┬────────────────────────────────────────┘
                     │ Library calls
                     ▼
┌─────────────────────────────────────────────────────────────┐
│ qb64fresh library crate                                     │
│ - lexer, parser, semantic analysis                          │
│ - Returns errors with source spans                          │
└─────────────────────────────────────────────────────────────┘
```

### Dual Binary Design

The project produces two binaries:

1. **`qb64fresh`** - CLI compiler for batch compilation
2. **`qb64fresh-lsp`** - Language server for IDE integration

Both link against the same library crate, ensuring consistency.

### Key Components

```rust
/// Shared state for the language server.
pub struct ServerState {
    /// Open documents indexed by URI.
    pub documents: HashMap<Url, DocumentState>,
}

/// The QB64Fresh Language Server.
pub struct QbLanguageServer {
    client: Client,        // For sending notifications back
    state: Arc<RwLock<ServerState>>,  // Thread-safe document cache
}
```

### LSP Capabilities

| Capability | Status | Notes |
|------------|--------|-------|
| `textDocumentSync` | Full | Full document content; analysis is incremental |
| `hoverProvider` | Basic | Shows token kind and position |
| `diagnosticProvider` | Complete | Lexer, parser, and semantic errors |
| `definitionProvider` | Planned | Go-to-definition |
| `completionProvider` | Planned | Auto-completion |
| `signatureHelpProvider` | Planned | Function signatures |
| `documentSymbolProvider` | Planned | Outline view |
| `referencesProvider` | Planned | Find all references |
| `renameProvider` | Planned | Rename symbol |

### Document Synchronization and Incremental Analysis

The server uses **full document sync** (full content on each change) but **incremental analysis** for performance:

- On `didOpen`: Store full document, run full analysis
- On `didChange`: Replace content, then **incremental lex** (merge tokens), **incremental parse** (re-parse from first affected statement), **incremental semantic analysis** (re-collect declarations, re-check from affected point)
- On `didClose`: Remove from cache, clear diagnostics

Incremental parsing uses statement-boundary detection so only the tail of the file is re-parsed when edits occur. Incremental semantic analysis re-uses the public `SemanticAnalyzer::analyze()` API and returns diagnostics from both parse and semantic phases. See `src/lsp/analysis/incremental.rs` and [AgenticLogs/2026-01-28_session-072_lsp-incremental-parsing-complete.md](../../AgenticLogs/2026-01-28_session-072_lsp-incremental-parsing-complete.md).

### Span-to-Position Conversion

A critical detail: LSP uses **UTF-16 code units** for character positions, while Rust uses byte offsets. The server handles this conversion:

```rust
fn offset_to_position(source: &str, offset: usize) -> Position {
    // Count UTF-16 code units, not bytes or code points
    character += c.len_utf16() as u32;
}
```

### Rationale

1. **Separate binary**: Clean separation of concerns; editor doesn't need compiler internals
2. **tower-lsp**: Battle-tested Rust LSP framework, handles JSON-RPC protocol
3. **Async/tokio**: Non-blocking I/O for responsive editor experience
4. **Incremental analysis**: Lex/parse/analyze only from the first affected statement (since 2026-01-28), adequate for BASIC file sizes
5. **Library reuse**: Same lexer/parser/semantic as CLI compiler

### Alternatives Considered

| Alternative | Reason Not Chosen |
|-------------|-------------------|
| **Built-in IDE** | Maintenance burden, limited editor choice |
| **Embedded LSP in compiler** | Complicates CLI usage, mixing concerns |
| **lsp-server crate** | Less ergonomic than tower-lsp |
| **Tree-sitter only** | Insufficient for type-aware features |

## Consequences

### Positive

- Works with any LSP-capable editor (VSCode, Neovim, Emacs, etc.)
- Clean separation: compiler team focuses on language, editors "just work"
- Standard protocol means leveraging years of LSP ecosystem work
- Async design keeps editor responsive
- Same analysis code as compiler ensures consistency
- Incremental analysis (since 2026-01-28) reduces re-parse/re-check cost on edits

### Negative

- Two binaries to build and distribute
- Full document content sync on each change; very large files may still incur noticeable latency
- JSON-RPC overhead (minimal in practice)
- LSP protocol complexity for advanced features

### Files

- `src/lsp/mod.rs` - Server implementation (~350 lines)
- `src/lsp/main.rs` - Binary entry point
- `vscode-qb64fresh/` - VSCode extension (sibling project)

### Future Work

1. **Incremental sync**: Optional; full sync is acceptable for typical BASIC file sizes
2. ~~**Incremental analysis**~~: **Done** (2026-01-28) — incremental lex/parse/semantic from first affected statement
3. **Semantic tokens**: Syntax highlighting from compiler
4. **Code actions**: Quick fixes for common errors
5. **Workspace support**: Multi-file projects, $INCLUDE resolution
