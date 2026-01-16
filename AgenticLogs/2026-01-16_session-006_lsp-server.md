# Session 006: LSP Server Implementation

**Date:** 2026-01-16
**Focus:** Implementing Language Server Protocol support for IDE integration

## Summary

Completed Phase 3.6: Created a basic LSP server that enables real-time diagnostics and hover information in any LSP-compatible editor.

## Architecture

### LSP Overview

The Language Server Protocol (LSP) is a standardized way for editors/IDEs to communicate with language tools. Features implemented:

1. **Diagnostics** - Show errors/warnings as you type (parser + semantic errors)
2. **Hover** - Show token info when hovering over identifiers
3. **Document Sync** - Full document synchronization on open/change/close

### Implementation Approach

Using `tower-lsp` crate - the standard Rust LSP framework:

```
Editor (VSCode, etc.)
    ↓ JSON-RPC over stdio
LSP Server (qb64fresh-lsp)
    ↓ Uses
QB64Fresh Compiler Library (lexer, parser, semantic)
```

## What Was Accomplished

### 1. Dependencies Added
Added LSP-related dependencies to `Cargo.toml`:
- `tower-lsp = "0.20"` - LSP framework
- `tokio = { version = "1", features = ["full"] }` - Async runtime
- `serde = { version = "1.0", features = ["derive"] }` - Serialization
- `serde_json = "1.0"` - JSON serialization

### 2. LSP Server Implementation (`src/lsp/mod.rs`)

Key components:
- **DocumentState** - Stores document content and version for open files
- **ServerState** - HashMap of open documents indexed by URI
- **QbLanguageServer** - Main server implementing `LanguageServer` trait

Implemented LSP methods:
- `initialize` - Announces server capabilities
- `initialized` - Logs server ready message
- `shutdown` - Clean shutdown
- `did_open` - Stores document and runs initial analysis
- `did_change` - Updates document and re-analyzes
- `did_close` - Removes document from state
- `hover` - Returns token info at cursor position

### 3. Diagnostics Pipeline

The `get_diagnostics()` method runs the full compiler pipeline:
1. Lexer - tokenize source
2. Parser - build AST (collect parse errors)
3. Semantic analysis - type check (collect semantic errors)

Each error is converted to an LSP `Diagnostic` with:
- Position range (line/character)
- Severity (ERROR)
- Source ("qb64fresh")
- Human-readable message

### 4. Position Conversion Utilities

- `span_to_range()` - Converts byte span to LSP Range
- `offset_to_position()` - Converts byte offset to line/character
- `position_to_offset()` - Converts line/character to byte offset

### 5. Binary Entry Point (`src/lsp/main.rs`)

Simple main function that:
- Initializes logging to stderr
- Creates LSP service with our server
- Runs the server over stdio

## Files Created/Modified

### New Files
- `src/lsp/mod.rs` (~360 lines) - LSP server implementation
- `src/lsp/main.rs` (~43 lines) - Binary entry point

### Modified Files
- `Cargo.toml` - Added LSP dependencies and binary target
- `src/lib.rs` - Added `pub mod lsp;` export

## Technical Notes

### Error Span Handling

Parse errors return `Option<Span>` because some errors like `UnexpectedEof` have no source location. For LSP, we use position (0,0) as fallback:

```rust
let range = match err.span() {
    Some(span) => span_to_range(source, span.start, span.end),
    None => Range {
        start: Position { line: 0, character: 0 },
        end: Position { line: 0, character: 0 },
    },
};
```

Semantic errors always have spans (they reference specific code), so no Option handling needed there.

### Document Synchronization Mode

Using `TextDocumentSyncKind::FULL` - the editor sends complete document content on each change. This is simpler than incremental sync and sufficient for our needs (BASIC files are typically small).

### Testing

Added 3 unit tests for position conversion utilities:
- `test_offset_to_position`
- `test_position_to_offset`
- `test_span_to_range`

Total test count now: 107 (86 compiler + 21 runtime)

## Future Enhancements

Features to add in future sessions:
- Go to Definition (find variable/function declarations)
- Document Symbols (outline view of SUBs/FUNCTIONs)
- Completion suggestions
- Semantic token highlighting
- Signature help for function calls

## Running the LSP Server

```bash
# Start the server (typically done by editor)
cargo run --bin qb64fresh-lsp

# Or with debug logging
RUST_LOG=debug cargo run --bin qb64fresh-lsp 2>lsp.log
```

The server communicates over stdio using JSON-RPC with LSP message framing.
