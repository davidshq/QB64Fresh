# Session 007: VSCode Extension

**Date:** 2026-01-16
**Focus:** Creating VSCode extension to connect to the QB64Fresh LSP server

## Summary

Created a complete VSCode extension that provides IDE features for BASIC files by connecting to the `qb64fresh-lsp` language server.

## Key Decision

**Extension Location:** Created as a sibling project at `/home/dave/repos/qb64contain/vscode-qb64fresh/` rather than inside QB64Fresh. This allows:
- Independent versioning
- Separate release cycles
- Cleaner separation (Rust compiler vs TypeScript extension)

## What Was Accomplished

### 1. Extension Structure

Created a standard VSCode extension with TypeScript:

```
vscode-qb64fresh/
├── package.json              # Extension manifest
├── tsconfig.json             # TypeScript config
├── language-configuration.json
├── README.md
├── .vscode/
│   ├── launch.json          # F5 debugging
│   └── tasks.json
├── src/
│   └── extension.ts         # LSP client
└── syntaxes/
    └── qb64fresh.tmLanguage.json
```

### 2. LSP Client (src/extension.ts)

Key features:
- Auto-detects `qb64fresh-lsp` binary in sibling QB64Fresh project
- Falls back to system PATH
- Configurable via `qb64fresh.serverPath` setting
- Uses `vscode-languageclient` for JSON-RPC over stdio

Server path search order:
1. User setting `qb64fresh.serverPath`
2. `../QB64Fresh/target/debug/qb64fresh-lsp`
3. `../QB64Fresh/target/release/qb64fresh-lsp`
4. System PATH

### 3. Language Configuration

- File associations: `.bas`, `.bi`, `.bm`
- Comment markers: `'` (apostrophe)
- Bracket pairs and auto-closing
- Code folding for SUB/FUNCTION/IF/FOR/etc.
- Indentation rules

### 4. Syntax Highlighting (TextMate Grammar)

Comprehensive highlighting for:
- Keywords (IF, FOR, SUB, FUNCTION, DIM, etc.)
- Control flow (THEN, ELSE, NEXT, WEND, etc.)
- Type declarations (INTEGER, LONG, STRING, etc.)
- Built-in functions (ABS, LEN, LEFT$, MID$, etc.)
- QB64 extensions (_LOADIMAGE, _RGB, _SNDPLAY, etc.)
- Strings, numbers (including hex &H, octal &O, binary &B)
- Comments (both `'` and `REM`)
- Variable suffixes (%, &, !, #, $)

### 5. Build & Debug Configuration

- `npm run compile` - Build TypeScript
- `npm run watch` - Watch mode
- F5 launches Extension Development Host
- Pre-configured to open sample .bas file

## Testing

Extension tested successfully:
- Syntax highlighting works
- LSP diagnostics appear (red squiggles on errors)
- Hover information displays

## Files Created

| File | Purpose |
|------|---------|
| `package.json` | Extension manifest, dependencies |
| `src/extension.ts` | LSP client setup (~100 lines) |
| `language-configuration.json` | BASIC editing settings |
| `syntaxes/qb64fresh.tmLanguage.json` | Syntax highlighting (~200 patterns) |
| `tsconfig.json` | TypeScript compiler config |
| `.vscode/launch.json` | Debug configurations |
| `.vscode/tasks.json` | Build tasks |
| `README.md` | Usage documentation |

## Dependencies

- `vscode-languageclient` ^9.0.1 - LSP client library
- `typescript` ^5.3.0 - Build tooling
- `@types/vscode` ^1.75.0 - VSCode API types

## Commit

```
feat: initial VSCode extension for QB64Fresh LSP
```

Committed to new git repository at `vscode-qb64fresh/`.

## Next Steps

Potential enhancements:
- Go to Definition (LSP `textDocument/definition`)
- Document Symbols (LSP `textDocument/documentSymbol`)
- Completion suggestions (LSP `textDocument/completion`)
- Package as `.vsix` for marketplace distribution
