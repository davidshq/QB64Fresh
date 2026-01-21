# QB64pe IDE - Complete Functionality Checklist

This document provides a comprehensive checklist of all functionality available in the QB64pe Integrated Development Environment (IDE). The IDE source code spans approximately **22,464 lines** across multiple source files located in `source/ide/`.

## QB64Fresh Implementation Status

QB64Fresh uses a **modern LSP-based architecture** instead of a monolithic IDE. Features are provided by:
- **Compiler** (`qb64fresh`) - Lexer, parser, semantic analysis, C code generation
- **LSP Server** (`qb64fresh-lsp`) - IDE features via Language Server Protocol
- **VSCode Extension** (`vscode-qb64fresh`) - Editor integration, syntax highlighting, build commands
- **Formatter** (`qb64fresh-fmt`) - Code formatting tool
- **Linter** (`qb64fresh-lint`) - Code quality analysis

Legend: ☑ = Implemented | ☐ = Not implemented | 🔶 = Partial/Different approach

---

## Table of Contents

1. [File Menu](#file-menu)
2. [Edit Menu](#edit-menu)
3. [View Menu](#view-menu)
4. [Search Menu](#search-menu)
5. [Run Menu](#run-menu)
6. [Debug Menu](#debug-menu)
7. [Options Menu](#options-menu)
8. [Tools Menu](#tools-menu)
9. [Help Menu](#help-menu)
10. [Editor Features](#editor-features)
11. [Context Menu](#context-menu-right-click)
12. [Keyboard Shortcuts](#keyboard-shortcuts)
13. [Compilation & Execution](#compilation--execution)
14. [Debugging Features](#debugging-features)
15. [Configuration & Settings](#configuration--settings)
16. [File Dialogs](#file-dialogs)
17. [Information & Dialogs](#information--dialogs)
18. [Wiki & Documentation](#wiki--documentation)
19. [Visual Feedback](#visual-feedback)
20. [Project Management](#project-management)
21. [Source Code Analysis](#source-code-analysis)
22. [Export Capabilities](#export-capabilities)

---

## File Menu
- Remaining not planned

---

## Edit Menu

---

## View Menu

---

## Search Menu

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| → Skip comments | - | ☐ | - |
| → Skip strings | - | ☐ | - |
| → Only in comments | - | ☐ | - |
| → Only in strings | - | ☐ | - |
| → Invert search | - | ☐ | - |

---

## Run Menu

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| Run Only (No EXE save) | - | ☐ | - |
| Output EXE to Source Folder toggle | - | ☐ | - |
| Generate License For EXE | - | ☐ | - |
| Modify COMMAND$ function parameter | - | ☐ | - |
| Change Terminal (Linux only) | - | ☐ | - |
| Set Default EXE Folder | - | ☐ | - |
| **Configure Logging** | | | |
| → Enable/disable logging | - | ☐ | - |
| → Set log level | - | ☐ | - |
| → Set log scopes | - | ☐ | - |
| → Set log handlers | - | ☐ | - |
| → Configure log file name | - | ☐ | - |

---

## Debug Menu

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| Start Paused | F7 / F8 | ☐ | Not yet implemented (needs DAP) |
| Toggle Breakpoint | F9 | ☐ | Not yet implemented (needs DAP) |
| → Set breakpoints on lines | - | ☐ | Not yet implemented |
| → Visual breakpoint indicators | - | ☐ | Not yet implemented |
| Clear All Breakpoints | F10 | ☐ | Not yet implemented |
| Toggle Skip Line | Ctrl+P | ☐ | Not yet implemented |
| → Skip marked lines during debug | - | ☐ | Not yet implemented |
| Unskip All Lines | Ctrl+F10 | ☐ | Not yet implemented |
| **Watch List...** | F4 | ☐ | Not yet implemented |
| → Add variables to watch | - | ☐ | Not yet implemented |
| → Remove variables from watch | - | ☐ | Not yet implemented |
| → Display format (DEC, HEX, BIN, OCT) | - | ☐ | Not yet implemented |
| → Watch array elements | - | ☐ | Not yet implemented |
| → Watch UDT members | - | ☐ | Not yet implemented |
| → Watchpoint conditions | - | ☐ | Not yet implemented |
| **Call Stack...** | F12 | ☐ | Not yet implemented |
| → View call stack history | - | ☐ | Not yet implemented |
| → Navigate through call stack | - | ☐ | Not yet implemented |
| Auto-add $DEBUG Metacommand toggle | - | ☐ | Not yet implemented |
| Output Watch List to Console toggle | - | ☐ | Not yet implemented |
| Set Base TCP/IP Port Number | - | ☐ | Not applicable |
| Purge C++ Libraries | - | ☐ | Not applicable (C backend) |

---

## Options Menu

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| **Code Layout...** | | | |
| **Compiler Settings...** | | | |
| → Optimization level | - | ☐ | Not yet exposed in settings |
| → Debug symbols | - | ☐ | Not yet exposed |
| → C++ compiler flags | - | ☐ | Not yet exposed |
| → Extra linker flags | - | ☐ | Not yet exposed |
| → System MinGW toggle | - | ☐ | Not applicable |
| **Undo/History...** | | | |
| Ignore Warnings toggle | - | ☐ | Not yet exposed |

---

## Tools Menu

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| **ASCII Chart** | | | |
| → Browse ASCII characters | - | ☐ | Not yet implemented |
| → Insert characters into code | - | ☐ | Not yet implemented |
| **Insert Quick Keycode** | Ctrl+K | ☐ | Not yet implemented |
| → Capture key codes | - | ☐ | Not yet implemented |
| → Insert key code values | - | ☐ | Not yet implemented |
| **Library Explorer** (if installed) | Ctrl+L | ☐ | Not yet implemented |
| **Math Evaluator** | | | |
| → Calculate expressions | - | ☐ | Not yet implemented |
| → Display results | - | ☐ | Not yet implemented |
| **RGB Color Mixer** | | | |
| → Mix colors visually | - | ☐ | Not yet implemented |
| → Generate _RGB statements | - | ☐ | Not yet implemented |
| → Insert into code | - | ☐ | Not yet implemented |

---

## Help Menu

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| View Help | Shift+F1 | 🔶 | LSP Server (hover provides built-in function docs) |
| → Last viewed article | - | ☐ | Not yet implemented |
| → Scrollable help window | - | ☐ | Not yet implemented |
| → Search within help | - | ☐ | Not yet implemented |
| Contents Page | - | ☐ | Not yet implemented |
| Keywords Index (alphabetical) | - | ☐ | Not yet implemented |
| Keywords by Usage | - | ☐ | Not yet implemented |
| Metacommands Overview | - | ☐ | Not yet implemented |
| Variable Types Overview | - | ☐ | Not yet implemented |
| Update Current Page (from Wiki) | - | ☐ | Not applicable |
| Update All Pages... (batch download) | - | ☐ | Not applicable |
| View Current Page On Wiki (browser) | - | ☐ | Not yet implemented |
| About... (version information) | - | ☐ | Not yet implemented |

---

## Editor Features

- Syntax Highlighting
- Auto-completion

### Auto-formatting/Layout

| Feature | Status | Implementation |
|---------|--------|----------------|
| $FORMAT metacommand support | ☐ | Not yet implemented |

- Code Navigation
- Bracket Handling
- Selection Features
- Clipboard Operations
- Mouse Support

---

## Context Menu (Right-Click)

| Feature | Status | Implementation |
|---------|--------|----------------|
| New SUB... | ☐ | Not yet implemented |
| New FUNCTION... | ☐ | Not yet implemented |
| RGB Color Mixer... | ☐ | Not yet implemented |

---

## Keyboard Shortcuts

| Shortcut | Function | Status | Implementation |
|----------|----------|--------|----------------|
| F1 | Context help (word at cursor) | 🔶 | LSP hover (on mouse hover) |
| Shift+F1 | Help main page | ☐ | Not yet implemented |
| F4 | Watch list | ☐ | Not yet implemented (needs DAP) |
| F7/F8 | Start paused (debug) | ☐ | Not yet implemented |
| F9 | Toggle breakpoint | ☐ | Not yet implemented |
| F10 | Clear all breakpoints | ☐ | Not yet implemented |
| Ctrl+F10 | Unskip all lines | ☐ | Not yet implemented |
| F12 | Call stack | ☐ | Not yet implemented |
| Ctrl+K | Quick keycode | ☐ | Not yet implemented |
| Ctrl+L | Library explorer | ☐ | Not yet implemented |
| Ctrl+P | Toggle skip line | ☐ | Not yet implemented |

---

## Compilation & Execution

| Feature | Status | Implementation |
|---------|--------|----------------|
| Line-by-line compilation progress | ☐ | Not yet implemented |
| Progress percentage display | ☐ | Not yet implemented |
| License file generation | ☐ | Not yet implemented |

---

## Debugging Features

> **Note:** Debugging requires a Debug Adapter Protocol (DAP) implementation, which is not yet implemented in QB64Fresh. These features would need a debug adapter.

### Breakpoints

| Feature | Status | Implementation |
|---------|--------|----------------|
| Set/clear breakpoints | ☐ | Needs DAP implementation |
| Conditional breakpoints | ☐ | Needs DAP implementation |
| Visual indicators | ☐ | Needs DAP implementation |

### Step Execution

| Feature | Status | Implementation |
|---------|--------|----------------|
| Start paused mode | ☐ | Needs DAP implementation |
| Step execution | ☐ | Needs DAP implementation |

### Variable Watch

| Feature | Status | Implementation |
|---------|--------|----------------|
| Watch global variables | ☐ | Needs DAP implementation |
| Watch local variables | ☐ | Needs DAP implementation |
| Watch array elements | ☐ | Needs DAP implementation |
| Watch UDT members | ☐ | Needs DAP implementation |
| Display format options (DEC/HEX/BIN/OCT) | ☐ | Needs DAP implementation |
| Watchpoint conditions | ☐ | Needs DAP implementation |

### Call Stack

| Feature | Status | Implementation |
|---------|--------|----------------|
| Call stack tracing | ☐ | Needs DAP implementation |
| Navigate through stack | ☐ | Needs DAP implementation |

### Other Debug Features

| Feature | Status | Implementation |
|---------|--------|----------------|
| Line skip (skip specific lines) | ☐ | Needs DAP implementation |
| $DEBUG metacommand | ☐ | Not applicable (different architecture) |
| TCP/IP debug communication | ☐ | Not applicable (would use DAP) |
| Debug console integration | ☐ | Needs DAP implementation |

---

## Configuration & Settings

### Settings Persistence

### Window Settings

### History Settings

### Auto-save

### Dictionaries

| Feature | Status | Implementation |
|---------|--------|----------------|
| Custom dictionaries for auto-complete | 🔶 | LSP Server provides built-in + user symbols |

---

## File Dialogs

### File Open Dialog

### File Save Dialog

### Other Dialogs

| Feature | Status | Implementation |
|---------|--------|----------------|
| Color picker dialog | ☐ | Not yet implemented |

---

## Information & Dialogs

### Error Reporting

### Status Bar

| Feature | Status | Implementation |
|---------|--------|----------------|
| Compilation progress | ☐ | Not yet implemented |

---

## Wiki & Documentation

| Feature | Status | Implementation |
|---------|--------|----------------|
| Integrated help system | 🔶 | LSP Server hover (50+ built-in function docs) |
| Wiki page download/caching | ☐ | Not yet implemented |
| Context-sensitive help (F1) | 🔶 | LSP hover on mouse over |
| Keyword-to-Wiki links | ☐ | Not yet implemented |
| Update single page | ☐ | Not applicable |
| Batch update all pages | ☐ | Not applicable |
| Online Wiki access | ☐ | Not yet implemented |

---

## Visual Feedback

| Feature | Status | Implementation |
|---------|--------|----------------|
| Debug line highlighting | ☐ | Needs DAP implementation |
| Breakpoint indicators | ☐ | Needs DAP implementation |
| Progress indicators | 🔶 | Terminal output during build |

---

## Project Management

| Feature | Status | Implementation |
|---------|--------|----------------|
| Bookmark persistence per file | 🔶 | VSCode extension (Bookmarks) |

---

## Source Code Analysis

### Warning Detection

### Analysis Features

| Feature | Status | Implementation |
|---------|--------|----------------|
| Call stack analysis | ☐ | Needs runtime (DAP) |

---

## Export Capabilities

| Feature | Status | Implementation |
|---------|--------|----------------|
| HTML export with syntax highlighting | ☐ | Not yet implemented |
| RTF export with colors | ☐ | Not yet implemented |
| Discord code block export | ☐ | Not yet implemented |
| Forum code box export | ☐ | Not yet implemented |
| Wiki format export | ☐ | Not yet implemented |
| Wiki link generation in exports | ☐ | Not yet implemented |

---

## QB64Fresh Source File Structure

QB64Fresh uses a modern Rust-based architecture instead of the monolithic BASIC IDE:

| Component | Location | Purpose |
|-----------|----------|---------|
| **Compiler** | `QB64Fresh/src/` | Main compiler crate |
| → Lexer | `src/lexer/` | Tokenization (200+ token types via logos) |
| → Parser | `src/parser/` | AST generation (Pratt + recursive descent) |
| → Semantic | `src/semantic/` | Type checking, symbol resolution |
| → Codegen | `src/codegen/` | C code generation backend |
| → Preprocessor | `src/preprocessor.rs` | $INCLUDE directive handling |
| **LSP Server** | `QB64Fresh/src/lsp/` | Language Server Protocol (~2000 lines) |
| **VSCode Extension** | `vscode-qb64fresh/` | Editor integration (~320 lines TypeScript) |
| → Syntax Grammar | `syntaxes/qb64fresh.tmLanguage.json` | TextMate grammar for highlighting |
| → Language Config | `language-configuration.json` | Brackets, comments, indentation |
| → Snippets | `snippets/qb64fresh.json` | Code templates |
| **Formatter** | `QB64Fresh/tools/fmt/` | Code formatting tool |
| **Linter** | `QB64Fresh/tools/lint/` | Code quality analysis |
| → Correctness | `rules/correctness.rs` | Error detection rules |
| → Style | `rules/style.rs` | Style enforcement rules |
| → Unused | `rules/unused.rs` | Dead code detection |

---

## Implementation Summary

### By Component

| Component | Implemented Features | Pending Features |
|-----------|---------------------|------------------|
| **Compiler** | Lexer, Parser, Type System, C Codegen, Preprocessor | LLVM backend (future) |
| **LSP Server** | Diagnostics, Hover, Definition, Completion, Symbols, References, Signature Help, Inlay Hints | Rename refactoring, Workspace symbols |
| **VSCode Extension** | Syntax highlighting, Build/Run, Keybindings, Settings | Debug adapter (DAP) |
| **Formatter** | Keyword case, Indentation, Spacing | $FORMAT metacommand |
| **Linter** | Unused detection, Style checks, Correctness hints | More rules |

### Feature Categories

| Category | ☑ Implemented | 🔶 Partial | ☐ Not Implemented |
|----------|---------------|------------|-------------------|
| File Operations | 8 | 0 | 5 |
| Edit Operations | 10 | 0 | 0 |
| View Features | 11 | 0 | 0 |
| Search Features | 14 | 4 | 5 |
| Run/Build | 2 | 0 | 9 |
| Debugging | 0 | 0 | 22 |
| Options/Settings | 29 | 0 | 5 |
| Tools | 0 | 0 | 11 |
| Help | 0 | 2 | 12 |
| Editor Features | 32 | 3 | 1 |
| Visual Feedback | 9 | 1 | 2 |
| Analysis | 6 | 0 | 1 |

### Key Differences from QB64PE

1. **Architecture**: LSP-based (editor-agnostic) vs. monolithic GUI
2. **Language**: Rust vs. BASIC
3. **Editor**: VSCode (or any LSP client) vs. built-in editor
4. **Debugging**: Needs DAP implementation vs. TCP/IP based
5. **Configuration**: VSCode settings vs. config.ini

---

## Notes

- **Total features identified**: 200+ distinct features from QB64PE
- **Legend**: ☑ = Implemented | 🔶 = Partial/Different approach | ☐ = Not implemented
- Features provided by VSCode are marked as implemented since they work with the QB64Fresh extension
- Debugging features require a Debug Adapter Protocol (DAP) implementation
- Many VSCode native features (undo, copy, paste, etc.) work out of the box

---

*Document updated to track QB64Fresh implementation status*
