# QB64pe IDE - Complete Functionality Checklist

This document provides a comprehensive checklist of all functionality available in the QB64pe Integrated Development Environment (IDE). The IDE source code spans approximately **22,464 lines** across multiple source files located in `source/ide/`.

## QB64Fresh Implementation Status

QB64Fresh uses a **modern LSP-based architecture** instead of a monolithic IDE. Features are provided by:

- **Compiler** (`qb64fresh`) - Lexer, parser, semantic analysis, C code generation; `--debug` emits breakpoint/line hooks
- **LSP Server** (`qb64fresh-lsp`) - IDE features via Language Server Protocol (diagnostics, hover, completion, go-to-definition, find refs, rename, workspace symbols, signature help, inlay hints)
- **VSCode Extension** (`vscode-qb64fresh`) - Editor integration, syntax highlighting, build/run, format-on-save, lint-on-save, rename, workspace search
- **Formatter** (`qb64fresh-fmt`) - Code formatting (keyword case, indentation, spacing, style presets)
- **Linter** (`qb64fresh-lint`) - Static analysis (correctness, style, unused)
- **Debugger** (`qb64fresh-debug`) - DAP server, breakpoints, step, call stack, symbols, watch; compiler `--debug` and runtime stubs. Runtime–debugger IPC integration in progress. See `tools/README.md`, `docs/adrs/ADR-0013-debugger-architecture.md`.

Legend: ☑ = Implemented | ☐ = Not implemented | 🔶 = Partial/Different approach

---

## Table of Contents

1. [Search Menu](#search-menu)
2. [Run Menu](#run-menu)
3. [Debug Menu](#debug-menu)
4. [Options Menu](#options-menu)
5. [Tools Menu](#tools-menu)
6. [Help Menu](#help-menu)
7. [Editor Features](#editor-features)
8. [Context Menu](#context-menu-right-click)
9. [Keyboard Shortcuts](#keyboard-shortcuts)
10. [Compilation & Execution](#compilation--execution)
11. [Debugging Features](#debugging-features)
12. [Configuration & Settings](#configuration--settings)
13. [File Dialogs](#file-dialogs)
14. [Information & Dialogs](#information--dialogs)
15. [Wiki & Documentation](#wiki--documentation)
16. [Visual Feedback](#visual-feedback)
17. [Project Management](#project-management)
18. [Source Code Analysis](#source-code-analysis)
19. [Export Capabilities](#export-capabilities)

---

## Search Menu

| Feature            | Shortcut | Status | Implementation |
| ------------------ | -------- | ------ | -------------- |
| → Skip comments    | -        | ☐      | -              |
| → Skip strings     | -        | ☐      | -              |
| → Only in comments | -        | ☐      | -              |
| → Only in strings  | -        | ☐      | -              |
| → Invert search    | -        | ☐      | -              |

---

## Run Menu

| Feature                            | Shortcut | Status | Implementation |
| ---------------------------------- | -------- | ------ | -------------- |
| Run Only (No EXE save)             | -        | ☐      | -              |
| Output EXE to Source Folder toggle | -        | ☐      | -              |
| Generate License For EXE           | -        | ☐      | -              |
| Modify COMMAND$ function parameter | -        | ☐      | -              |
| Change Terminal (Linux only)       | -        | ☐      | -              |
| Set Default EXE Folder             | -        | ☐      | -              |
| **Configure Logging**              |          |        |                |
| → Enable/disable logging           | -        | ☐      | -              |
| → Set log level                    | -        | ☐      | -              |
| → Set log scopes                   | -        | ☐      | -              |
| → Set log handlers                 | -        | ☐      | -              |
| → Configure log file name          | -        | ☐      | -              |

---

## Debug Menu

| Feature                               | Shortcut | Status | Implementation                                                                 |
| ------------------------------------- | -------- | ------ | ------------------------------------------------------------------------------ |
| Start Paused                          | F7 / F8  | 🔶     | qb64fresh-debug supports; runtime IPC in progress                               |
| Toggle Breakpoint                     | F9       | 🔶     | DAP/types in tools/debug; VSCode DAP client integration in progress            |
| → Set breakpoints on lines            | -        | 🔶     | tools/debug                                                                    |
| → Visual breakpoint indicators        | -        | 🔶     | Via DAP / editor when connected                                                 |
| Clear All Breakpoints                 | F10      | 🔶     | tools/debug                                                                    |
| Toggle Skip Line                      | Ctrl+P   | ☐      | Not yet implemented                                                            |
| → Skip marked lines during debug      | -        | ☐      | Not yet implemented                                                            |
| Unskip All Lines                      | Ctrl+F10 | ☐      | Not yet implemented                                                            |
| **Watch List...**                     | F4       | 🔶     | watch.rs, values.rs; runtime variable read in progress                          |
| → Add variables to watch              | -        | 🔶     | tools/debug                                                                    |
| → Remove variables from watch         | -        | 🔶     | tools/debug                                                                    |
| → Display format (DEC, HEX, BIN, OCT) | -        | ☐      | Not yet implemented                                                            |
| → Watch array elements                | -        | 🔶     | watch.rs parses; runtime read in progress                                      |
| → Watch UDT members                   | -        | 🔶     | values.rs; runtime read in progress                                            |
| → Watchpoint conditions               | -        | ☐      | Not yet implemented                                                            |
| **Call Stack...**                     | F12      | 🔶     | frames.rs, DAP; runtime integration in progress                                 |
| → View call stack history             | -        | 🔶     | tools/debug                                                                    |
| → Navigate through call stack         | -        | 🔶     | tools/debug                                                                    |
| Auto-add $DEBUG Metacommand toggle    | -        | ☐      | Not yet implemented                                                            |
| Output Watch List to Console toggle   | -        | ☐      | Not yet implemented                                                            |
| Set Base TCP/IP Port Number           | -        | ☐      | Not applicable (DAP-based)                                                     |
| Purge C++ Libraries                   | -        | ☐      | Not applicable (C backend)                                                     |

---

## Options Menu

| Feature                  | Shortcut | Status | Implementation              |
| ------------------------ | -------- | ------ | --------------------------- |
| **Code Layout...**       |          |        |                             |
| **Compiler Settings...** |          |        |                             |
| → Optimization level     | -        | ☐      | Not yet exposed in settings |
| → Debug symbols          | -        | ☐      | Not yet exposed             |
| → C++ compiler flags     | -        | ☐      | Not yet exposed             |
| → Extra linker flags     | -        | ☐      | Not yet exposed             |
| → System MinGW toggle    | -        | ☐      | Not applicable              |
| **Undo/History...**      |          |        |                             |
| Ignore Warnings toggle   | -        | ☐      | Not yet exposed             |

---

## Tools Menu

| Feature                             | Shortcut | Status | Implementation      |
| ----------------------------------- | -------- | ------ | ------------------- |
| **ASCII Chart**                     |          |        |                     |
| → Browse ASCII characters           | -        | ☐      | Not yet implemented |
| → Insert characters into code       | -        | ☐      | Not yet implemented |
| **Insert Quick Keycode**            | Ctrl+K   | ☐      | Not yet implemented |
| → Capture key codes                 | -        | ☐      | Not yet implemented |
| → Insert key code values            | -        | ☐      | Not yet implemented |
| **Library Explorer** (if installed) | Ctrl+L   | ☐      | Not yet implemented |
| **Math Evaluator**                  |          |        |                     |
| → Calculate expressions             | -        | ☐      | Not yet implemented |
| → Display results                   | -        | ☐      | Not yet implemented |
| **RGB Color Mixer**                 |          |        |                     |
| → Mix colors visually               | -        | ☐      | Not yet implemented |
| → Generate \_RGB statements         | -        | ☐      | Not yet implemented |
| → Insert into code                  | -        | ☐      | Not yet implemented |

---

## Help Menu

| Feature                              | Shortcut | Status | Implementation                                     |
| ------------------------------------ | -------- | ------ | -------------------------------------------------- |
| View Help                            | Shift+F1 | 🔶     | LSP Server (hover provides built-in function docs) |
| → Last viewed article                | -        | ☐      | Not yet implemented                                |
| → Scrollable help window             | -        | ☐      | Not yet implemented                                |
| → Search within help                 | -        | ☐      | Not yet implemented                                |
| Contents Page                        | -        | ☐      | Not yet implemented                                |
| Keywords Index (alphabetical)        | -        | ☐      | Not yet implemented                                |
| Keywords by Usage                    | -        | ☐      | Not yet implemented                                |
| Metacommands Overview                | -        | ☐      | Not yet implemented                                |
| Variable Types Overview              | -        | ☐      | Not yet implemented                                |
| Update Current Page (from Wiki)      | -        | ☐      | Not applicable                                     |
| Update All Pages... (batch download) | -        | ☐      | Not applicable                                     |
| View Current Page On Wiki (browser)  | -        | ☐      | Not yet implemented                                |

---

## Editor Features

- Syntax Highlighting
- Auto-completion

### Auto-formatting/Layout

| Feature                     | Status | Implementation                                                                                                                                 |
| --------------------------- | ------ | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| $FORMAT metacommand support | 🔶     | Parsed and emitted as no-op in codegen (IDE support). QB64pe’s `$FORMAT:ON/OFF` toggle maps to editor format-on-save; VSCode has this globally. |

- Code Navigation
- Bracket Handling
- Selection Features
- Clipboard Operations
- Mouse Support

---

## Context Menu (Right-Click)

| Feature            | Status | Implementation      |
| ------------------ | ------ | ------------------- |
| New SUB...         | ☐      | Not yet implemented |
| New FUNCTION...    | ☐      | Not yet implemented |
| RGB Color Mixer... | ☐      | Not yet implemented |

---

## Keyboard Shortcuts

| Shortcut | Function              | Status | Implementation                  |
| -------- | --------------------- | ------ | ------------------------------- |
| Shift+F1 | Help main page        | ☐      | Not yet implemented             |
| F4       | Watch list            | ☐      | Not yet implemented (needs DAP) |
| F7/F8    | Start paused (debug)  | ☐      | Not yet implemented             |
| F9       | Toggle breakpoint     | ☐      | Not yet implemented             |
| F10      | Clear all breakpoints | ☐      | Not yet implemented             |
| Ctrl+F10 | Unskip all lines      | ☐      | Not yet implemented             |
| F12      | Call stack            | ☐      | Not yet implemented             |
| Ctrl+K   | Quick keycode         | ☐      | Not yet implemented             |
| Ctrl+L   | Library explorer      | ☐      | Not yet implemented             |
| Ctrl+P   | Toggle skip line      | ☐      | Not yet implemented             |

---

## Compilation & Execution

| Feature                           | Status | Implementation      |
| --------------------------------- | ------ | ------------------- |
| Line-by-line compilation progress | ☐      | Not yet implemented |
| Progress percentage display       | ☐      | Not yet implemented |
| License file generation           | ☐      | Not yet implemented |

---

## Debugging Features

> **Note:** QB64Fresh provides **qb64fresh-debug** (`tools/debug`): a DAP server with breakpoints, step, call stack, watch, and symbols. The compiler supports `--debug` (emits `qb_dbg_line`, `qb_dbg_enter_proc`/`qb_dbg_exit_proc`); the inline C runtime includes stubs. Full runtime–debugger IPC (launch/attach, variable inspection in running process) is in progress. See `tools/README.md`, `docs/adrs/ADR-0013-debugger-architecture.md`.

### Breakpoints

| Feature                 | Status | Implementation                                               |
| ----------------------- | ------ | ------------------------------------------------------------ |
| Set/clear breakpoints   | 🔶     | tools/debug, DAP; compiler --debug emits hooks; runtime IPC in progress |
| Conditional breakpoints | ☐      | Not yet implemented                                          |
| Visual indicators       | 🔶     | Via DAP/editor when connected                                |

### Step Execution

| Feature           | Status | Implementation                                               |
| ----------------- | ------ | ------------------------------------------------------------ |
| Start paused mode | 🔶     | tools/debug; runtime integration in progress                 |
| Step execution    | 🔶     | tools/debug (step, stepin, stepout); runtime integration in progress |

### Variable Watch

| Feature                                  | Status | Implementation                                               |
| ---------------------------------------- | ------ | ------------------------------------------------------------ |
| Watch global variables                   | 🔶     | watch.rs, values.rs; runtime variable read in progress       |
| Watch local variables                    | 🔶     | tools/debug; runtime read in progress                        |
| Watch array elements                     | 🔶     | watch.rs; runtime read in progress                           |
| Watch UDT members                        | 🔶     | values.rs; runtime read in progress                          |
| Display format options (DEC/HEX/BIN/OCT) | ☐      | Not yet implemented                                          |
| Watchpoint conditions                    | ☐      | Not yet implemented                                          |

### Call Stack

| Feature                | Status | Implementation                                    |
| ---------------------- | ------ | ------------------------------------------------- |
| Call stack tracing     | 🔶     | frames.rs, qb_dbg_enter_proc/exit_proc; runtime integration in progress |
| Navigate through stack | 🔶     | tools/debug                                      |

### Other Debug Features

| Feature                         | Status | Implementation                                   |
| ------------------------------- | ------ | ------------------------------------------------ |
| Line skip (skip specific lines) | ☐      | Not yet implemented                              |
| $DEBUG metacommand              | ☐      | Not applicable (different architecture)          |
| TCP/IP debug communication      | ☐      | Not applicable (DAP-based)                       |
| Debug console integration       | 🔶     | tools/debug interactive CLI; DAP in progress    |

---

## Configuration & Settings

### Settings Persistence

### Window Settings

### History Settings

### Auto-save

### Dictionaries

| Feature                               | Status | Implementation                              |
| ------------------------------------- | ------ | ------------------------------------------- |
| Custom dictionaries for auto-complete | 🔶     | LSP Server provides built-in + user symbols |

---

## File Dialogs

### File Open Dialog

### File Save Dialog

### Other Dialogs

| Feature             | Status | Implementation      |
| ------------------- | ------ | ------------------- |
| Color picker dialog | ☐      | Not yet implemented |

---

## Information & Dialogs

### Error Reporting

### Status Bar

| Feature              | Status | Implementation      |
| -------------------- | ------ | ------------------- |
| Compilation progress | ☐      | Not yet implemented |

---

## Wiki & Documentation

| Feature                     | Status | Implementation                                |
| --------------------------- | ------ | --------------------------------------------- |
| Integrated help system      | 🔶     | LSP Server hover (50+ built-in function docs) |
| Wiki page download/caching  | ☐      | Not yet implemented                           |
| Context-sensitive help (F1) | 🔶     | LSP hover on mouse over                       |
| Keyword-to-Wiki links       | ☐      | Not yet implemented                           |
| Update single page          | ☐      | Not applicable                                |
| Batch update all pages      | ☐      | Not applicable                                |
| Online Wiki access          | ☐      | Not yet implemented                           |

---

## Visual Feedback

| Feature                 | Status | Implementation               |
| ----------------------- | ------ | ---------------------------- |
| Debug line highlighting | ☐      | Needs DAP implementation     |
| Breakpoint indicators   | ☐      | Needs DAP implementation     |
| Progress indicators     | 🔶     | Terminal output during build |

---

## Project Management

| Feature                       | Status | Implementation               |
| ----------------------------- | ------ | ---------------------------- |
| Bookmark persistence per file | 🔶     | VSCode extension (Bookmarks) |

---

## Source Code Analysis

### Warning Detection

### Analysis Features

| Feature             | Status | Implementation      |
| ------------------- | ------ | ------------------- |
| Call stack analysis | ☐      | Needs runtime (DAP) |

---

## Export Capabilities

| Feature                              | Status | Implementation      |
| ------------------------------------ | ------ | ------------------- |
| HTML export with syntax highlighting | ☐      | Not yet implemented |
| RTF export with colors               | ☐      | Not yet implemented |
| Discord code block export            | ☐      | Not yet implemented |
| Forum code box export                | ☐      | Not yet implemented |
| Wiki format export                   | ☐      | Not yet implemented |
| Wiki link generation in exports      | ☐      | Not yet implemented |

---

## QB64Fresh Source File Structure

QB64Fresh uses a modern Rust-based architecture instead of the monolithic BASIC IDE:

| Component            | Location                             | Purpose                                    |
| -------------------- | ------------------------------------ | ------------------------------------------ |
| **Compiler**         | `QB64Fresh/src/`                     | Main compiler crate                        |
| → Lexer              | `src/lexer/`                         | Tokenization (200+ token types via logos)  |
| → Parser             | `src/parser/`                        | AST generation (Pratt + recursive descent) |
| → Semantic           | `src/semantic/`                      | Type checking, symbol resolution           |
| → Codegen            | `src/codegen/`                       | C code generation backend                  |
| → Preprocessor       | `src/preprocessor.rs`                | $INCLUDE directive handling                |
| **LSP Server**       | `QB64Fresh/src/lsp/`                 | Language Server Protocol (~2000 lines)     |
| **VSCode Extension** | `vscode-qb64fresh/`                  | Editor integration (~320 lines TypeScript) |
| → Syntax Grammar     | `syntaxes/qb64fresh.tmLanguage.json` | TextMate grammar for highlighting          |
| → Language Config    | `language-configuration.json`        | Brackets, comments, indentation            |
| → Snippets           | `snippets/qb64fresh.json`            | Code templates                             |
| **Formatter**        | `QB64Fresh/tools/fmt/`               | Code formatting tool                       |
| **Linter**           | `QB64Fresh/tools/lint/`              | Code quality analysis                      |
| → Correctness        | `rules/correctness.rs`               | Error detection rules                      |
| → Style              | `rules/style.rs`                     | Style enforcement rules                    |
| → Unused             | `rules/unused.rs`                    | Dead code detection                        |
| **Debugger**         | `QB64Fresh/tools/debug/`             | DAP server, breakpoints, step, call stack   |
| → DAP, server        | `src/dap.rs`, `server.rs`           | Debug Adapter Protocol                     |
| → Symbols, values    | `symbols.rs`, `values.rs`           | Variable/types, watch parsing              |
| → Frames, sources    | `frames.rs`, `sources.rs`           | Call stack, $INCLUDE-aware source map      |
| → watch              | `watch.rs`                           | Watch expressions                          |

---

## Implementation Summary

### By Component

| Component            | Implemented Features                                                                                  | Pending / Partial                                                                  |
| -------------------- | ----------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------- |
| **Compiler**         | Lexer, Parser, Type System, C Codegen, Preprocessor, `--debug` (qb_dbg_line, qb_dbg_enter/exit_proc)  | LLVM backend (future)                                                               |
| **LSP Server**       | Diagnostics, Hover, Go-to-definition, Find references, Rename, Completion, Symbols, References, Signature Help, Inlay Hints | -                                                                                   |
| **VSCode Extension** | Syntax highlighting, Build/Run, Format-on-save, Lint-on-save, Keybindings, Rename, Workspace search (Ctrl+T), Settings | DAP client UI for qb64fresh-debug (launch config in progress)                       |
| **Formatter**        | Keyword case, Indentation, Spacing, Style presets (default, minimal, qb64, pretty)                   | $FORMAT:ON/OFF toggle (parsed as no-op; IDE toggle = VSCode format-on-save)         |
| **Linter**           | Unused detection, Style checks, Correctness hints                                                     | More rules                                                                          |
| **Debugger**         | tools/debug: DAP server, breakpoints, step, call stack, symbols, values, frames, watch, sources       | Runtime–debugger IPC, variable inspection in running process; VSCode DAP client     |

### Feature Categories

| Category         | ☑ Implemented | 🔶 Partial | ☐ Not Implemented |
| ---------------- | ------------- | ---------- | ----------------- |
| File Operations  | 8             | 0          | 5                 |
| Edit Operations  | 10            | 0          | 0                 |
| View Features    | 11            | 0          | 0                 |
| Search Features  | 14            | 4          | 5                 |
| Run/Build        | 2             | 0          | 9                 |
| Debugging        | 0             | 12         | 10                |
| Options/Settings | 29            | 0          | 5                 |
| Tools            | 0             | 0          | 11                |
| Help             | 0             | 2          | 12                |
| Editor Features  | 32            | 4          | 0                 |
| Visual Feedback  | 9             | 2          | 1                 |
| Analysis         | 6             | 0          | 1                 |

### Key Differences from QB64PE

1. **Architecture**: LSP-based (editor-agnostic) vs. monolithic GUI
2. **Language**: Rust vs. BASIC
3. **Editor**: VSCode (or any LSP client) vs. built-in editor
4. **Debugging**: DAP-based (qb64fresh-debug); compiler `--debug` and runtime stubs. Runtime–debugger IPC in progress. vs. QB64pe TCP/IP.
5. **Configuration**: VSCode settings vs. config.ini

---

## Notes

- **Total features identified**: 200+ distinct features from QB64PE
- **Legend**: ☑ = Implemented | 🔶 = Partial/Different approach | ☐ = Not implemented
- Features provided by VSCode are marked as implemented since they work with the QB64Fresh extension
- Debugging features require a Debug Adapter Protocol (DAP) implementation
- Many VSCode native features (undo, copy, paste, etc.) work out of the box

---

_Document updated to track QB64Fresh implementation status. Last updated: 2026-01-25._
