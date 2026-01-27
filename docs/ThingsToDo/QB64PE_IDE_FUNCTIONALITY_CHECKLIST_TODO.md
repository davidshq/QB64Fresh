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

Legend: ☑ = Implemented | 📋 = Plan to implement | 🔶 = Partial/Different approach | 💭 = Maybe someday | ☐ = Not implemented | ❌ = Not planned / Not applicable

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
| → Skip comments    | -        | 💭      | VSCode has some regex support - may not need custom implementation |
| → Skip strings     | -        | 💭      | VSCode has some regex support - may not need custom implementation |
| → Only in comments | -        | 💭      | VSCode has some regex support - may not need custom implementation |
| → Only in strings  | -        | 💭      | VSCode has some regex support - may not need custom implementation |
| → Invert search    | -        | 💭      | VSCode has some regex support - may not need custom implementation |

---

## Run Menu

| Feature                            | Shortcut | Status | Implementation |
| ---------------------------------- | -------- | ------ | -------------- |
| Run Only (No EXE save)             | -        | 📋      | Core workflow feature - compile and run without saving executable |
| Output EXE to Source Folder toggle | -        | 📋      | Common workflow need - control output location |
| Generate License For EXE           | -        | 💭      | Nice-to-have but not essential |
| Modify COMMAND$ function parameter | -        | 💭      | Niche feature - may not be needed |
| Change Terminal (Linux only)       | -        | 💭      | Platform-specific - low priority |
| Set Default EXE Folder             | -        | 📋      | Core workflow feature - configure default build output |
| **Configure Logging**              |          |        |                |
| → Enable/disable logging           | -        | 💭      | Unnecessary complexity for most users |
| → Set log level                    | -        | 💭      | Unnecessary complexity for most users |
| → Set log scopes                   | -        | 💭      | Unnecessary complexity for most users |
| → Set log handlers                 | -        | 💭      | Unnecessary complexity for most users |
| → Configure log file name          | -        | 💭      | Unnecessary complexity for most users |

---

## Debug Menu

| Feature                               | Shortcut | Status | Implementation                                                                 |
| ------------------------------------- | -------- | ------ | ------------------------------------------------------------------------------ |
| Start Paused                          | F7 / F8  | 🔶     | qb64fresh-debug supports; runtime IPC in progress                               |
| Toggle Breakpoint                     | F9       | 🔶     | DAP `setBreakpoints` implemented (`tools/debug/src/server.rs:535`), needs runtime integration |
| Clear All Breakpoints                 | F10      | 🔶     | tools/debug                                                                    |
| Toggle Skip Line                      | Ctrl+P   | 💭      | Niche debugging feature - low priority |
| → Skip marked lines during debug      | -        | 💭      | Niche debugging feature - low priority |
| Unskip All Lines                      | Ctrl+F10 | 💭      | Niche debugging feature - low priority |
| **Watch List...**                     | F4       | 🔶     | watch.rs, values.rs; runtime variable read in progress                          |
| → Add variables to watch              | -        | 🔶     | tools/debug                                                                    |
| → Remove variables from watch         | -        | 🔶     | tools/debug                                                                    |
| → Display format (DEC, HEX, BIN, OCT) | -        | 💭      | Nice-to-have for low-level debugging but not blocking |
| → Watch array elements                | -        | 🔶     | watch.rs parses; runtime read in progress                                      |
| → Watch UDT members                   | -        | 🔶     | values.rs; runtime read in progress                                            |
| → Watchpoint conditions               | -        | 📋      | Core debugging feature - DAP supports data breakpoints |
| **Call Stack...**                     | F12      | 🔶     | frames.rs, DAP; VSCode UI ready, runtime integration in progress               |
| → View call stack history             | -        | 🔶     | tools/debug; VSCode Debug panel shows call stack                               |
| → Navigate through call stack         | -        | 🔶     | tools/debug; VSCode supports frame navigation                                  |
| Auto-add $DEBUG Metacommand toggle    | -        | ❌      | Not applicable (different architecture) |
| Output Watch List to Console toggle   | -        | 💭      | Nice-to-have but not essential |
| Set Base TCP/IP Port Number           | -        | ❌      | Not applicable (DAP-based)                                                     |
| Purge C++ Libraries                   | -        | ❌      | Not applicable (C backend)                                                     |

---

## Options Menu

| Feature                  | Shortcut | Status | Implementation              |
| ------------------------ | -------- | ------ | --------------------------- |
| **Code Layout...**       |          |        |                             |
| **Compiler Settings...** |          |        |                             |
| → Optimization level     | -        | 📋      | Essential for production builds - expose in VSCode settings |
| → Debug symbols          | -        | 📋      | Essential for debugging - expose in VSCode settings |
| → C++ compiler flags     | -        | 📋      | Important for advanced users - expose in VSCode settings |
| → Extra linker flags     | -        | 📋      | Important for advanced users - expose in VSCode settings |
| → System MinGW toggle    | -        | ❌      | Not applicable              |
| **Undo/History...**      |          |        |                             |
| Ignore Warnings toggle   | -        | 📋      | Useful for legacy code - expose in VSCode settings |

---

## Tools Menu

| Feature                             | Shortcut | Status | Implementation      |
| ----------------------------------- | -------- | ------ | ------------------- |
| **ASCII Chart**                     |          |        |                     |
| → Browse ASCII characters           | -        | 💭      | Nice-to-have but not essential |
| → Insert characters into code       | -        | 💭      | Nice-to-have but not essential |
| **Insert Quick Keycode**            | Ctrl+K   | 💭      | Niche feature - low priority |
| → Capture key codes                 | -        | 💭      | Niche feature - low priority |
| → Insert key code values            | -        | 💭      | Niche feature - low priority |
| **Library Explorer** (if installed) | Ctrl+L   | 💭      | Unclear value in QB64Fresh context |
| **Math Evaluator**                  |          |        |                     |
| → Calculate expressions             | -        | 💭      | Nice but not essential |
| → Display results                   | -        | 💭      | Nice but not essential |
| **RGB Color Mixer**                 |          |        |                     |
| → Mix colors visually               | -        | 💭      | Useful for graphics but not core functionality |
| → Generate \_RGB statements         | -        | 💭      | Useful for graphics but not core functionality |
| → Insert into code                  | -        | 💭      | Useful for graphics but not core functionality |

---

## Help Menu

| Feature                              | Shortcut | Status | Implementation                                     |
| ------------------------------------ | -------- | ------ | -------------------------------------------------- |
| View Help                            | Shift+F1 | 🔶     | LSP Server (hover provides built-in function docs) |
| → Last viewed article                | -        | 💭      | Nice-to-have but hover docs might be sufficient |
| → Scrollable help window             | -        | 💭      | Nice-to-have but hover docs might be sufficient |
| → Search within help                 | -        | 💭      | Nice-to-have but hover docs might be sufficient |
| Contents Page                        | -        | 💭      | Nice-to-have but hover docs might be sufficient |
| Keywords Index (alphabetical)        | -        | 💭      | Nice-to-have but hover docs might be sufficient |
| Keywords by Usage                    | -        | 💭      | Nice-to-have but hover docs might be sufficient |
| Metacommands Overview                | -        | 💭      | Nice-to-have but hover docs might be sufficient |
| Variable Types Overview              | -        | 💭      | Nice-to-have but hover docs might be sufficient |
| Update Current Page (from Wiki)      | -        | ❌      | Not applicable                                     |
| Update All Pages... (batch download) | -        | ❌      | Not applicable                                     |
| View Current Page On Wiki (browser)  | -        | 💭      | Nice-to-have but hover docs might be sufficient |

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
| New SUB...         | 💭      | Snippets might be sufficient |
| New FUNCTION...    | 💭      | Snippets might be sufficient |
| RGB Color Mixer... | 💭      | Useful for graphics but not core functionality |

---

## Keyboard Shortcuts

> **Note:** Keyboard shortcuts are documented in their respective menu sections above. See:
> - **Debug shortcuts** (F4, F7/F8, F9, F10, F12, Ctrl+P, Ctrl+F10): [Debug Menu](#debug-menu)
> - **Tools shortcuts** (Ctrl+K, Ctrl+L): [Tools Menu](#tools-menu)
> - **Help shortcuts** (Shift+F1): [Help Menu](#help-menu)

---

## Compilation & Execution

| Feature                           | Status | Implementation      |
| --------------------------------- | ------ | ------------------- |
| Line-by-line compilation progress | 📋      | UX improvement - show progress during build |
| Progress percentage display       | 📋      | UX improvement - show progress during build |
| License file generation           | 💭      | Nice-to-have but not essential |

---

## Debugging Features

> **Note:** QB64Fresh provides **qb64fresh-debug** (`tools/debug`): a DAP server with breakpoints, step, call stack, watch, and symbols. The compiler supports `--debug` (emits `qb_dbg_line`, `qb_dbg_enter_proc`/`qb_dbg_exit_proc`); the inline C runtime includes stubs. Full runtime–debugger IPC (launch/attach, variable inspection in running process) is in progress. See `tools/README.md`, `docs/adrs/ADR-0013-debugger-architecture.md`.
>
> **See [Debug Menu](#debug-menu) above for detailed feature status with shortcuts.**

### Additional Debug Features

| Feature                         | Status | Implementation                                   |
| ------------------------------- | ------ | ------------------------------------------------ |
| Conditional breakpoints         | 📋      | Core debugging feature - DAP infrastructure ready, needs implementation |
| Step execution (step, stepin, stepout) | 🔶     | tools/debug; runtime integration in progress |
| Debug console integration       | 🔶     | tools/debug interactive CLI; DAP in progress    |

---

## Configuration & Settings

> **Note:** Configuration is handled via VSCode settings (workspace/user settings.json). Most IDE settings from QB64pe map to VSCode's native configuration system.

---

## File Dialogs

> **Note:** File dialogs are provided by VSCode's native file picker (Ctrl+P for quick open, File menu for save/open).

| Feature             | Status | Implementation      |
| ------------------- | ------ | ------------------- |
| Color picker dialog | 💭      | Useful for graphics but not core functionality |

---

## Information & Dialogs

> **Note:** Error reporting is provided by LSP diagnostics (shown inline in editor). Status bar information is provided by VSCode.

| Feature              | Status | Implementation      |
| -------------------- | ------ | ------------------- |
| Compilation progress | 📋      | UX improvement - show progress during build |

---

## Wiki & Documentation

| Feature                     | Status | Implementation                                |
| --------------------------- | ------ | --------------------------------------------- |
| Integrated help system      | 🔶     | LSP Server hover (50+ built-in function docs) |
| Wiki page download/caching  | 💭      | Nice-to-have but hover docs might be sufficient |
| Context-sensitive help (F1) | 🔶     | LSP hover on mouse over                       |
| Keyword-to-Wiki links       | 💭      | Nice-to-have but hover docs might be sufficient |
| Update single page          | ❌      | Not applicable                                |
| Batch update all pages      | ❌      | Not applicable                                |
| Online Wiki access          | 💭      | Nice-to-have but hover docs might be sufficient |

---

## Visual Feedback

| Feature                 | Status | Implementation               |
| ----------------------- | ------ | ---------------------------- |
| Progress indicators     | 🔶     | Terminal output during build |

---

## Project Management

> **Note:** Project management is handled via VSCode workspace folders. Multi-file projects are supported via `$INCLUDE` directives.

## Source Code Analysis

> **Note:** Warning detection and static analysis are provided by the LSP server (diagnostics) and `qb64fresh-lint` tool.

| Feature             | Status | Implementation      |
| ------------------- | ------ | ------------------- |
| Call stack analysis | ☐      | Needs runtime (DAP) |

---

## Export Capabilities

| Feature                              | Status | Implementation      |
| ------------------------------------ | ------ | ------------------- |
| HTML export with syntax highlighting | 💭      | Nice-to-have for sharing code but not essential |
| RTF export with colors               | 💭      | Nice-to-have for documentation but not essential |
| Discord code block export            | 💭      | Niche use case - low priority |
| Forum code box export                | 💭      | Niche use case - low priority |
| Wiki format export                   | 💭      | Niche use case - low priority |
| Wiki link generation in exports      | 💭      | Niche use case - low priority |

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
| **VSCode Extension** | Syntax highlighting, Build/Run, Format-on-save, Lint-on-save, Keybindings, Rename, Workspace search (Ctrl+T), Settings | DAP client UI for qb64fresh-debug (launch config complete, breakpoints, step, variables) |
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

---

_Document updated to track QB64Fresh implementation status. Last updated: 2026-01-27._

**Note:** Completed items (☑) are moved to `docs/archive/IDE-FUNCTIONALITY-COMPLETED.md` to keep this checklist focused on pending work.
