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

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| New | Ctrl+N | ☑ | VSCode native |
| Open | Ctrl+O | ☑ | VSCode native |
| Recent Files submenu | - | ☑ | VSCode native |
| Clear Recent Files List | - | ☑ | VSCode native |
| Save | Ctrl+S | ☑ | VSCode native |
| Save As... | - | ☑ | VSCode native |
| Export As... | - | ☐ | - |
| → HTML (.htm) with syntax highlighting | - | ☐ | - |
| → Rich Text (.rtf) with colors | - | ☐ | - |
| → Discord codebox (to clipboard) | - | ☐ | - |
| → Forum codebox (to clipboard) | - | ☐ | - |
| → Wiki example (to clipboard) | - | ☐ | - |
| Auto-save/Backup functionality | - | ☑ | VSCode native |
| Undo/Restore from backup | - | ☑ | VSCode native (Timeline view) |

---

## Edit Menu

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| Undo | Ctrl+Z | ☑ | VSCode native |
| Redo | Ctrl+Y | ☑ | VSCode native |
| Cut | Ctrl+X / Shift+Delete | ☑ | VSCode native |
| Copy | Ctrl+C / Ctrl+Insert | ☑ | VSCode native |
| Paste | Ctrl+V / Shift+Insert | ☑ | VSCode native |
| Select All | Ctrl+A | ☑ | VSCode native |
| Comment (Add REM) | Ctrl+R | ☑ | VSCode native (Ctrl+/) |
| Toggle Comment | Ctrl+T | ☑ | VSCode native (Ctrl+/) |
| Uncomment | Ctrl+Shift+R | ☑ | VSCode native (Ctrl+/) |
| Duplicate Line | Ctrl+D | ☑ | VSCode native (Alt+Shift+↓) |

---

## View Menu

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| SUBs/FUNCTIONs List | F2 | ☑ | LSP Server (documentSymbol) + VSCode Outline |
| → Filter by name | - | ☑ | VSCode Outline (Ctrl+Shift+O) |
| → Jump to selected sub/function | - | ☑ | LSP Server (definition) |
| → Sort options | - | ☑ | VSCode Outline view |
| Line Numbers Display | - | ☑ | VSCode native |
| → Show/Hide line numbers | - | ☑ | VSCode settings |
| → Line number background color | - | ☑ | VSCode theme |
| → Separator line | - | ☑ | VSCode native |
| Compiler Warnings | Ctrl+W | ☑ | LSP Server (diagnostics) + Problems panel |
| → View warning list | - | ☑ | VSCode Problems panel |
| → Jump to warnings in code | - | ☑ | LSP diagnostics click-to-navigate |

---

## Search Menu

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| Find... | Ctrl+F3 | ☑ | VSCode native (Ctrl+F) |
| → Case-sensitive search | - | ☑ | VSCode native |
| → Whole word search | - | ☑ | VSCode native |
| → Search backwards | - | ☑ | VSCode native (Shift+Enter) |
| → Skip comments | - | ☐ | - |
| → Skip strings | - | ☐ | - |
| → Only in comments | - | ☐ | - |
| → Only in strings | - | ☐ | - |
| → Invert search | - | ☐ | - |
| Repeat Last Find | F3 / Shift+F3 | ☑ | VSCode native |
| Change/Replace... | Alt+F3 | ☑ | VSCode native (Ctrl+H) |
| → Find and replace with confirmation | - | ☑ | VSCode native |
| → Replace all option | - | ☑ | VSCode native |
| Clear Search History | - | ☑ | VSCode native |
| Quick Navigation (Back arrow) | - | ☑ | VSCode (Alt+←) |
| → Navigate to previous positions | - | ☑ | VSCode native |
| **Bookmarks** | | | |
| Add/Remove Bookmark | Alt+Left | 🔶 | VSCode extension (Bookmarks) |
| Next Bookmark | Alt+Down | 🔶 | VSCode extension (Bookmarks) |
| Previous Bookmark | Alt+Up | 🔶 | VSCode extension (Bookmarks) |
| Bookmark persistence (saved to file) | - | 🔶 | VSCode extension (Bookmarks) |
| Go To Line... | Ctrl+G | ☑ | VSCode native |

---

## Run Menu

| Feature | Shortcut | Status | Implementation |
|---------|----------|--------|----------------|
| Start Compilation & Run | F5 | ☑ | VSCode Extension (qb64fresh.buildAndRun) |
| Run Only (No EXE save) | - | ☐ | - |
| Make EXE Only | F11 | ☑ | VSCode Extension (qb64fresh.build, Ctrl+Shift+B) |
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
| **Display... (Screen & Font)** | | | |
| → Screen resolution/width/height | - | ☑ | VSCode native |
| → Font selection (built-in 8 or 16) | - | ☑ | VSCode settings |
| → Custom TTF font support | - | ☑ | VSCode settings |
| → Font size adjustment | - | ☑ | VSCode settings |
| → Window position auto-restore | - | ☑ | VSCode native |
| **IDE Colors...** | | | |
| → Color scheme selection | - | ☑ | VSCode themes |
| → Customize keyword colors | - | ☑ | VSCode theme + TextMate grammar |
| → Customize comment colors | - | ☑ | VSCode theme + TextMate grammar |
| → Customize string/quote colors | - | ☑ | VSCode theme + TextMate grammar |
| → Customize number colors | - | ☑ | VSCode theme + TextMate grammar |
| → Customize text colors | - | ☑ | VSCode theme |
| → Customize background color | - | ☑ | VSCode theme |
| → Customize bracket highlight | - | ☑ | VSCode settings |
| → Customize error indicator | - | ☑ | VSCode settings |
| → Save custom color schemes | - | ☑ | VSCode themes |
| **Code Layout...** | | | |
| → Auto-format toggle | - | ☑ | Formatter (qb64fresh-fmt) |
| → Auto-indent toggle | - | ☑ | VSCode + Formatter |
| → Indent size configuration | - | ☑ | Formatter config |
| → Keyword style options | - | ☑ | Formatter (KeywordCase enum) |
| **Compiler Settings...** | | | |
| → Optimization level | - | ☐ | Not yet exposed in settings |
| → Debug symbols | - | ☐ | Not yet exposed |
| → C++ compiler flags | - | ☐ | Not yet exposed |
| → Extra linker flags | - | ☐ | Not yet exposed |
| → System MinGW toggle | - | ☐ | Not applicable |
| **Language...** | | | |
| → Code page selection | - | ☑ | Compiler (auto-detects encoding) |
| → TTF font language support | - | ☑ | VSCode native |
| **Undo/History...** | | | |
| → Undo limit configuration | - | ☑ | VSCode settings |
| → Recent files limit | - | ☑ | VSCode settings |
| → Search history limit | - | ☑ | VSCode native |
| Syntax Highlighter toggle | - | ☑ | VSCode Extension (TextMate grammar) |
| Swap Mouse Buttons toggle | - | ☑ | VSCode settings |
| Cursor After Paste toggle | - | ☑ | VSCode settings |
| Auto-Close Brackets toggle | - | ☑ | VSCode + language-configuration.json |
| Syntax Checker (real-time errors) | - | ☑ | LSP Server (real-time diagnostics) |
| Ignore Warnings toggle | - | ☐ | Not yet exposed |
| GUI Dialogs toggle (native dialogs) | - | ☑ | VSCode native |

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

### Syntax Highlighting

| Feature | Status | Implementation |
|---------|--------|----------------|
| Keywords highlighting | ☑ | VSCode Extension (TextMate grammar) |
| Comments highlighting | ☑ | VSCode Extension (TextMate grammar) |
| Strings highlighting | ☑ | VSCode Extension (TextMate grammar) |
| Numbers highlighting | ☑ | VSCode Extension (TextMate grammar) |
| Metacommands highlighting | ☑ | VSCode Extension (TextMate grammar) |
| Customizable colors | ☑ | VSCode themes |
| Real-time highlighting (toggleable) | ☑ | VSCode native |

### Auto-completion

| Feature | Status | Implementation |
|---------|--------|----------------|
| Keyword suggestions | ☑ | LSP Server (completion) |
| Variable name completion | ☑ | LSP Server (completion from symbol table) |
| Function/SUB name completion | ☑ | LSP Server (completion) |
| Context-aware suggestions | ☑ | LSP Server (40+ built-in function signatures) |

### Auto-formatting/Layout

| Feature | Status | Implementation |
|---------|--------|----------------|
| Keyword case normalization | ☑ | Formatter (KeywordCase: UPPER/lower/Title) |
| Automatic indentation | ☑ | Formatter + VSCode |
| Smart spacing | ☑ | Formatter (operator spacing, keyword spacing) |
| $FORMAT metacommand support | ☐ | Not yet implemented |

### Code Navigation

| Feature | Status | Implementation |
|---------|--------|----------------|
| Go to line | ☑ | VSCode native (Ctrl+G) |
| Quick SUB/FUNCTION jump (F2) | ☑ | LSP Server (documentSymbol) + VSCode Outline |
| Bookmarks | 🔶 | VSCode extension (Bookmarks) |
| Quick navigation history | ☑ | VSCode (Alt+←/→) |
| Find and go to | ☑ | LSP Server (definition) |

### Bracket Handling

| Feature | Status | Implementation |
|---------|--------|----------------|
| Matching bracket highlighting | ☑ | VSCode + language-configuration.json |
| Multi-line bracket pairs | ☑ | VSCode native |
| Auto-close brackets | ☑ | language-configuration.json |

### Selection Features

| Feature | Status | Implementation |
|---------|--------|----------------|
| Click-and-drag selection | ☑ | VSCode native |
| Shift+click selection | ☑ | VSCode native |
| Ctrl+A select all | ☑ | VSCode native |
| Double-click word selection | ☑ | VSCode native |

### Clipboard Operations

| Feature | Status | Implementation |
|---------|--------|----------------|
| Copy/cut/paste | ☑ | VSCode native |
| Clipboard history (recent items) | ☑ | VSCode native (Ctrl+Shift+V) |
| Format on paste | ☑ | VSCode settings |

### Mouse Support

| Feature | Status | Implementation |
|---------|--------|----------------|
| Click positioning | ☑ | VSCode native |
| Mouse wheel scrolling | ☑ | VSCode native |
| Right-click context menu | ☑ | VSCode native |
| Mouse button swap option | ☑ | VSCode settings |

---

## Context Menu (Right-Click)

| Feature | Status | Implementation |
|---------|--------|----------------|
| Cut | ☑ | VSCode native |
| Copy | ☑ | VSCode native |
| Paste | ☑ | VSCode native |
| New SUB... | ☐ | Not yet implemented |
| New FUNCTION... | ☐ | Not yet implemented |
| RGB Color Mixer... | ☐ | Not yet implemented |
| Find (F3) | ☑ | VSCode native |
| Replace (Alt+F3) | ☑ | VSCode native |

---

## Keyboard Shortcuts

| Shortcut | Function | Status | Implementation |
|----------|----------|--------|----------------|
| F1 | Context help (word at cursor) | 🔶 | LSP hover (on mouse hover) |
| Shift+F1 | Help main page | ☐ | Not yet implemented |
| F2 | SUBs/FUNCTIONs list | ☑ | VSCode Outline (Ctrl+Shift+O) |
| F3 | Find next | ☑ | VSCode native |
| Shift+F3 | Find previous | ☑ | VSCode native |
| Ctrl+F3 | Find dialog | ☑ | VSCode native (Ctrl+F) |
| Alt+F3 | Replace dialog | ☑ | VSCode native (Ctrl+H) |
| F4 | Watch list | ☐ | Not yet implemented (needs DAP) |
| F5 | Compile and run | ☑ | VSCode Extension (qb64fresh.buildAndRun) |
| F6 | Switch windows (editor/help) | ☑ | VSCode native |
| F7/F8 | Start paused (debug) | ☐ | Not yet implemented |
| F9 | Toggle breakpoint | ☐ | Not yet implemented |
| F10 | Clear all breakpoints | ☐ | Not yet implemented |
| Ctrl+F10 | Unskip all lines | ☐ | Not yet implemented |
| F11 | Make EXE only | ☑ | VSCode Extension (Ctrl+Shift+B) |
| F12 | Call stack | ☐ | Not yet implemented |
| Ctrl+A | Select all | ☑ | VSCode native |
| Ctrl+C | Copy | ☑ | VSCode native |
| Ctrl+D | Duplicate line | ☑ | VSCode native |
| Ctrl+G | Go to line | ☑ | VSCode native |
| Ctrl+K | Quick keycode | ☐ | Not yet implemented |
| Ctrl+L | Library explorer | ☐ | Not yet implemented |
| Ctrl+N | New file | ☑ | VSCode native |
| Ctrl+O | Open file | ☑ | VSCode native |
| Ctrl+P | Toggle skip line | ☐ | Not yet implemented |
| Ctrl+R | Comment | ☑ | VSCode (Ctrl+/) |
| Ctrl+S | Save | ☑ | VSCode native |
| Ctrl+T | Toggle comment | ☑ | VSCode (Ctrl+/) |
| Ctrl+V | Paste | ☑ | VSCode native |
| Ctrl+W | Compiler warnings | ☑ | Problems panel |
| Ctrl+X | Cut | ☑ | VSCode native |
| Ctrl+Y | Redo | ☑ | VSCode native |
| Ctrl+Z | Undo | ☑ | VSCode native |
| Ctrl+Shift+R | Uncomment | ☑ | VSCode (Ctrl+/) |
| Alt+Left | Add/remove bookmark | 🔶 | VSCode extension (Bookmarks) |
| Alt+Up | Previous bookmark | 🔶 | VSCode extension (Bookmarks) |
| Alt+Down | Next bookmark | 🔶 | VSCode extension (Bookmarks) |
| Shift+Delete | Cut | ☑ | VSCode native |
| Ctrl+Insert | Copy | ☑ | VSCode native |
| Shift+Insert | Paste | ☑ | VSCode native |

---

## Compilation & Execution

| Feature | Status | Implementation |
|---------|--------|----------------|
| Pre-compilation syntax check | ☑ | LSP Server (real-time diagnostics) |
| Real-time error detection | ☑ | LSP Server (lexer/parser/semantic errors) |
| Line-by-line compilation progress | ☐ | Not yet implemented |
| Progress percentage display | ☐ | Not yet implemented |
| C++ compilation (MinGW/GCC/Clang) | ☑ | Compiler (C backend + external CC) |
| EXE output to QB64pe folder | ☐ | Not yet configurable |
| EXE output to source folder | ☑ | Compiler default behavior |
| Console output capture | ☑ | VSCode Extension (terminal output) |
| Program launch | ☑ | VSCode Extension (qb64fresh.run) |
| Process termination | ☑ | VSCode terminal |
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

| Feature | Status | Implementation |
|---------|--------|----------------|
| config.ini file handling | ☑ | VSCode settings.json |
| Per-instance settings | ☑ | VSCode workspace settings |
| Global settings | ☑ | VSCode user settings |

### Window Settings

| Feature | Status | Implementation |
|---------|--------|----------------|
| Window position/size persistence | ☑ | VSCode native |
| Font settings persistence | ☑ | VSCode settings |
| Color scheme persistence | ☑ | VSCode themes |

### History Settings

| Feature | Status | Implementation |
|---------|--------|----------------|
| Recent files list | ☑ | VSCode native |
| Search history | ☑ | VSCode native |
| Bookmark persistence | 🔶 | VSCode extension (Bookmarks) |

### Auto-save

| Feature | Status | Implementation |
|---------|--------|----------------|
| Auto-save functionality | ☑ | VSCode native |
| Crash recovery | ☑ | VSCode native (Timeline + git) |
| Backup files | ☑ | VSCode native |

### Dictionaries

| Feature | Status | Implementation |
|---------|--------|----------------|
| Custom dictionaries for auto-complete | 🔶 | LSP Server provides built-in + user symbols |

---

## File Dialogs

### File Open Dialog

| Feature | Status | Implementation |
|---------|--------|----------------|
| Native GUI dialogs (optional) | ☑ | VSCode native |
| Recent file shortcuts | ☑ | VSCode native |
| Multiple file type filters | ☑ | VSCode native |

### File Save Dialog

| Feature | Status | Implementation |
|---------|--------|----------------|
| Directory navigation | ☑ | VSCode native |
| New folder creation | ☑ | VSCode native |
| File overwrite confirmation | ☑ | VSCode native |

### Other Dialogs

| Feature | Status | Implementation |
|---------|--------|----------------|
| Color picker dialog | ☐ | Not yet implemented |
| Folder selection dialog | ☑ | VSCode native |

---

## Information & Dialogs

### Message Boxes

| Feature | Status | Implementation |
|---------|--------|----------------|
| Information dialogs | ☑ | VSCode native |
| Warning dialogs | ☑ | VSCode native |
| Confirmation dialogs (Yes/No) | ☑ | VSCode native |
| Multi-button dialogs | ☑ | VSCode native |

### Input Dialogs

| Feature | Status | Implementation |
|---------|--------|----------------|
| Text input | ☑ | VSCode native (Quick Input) |
| Numeric input validation | ☑ | VSCode native |
| Multi-line input | ☑ | VSCode native |

### Error Reporting

| Feature | Status | Implementation |
|---------|--------|----------------|
| Compile error display | ☑ | LSP Server (diagnostics) |
| IDE error display | ☑ | VSCode Problems panel |
| Error line highlighting | ☑ | LSP diagnostics (squiggly underlines) |
| Error navigation | ☑ | Click diagnostic to navigate |

### Status Bar

| Feature | Status | Implementation |
|---------|--------|----------------|
| Compilation progress | ☐ | Not yet implemented |
| Cursor position | ☑ | VSCode native |
| Selection length | ☑ | VSCode native |
| Line/column display | ☑ | VSCode native |
| Warning indicators | ☑ | VSCode Problems panel badge |

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
| Syntax highlighting | ☑ | VSCode Extension (TextMate grammar) |
| Bracket matching/highlighting | ☑ | VSCode + language-configuration.json |
| Current line highlighting | ☑ | VSCode native |
| Error line highlighting | ☑ | LSP diagnostics |
| Debug line highlighting | ☐ | Needs DAP implementation |
| Breakpoint indicators | ☐ | Needs DAP implementation |
| Selection highlighting | ☑ | VSCode native |
| Status messages | ☑ | VSCode status bar |
| Progress indicators | 🔶 | Terminal output during build |
| Window title updates | ☑ | VSCode native |
| Cursor position indicators | ☑ | VSCode native |

---

## Project Management

| Feature | Status | Implementation |
|---------|--------|----------------|
| New project creation | ☑ | VSCode native (workspace) |
| Open existing projects | ☑ | VSCode native |
| Recent projects list | ☑ | VSCode native |
| Project save/auto-save | ☑ | VSCode native |
| SUB/FUNCTION organization view | ☑ | LSP Server (documentSymbol) + Outline |
| Multi-file support (via $INCLUDE) | ☑ | Compiler (preprocessor.rs) |
| Bookmark persistence per file | 🔶 | VSCode extension (Bookmarks) |

---

## Source Code Analysis

### Warning Detection

| Feature | Status | Implementation |
|---------|--------|----------------|
| Unused variables | ☑ | Linter (qb64fresh-lint, unused.rs) |
| Unused SUBs/FUNCTIONs | ☑ | Linter (qb64fresh-lint, unused.rs) |
| Undefined variables | ☑ | LSP Server + Linter (correctness.rs) |
| Type mismatches | ☑ | Compiler (semantic/checker) |

### Analysis Features

| Feature | Status | Implementation |
|---------|--------|----------------|
| Real-time syntax checking | ☑ | LSP Server (parser diagnostics) |
| Variable scope analysis | ☑ | Compiler (semantic/symbols.rs) |
| Call stack analysis | ☐ | Needs runtime (DAP) |
| Compiler diagnostics | ☑ | LSP Server (lexer/parser/semantic errors) |

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
| UTF-8 encoding for extended ASCII | ☑ | Compiler (supports UTF-8 + legacy encodings) |

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
