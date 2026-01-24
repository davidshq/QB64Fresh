# QB64Fresh Tools

This directory contains utility tools for working with BASIC source files.

## qb64fresh-fmt

A code formatter for QB64/QBasic BASIC source files. Standardizes code style across your projects.

### Features

- **Keyword capitalization** - UPPERCASE, lowercase, Title Case, or preserve original
- **Operator spacing** - Consistent spaces around `=`, `+`, `-`, `*`, `/`, etc.
- **Semicolon/comma spacing** - Proper spacing in PRINT statements
- **Indentation** - Spaces or tabs, configurable width
- **Comment formatting** - Proper spacing after `'` and `REM`
- **Style presets** - Default, minimal, QB64 IDE style, pretty

### Installation

Build from the QB64Fresh workspace root:

```bash
cargo build --release -p qb64fresh-fmt
```

The binary will be at `target/release/qb64fresh-fmt`.

### Usage

```
qb64fresh-fmt [OPTIONS] <FILES>...

Options:
  -c, --check              Check if files are formatted (exit 1 if not)
      --stdout             Write output to stdout instead of modifying files
      --diff               Show diff of changes
  -b, --backup             Create .bak backup files before modifying
  -r, --recursive          Process directories recursively
      --style <STYLE>      Style preset: default, minimal, qb64, pretty
      --keyword-case <KC>  Keyword case: upper, lower, title, preserve
      --indent-style <IS>  Indent style: spaces, tabs
      --indent-width <N>   Spaces per indent level (default: 4)
  -v, --verbose            Show detailed information
  -q, --quiet              Only show errors
  -h, --help               Show help message
```

### Examples

```bash
# Format a single file in place
qb64fresh-fmt myprogram.bas

# Check formatting without making changes (for CI)
qb64fresh-fmt --check *.bas

# Preview changes with diff
qb64fresh-fmt --diff myprogram.bas

# Format from stdin
echo 'print "hello"' | qb64fresh-fmt -
# Output: PRINT "hello"

# Use lowercase keywords
qb64fresh-fmt --keyword-case lower myprogram.bas

# Use QB64 IDE style
qb64fresh-fmt --style qb64 myprogram.bas

# Process all .bas files recursively with backup
qb64fresh-fmt --backup --recursive ./src/
```

### Style Presets

| Preset | Keywords | Indent | Operators | Description |
|--------|----------|--------|-----------|-------------|
| `default` | UPPERCASE | 4 spaces | spaced | Standard formatting |
| `minimal` | preserve | 4 spaces | preserve | Minimal changes |
| `qb64` | UPPERCASE | 4 spaces | spaced | QB64 IDE compatible |
| `pretty` | UPPERCASE | 4 spaces | spaced | Full enhancements |

### Before/After Example

**Before:**
```basic
if x>1 then
print "hello";x
end if
```

**After (default style):**
```basic
IF x > 1 THEN
    PRINT "hello"; x
END IF
```

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success (or check passed) |
| 1 | Check failed (files would be formatted) |
| Non-zero | Error occurred |

See [tools/fmt/README.md](fmt/README.md) for complete documentation.

---

## fix_encoding

A utility for fixing encoding issues in legacy BASIC source files from the DOS era.

### Features

- **CP437 (DOS) encoding conversion** - Converts IBM PC Code Page 437 characters (box-drawing like ╔═╗║╚╝, Greek letters, math symbols) to UTF-8
- **Latin1 (ISO-8859-1) conversion** - Converts Western European accented characters to UTF-8
- **DOS Control-Z stripping** - Removes the `0x1A` byte that DOS used as an end-of-file marker
- **Line ending normalization** - Converts CRLF (`\r\n`) and CR (`\r`) to Unix-style LF (`\n`)
- **Binary DATA preservation** - Intelligently detects and preserves embedded sprite/image data in DATA statements

### Installation

Build from the QB64Fresh workspace root:

```bash
cargo build --release -p fix_encoding
```

The binary will be at `target/release/fix_encoding`.

### Usage

```
fix_encoding [OPTIONS] <FILE>...

Options:
  --dry-run       Show what would be changed without modifying files
  --backup        Create .bak backup files before modifying
  --encoding <E>  Force encoding: cp437, latin1, auto (default: auto)
  --recursive     Process directories recursively
  --verbose       Show detailed information about changes
  --analyze       Only analyze files, don't make any changes
  --help          Show help message
```

### Examples

```bash
# Analyze files without making changes
fix_encoding --analyze --verbose *.bas

# Preview what would be changed
fix_encoding --dry-run myfile.bas

# Convert with automatic backup
fix_encoding --backup myfile.bas

# Process an entire directory recursively
fix_encoding --backup --recursive ./legacy_code/

# Force CP437 encoding (skip auto-detection)
fix_encoding --encoding cp437 oldgame.bas
```

### How It Works

#### Encoding Detection

The tool analyzes each file to detect:
1. **ASCII** - Pure 7-bit ASCII, no conversion needed
2. **UTF-8** - Already valid UTF-8, no conversion needed
3. **CP437** - IBM PC character set, common in DOS programs (detected by box-drawing character patterns)
4. **Latin1** - ISO-8859-1 Western European encoding
5. **Binary** - Too much high-byte content to be text, skipped
6. **BasicWithBinaryData** - BASIC file with embedded binary sprite data in DATA statements

#### Binary DATA Detection

DOS-era BASIC programs often embedded graphics data directly in DATA statements:

```basic
DATA "████████████████"   ' Binary sprite data
DATA "██  ██  ██  ██  "   ' More sprite rows
```

The tool detects these patterns by:
1. Identifying DATA statement lines
2. Counting high bytes (0x80-0xFF) per line
3. If DATA lines contain >10 high bytes, they're flagged as binary
4. If most high bytes are in DATA lines (not code), the file is classified as `BasicWithBinaryData`

For `BasicWithBinaryData` files, the tool:
- Converts encoding in code lines (comments, PRINT statements, etc.)
- Preserves DATA lines with binary content exactly as-is
- Normalizes line endings without corrupting binary data

#### Example Output

```
$ fix_encoding --analyze --verbose forest.bas

forest.bas
  Encoding: BasicWithBinaryData
  Total bytes: 54134
  High bytes (0x80-0xFF): 28633
  Binary DATA lines: 302 (28633 high bytes preserved)
  High bytes in code: 0
```

### Supported Encodings

| Encoding | Description | Common Use |
|----------|-------------|------------|
| CP437 | IBM PC Code Page 437 | DOS programs, box-drawing characters |
| Latin1 | ISO-8859-1 | Western European text |

### CP437 Character Examples

| Byte Range | Characters | Use |
|------------|------------|-----|
| 0x80-0x9F | Ç ü é â ä à å ç ê ë è ï î ì Ä Å É æ Æ ô ö ò û ù ÿ Ö Ü ¢ £ ¥ ₧ ƒ | Accented letters, currency |
| 0xB0-0xDF | ░ ▒ ▓ │ ┤ ╡ ╢ ╖ ╕ ╣ ║ ╗ ╝ ╜ ╛ ┐ └ ┴ ┬ ├ ─ ┼ ╞ ╟ ╚ ╔ ╩ ╦ ╠ ═ ╬ ... | Box-drawing, shading |
| 0xE0-0xFF | α ß Γ π Σ σ µ τ Φ Θ Ω δ ∞ φ ε ∩ ≡ ± ≥ ≤ ⌠ ⌡ ÷ ≈ ° ∙ · √ ⁿ ² ■ | Greek letters, math symbols |

### Safety Features

- **--dry-run** - Preview changes before applying
- **--backup** - Automatic backup creation
- **Binary detection** - Automatically skips non-text files
- **DATA preservation** - Protects embedded sprite/image data from corruption

---

## qb64fresh-debug

A source-level debugger for QB64Fresh programs with full Debug Adapter Protocol (DAP) support for VS Code, Cursor, and other DAP-compatible IDEs.

### Features

- **Breakpoints** - Set, clear, enable, and disable line breakpoints
- **Step execution** - Step into, step over, step out of procedures
- **Call stack** - View the complete call hierarchy
- **Variable inspection** - Examine variable values at runtime
- **DAP server** - Full Debug Adapter Protocol for IDE integration
- **Interactive CLI** - Debug directly from the terminal

### Installation

Build from the QB64Fresh workspace root:

```bash
cargo build --release -p qb64fresh-debug
```

The binary will be at `target/release/qb64fresh-debug`.

### Usage

```
qb64fresh-debug [OPTIONS] <FILE>

Options:
  -b, --break <LOCATION>    Set initial breakpoint (line number or function name)
      --break-on-entry      Break on program entry
      --break-on-error      Break on runtime errors (default: true)
      --dap                 Start in DAP mode for IDE integration
      --port <PORT>         Port for DAP mode (default: 4711)
      --verbosity <LEVEL>   Output verbosity: quiet, normal, verbose, trace
      --config <FILE>       Configuration file path
  -I, --source-path <PATH>  Source file search paths
      --list-commands       List available debugger commands
  -h, --help                Show help message
```

### Interactive Commands

When running in interactive mode, the following commands are available:

| Command | Alias | Description |
|---------|-------|-------------|
| `run` | `r` | Start/restart program execution |
| `continue` | `c` | Continue execution until next breakpoint |
| `step` | `s`, `n` | Step to next statement (step over) |
| `stepin` | `si` | Step into function/sub call |
| `stepout` | `so` | Step out of current function/sub |
| `break [loc]` | `b` | Set breakpoint or list all breakpoints |
| `delete [id]` | `d` | Delete breakpoint (all if no ID given) |
| `enable <id>` | | Enable a breakpoint |
| `disable <id>` | | Disable a breakpoint |
| `list [line]` | `l` | Show source code around line |
| `info breakpoints` | | List all breakpoints |
| `info sources` | | List loaded source files |
| `quit` | `q` | Exit the debugger |

### Examples

```bash
# Debug a program interactively
qb64fresh-debug myprogram.bas

# Debug with breakpoint set at startup
qb64fresh-debug --break 10 myprogram.bas
qb64fresh-debug --break main myprogram.bas

# Start in DAP mode for VS Code
qb64fresh-debug --dap

# Run with verbose output
qb64fresh-debug --verbosity verbose myprogram.bas
```

### VS Code Integration

To use with VS Code, add a debug configuration to `.vscode/launch.json`:

```json
{
    "version": "0.2.0",
    "configurations": [{
        "type": "qb64fresh",
        "request": "launch",
        "name": "Debug BASIC",
        "program": "${file}"
    }]
}
```

### Compiling with Debug Support

To enable debugging, compile your BASIC program with the `--debug` flag:

```bash
qb64fresh myprogram.bas --emit-c --debug
```

This adds debug hooks to the generated C code:
- `qb_dbg_line()` calls before each statement for breakpoint checking
- `qb_dbg_enter_proc()`/`qb_dbg_exit_proc()` for call stack tracking
- Named pipe IPC for debugger communication

### Architecture

```
┌─────────────┐     ┌──────────────────┐     ┌─────────────────────┐
│ VS Code /   │ DAP │ qb64fresh-debug  │pipe │ Compiled program    │
│ Cursor      │◄───►│ (DAP server)     │◄───►│ + debug hooks       │
└─────────────┘     └──────────────────┘     └─────────────────────┘
```

See [ADR-0013: Debugger Architecture](../docs/adrs/ADR-0013-debugger-architecture.md) for design details.
