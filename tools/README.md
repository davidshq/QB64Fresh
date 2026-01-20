# QB64Fresh Tools

This directory contains utility tools for working with BASIC source files.

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
