# qb64fresh-fmt

Code formatter for QB64Fresh BASIC source files.

## Features

- **Keyword capitalization**: Standardize keyword casing (UPPERCASE, lowercase, Title Case)
- **Spacing**: Normalize spacing around operators and after keywords
- **Indentation**: Consistent indentation for control structures
- **Comment formatting**: Proper spacing in comments
- **Multiple presets**: Default, minimal, QB64 IDE style

## Installation

Build from the workspace root:

```bash
cargo build --release -p qb64fresh-fmt
```

The binary will be at `target/release/qb64fresh-fmt`.

## Usage

### Format files in place

```bash
qb64fresh-fmt myprogram.bas
qb64fresh-fmt *.bas
```

### Check formatting (CI/pre-commit)

```bash
qb64fresh-fmt --check myprogram.bas
# Exit code 1 if files would be changed
```

### Preview changes

```bash
qb64fresh-fmt --diff myprogram.bas
qb64fresh-fmt --stdout myprogram.bas
```

### Format from stdin

```bash
echo 'print "hello"' | qb64fresh-fmt -
# Output: PRINT "hello"
```

### Backup before formatting

```bash
qb64fresh-fmt --backup myprogram.bas
# Creates myprogram.bas.bak
```

### Process directories

```bash
qb64fresh-fmt --recursive ./src/
```

## Style Presets

| Preset | Description |
|--------|-------------|
| `--style default` | UPPERCASE keywords, 4-space indent, spaces around operators |
| `--style minimal` | Preserve original style, minimal changes |
| `--style qb64` | QB64 IDE style with comment alignment |
| `--style pretty` | Full formatting with all enhancements |

## Options

| Option | Description |
|--------|-------------|
| `--keyword-case` | `upper`, `lower`, `title`, `preserve` |
| `--indent-style` | `spaces`, `tabs` |
| `--indent-width` | Number of spaces per indent (default: 4) |
| `--space-around-operators` | Add spaces around `=`, `+`, etc. |
| `--space-after-comma` | Add space after commas |

## Examples

### Lowercase keywords

```bash
qb64fresh-fmt --keyword-case lower myprogram.bas
```

Before:
```basic
PRINT "Hello"
IF x > 1 THEN PRINT x
```

After:
```basic
print "Hello"
if x > 1 then print x
```

### No spaces around operators

```bash
qb64fresh-fmt --space-around-operators false myprogram.bas
```

Before:
```basic
x = 1 + 2
```

After:
```basic
x=1+2
```

## Library Usage

The formatter can also be used as a Rust library:

```rust
use qb64fresh_fmt::{Formatter, FormatterConfig};

let source = "print \"hello\"";
let config = FormatterConfig::default();
let formatter = Formatter::new(config);
let formatted = formatter.format(source).unwrap();

assert_eq!(formatted, "PRINT \"hello\"\n");
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success (or check passed) |
| 1 | Check failed (files would be formatted) |
| Non-zero | Error occurred |
