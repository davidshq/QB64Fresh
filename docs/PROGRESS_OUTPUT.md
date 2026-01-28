# Real-Time Progress Output for QB64pe Compilation

## Problem

When compiling large files like QB64pe (24,757 lines), the compilation takes 5+ minutes with no visible progress. Output is buffered and only appears at the end.

## Solution

### Option 1: Use Unbuffered Script (Recommended)

Use the new script that disables output buffering:

```bash
./scripts/test-full-qb64pe-unbuffered.sh
```

This script:
- Uses `stdbuf` to disable line buffering (if available)
- Uses `--verbose` flag for progress messages
- Shows progress in real-time

### Option 2: Use --verbose Flag Directly

```bash
cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c --verbose
```

The `--verbose` flag now shows progress:
- `[1/4] Lexing...` → `[1/4] Lexing complete: N tokens`
- `[2/4] Parsing...` → `[2/4] Parsing complete: N statements`
- `[3/4] Semantic analysis...` → `[3/4] Semantic analysis complete: N typed statements`
- `[4/4] Code generation...` → `Generated: output.c`

### Option 3: Use stdbuf Manually

If you want to run manually with unbuffered output:

```bash
stdbuf -oL -eL cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c --verbose
```

## Progress Messages Added

The compiler now prints progress messages during each phase:

1. **Lexing** - Tokenizes the source code
2. **Parsing** - Builds AST from tokens
3. **Semantic Analysis** - Type checking and symbol resolution
4. **Code Generation** - Generates C code

Each phase shows:
- Start message: `[N/4] Phase name...`
- Completion message: `[N/4] Phase name complete: statistics`

## Why Output Was Buffered

Rust's `println!` is:
- **Line-buffered** when connected to a terminal (appears immediately)
- **Fully buffered** when redirected to a file (appears at end)

Using `stdbuf -oL` forces line buffering even when redirected.

## Example Output

```
[1/4] Lexing...
[1/4] Lexing complete: 125432 tokens
[2/4] Parsing...
[2/4] Parsing complete: 8473 statements
[3/4] Semantic analysis...
[3/4] Semantic analysis complete: 8473 typed statements
[4/4] Code generation...
Generated: /tmp/qb64pe_full_test.c
Output size: 2456789 bytes
```

## Notes

- Progress messages use `eprintln!` so they go to stderr (won't interfere with output)
- Messages only appear with `--verbose` flag
- `stdbuf` may not be available on all systems (it's a GNU coreutils tool)
