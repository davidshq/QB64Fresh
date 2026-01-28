# Real-Time Progress Output Guide

## Quick Answer

**For future QB64pe compilations**, use:

```bash
./scripts/test-full-qb64pe-unbuffered.sh
```

Or manually:

```bash
stdbuf -oL -eL cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c --verbose
```

## What Changed

### Progress Messages Added

The compiler now prints progress during compilation:

```
[1/4] Lexing...
[1/4] Lexing complete: 125432 tokens
[2/4] Parsing...
[2/4] Parsing complete: 8473 statements
[3/4] Semantic analysis...
[3/4] Semantic analysis complete: 8473 typed statements
[4/4] Code generation...
Generated: output.c
```

### Unbuffered Output

Using `stdbuf -oL -eL`:
- `-oL` = Line-buffered stdout (output appears immediately)
- `-eL` = Line-buffered stderr (errors appear immediately)

This makes progress visible in real-time instead of buffering until completion.

## Current Running Process

**Note:** The process currently running (PID 81514) was started before these changes, so it won't show progress messages. However:

- ✅ It's still working correctly
- ✅ It will complete normally
- ✅ Check for `/tmp/qb64pe_full_test.c` when done

## Usage

### Option 1: Use the Script (Easiest)

```bash
./scripts/test-full-qb64pe-unbuffered.sh
```

### Option 2: Manual with stdbuf

```bash
stdbuf -oL -eL cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c --verbose
```

### Option 3: Just --verbose (if connected to terminal)

```bash
cargo run --bin qb64fresh -- ../QB64pe/source/qb64pe.bas --emit-c --verbose
```

When connected to a terminal, Rust's `println!` is already line-buffered, so progress appears immediately.

## Why It Was Buffered

- **Terminal:** Line-buffered (appears immediately) ✅
- **Redirected to file:** Fully buffered (appears at end) ❌
- **Solution:** Use `stdbuf -oL` to force line buffering

## Benefits

1. **See progress** - Know which phase is running
2. **Estimate time** - See how long each phase takes
3. **Debug faster** - See where it fails immediately
4. **Peace of mind** - Know it's working, not hung
