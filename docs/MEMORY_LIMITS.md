# Memory Limits for QB64Fresh and QB64pe

**Last Updated:** 2026-01-26

## Critical: Always Set Memory Limits

When running QB64Fresh or QB64pe (whether the original or our bootstrapped version), **always constrain memory usage** to prevent system crashes.

### The Problem

Both compilers can consume excessive memory (25GB+) during certain operations, which can:
- Freeze or crash the entire system
- Trigger the OOM killer
- Make the system unresponsive

### Solution: Use ulimit

Before running either compiler, set a virtual memory limit:

```bash
# Set 16GB memory limit (in KB)
ulimit -v 16777216

# Then run the compiler
./qb64fresh input.bas --emit-c -o output.c
```

Or as a one-liner:

```bash
bash -c 'ulimit -v 16777216 && ./qb64fresh input.bas --emit-c -o output.c'
```

### Helper Script

Create a wrapper script for convenience:

```bash
#!/bin/bash
# run_limited.sh - Run command with 16GB memory limit
ulimit -v 16777216
exec "$@"
```

Usage:
```bash
./run_limited.sh ./qb64fresh input.bas --emit-c -o output.c
./run_limited.sh ./qb64pe_fresh -x input.bas -o output
```

### Memory Limit Values

| Limit | ulimit -v value |
|-------|-----------------|
| 8 GB  | 8388608         |
| 16 GB | 16777216        |
| 32 GB | 33554432        |

### Notes

- The 16GB limit is a reasonable default that allows most compilations while protecting the system
- If compilation fails with a segfault at this limit, there may be a bug causing excessive memory usage
- The bootstrapped QB64pe currently crashes when attempting to compile programs (memory issue under investigation)
