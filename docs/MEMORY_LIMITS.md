# Memory Limits for QB64Fresh and QB64pe

**Last Updated:** 2026-01-28

## Critical: Always Set Memory Limits

When running QB64Fresh or QB64pe (whether the original or our bootstrapped version), **always constrain memory usage** to prevent system crashes.

### The Problem

Both compilers can consume excessive memory (25GB+) during certain operations, which can:
- Freeze or crash the entire system
- Trigger the OOM killer
- Make the system unresponsive

### Known Issue: Memory Growth Under 4GB

With a **4GB** virtual memory limit (`ulimit -v 4194304`), compiling very large files (e.g. full QB64pe) can still **grow until the process is killed** (OOM). The compiler has not yet been fully profiled for unbounded allocations on huge inputs; the limit caps damage but does not fix the underlying growth. If you see the process die under 4GB, increase the limit (e.g. 8GB) or reduce the input size until the issue is addressed.

### Solution: Use ulimit

Before running either compiler, set a virtual memory limit:

```bash
# Set 4GB memory limit (in KB)
ulimit -v 4194304

# Then run the compiler
./qb64fresh input.bas --emit-c -o output.c
```

Or as a one-liner:

```bash
bash -c 'ulimit -v 4194304 && ./qb64fresh input.bas --emit-c -o output.c'
```

### Helper Script

Use the project wrapper (limits to 4GB):

```bash
./run_limited.sh ./qb64fresh input.bas --emit-c -o output.c
./run_limited.sh ./qb64pe_fresh -x input.bas -o output
```

### Memory Limit Values

| Limit | ulimit -v value |
|-------|-----------------|
| 4 GB  | 4194304         |
| 8 GB  | 8388608         |
| 16 GB | 16777216        |
| 32 GB | 33554432        |

### Notes

- The **4GB** limit is the default (see `run_limited.sh`); it protects the system but very large compilations may still OOM.
- If the process grows until it dies at 4GB, try 8GB or 16GB, or use incremental/smaller inputs.
- The bootstrapped QB64pe currently crashes when attempting to compile programs (memory issue under investigation).
