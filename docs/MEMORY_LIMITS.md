# Memory Limits for QB64Fresh and QB64pe

**Last Updated:** 2026-01-29

## Critical: Always Set Memory Limits

When running QB64Fresh or QB64pe (whether the original or our bootstrapped version), **always constrain memory usage** to prevent system crashes.

### The Problem

Both compilers can consume excessive memory (25GB+) during certain operations, which can:
- Freeze or crash the entire system
- Trigger the OOM killer
- Make the system unresponsive

### Built-in Source Size Limits

QB64Fresh now enforces configurable limits on source file sizes to fail fast instead of OOM:

- **Preprocessed source limit**: Default 100MB (configurable via `QB64FRESH_MAX_SOURCE_BYTES`)
- **Raw input file limit**: Default 100MB (configurable via `QB64FRESH_MAX_INPUT_BYTES`)

If a file exceeds these limits, the compiler will exit with a clear error message instead of attempting to process it and potentially consuming excessive memory. This provides a safety guardrail when `ulimit` is not set.

**Important:** The memory crash typically occurs when **gcc compiles the generated C code** (not during QB64Fresh's compilation phases). By limiting the preprocessed source size, we prevent generating C files that are too large for gcc to compile without OOM. If you hit the limit, either increase it via environment variables or split your program into smaller modules.

**To override limits:**
```bash
# Allow up to 200MB for preprocessed source
export QB64FRESH_MAX_SOURCE_BYTES=200000000

# Allow up to 200MB for raw input
export QB64FRESH_MAX_INPUT_BYTES=200000000

# Then run the compiler
./qb64fresh input.bas --emit-c -o output.c
```

**Note:** Very large programs (e.g. full QB64pe with all includes) may require higher limits. If you hit the limit, either:
1. Increase the environment variable (e.g. `QB64FRESH_MAX_SOURCE_BYTES=500000000` for 500MB)
2. Use `ulimit -v` to limit virtual memory (recommended for very large compilations)
3. Split your program into smaller modules

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

### Memory Diagnostics: RSS Reporting

To diagnose which compilation phase consumes the most memory, enable RSS (Resident Set Size) reporting:

```bash
# Enable RSS reporting via verbose mode
./qb64fresh input.bas --emit-c -o output.c --verbose

# Or via environment variable
QB64FRESH_REPORT_RSS=1 ./qb64fresh input.bas --emit-c -o output.c
```

This will print RSS after each phase:
- After read
- After preprocess
- After lex
- After parse
- After semantic
- After codegen

This helps identify which phase dominates memory usage for future optimization.

**Note:** RSS reporting is currently only available on Linux (reads `/proc/self/status`). On other platforms, it will silently skip reporting.

### Notes

- The **4GB** limit is the default (see `run_limited.sh`); it protects the system but very large compilations may still OOM.
- If the process grows until it dies at 4GB, try 8GB or 16GB, or use incremental/smaller inputs.
- The built-in source size limits (100MB default) provide an additional safety guardrail when `ulimit` is not set.
- **Binary crash (runtime):** The bootstrapped QB64pe **executable** (qb64pe_fresh) can crash at runtime with "suspicious length" and heap corruption. This is a **runtime/codegen bug** (string pointer passed as address instead of value), not a compiler or gcc OOM. See [AgenticLogs/IndividualProblems/2026-01-29_qb64pe-binary-crash-suspicious-length.md](../AgenticLogs/IndividualProblems/2026-01-29_qb64pe-binary-crash-suspicious-length.md) for diagnosis and GDB steps.
