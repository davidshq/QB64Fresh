# ADR-0018: Compiler Execution Resource Limits (Memory Limits)

## Status

**Accepted** - January 28, 2026

## Context

QB64Fresh and QB64pe (the reference compiler) can consume very large amounts of memory (25GB+ reported) during certain compilation operations. Running either compiler without limits can:

- Freeze or crash the system
- Trigger the OOM killer
- Make the machine unresponsive

This affects developers, CI, and anyone invoking the compiler. We need a clear policy: recommend or require memory limits when running these compilers.

Key considerations:

- We do not control QB64pe's memory use; we can document and recommend limits
- QB64Fresh may have similar peaks during heavy parsing/codegen
- CI and local runs should be safe by default where possible
- Documentation and automation (e.g. wrapper scripts) should make limits easy to apply

## Decision

**We recommend and document mandatory memory limits when running QB64Fresh or QB64pe. Use `ulimit -v` (or equivalent) to cap virtual memory so that compilation cannot exhaust system memory.**

### Policy

- **Document**: [MEMORY_LIMITS.md](../MEMORY_LIMITS.md) describes the problem, recommended limits (e.g. 16GB), and usage (ulimit, wrapper script).
- **Recommend**: Always run the compiler under a memory limit in development and CI (e.g. `ulimit -v 16777216` for 16GB).
- **Automation**: Provide or document a wrapper (e.g. `run_limited.sh`) that sets the limit and execs the command.
- **CLAUDE.md / tooling**: Project rules for AI and contributors state that memory limits are required when running QB64Fresh or QB64pe.

### Example

```bash
# Set 16GB virtual memory limit (value in KB)
ulimit -v 16777216
./qb64fresh input.bas --emit-c -o output.c
```

Or one-liner:

```bash
bash -c 'ulimit -v 16777216 && ./qb64fresh input.bas --emit-c -o output.c'
```

### Rationale

- Prevents system-wide impact from a single compile
- Simple to apply (one ulimit call or wrapper)
- No change to compiler code required; operational policy only

## Consequences

### Positive

- Reduces risk of OOM and system instability
- Clear, consistent guidance for all users and automation
- Easy to adopt (documentation + optional script)

### Negative

- Compiles that genuinely need more than the limit may fail; users can raise the limit or fix the input/tooling
- Slightly more setup for first-time runs (unless wrapper is used by default)

## References

- [MEMORY_LIMITS.md](../MEMORY_LIMITS.md) – Full documentation, values, and scripts
- CLAUDE.md – Project rules (memory limits when running compilers)
