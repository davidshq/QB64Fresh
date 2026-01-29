# Session 085: Memory limit 4GB and known memory-growth issue

**Date:** 2026-01-28  
**Summary:** Reduced default memory limit from 16GB to 4GB and documented that the process can still grow until OOM (known issue).

## Accomplished

1. **Reduced limit to 4GB everywhere**
   - `run_limited.sh`: `ulimit -v 4194304` (4GB in KB), comment that very large compilations may still OOM.
   - `docs/MEMORY_LIMITS.md`: Default 4GB, new section "Known Issue: Memory Growth Under 4GB", table includes 4GB.
   - `CLAUDE.md`: 4GB in examples, note that full QB64pe may still OOM at 4GB.
   - `docs/QB64PE_INCREMENTAL_TESTING.md`: Full-validation command uses 4GB, note about possible OOM.
   - Scripts: `test-qb64pe-incremental.sh`, `test-full-qb64pe.sh`, `test-full-qb64pe-unbuffered.sh`, `test-full-qb64pe-progress.sh`, `test-all-qb64pe-sections.sh` — all set to 4GB.

2. **Documented known issue**
   - MEMORY_LIMITS.md: "With a 4GB virtual memory limit, compiling very large files (e.g. full QB64pe) can still grow until the process is killed (OOM). The compiler has not yet been fully profiled for unbounded allocations on huge inputs; the limit caps damage but does not fix the underlying growth."

## Verification

- Ran compile under 4GB (and 2GB, 1GB) in this environment: process stayed at ~210 MB RSS and succeeded. The OOM behavior the user sees may be environment-specific or occur in a different code path (e.g. LSP, different input, or running the compiled IDE).

## Reference

- 4GB in KB: `4194304` (4 * 1024 * 1024).
