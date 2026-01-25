# QB64pe Bootstrap: Remaining Work

*Updated: 2026-01-25*

This document tracks what remains to complete the QB64pe bootstrap project. For full history, see [BOOTSTRAP_PLAN_FULL.md](../archive/BOOTSTRAP_PLAN_FULL.md).

---

## Current Status

**Achieved:**
- ✅ QB64Fresh parses QB64pe source (100K+ lines)
- ✅ Semantic analysis complete (0 errors)
- ✅ Code generation produces ~115K lines of valid C
- ✅ GCC compiles the C code (warnings only)
- ✅ Executable runs basic commands (`qb64pe_fresh -h` works!)

**Current Blocker:**
- ❌ Memory exhaustion when compiling BASIC programs
- The bootstrapped QB64pe crashes (or uses 25GB+ memory) during compilation

---

## Bug: Memory Exhaustion During Compilation

### Symptoms
- `qb64pe_fresh -x test.bas -o test` consumes excessive memory
- With `ulimit -v 16777216`, crashes with segfault
- Without memory limit, can consume 25GB+ and freeze the system

### Investigation Needed
1. **Where is memory allocated?** - Add profiling or debug output
2. **Is it a leak?** - Strings/arrays not being freed?
3. **Is it a loop?** - Some compilation loop running endlessly?
4. **Is it data growth?** - Symbol table or other structure growing exponentially?

### Hypotheses
1. **String temp pool not cleaning up** - `qbs_cleanup()` called thousands of times
2. **Array reallocation spiral** - Some array keeps growing
3. **Hash table issue** - QB64pe uses hash tables extensively
4. **INI file system** - Known to be O(n*m), could be worse

---

## Remaining Work

### Priority 1: Fix Memory Issue
- [ ] Add memory tracking debug output to generated C
- [ ] Create minimal test case that reproduces the crash
- [ ] Compare memory behavior with original QB64pe
- [ ] Identify and fix the root cause

### Priority 2: Validate Compilation
- [ ] Bootstrap compiles `PRINT "Hello"`
- [ ] Bootstrap compiles program with arrays
- [ ] Bootstrap compiles program with file I/O

### Priority 3: Full Bootstrap
- [ ] Bootstrap compiles QB64pe itself
- [ ] Behavior matches original QB64pe

---

## Success Criteria

| Milestone | Status |
|-----------|--------|
| Parse QB64pe | ✅ |
| Generate C code | ✅ |
| GCC compiles | ✅ |
| Run `-h` | ✅ |
| Compile simple program | ❌ Blocked by memory issue |
| Self-hosting bootstrap | ❌ |

---

## Safety: Always Use Memory Limits

**CRITICAL:** See [MEMORY_LIMITS.md](../MEMORY_LIMITS.md) for required ulimit settings.

```bash
# Always run QB64pe (fresh or original) with memory limits
bash -c 'ulimit -v 16777216 && ./qb64pe_fresh -x program.bas -o program'
```

---

*For detailed history of all fixes and implementation phases, see [BOOTSTRAP_PLAN_FULL.md](../archive/BOOTSTRAP_PLAN_FULL.md).*
