# QB64Fresh Next Steps

*Updated: 2026-01-25*

This document provides clear guidance on what to work on next.

---

## Immediate Priority: Bootstrap Memory Issue

**The Problem:**
The bootstrapped QB64pe executable crashes (memory exhaustion) when compiling BASIC programs.

**Status:**
- ✅ QB64pe source compiles to C (~115K lines)
- ✅ GCC compiles without errors
- ✅ `qb64pe_fresh -h` shows help text
- ❌ `qb64pe_fresh -x test.bas -o test` crashes with memory exhaustion

**Next Actions:**

1. **Create minimal reproduction**
   ```bash
   echo 'PRINT "Hello"' > /tmp/tiny.bas
   bash -c 'ulimit -v 16777216 && timeout 60 ./qb64pe_fresh -x /tmp/tiny.bas -o /tmp/tiny 2>&1'
   ```

2. **Add memory tracking**
   - Add profiling or debug output to generated C
   - Find where the spiral begins (leak, loop, or data growth)

3. **Investigate hypotheses** (see [BOOTSTRAP_PLAN_REMAINING](docs/ThingsToDo/BOOTSTRAP_PLAN_REMAINING.md))
   - String temp pool / `qbs_cleanup()` usage
   - Array reallocation or hash-table growth
   - INI / QB64pe runtime behavior vs original

4. **Compare with original**
   - Does original QB64pe have the same memory pattern on the same input?

5. **Fix the bug**
   - Once identified, fix in QB64Fresh codegen (or generated C/runtime)
   - Regenerate and test

---

## Safety Reminder

**ALWAYS run with memory limits:**
```bash
bash -c 'ulimit -v 16777216 && ./qb64pe_fresh ...'
```

See [docs/MEMORY_LIMITS.md](docs/MEMORY_LIMITS.md).

---

## Recent (Completed)

- **`--no-shell`** — Compile-time flag to reject SHELL / _SHELLHIDE when using `--emit-c` (Session 058)
- **SECURITY_MODEL.md** — SHELL, path handling, and sandboxing options documented

---

## After Bootstrap: Lower Priority Items

### Graphics System (Phase 3)
- [ ] Alpha blending support
- [ ] Hardware acceleration
- [ ] Multiple screen pages

### Advanced Features (Phase 5)
- [ ] Network stream I/O
- [ ] Joystick/gamepad support
- [ ] Touch input support
- [ ] `_THREAD` multi-threading

### Tooling (Phase 6)
- [ ] Debugger runtime integration (symbols, DAP, etc. scaffolded; needs runtime hooks)
- [ ] Dead code elimination
- [ ] Loop optimization
- [ ] Inline small functions

### Documentation
- [ ] Tutorial / getting started guide
- [ ] More examples

---

## Related Documents

| Document | Purpose |
|----------|---------|
| [docs/ThingsToDo/BOOTSTRAP_PLAN_REMAINING.md](docs/ThingsToDo/BOOTSTRAP_PLAN_REMAINING.md) | Bootstrap status, hypotheses, and remaining work |
| [TODO.md](TODO.md) | Full prioritized roadmap |
| [docs/MEMORY_LIMITS.md](docs/MEMORY_LIMITS.md) | Memory limit requirements |
| [docs/SECURITY_MODEL.md](docs/SECURITY_MODEL.md) | SHELL, paths, and sandboxing |
| [docs/ThingsToDo/CODEBASE_REVIEW_CONSOLIDATED.md](docs/ThingsToDo/CODEBASE_REVIEW_CONSOLIDATED.md) | Code quality and test coverage |
| [docs/ThingsToDo/](docs/ThingsToDo/) | Detailed task tracking |

---

## Quick Reference

```bash
# Build QB64Fresh
cargo build --release

# Generate C from QB64pe
./target/release/qb64fresh ../QB64pe/source/qb64pe.bas --emit-c -o ../QB64pe/qb64pe.c

# Compile to executable
cd ../QB64pe && gcc -O2 -o qb64pe_fresh qb64pe.c -lm

# Test (with memory limit!)
bash -c 'ulimit -v 16777216 && ./qb64pe_fresh -h'
```
