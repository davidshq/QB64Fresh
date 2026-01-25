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
   # Find smallest program that crashes
   echo 'PRINT "Hello"' > /tmp/tiny.bas
   bash -c 'ulimit -v 16777216 && timeout 60 ./qb64pe_fresh -x /tmp/tiny.bas -o /tmp/tiny 2>&1'
   ```

2. **Add memory tracking**
   - Modify `strings.rs` to count allocations
   - Add checkpoints to track memory growth
   - Find where the spiral begins

3. **Compare with original**
   - Does original QB64pe have the same memory pattern?
   - What differs between them?

4. **Fix the bug**
   - Once identified, fix in QB64Fresh codegen
   - Regenerate and test

---

## Safety Reminder

**ALWAYS run with memory limits:**
```bash
bash -c 'ulimit -v 16777216 && ./qb64pe_fresh ...'
```

See [docs/MEMORY_LIMITS.md](docs/MEMORY_LIMITS.md).

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
- [ ] Debugger runtime integration
- [ ] Dead code elimination
- [ ] Loop optimization

### Documentation
- [ ] Tutorial/getting started guide
- [ ] More examples

---

## Related Documents

| Document | Purpose |
|----------|---------|
| [BOOTSTRAP_PLAN.md](BOOTSTRAP_PLAN.md) | Bootstrap status and details |
| [TODO.md](TODO.md) | Full prioritized roadmap |
| [docs/MEMORY_LIMITS.md](docs/MEMORY_LIMITS.md) | Memory limit requirements |
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
