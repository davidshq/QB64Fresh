# QB64Fresh Next Steps

*Updated: 2026-01-25*

This document provides clear guidance on what to work on next.

---

## Safety Reminder

**Use memory limits when running the compiler on large inputs** (e.g. bootstrapping QB64pe or big .bas files):

```bash
./run_limited.sh ./target/release/qb64fresh large.bas --emit-c -o out.c
# or: bash -c 'ulimit -v 16777216 && ./target/release/qb64fresh large.bas --emit-c -o out.c'
```

See [docs/MEMORY_LIMITS.md](docs/MEMORY_LIMITS.md).

---

## Remaining Work (aligned with [TODO.md](TODO.md))

### Graphics System (Phase 3)
- [ ] Hardware acceleration option *(Large — GPU backend work)*

### Advanced Features (Phase 5)
- [ ] Network stream I/O (PUT/GET with network handles)
- [ ] Touch input support

### Tooling (Phase 6)
- [ ] Debugger runtime integration (symbols, DAP scaffolded; needs runtime hooks, debug info in C, breakpoints)
- [ ] Dead code elimination
- [ ] Loop optimization
- [ ] Inline small functions

---

## Recently Completed (moved from plan)

- **Alpha blending** — _BLEND, _DONTBLEND, _CLEARCOLOR in SDL2 runtime
- **Multiple screen pages** — SCREEN active/visual page, PCOPY; 4 pages in `runtime/src/graphics/sdl2.rs`
- **Joystick/gamepad** — STICK, STRIG, _DEVICES, _AXIS, _BUTTON, ON STRIG, STRIG ON/OFF/STOP in `runtime/src/joystick.rs` (SDL2)

---

## Related Documents

| Document | Purpose |
|----------|---------|
| [TODO.md](TODO.md) | Full prioritized roadmap |
| [docs/MEMORY_LIMITS.md](docs/MEMORY_LIMITS.md) | Memory limit requirements |
| [docs/ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md](docs/ThingsToDo/TESTING_INFRASTRUCTURE_PLAN.md) | Test suites, gaps, QB64pe compat (122/141, 86.5%) |
| [docs/ThingsToDo/RUNTIME_IMPLEMENTATION_PLAN.md](docs/ThingsToDo/RUNTIME_IMPLEMENTATION_PLAN.md) | Runtime and stub status |
| [docs/ThingsToDo/](docs/ThingsToDo/) | Other task tracking |
| [docs/archive/STUB_FUNCTIONS_FULL.md](docs/archive/STUB_FUNCTIONS_FULL.md) | Implemented and will-not-implement functions |
| [docs/QB64pe/](docs/QB64pe/) | QB64pe architecture, debugging, behavioral diffs, migration |

---

## Quick Reference

```bash
# Build QB64Fresh
cargo build --release

# Compile a .bas to C (inline runtime, default)
./target/release/qb64fresh examples/hello.bas --emit-c
gcc examples/hello.c -o hello -lm && ./hello

# For large files, use a memory limit
./run_limited.sh ./target/release/qb64fresh program.bas --emit-c -o program.c
gcc program.c -o program -lm && ./program

# Bootstrap QB64pe (advanced; use run_limited.sh for the qb64fresh step)
./run_limited.sh ./target/release/qb64fresh ../QB64pe/source/qb64pe.bas --emit-c -o ../QB64pe/qb64pe.c
cd ../QB64pe && gcc -O2 -o qb64pe_fresh qb64pe.c -lm && ./qb64pe_fresh -h
```
