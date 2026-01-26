# Session 059: Getting Started Tutorial

**Date:** 2026-01-25

## Objective

Implement the TODO item: *Tutorial/getting started guide (Medium - 2-3 sessions)*.

## What Was Done

### 1. `docs/GETTING_STARTED.md` (new)

A focused, linear tutorial:

- **Prerequisites** — Rust, C compiler; install commands for Linux/macOS/Windows
- **Build** — clone, `cargo build --release`
- **Memory limits** — `ulimit -v 16777216` and `./run_limited.sh`; pointer to MEMORY_LIMITS.md
- **First program** — minimal `hello.bas` or `examples/hello.bas`
- **Compile** — `qb64fresh prog.bas --emit-c` (writes `prog.c` beside input)
- **Build & run** — `gcc prog.c -o prog -lm` and `./prog` for **inline** runtime
- **Pipeline flags** — `--tokens`, `--ast`, `--typed-ir`, `--emit-c`
- **Runtime modes** — inline (default) vs external; when to use each and link commands
- **Common options** — `-o`, `--no-preprocess`, `--no-shell`, `--write-preprocessed`
- **Next steps** — Handbook, examples, Language Reference, DEVELOPMENT, VSCode

Inline is the default path; external is documented for graphics/SDL2.

### 2. Handbook and README updates

- **`docs/QB64Fresh_HANDBOOK.md`**
  - Intro: now points to GETTING_STARTED for the tutorial; handbook remains the wide reference
  - "Compiling Your First Program": removed incorrect `> hello.c` and `-o myprogram`; added correct `--emit-c` → `gcc ... -lm` flow and memory-limit note; link to GETTING_STARTED
  - Quick Example: fixed `cargo run -- hello.bas --emit-c > hello.c` to `cargo run --release -- hello.bas --emit-c` (writes `hello.c`) and added memory-limit pointer

- **`README.md`**
  - User Guides: added **Getting Started** as the first item, linking to `docs/GETTING_STARTED.md`
  - Quick Start: replaced the pipeline-only `cargo run --release -- examples/hello.bas` with the full flow: `--emit-c`, then `gcc examples/hello.c -o hello -lm && ./hello`

### 3. `examples/README.md`

- **Running Examples**
  - Primary: **inline** runtime (no SDL2, no `-lqb64fresh_rt`): `--emit-c` → `gcc ... -lm` → `./program`
  - Corrected output path: compiler writes `examples/basics/hello.c` for `examples/basics/hello.bas` (not a generic `output.c`)
  - Secondary: **external** runtime for graphics, with build and link commands

## Decisions

- **GETTING_STARTED as the tutorial** — Short, correct, step-by-step. Handbook stays as the broad reference.
- **Inline runtime first** — Simpler (`gcc ... -lm` only). External and SDL2 are optional, documented.
- **Memory limits in the tutorial** — Required by MEMORY_LIMITS and CLAUDE; included in GETTING_STARTED and mentioned in Handbook/Examples.

## Files Touched

- `docs/GETTING_STARTED.md` (new)
- `docs/QB64Fresh_HANDBOOK.md`
- `README.md`
- `examples/README.md`
- `docs/archive/TODO-completed.md`
- `TODO.md` (Documentation item removed; in TODO-completed)
- `Cargo.toml` — added `default-run = "qb64fresh"` so `cargo run` (without `--bin`) uses the compiler at the workspace root.

## TODO.md

- [x] Tutorial/getting started guide — done. To move to TODO-completed.
