# QB64Fresh Examples

This directory contains example programs demonstrating QB64Fresh features.

## Directory Structure

```
examples/
├── basics/          Core language features
├── graphics/        Drawing and visual output
├── games/           Simple playable games
├── audio/           Sound and music
├── files/           File I/O operations
└── advanced/        DECLARE LIBRARY, networking, etc.
```

## Running Examples

**Inline runtime (simplest)** — console and simple I/O. The compiler writes a `.c` file beside the input (e.g. `examples/basics/hello.c`).

```bash
# From QB64Fresh repo root. Use a memory limit (see docs/MEMORY_LIMITS.md)
ulimit -v 16777216
cargo run --release -- examples/basics/hello.bas --emit-c

# Build and run (inline = just -lm)
gcc examples/basics/hello.c -o program -lm
./program
```

**External runtime** — for graphics and SDL2. Build the runtime, then use `--runtime external` and link:

```bash
cargo build -p qb64fresh-runtime --release
cargo run --release -- examples/graphics/drawing.bas --emit-c --runtime external -o drawing.c
gcc -I runtime/include drawing.c -L target/release -lqb64fresh_rt $(pkg-config --libs sdl2) -lm -lpthread -ldl -o drawing
./drawing
```

## Example Index

### Basics (`basics/`)
| File | Description |
|------|-------------|
| `hello.bas` | Hello World and basic I/O |
| `variables.bas` | Data types and variables |
| `control_flow.bas` | IF, FOR, WHILE, SELECT CASE |
| `arrays.bas` | Arrays and REDIM |
| `strings.bas` | String manipulation |
| `procedures.bas` | SUB and FUNCTION |
| `types.bas` | User-defined types (TYPE) |
| `recursion.bas` | Recursive functions |

### Graphics (`graphics/`)
| File | Description |
|------|-------------|
| `drawing.bas` | LINE, CIRCLE, PSET, PAINT |
| `colors.bas` | COLOR, PALETTE, RGB |
| `animation.bas` | Simple sprite animation |
| `mouse_input.bas` | Mouse tracking and clicks |

### Games (`games/`)
| File | Description |
|------|-------------|
| `guess_number.bas` | Number guessing game (console) |
| `snake.bas` | Classic snake game |
| `pong.bas` | Two-player pong |

### Audio (`audio/`)
| File | Description |
|------|-------------|
| `beep_sound.bas` | BEEP and SOUND |
| `play_music.bas` | PLAY command (MML) |
| `sound_files.bas` | _SNDOPEN, _SNDPLAY |

### Files (`files/`)
| File | Description |
|------|-------------|
| `text_files.bas` | Sequential file I/O |
| `binary_files.bas` | Random access and binary |
| `file_system.bas` | MKDIR, KILL, NAME, SHELL |

### Advanced (`advanced/`)
| File | Description |
|------|-------------|
| `declare_library.bas` | C library integration |
| `networking.bas` | TCP client/server |
| `memory.bas` | DEF SEG, PEEK, POKE |
| `error_handling.bas` | ON ERROR GOTO |

---

*These examples are tested with QB64Fresh and demonstrate working features.*
