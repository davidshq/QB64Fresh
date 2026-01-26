# Getting Started with QB64Fresh

This tutorial gets you from zero to running your first BASIC program with QB64Fresh in a few minutes.

---

## What is QB64Fresh?

QB64Fresh is a **QuickBASIC-compatible compiler** that turns `.bas` source into native executables via C. You get:

- **Classic QBasic semantics** — `PRINT`, `INPUT`, `FOR`/`NEXT`, `GOSUB`, files, graphics
- **QB64 extensions** — `_SNDOPEN`, `_MOUSEX`, `SCREEN _NEWIMAGE`, `DECLARE LIBRARY`, and 240+ built-in functions (405+ built-in registrations)
- **Modern tooling** — LSP, formatter, linter; use any editor (VS Code, Vim, etc.)
- **99.1% QB4.5 compatibility** — 114/115 test files from QB64pe test suite pass

---

## Prerequisites

| Tool        | Purpose              | Check                |
|------------|----------------------|----------------------|
| **Rust**   | Build the compiler    | `rustc --version`    |
| **C compiler** | Build executables | `gcc --version` or `clang --version` |

**Install Rust** (if needed):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

**C compiler:**

- **Linux:** `sudo apt install build-essential` (Debian/Ubuntu) or `sudo dnf install gcc` (Fedora)
- **macOS:** Xcode Command Line Tools: `xcode-select --install`
- **Windows:** [MinGW-w64](https://www.mingw-w64.org/) or [MSYS2](https://www.msys2.org/)

---

## 1. Build the compiler

```bash
git clone <repository-url>
cd QB64Fresh
cargo build --release
```

The `qb64fresh` binary is at `target/release/qb64fresh`. You can add it to your `PATH` or run it via `cargo run --release --` as shown below.

---

## 2. Memory limits (important)

The compiler can use a lot of memory on large programs. **Always run it with a memory limit** to avoid system freezes:

```bash
# One-liner: 16GB limit
ulimit -v 16777216

# Or use the project helper (from the QB64Fresh repo root)
./run_limited.sh cargo run --release -- examples/hello.bas --emit-c
```

If you use `ulimit`, set it once per shell; it applies to all following commands. See [MEMORY_LIMITS.md](MEMORY_LIMITS.md) for details.

---

## 3. Your first program

Create `hello.bas`:

```basic
PRINT "Hello, World!"
DIM name AS STRING
INPUT "Your name? ", name
PRINT "Hi, "; name; "!"
END
```

Or use the included example: `examples/hello.bas` or `examples/basics/hello.bas`.

---

## 4. Compile BASIC to C

From the QB64Fresh repo root:

```bash
ulimit -v 16777216   # if not already set
cargo run --release -- examples/hello.bas --emit-c
```

You should see:

```
Generated: examples/hello.c
```

The compiler writes a `.c` file next to the input by default. Use `-o path/to/output.c` to choose a different path.

---

## 5. Build and run the executable

**Inline runtime (default)** — everything is in the generated C. You only need the C compiler and the math library:

```bash
gcc examples/hello.c -o hello -lm
./hello
```

For `examples/basics/hello.bas`, the generated file is `examples/basics/hello.c`:

```bash
gcc examples/basics/hello.c -o hello -lm
./hello
```

**External runtime** — for full graphics (SDL2, windows, images) you must link the runtime and SDL2. See [Runtime modes](#runtime-modes-inline-vs-external) below.

---

## 6. Explore the pipeline

You can stop after any stage to inspect output:

| Flag        | Stage            | Output                    |
|-------------|------------------|---------------------------|
| `--tokens`  | Lexer            | Tokens                    |
| `--ast`     | Parser           | Abstract syntax tree      |
| `--typed-ir`| Semantic         | Typed IR                  |
| `--emit-c`  | Code generation  | C source (and then gcc)   |

Examples:

```bash
cargo run --release -- examples/hello.bas --tokens
cargo run --release -- examples/hello.bas --ast
cargo run --release -- examples/hello.bas --typed-ir
cargo run --release -- examples/hello.bas --emit-c
```

Use `--verbose` for extra details.

---

## 7. Runtime modes: inline vs external

| Mode       | `--runtime` | Use case              | Link command                           |
|------------|-------------|------------------------|----------------------------------------|
| **Inline** | `inline` (default) | Console, simple I/O, learning | `gcc prog.c -o prog -lm`              |
| **External** | `external`  | Graphics, audio, SDL2 | `gcc -I runtime/include prog.c -L target/release -lqb64fresh_rt $(pkg-config --libs sdl2) -lm -lpthread -ldl -o prog` |

**Inline:** Graphics calls are stubs (e.g. no-op with frame limiting). Fine for `PRINT`, `INPUT`, files, and most logic.

**External:** Build the runtime first, then pass `--runtime external` and link as above. See [GRAPHICS.md](GRAPHICS.md) and [CLAUDE.md](../CLAUDE.md) for full commands.

---

## 8. Common options

| Option            | Description                                      |
|-------------------|--------------------------------------------------|
| `-o FILE`         | Output `.c` file path                            |
| `--no-preprocess` | Skip `$INCLUDE` expansion                        |
| `--no-shell`      | Disable `SHELL`/`_SHELLHIDE` (compile error if used) |
| `--write-preprocessed FILE` | Write preprocessed source to a file      |

---

## Next steps

- **[QB64Fresh Handbook](QB64Fresh_HANDBOOK.md)** — Language basics, types, control flow, graphics, audio, files, `DECLARE LIBRARY`
- **[Examples](examples/)** — `basics/`, `graphics/`, `games/`, `audio/`, `files/`, `advanced/`
- **[Language Reference](QB64Fresh_LANGUAGE_REFERENCE.md)** — All supported keywords and functions
- **[DEVELOPMENT.md](DEVELOPMENT.md)** — Build, test, contribute
- **[VSCode extension](../README.md#vscode-extension)** — LSP, format, lint, build & run

---

*Last updated: 2026-01-26*
