# Session 084: QB64pe compile with QB64Fresh and run IDE GUI

**Date:** 2026-01-28  
**Summary:** Built QB64Fresh, used it to compile QB64pe in the QB64pe directory, built the C output with the external runtime, and launched the QB64pe IDE GUI.

## Accomplished

1. **Built QB64Fresh**  
   - `cargo build --release` in QB64Fresh (binary: `target/release/qb64fresh`).

2. **Compiled QB64pe with QB64Fresh**  
   - From QB64pe directory, with memory limit:
     - `ulimit -v 16777216 && qb64fresh source/qb64pe.bas --emit-c -o qb64pe_fresh.c`
   - Generated **qb64pe_fresh.c** (~114,934 lines) with default `--runtime external`.

3. **Built runtime and linked**  
   - Built runtime: `cargo build --release -p qb64fresh-runtime` → `target/release/libqb64fresh_rt.a`.
   - First gcc link failed with: `undefined reference to symbol 'wl_event_queue_destroy'` (Wayland).
   - Added `-lwayland-client` to the link line; link succeeded.
   - Final command (from QB64pe):
     ```bash
     gcc -I /path/to/QB64Fresh/runtime/include qb64pe_fresh.c \
       -L /path/to/QB64Fresh/target/release -lqb64fresh_rt \
       $(pkg-config --libs sdl2) -lwayland-client -lasound -lm -lpthread -ldl -o qb64pe_fresh
     ```

4. **Ran QB64pe IDE**  
   - `./qb64pe_fresh` from QB64pe directory (started in background). Process ran with no stderr/stdout errors; IDE GUI expected to be visible on display.

## Decisions / Notes

- **Include path:** QB64Fresh preprocessor uses the **parent of the input file** as the base for `$INCLUDE`. Using `source/qb64pe.bas` from the QB64pe directory correctly resolves paths like `global\version.bas` to `source/global/version.bas`.
- **Linker:** On this Linux setup, SDL2 pulls in Wayland; linking requires `-lwayland-client` in addition to `$(pkg-config --libs sdl2)`.
- **Run directory:** The QB64pe binary must be run from the QB64pe repo root so it can find `internal/`, `source/`, etc.

## Commands reference

```bash
# 1. Build QB64Fresh
cd QB64Fresh && cargo build --release

# 2. Compile QB64pe to C (from QB64pe directory, with memory limit)
cd QB64pe && bash -c 'ulimit -v 16777216 && /path/to/QB64Fresh/target/release/qb64fresh source/qb64pe.bas --emit-c -o qb64pe_fresh.c'

# 3. Build runtime
cd QB64Fresh && cargo build --release -p qb64fresh-runtime

# 4. Compile and link (from QB64pe)
gcc -I QB64Fresh/runtime/include qb64pe_fresh.c -L QB64Fresh/target/release -lqb64fresh_rt $(pkg-config --libs sdl2) -lwayland-client -lasound -lm -lpthread -ldl -o qb64pe_fresh

# 5. Run IDE
./qb64pe_fresh
```
