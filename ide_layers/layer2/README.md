# Layer 2: Full IDE (qb64pe_fresh)

Layer 2 is the full QB64pe IDE: qb64pe.bas compiled to C by QB64Fresh, linked with the runtime.

Do **not** add code here; use the existing QB64pe build. See **QB64pe/BUILD_WITH_QB64FRESH.md** and **QB64pe/run_qb64pe_fresh.sh**.

## Build and run (from QB64Fresh repo root)

```bash
./ide_layers/layer2/build.sh
./ide_layers/layer2/run.sh
```

- **build.sh** — Builds QB64Fresh, runtime, compiles qb64pe.bas to C, links `qb64pe_fresh` into QB64pe/.
- **run.sh** — Ensures settings/recent.bin, sets SDL_VIDEODRIVER, runs `qb64pe_fresh` with ulimit and QB64FRESH_IDE_COMPAT=1.

Alternative: from QB64pe, run `./run_qb64pe_fresh.sh` (same result; can use SKIP_BUILD=1 to skip build and just run).

## Manual steps (if you prefer)

From QB64Fresh repo root:

1. Build QB64Fresh and runtime: `cargo build --release` and `cargo build -p qb64fresh-runtime --release --features graphics-sdl2`
2. From QB64pe/source: `ulimit -v 16777216 && ../../QB64Fresh/target/release/qb64fresh qb64pe.bas --emit-c --runtime external -o qb64pe_fresh.c`
3. Link: `gcc -I ../../QB64Fresh/runtime/include qb64pe_fresh.c -L ../../QB64Fresh/target/release -lqb64fresh_rt $(pkg-config --libs sdl2 alsa wayland-client) -lm -lpthread -ldl -o ../qb64pe_fresh`
4. From QB64pe: `mkdir -p settings source/settings && touch settings/recent.bin source/settings/recent.bin`, then `ulimit -v 4194304 && ./qb64pe_fresh`

## Run/Make test (manual, in the IDE)

Run/Make must be tested **in the IDE** (no automated script): start the IDE, open a .bas file, then Run → Make.

1. **Start the IDE** (GUI): `ide_layers/layer2/run.sh` (from QB64Fresh root) or `./run_qb64pe_fresh.sh` from QB64pe. Ensure you run from QB64pe root so `internal/`, Makefile, and tools are available.
2. In the IDE: **File → Open** and open `source/hello.bas` (or `QB64Fresh/ide_layers/layer2/sample.bas`), or **File → New** and paste minimal code.
3. **File → Save As** and save under QB64pe (e.g. `source/hello.bas`) if needed.
4. **Run → Make** (or the IDE’s Make/Run action). The IDE will transpile the current program to C++, write to `internal/temp`, and SHELL make.
5. **Success:** an executable is produced (location depends on QB64pe’s Makefile; often `internal/temp/` or the source dir) and you can run it, or the compile log shows success without errors.

If Make fails: ensure CWD is QB64pe root when starting the IDE; `make` and `g++` on PATH; `internal/` and Makefile exist in QB64pe.

**Note:** `./qb64pe_fresh -x source/hello.bas` runs compile-from-CLI (no GUI). It can take a long time or hang in some environments; use the in-IDE Run → Make test instead.

## Troubleshooting

**Window never opens** — Run manually to see any errors:
```bash
cd /path/to/QB64pe && ulimit -v 4194304 && ./qb64pe_fresh 2>&1
```
If no window still: try forcing X11: `SDL_VIDEODRIVER=x11 ./qb64pe_fresh`. Run from a terminal in the same graphical session (not over SSH without X forwarding).

If layer0 or layer1 shows a window but layer2 does not, the issue is in the full IDE path (qb64pe.bas / generated C), not the runtime. See QB64pe/BUILD_WITH_QB64FRESH.md (black window, close button, IDE includes, Error 53).
