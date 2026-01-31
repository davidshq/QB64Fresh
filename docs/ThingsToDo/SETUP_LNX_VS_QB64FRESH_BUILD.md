# setup_lnx.sh vs QB64Fresh Build Path

This document walks through how **setup_lnx.sh** (and the QB64pe Makefile with `OS=lnx`) work, then compares that to compiling QB64pe with **QB64Fresh** and identifies gaps.

**Note:** There is no `make_lnx.sh` in QB64pe. The Linux build is driven by **setup_lnx.sh** (installer) and **Makefile** with `OS=lnx`.

**Repo layout:** QB64pe and QB64Fresh are sibling directories (e.g. under `qb64contain/`). The QB64Fresh-built IDE flow uses **QB64pe/run_qb64pe_fresh.sh** and **QB64pe/BUILD_WITH_QB64FRESH.md** (both live in the QB64pe tree).

---

## 1. How setup_lnx.sh Works

### 1.1 Script location and flow

- **File:** `QB64pe/setup_lnx.sh`
- **Runs from:** QB64pe directory (script `cd`s to `$(dirname "$0")`).

**Steps:**

1. **Package installation (distro-dependent)**  
   Detects distro (Arch, Debian/Ubuntu/Mint/Zorin, Fedora/RHEL/CentOS, Void) and installs build deps, e.g.:
   - Debian: `build-essential`, `x11-utils`, `mesa-common-dev`, `libglu1-mesa-dev`, `libasound2-dev`, `libpng-dev`, `libcurl4-openssl-dev`
   - Arch: `gcc`, `make`, `zlib`, `curl`
   - Fedora: `gcc-c++`, `make`, `mesa-libGLU-devel`, `alsa-lib-devel`, `libpng-devel`, `libcurl-devel`

2. **Clean**  
   `make clean OS=lnx`  
   Removes object files, temp builds (see Makefile `CLEAN_LIST`); keeps e.g. `internal/temp/temp.bin`.

3. **Build**  
   `make OS=lnx BUILD_QB64=y -j3`  
   Builds the `qb64pe` executable (see Makefile flow below).

4. **Post-build (if `./qb64pe` exists)**  
   - Writes `./run_qb64pe.sh` (cd to QB64pe, run `./qb64pe &`).
   - Adds `~/.local/share/applications/qb64pe.desktop` (menu entry, icon, Exec, Path).
   - Optionally runs `./qb64pe &`.

---

## 2. How the Makefile (OS=lnx, BUILD_QB64=y) Works

### 2.1 Variables (OS=lnx)

- `PATH_INTERNAL := ./internal`
- `PATH_INTERNAL_SRC := ./internal/source`
- `PATH_INTERNAL_TEMP := ./internal/temp`
- `PATH_INTERNAL_C := ./internal/c`
- `PATH_LIBQB := ./internal/c/libqb`
- `PLATFORM := posix`, no `EXTENSION` for executable name.
- CXXFLAGS: `-no-pie`, `-std=gnu++20`, `-fno-strict-aliasing`, `-DFREEGLUT_STATIC`, etc.
- CXXLIBS: `-lGL -lGLU -lX11 -lpthread -ldl -lrt -lxcb`, and later `-lm -lasound` (audio), curl, etc.

### 2.2 BUILD_QB64=y

1. **Copy source → temp**  
   `internal/source/*` → `internal/temp/`  
   So all pre-generated `.txt` files (main.txt, chain.txt, global.txt, regsf.txt, dyninfo.txt, clear.txt, inpchain.txt, onstrig.txt, onkey.txt, ontimer.txt, maindata.txt, mainerr.txt, runline.txt, ontimerj.txt, onkeyj.txt, onstrigj.txt, plus data*.txt, free*.txt, ret*.txt, etc.) land in `internal/temp/`.

2. **Dependencies for QB64pe exe**  
   Sets: `DEP_FONT`, `DEP_ICON`, `DEP_ICON_RC`, `DEP_SOCKETS`, `DEP_HTTP`, `DEP_CONSOLE`, `DEP_ZLIB`.

3. **Include libqb and parts**  
   - `internal/c/libqb/build.mk`
   - Parts: audio, core, input/game_controller, video/font, video/image, gui, network/http, data, os/clipboard.

4. **Main program object**  
   - `QB_QBX_SRC := internal/c/qbx$(TEMP_ID).cpp` (usually `internal/c/qbx.cpp`).
   - `QB_QBX_OBJ` depends on `$(wildcard internal/temp/*.txt)`.
   - So `qbx.o` is rebuilt when any of the temp `.txt` files change.

5. **qbx.cpp**  
   - Lives in repo at `internal/c/qbx.cpp`.
   - It is a **C++ wrapper** that:
     - Includes libqb and part headers.
     - `#include "../temp/global.txt"`, `"../temp/regsf.txt"`, `"../temp/dyninfo.txt"`, `"../temp/clear.txt"`, `"../temp/inpchain.txt"`, `"../temp/chain.txt"`, `"../temp/onstrig.txt"`, `"../temp/onkey.txt"`, `"../temp/ontimer.txt"`, `"../temp/maindata.txt"`, `"../temp/mainerr.txt"`, `"../temp/runline.txt"`, `"../temp/ontimerj.txt"`, `"../temp/onkeyj.txt"`, `"../temp/onstrigj.txt"`, **`"../temp/main.txt"`** (the compiled BASIC-as-C++).
   - So the “compiled program” is the content of `internal/temp/main.txt` (and related .txt) produced by the **QB64pe compiler** (BASIC→C++ emitter). The repo ships a **pre-generated** `internal/source/` tree that is copied to `internal/temp/` so `make` can build without running the QB64pe IDE.

6. **Link**  
   `exe` target: link `$(EXE_OBJS)` (includes `libqb_make_*.o`, `qbx.o`, icon if win) with `$(EXE_LIBS)` (libqb parts, FreeType, audio, data, gui, http, core, GLEW, etc.) and `$(CXXLIBS)` → produces `qb64pe` (or `EXE=` name).

7. **Optional**  
   - Strip (if `STRIP_SYMBOLS` not `n`): `objcopy --only-keep-debug`, `objcopy --strip-unneeded`.
   - License: `GENERATE_LICENSE=y` → cat license files into `$(EXE).license.txt`.

### 2.3 Summary of native path

- **Input:** Pre-generated `internal/source/*.txt` (from a prior run of the QB64pe compiler on `qb64pe.bas`).
- **Build:** Copy to `internal/temp/`, compile `internal/c/qbx.cpp` (which #includes those .txt), build libqb and parts, link → `qb64pe`.
- **Runtime stack:** C++ + libqb + OpenGL/FreeGLUT, ALSA, libpng, libcurl, X11, etc.

---

## 3. How We Build QB64pe with QB64Fresh

### 3.1 Our path (QB64pe/run_qb64pe_fresh.sh and QB64pe/BUILD_WITH_QB64FRESH.md)

1. **Build QB64Fresh and runtime**  
   - From QB64Fresh: `cargo build --release` (qb64fresh CLI).  
   - `cargo build -p qb64fresh-runtime --release --features graphics-sdl2`.

2. **Compile QB64pe BASIC → C**  
   - From `QB64pe/source`, with memory cap (e.g. 16GB):  
     `ulimit -v 16777216 && ../../QB64Fresh/target/release/qb64fresh qb64pe.bas --emit-c --runtime external -o qb64pe_fresh.c`  
   - Output: `QB64pe/source/qb64pe_fresh.c`.  
   - We do **not** use `internal/source`, `internal/temp`, or `internal/c/qbx.cpp`.

3. **Link**  
   - From `QB64pe/source`:  
     `gcc -I ../../QB64Fresh/runtime/include qb64pe_fresh.c -L ../../QB64Fresh/target/release -lqb64fresh_rt $(pkg-config --libs sdl2 alsa wayland-client) -lm -lpthread -ldl -o ../qb64pe_fresh`  
   - Executable is written to **QB64pe root** (`qb64pe_fresh`), not inside `source/`. Single C file + our runtime; no Makefile, no libqb.

4. **Run**  
   - From QB64pe root: ensure `settings/recent.bin` and `source/settings/recent.bin` exist (script creates both), then run with memory limit and IDE compat:  
     `ulimit -v 4194304 && QB64FRESH_IDE_COMPAT=1 ./qb64pe_fresh`  
   - For lower-memory runs, use **QB64pe/run_qb64pe_fresh_limited.sh** (tighter ulimit).

### 3.2 What we do not do (vs setup_lnx + Makefile)

- We do **not** run `setup_lnx.sh` or `make OS=lnx`.
- We do **not** install distro packages (we assume SDL2, ALSA, Wayland are present; no package script).
- We do **not** copy `internal/source` → `internal/temp`.
- We do **not** build or use libqb, FreeGLUT, or any QB64pe C++ parts.
- We do **not** produce or use `internal/c/qbx.cpp` or any `internal/temp/*.txt`.
- We do **not** create `run_qb64pe.sh` or a `.desktop` file under QB64pe.
- We do **not** generate a license file for the executable.
- We do **not** strip the binary or keep separate debug symbols.

---

## 4. Gaps and Recommendations

### 4.1 Things we could add when “compiling QB64pe” with QB64Fresh

| Item | setup_lnx / Make | Our build | Recommendation |
|------|------------------|-----------|----------------|
| **Package / dependency check** | setup_lnx installs gcc, make, mesa, alsa, libpng, libcurl | None | Optional: document or add a small script that checks for SDL2, ALSA, Wayland (e.g. `pkg-config --exists sdl2 alsa wayland-client`) and suggests packages (see below). |
| **run script in QB64pe** | Creates `run_qb64pe.sh` (cd + `./qb64pe &`) | We run from our script only | Optional: have `run_qb64pe_fresh.sh` (or a “post-build” step) write `QB64pe/run_qb64pe_fresh.sh` so users can start the IDE from the QB64pe tree without re-running the full build. |
| **Desktop entry** | Creates `~/.local/share/applications/qb64pe.desktop` | None | Optional: add a step or doc to create a similar .desktop for `qb64pe_fresh` if desired. |
| **License file** | Makefile can generate `$(EXE).license.txt` | None | Low priority unless you need to ship a combined license for the QB64Fresh-built IDE. |
| **Strip / debug symbols** | Makefile can strip and keep debug with objcopy | We don’t strip | Optional: add `strip` or `objcopy` to our link step if you want smaller binaries or separate debug files. |
| **settings/recent.bin** | N/A (IDE creates when needed; may error on first run) | We create before run to avoid Error 53 | **Done** in run script and documented in BUILD_WITH_QB64FRESH.md. |

### 4.1.1 Optional dependency check (Linux)

Before building with QB64Fresh, you can verify system libraries used at link time:

```bash
pkg-config --exists sdl2 alsa wayland-client && echo "OK" || echo "Missing: install libsdl2-dev, libasound2-dev, libwayland-dev (Debian/Ubuntu) or equivalent"
```

Debian/Ubuntu: `libsdl2-dev`, `libasound2-dev`, `libwayland-dev`. Fedora: `SDL2-devel`, `alsa-lib-devel`, `wayland-devel`. Arch: `sdl2`, `alsa-lib`, `wayland`.

### 4.2 “Make” from inside the IDE (important)

When the user runs **Run/Make** inside the QB64pe IDE:

- The IDE builds **makeline$** (see `qb64pe.bas`: `GetMakeExecutable$`, `OS=lnx`, `EXE=...`, `CXXFLAGS_EXTRA`, etc.) and runs it via **SHELL _HIDE** (e.g. `make OS=lnx EXE='...' ... 1>> compilelog$ 2>&1`).
- That invokes the **QB64pe Makefile**, which expects:
  - `internal/temp/*.txt` to exist (written by the **IDE’s own compiler** when it compiles the current project).
  - `internal/c/qbx.cpp` to be the driver that `#include`s those .txt files.
- So **Make** always builds a **C++ + libqb** executable, not a QB64Fresh C + libqb64fresh_rt executable.

Therefore:

- **Compiling *another* program (e.g. user’s game.bas) from the QB64Fresh-built IDE:**  
  Make still runs the **QB64pe** in-IDE compiler (emits to `internal/temp/*.txt`), then runs `make OS=lnx` → result is **C++ + libqb**. So the IDE built with QB64Fresh can still compile other programs the “QB64pe way”; no change needed unless you want a “Compile with QB64Fresh” option.

- **If we wanted “Compile with QB64Fresh” from the IDE:**  
  We would need the IDE to call **qb64fresh** + **gcc** + **libqb64fresh_rt** instead of the built-in emitter + make. That would require IDE changes (e.g. new menu option or build mode that runs a wrapper script or different command). Not required for “IDE runs and can compile user programs with the existing QB64pe pipeline.”

### 4.3 Summary

- **setup_lnx.sh** = distro package install + `make clean OS=lnx` + `make OS=lnx BUILD_QB64=y` + run script + desktop entry. It builds the **native** QB64pe (C++ + libqb, OpenGL/FreeGLUT, etc.).
- **Our path** = build QB64Fresh + runtime, emit single C from `qb64pe.bas`, link with our runtime (SDL2, ALSA, Wayland). We intentionally **do not** use the Makefile or libqb for the IDE binary.
- **Gaps we might implement:** dependency check or doc, optional `run_qb64pe_fresh.sh` and .desktop in QB64pe, optional strip/license. **Not required for “QB64pe compiled with QB64Fresh runs and IDE works.”**
- **IDE “Make”** will always use the QB64pe Makefile and produce a libqb executable for the *current* project; that is consistent with the existing IDE design. Supporting “compile current project with QB64Fresh” would be a separate feature (IDE + possibly wrapper script).

---

*Created: 2026-01-30. Last updated: 2026-01-31. Reflects QB64pe setup_lnx.sh and Makefile (OS=lnx, BUILD_QB64=y) and QB64pe run_qb64pe_fresh.sh / BUILD_WITH_QB64FRESH.md; aligned section 3 with actual script paths, ulimits, and executable location; added optional dependency-check example.*
