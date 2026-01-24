# QB64Fresh Installer and Distribution Plan

**Status:** Plan  
**Created:** 2026-01-24

This document outlines a plan to distribute QB64Fresh so that **users do not need Rust** to use the compiler, and to provide installers that handle QB64Fresh binaries plus the C toolchain.

---

## Goals

1. **No Rust required** — End users run pre-compiled binaries only.
2. **Pre-compiled binaries** — Windows, macOS, and Linux (primary architectures).
3. **C compiler handling** — Installer detects, guides, or installs a C compiler so users can build executables from generated C.
4. **Clear scope** — What we ship, what we expect from the system, and what is optional.

---

## 1. What We Distribute

### 1.1 Binaries (required in all installers)

| Binary | Purpose | Priority |
|--------|---------|----------|
| `qb64fresh` | Compiler CLI (lexer → parser → semantic → codegen → C) | **Essential** |
| `qb64fresh-lsp` | Language Server for IDE integration (VSCode, etc.) | **High** |

### 1.2 Optional / bundled-in-full-install

| Binary | Purpose | When to include |
|--------|---------|-----------------|
| `qb64fresh-fmt` | Code formatter | Full/developer installs |
| `qb64fresh-lint` | Static analyzer | Full/developer installs |
| `qb64fresh-debug` | DAP debugger (infrastructure) | Optional, niche |
| `fix_encoding` | Legacy encoding (CP437, Latin1) conversion | Optional, niche |

**Recommendation:**  
- **Minimal install:** `qb64fresh`, `qb64fresh-lsp`  
- **Full install:** also `qb64fresh-fmt`, `qb64fresh-lint`

### 1.3 Runtime artifacts (for `--runtime external`)

| Artifact | Purpose |
|----------|---------|
| `libqb64fresh_rt.a` (Linux/macOS) or `qb64fresh_rt.lib` (Windows) | Static runtime library for graphics/audio/dialogs |
| `qb64fresh_rt.h` | C header for linking |

**Note:** The external runtime links against **SDL2** (and on Linux, often **ALSA** for audio). We do not necessarily bundle these; the plan below treats SDL2 as a system/sideload dependency for `--runtime external`.

---

## 2. Runtime Modes (affects what the installer must provide)

| Mode | C compiler | Runtime library | SDL2 / ALSA | When to use |
|------|------------|-----------------|-------------|-------------|
| `--runtime inline` (default) | **Required** | None (embedded in generated C) | Not needed | Console, file I/O, most learning/scripts |
| `--runtime external` | **Required** | `libqb64fresh_rt` + `qb64fresh_rt.h` | **Required** (SDL2; on Linux, ALSA typical) | Graphics, audio, dialogs, games |

**Installer implications:**

- For **inline** only: we need to ensure a C compiler is available. No runtime lib, no SDL2.
- For **external**: we must also provide (or clearly guide) the runtime lib, header, and SDL2. On Windows we may bundle SDL2 DLLs; on macOS/Linux we typically rely on system/Homebrew/apt.

---

## 3. C Compiler Handling by Platform

### 3.1 Windows

- **Preferred:** MinGW-w64 (GCC) — matches Linux/macOS behavior, one toolchain for all.
- **Alternatives:** MSVC (cl.exe) — possible but different flags and CRT; we’d need to document both.

**Installer options (in order of robustness):**

1. **Bundle MinGW-w64**
   - Ship a minimal, relocatable MinGW (e.g. winlibs build) under `QB64Fresh/mingw` and add `bin` to PATH for the user (or for the current user only).
   - **Pros:** Works offline, no extra download. **Cons:** Install size (~100–200 MB), need to pick arch (x64, possibly arm64).

2. **Detect and guide**
   - If `gcc`/`clang` in PATH or common locations (e.g. `C:\msys64\mingw64\bin`, `C:\mingw-w64\...`) → do nothing.
   - Else: offer to run `winget install -e --id mingw-w64.mingw-w64` or open a “Install MinGW” help page with direct links.

3. **Integrate with a C-toolchain manager**  
   - e.g. `scoop install gcc`, `choco install mingw` — we can detect and suggest, or document.

**Recommendation for v1:**  
- **Option 2** (detect + guide via winget / doc) to avoid install size and maintenance.  
- **Option 1** as an advanced “portable / offline” variant or second installer type later.

### 3.2 macOS

- **Standard:** Xcode Command Line Tools (provides `clang`, `lldb`).  
  - `xcode-select --install` opens the system dialog to install.

**Installer behavior:**

- Run `xcode-select -p` (or check for `clang` in PATH).  
- If missing: run `xcode-select --install` and show a short message: “Apple will open a dialog to install the Command Line Tools. Click Install and rerun the QB64Fresh installer or compile step when done.”

**Architectures:**  
- **x86_64** (Intel), **aarch64** (Apple Silicon). We should ship both if we do universal binaries, or at least `aarch64` for current Macs.

### 3.3 Linux

- **Standard:** `gcc` or `clang` from the distro.

**Installer behavior:**

- If `gcc` or `clang` in PATH → done.
- Else: suggest:
  - Debian/Ubuntu: `sudo apt install build-essential`
  - Fedora: `sudo dnf groupinstall "Development Tools"` or `sudo dnf install gcc`
  - Arch: `sudo pacman -S base-devel`
  - (We can maintain a short table in docs or in the installer UI.)

**Architectures:**  
- **x86_64** (priority), **aarch64** (e.g. Raspberry Pi, ARM servers).  
- Other (e.g. armv7, powerpc) as “best effort” or community builds.

---

## 4. Installer Scope by Platform

### 4.1 Windows

- **Formats to consider:**
  - **NSIS / Inno Setup / WiX / MSIX** — “Setup.exe” style: choose install dir, add to PATH, Start Menu shortcut, optionally desktop shortcut.
  - **Portable .zip** — Binaries + `qb64fresh_rt.h` (and optionally `libqb64fresh_rt.a` / `.lib`), a small `README.txt` with PATH and C compiler instructions. No C compiler installation.
- **PATH:** Add `QB64Fresh\bin` (or chosen prefix) to the user’s PATH (User env var preferred to avoid admin).
- **C compiler:** Per §3.1: detect first; if not found, guide (winget / doc). Optional: separate “QB64Fresh + MinGW” bundle.
- **Runtime (external):**  
  - Include `qb64fresh_rt.h` and prebuilt `qb64fresh_rt.lib` in the install (e.g. `include/`, `lib/`).  
  - **SDL2:** Either (a) document “install SDL2 and point linker at it”, or (b) bundle SDL2 DLLs in `bin` and document `%QB64FRESH%\bin` for runtime. (b) improves “it just runs” for new users.

### 4.2 macOS

- **Formats:**
  - **.pkg** — Standard installer: copy binaries to `/usr/local` or `/opt/qb64fresh`, or `~/Applications` / `~/bin` for non-admin.
  - **.dmg** — Drag `QB64Fresh` to `Applications` or a custom folder; postflight or README explains PATH.
  - **Homebrew (formula)** — `brew install qb64fresh` (or tap). Complements the pkg/dmg; good for developers.
- **PATH:**  
  - For pkg: add symlinks in `/usr/local/bin` or instruct user to add `/usr/local/qb64fresh/bin` (or chosen prefix) to `~/.zshrc` / `~/.bash_profile`.
  - For Homebrew: formula handles it.
- **C compiler:** §3.2 — `xcode-select --install` if no `clang`.
- **Runtime (external):**  
  - Include `libqb64fresh_rt.a` and `qb64fresh_rt.h`.  
  - SDL2: `brew install sdl2` — document and optionally run if `brew` is present and SDL2 missing.

### 4.3 Linux

- **Formats:**
  - **.deb / .rpm** — Integrate with package manager; put binaries in `/usr/bin` or `/usr/local/bin`, headers/libs in `/usr/include` / `/usr/lib`.
  - **AppImage** — Single file, no root; can include binaries + optional runtime; user must still have `gcc`/`clang` (and SDL2 if using external) on the host.
  - **Standalone .tar.xz** — e.g. `qb64fresh-{version}-linux-x86_64.tar.xz` with `bin/`, `include/`, `lib/`, README; user adds `bin` to PATH.
- **PATH:** Handled by .deb/.rpm; for tarball, document.
- **C compiler:** §3.3 — detect, suggest `apt`/`dnf`/`pacman` as appropriate.
- **Runtime (external):**  
  - Distribute `libqb64fresh_rt.a` and `qb64fresh_rt.h` in package or tarball.  
  - SDL2/ALSA: rely on system (`libsdl2-dev`, `libasound2-dev` or equivalent); document in README and, if we have a simple “check” command, we can report missing libs.

---

## 5. Build and Release Pipeline

### 5.1 CI / CD for binaries

- **GitHub Actions** (or equivalent) to:
  - Build `qb64fresh`, `qb64fresh-lsp`, and optionally `qb64fresh-fmt`, `qb64fresh-lint` (and `fix_encoding`, `qb64fresh-debug` if we include them) in release mode.
  - Build `qb64fresh-runtime` as a static lib (`libqb64fresh_rt.a` / `qb64fresh_rt.lib`) with `graphics-sdl2`, `audio-rodio`, `dialogs` (and optionally `graphics-sdl2-ttf`).
  - For runtime: we need SDL2 (and on Linux, ALSA) on the build workers; our current `sdl2`/`rodio` setup assumes that.

**Targets (minimum for v1):**

| OS | Arch | Compiler | Runtime (static lib) |
|----|------|----------|----------------------|
| Windows | x86_64 | MSVC or MinGW (pick one for our binary) | `qb64fresh_rt.lib` (MSVC) or `libqb64fresh_rt.a` (MinGW) — match the C compiler we recommend) |
| macOS | x86_64, aarch64 | clang | `libqb64fresh_rt.a` |
| Linux | x86_64 | gcc | `libqb64fresh_rt.a` |

- **Optional:** aarch64 Linux, and Windows arm64, as capacity allows.

### 5.2 Versioning and artifacts

- **Version:** Sync with `Cargo.toml` (e.g. `0.1.0`). Tags: `v0.1.0`.
- **Artifacts per release:**
  - `qb64fresh-{version}-windows-x86_64.zip` (or `.msi` if we have WiX/NSIS in CI)
  - `qb64fresh-{version}-macos-x86_64.tar.gz`, `qb64fresh-{version}-macos-aarch64.tar.gz` (or universal)
  - `qb64fresh-{version}-linux-x86_64.tar.xz`
  - Checksums (SHA-256) and optionally GPG signatures.

### 5.3 Installer assembly

- **Windows:** Use NSIS, Inno Setup, or WiX in a CI job that takes the built binaries + runtime `include/` and `lib/` (and optionally SDL2 DLLs) and produces `QB64Fresh-Setup-{version}.exe`.
- **macOS:** `pkgbuild` / `productbuild` to produce `.pkg`; or a `dmg` with app bundle / README.
- **Linux:** `dpkg`/`rpm` build in CI from the same binaries + include/lib.

---

## 6. What Else to Include

### 6.1 Documentation and examples

- **In install directory / package:**
  - `README.txt` or `GETTING_STARTED.txt`: minimal “compile and run” for `--runtime inline` and, in one paragraph, `--runtime external` plus SDL2.
  - Link or short section to the full [QB64Fresh Handbook](QB64Fresh_HANDBOOK.md) and [Migration Guide](QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md).
- **Optional:** A small `examples/` (e.g. `hello.bas`, one graphics `hello_gfx.bas`) in the install.

### 6.2 PATH and environment

- Installer must add the binary directory to the **user’s** PATH (not only system) where possible, so `qb64fresh` works in a new terminal.
- **Optional env var:** `QB64FRESH_HOME` or `QB64FRESH_ROOT` pointing to the install root, so users and IDEs can find `include/`, `lib/`, and any bundled SDL2.

### 6.3 Uninstaller

- **Windows:** Add/Remove Programs entry (standard with NSIS/Inno/WiX); remove our dir and PATH edits.
- **macOS:** pkg uninstall or “drag to Trash” for dmg; Homebrew: `brew uninstall`.
- **Linux:** `apt remove` / `dnf remove` / `pacman -R`; for tarball, document manual removal.

### 6.4 VSCode extension

- Do **not** bundle the VSCode extension in the installer (marketplace and side-load are better).
- **Do:** In README and in installer “Finish” screen, suggest: “For the best experience, install the VSCode extension: search ‘QB64Fresh’ in the Extensions view.”
- Optionally: `qb64fresh --version` or a `qb64fresh doctor` that reports “VSCode extension: not installed / installed”.

### 6.5 “Compile and run” UX (optional, future)

- Today: `qb64fresh foo.bas --emit-c` produces `foo.c`; user runs `gcc foo.c -o foo` (and for external: `-I... -L... -lqb64fresh_rt` + SDL2).
- **Future:** `qb64fresh foo.bas --run` or `qb64fresh foo.bas --build -o foo` that (1) emits C, (2) invokes the detected C compiler with the right flags, (3) optionally runs the binary. This would greatly simplify first-run experience and can be added in a later phase; the installer’s C compiler detection would then directly support this.

### 6.6 Integrity and trust

- **Checksums:** Publish SHA-256 for all binaries and installers.
- **Signing (as we grow):**
  - **Windows:** Authenticode signing for `qb64fresh.exe` and the installer (requires a cert).
  - **macOS:** Notarization for pkg/dmg and binaries; optionally Developer ID.
  - **Linux:** GPG-sign release tags and tarballs; .deb/.rpm can be signed.

### 6.7 Updates

- **v1:** Document “check GitHub Releases for new versions” and optionally `qb64fresh --version`.
- **Later:** In-installer “Check for updates” or a separate `qb64freshup` / `qb64fresh self update` that replaces the binary. Not required for the first installer.

---

## 7. Phased Rollout

### Phase 1 — Minimal (no installer, binaries only)

- [ ] GitHub Actions: build `qb64fresh` and `qb64fresh-lsp` for Windows x64, macOS (x64 + arm64), Linux x64.
- [ ] Publish tarballs/zips on GitHub Releases with SHA-256.
- [ ] README in each archive: how to add to PATH, how to install a C compiler (link to §3), and one-liner for `--runtime inline` compile.
- [ ] Do **not** yet build or ship the runtime static lib; only document `--runtime inline`.

### Phase 2 — C compiler and runtime

- [ ] Build and ship `libqb64fresh_rt.a` / `qb64fresh_rt.lib` and `qb64fresh_rt.h` in the same OS/arch as the compiler.
- [ ] Document `--runtime external`, link flags, and SDL2 (and ALSA on Linux) in the shipped README.
- [ ] Implement **detect + guide** for C compiler on each platform (scripts or small helper in the archive).

### Phase 3 — Installers

- [ ] **Windows:** NSIS or Inno Setup: choose dir, copy binaries + include + lib, add to PATH, C compiler detection + winget/doc guidance; optional SDL2 DLL bundle.
- [ ] **macOS:** .pkg or .dmg: copy to `/usr/local` or `/opt` or user-chosen dir, PATH instructions, `xcode-select --install` when no clang.
- [ ] **Linux:** .deb and .rpm (or at least .deb for Ubuntu/Debian); alternatively improve the tarball with a `install.sh` that does PATH and optional C compiler suggestions.

### Phase 4 — Polish

- [ ] Optional: `qb64fresh-fmt`, `qb64fresh-lint` in “full” install; separate “minimal” vs “full” on download page.
- [ ] Optional: `qb64fresh doctor` (or `--check-env`) for C compiler, SDL2 (if external), and VSCode extension.
- [ ] Optional: `--build` / `--run` in the compiler.
- [ ] Code signing / notarization where feasible.

---

## 8. Open Questions

1. **Windows C compiler:** Prefer recommending MinGW over MSVC for consistency, or support both with different distro variants?
2. **SDL2 on Windows:** Bundle SDL2 DLLs in the installer for `--runtime external`, or always “user installs SDL2”?
3. **Homebrew / apt / scoop / choco:** Do we maintain formulas/packages ourselves, or rely on community and only provide tarballs/installers?
4. **Architectures:** How early to add aarch64 Linux and Windows arm64?
5. **`qb64fresh --build`:** Priority for Phase 3 or 4?

---

## 9. References

- [ARCHITECTURE.md](../ARCHITECTURE.md) — Pipeline, runtime modes, inline vs external.
- [QB64Fresh Handbook](../QB64Fresh_HANDBOOK.md) — User-facing usage and options.
- [ARCHIVE / STUB_FUNCTIONS_FULL.md](../archive/STUB_FUNCTIONS_FULL.md) — Inline vs external runtime behavior.
- [DEVELOPMENT.md](../DEVELOPMENT.md) — How to build from source (for contributors, not end users).
