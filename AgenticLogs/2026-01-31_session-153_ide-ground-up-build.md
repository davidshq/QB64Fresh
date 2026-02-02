# Session 153: QB64pe IDE Build from Ground Up

**Date:** 2026-01-31

## Summary

Building the QB64pe IDE from the ground up: start with the barest runnable (minimal window), run it, layer functionality, fix issues. Goal: working QB64pe IDE that can successfully compile QB64pe (native pipeline: transpiler + make + libqb), not just our rewrite.

## Plan (layers)

1. **Layer 0 – Barest runnable:** Minimal .bas that opens a window and runs event loop (e.g. `tests/ide_equivalence/01_window_display.bas`). Compile with QB64Fresh (--runtime external), link with libqb64fresh_rt, run. Verify window appears and closes. Confirms QB64Fresh + runtime work for minimal GUI.
2. **Layer 1 – IDE starts:** Build qb64pe_fresh (qb64pe.bas → C via qb64fresh, link libqb64fresh_rt). Run from QB64pe root via `run_qb64pe_fresh.sh`. Verify IDE window appears, doesn’t crash, closes with X. Fix startup issues (recent.bin, SDL, stop_program, etc.).
3. **Layer 2 – IDE Run/Make:** In IDE: File → New, paste minimal .bas, Save As, Run/Make. IDE transpiles to C++, SHELLs make. Requires CWD = QB64pe root (internal/, Makefile present), make and g++ on PATH. Fix path/CWD, make invocation, compilelog.
4. **Layer 3 – Polish:** File menu, recent list, dialogs; fix as needed.

## Decisions

- Use existing `run_qb64pe_fresh.sh` and BUILD_WITH_QB64FRESH.md as reference; add a dedicated “ground up” plan doc under docs/ThingsToDo.
- Layer 0 uses `tests/ide_equivalence/01_window_display.bas` (SCREEN 12, _TITLE, evnt, INKEY$). If evnt/SCREEN 12 fail under QB64Fresh we document and either fix runtime/codegen or use an even smaller repro (e.g. $CONSOLE:ONLY + PRINT) for “window” equivalence later.
- IDE compile path remains the **native** QB64pe path: transpiler in qb64pe.bas writes C++ to internal/temp, then SHELL make. No change to use QB64Fresh for user programs in this phase; goal is “IDE works and can compile QB64pe.”

## Progress

- [x] Layer 0: minimal window build/run — compiled 01_window_display.bas with QB64Fresh (external runtime), linked, ran; no crash (timeout 3s).
- [x] Layer 1: IDE starts — built qb64pe_fresh (qb64pe.bas → C, link libqb64fresh_rt), ran from QB64pe root with run setup; close button fix verified.
- [x] Layer 2: full IDE runs — user confirmed layer2 (build.sh + run.sh) works; IDE window appears and closes with X.
- [ ] Layer 2 Run/Make (next): **Manual test in IDE only.** Start IDE (run.sh), File → Open source/hello.bas, Run → Make; confirm exe produced. Do not use `qb64pe_fresh -x` for this test (no GUI, can hang/timeout). Steps in ide_layers/layer2/README.md. source/hello.bas already created.
- [ ] Layer 3: polish

## Revert and layer-by-layer (separate versions)

User asked to delete what was done and start again: build the IDE layer by layer, **don’t add code**, keep each version separate so they can go back.

**Reverted:**
- Runtime: removed all IDE-compat logic from `qb_screenhide()` and `qb_gfx_screen()` in `runtime/src/graphics_ffi.rs`.
- Deleted: `QB64pe/run_layer1_steps.sh`, `QB64Fresh/scripts/build_qb64pe_ide_ground_up.sh`.

**New: ide_layers (separate versions)**
- **ide_layers/README.md** — Describes layer0, layer1, layer2; each layer is a separate version; do not edit a previous layer.
- **ide_layers/layer0/** — Minimal C: `qb_screenshow()`, event loop until `stop_program`. No QB64pe. `main.c`, `build.sh`, `run.sh`. Proves runtime + SDL can show a window.
- **ide_layers/layer1/** — Same but with `qb_gfx_screen(-1,-1,1,0)` before show. Separate `main.c`, `build.sh`, `run.sh`.
- **ide_layers/layer2/** — build.sh, run.sh, README.md. build.sh builds QB64Fresh, runtime, compiles qb64pe.bas to C, links qb64pe_fresh. run.sh runs from QB64pe root with ulimit and QB64FRESH_IDE_COMPAT=1. Points to QB64pe/BUILD_WITH_QB64FRESH.md.

**Docs:** QB64PE_IDE_GROUND_UP_BUILD.md and QB64PE_IDE_LAYER1_STEPS.md now point to ide_layers only.

**How to run:** From QB64Fresh repo root: `./ide_layers/layer0/build.sh` then `./ide_layers/layer0/run.sh`. If layer0 shows a window, try layer1; if layer1 works, try layer2 (full IDE). If a layer fails, run the previous layer to go back.

**Layer1: blank black screen, close button not working**

- **Blank screen:** Expected. Layer1 calls `qb_gfx_screen(-1, -1, 1, 0)` so active page = 1, visual page = 0. We show page 0 (cleared to black) while drawing would go to page 1. See `ide_layers/layer1/README.md`.
- **Close button:** Event loop must call `qb_gfx_poll_events()` (so SDL Quit/Close sets `stop_program`) and yield to the OS so the WM can deliver events. Fix: added `qb_sleep(0.01)` in the layer0 and layer1 event loops so we don’t spin at 100% CPU; close (X) and Escape now exit. Rebuild runtime and layer1 after any runtime change. **Verified:** user confirmed close button works after rebuild.

## Deliverables

- **ide_layers/README.md** — Layer-by-layer approach; layer0, layer1, layer2 as separate versions.
- **ide_layers/layer0/** — main.c, build.sh, run.sh (minimal window only).
- **ide_layers/layer1/** — main.c, build.sh, run.sh (qb_gfx_screen then show).
- **ide_layers/layer2/** — build.sh, run.sh, README.md (full IDE: qb64pe.bas → C, link, run).
- **docs/ThingsToDo/QB64PE_IDE_GROUND_UP_BUILD.md** — Points to ide_layers.
- **docs/ThingsToDo/QB64PE_IDE_LAYER1_STEPS.md** — Points to ide_layers.
