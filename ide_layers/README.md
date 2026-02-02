# QB64pe IDE — Layer-by-Layer Build

Build the IDE **incrementally**: one layer creates a window, then add **one** item, test. If it breaks, we found the culprit. If not, add the next single item. Repeat until we reach the full IDE or find where it breaks.

Each **layer** is a **separate version**. Do not edit a previous layer; add a new directory for the next step.

## Layers

| Layer | What it is | Pass criterion |
|-------|------------|----------------|
| **layer0** | Minimal C: show window, event loop, exit on close. | Window appears, closes with X. |
| **layer1** | Layer0 + `qb_gfx_screen(-1,-1,1,0)` before show (IDE screen init). | Window appears, closes with X. |
| **layer1_step2** | Layer1 + **one addition:** `qb_screenhide()` before `qb_screenshow()` (IDE’s $SCREENHIDE then _SCREENSHOW). | Window appears, closes with X. If not, the break is hide-before-show. |
| **layer1_step3** | Layer1_step2 + `qb_icon()` after `qb_screenshow()` (IDE's _ICON). | Window appears, closes with X. |
| **layer1_step4** | Layer1_step3 + `qb_screenmove(0, 0)` (IDE's _SCREENMOVE). | Window appears, closes with X. |
| **layer1_step5** | Layer1_step4 + **batch:** `qb_gfx_cls()`, `qb_gfx_display()`, `qb_gfx_autodisplay(1)`. | If it breaks, remove calls one by one to find culprit (see layer1_step5/README.md). |
| **layer1_step6** | Layer1_step5 + title, color, print one line. | Window shows title and text; close with X. |
| **layer1_step7** | Layer1_step6 + **~25% IDE-like calls:** dimensions, colors, cursor, view/window/palette resets, drawing (pset/line/box/circle), mouse, keyboard. **Bisect:** `qb_gfx_view_reset`/`qb_gfx_window_reset` hide shapes — commented out in step7 so shapes show. | Title + text + shapes; close with X. |
| **layer1_step8** | Layer1_step7 + **another ~25% (~50% total):** viewport/window/coords, point/pset_step/line_step/paint, images/font/fullscreen/console (view_print, inkey, etc.). | Title + text + shapes; close with X. |
| **layer1_step9** | Layer1_step8 + **another ~25% (~75% total):** more mouse (button, wheel, movement, move), printstring, icon1, print_newline, cls, box_step, circle_step. | Title + text + shapes; close with X. |
| **layer1_step10** | Layer1_step9 + **final batch (~100% total):** keydown_vk/keyup_vk, print_tab/space/flush, depthbuffer, screenclick. | Same as step9; close with X. |
| **layer2a** | **BASIC → C:** Compile `01_window_display.bas` — SCREEN 12, PRINT, event loop. | Window with text "OK - Esc or X to close", closes with X or Esc. |
| **layer2b** | `01_window_display.bas` with `_PRINTSTRING` — tests graphics text rendering. | Same as layer2a. |
| **layer2c** | `02_event_loop.bas` — INKEY$ echo loop. | Keys echoed to window, Esc exits. |
| **layer2d** | `03_graphics_image.bas` — _NEWIMAGE, _DEST, LINE, _PUTIMAGE. | Yellow box displayed for 2 sec. |
| **layer2e** | `04_font_text.bas` — _PRINTSTRING, _FONTHEIGHT, _PRINTWIDTH. | Text displayed for 2 sec. |
| **layer2f** | `05_file_io.bas` — OPEN, PRINT #, INPUT #, CLOSE. | File I/O test output. |
| **layer2g** | `06_filesystem.bas` — _CWD$, _DIR$, _FILEEXISTS. | Filesystem info output. |
| **layer2** | Full IDE: qb64pe.bas compiled to C, linked with runtime. | IDE window appears, closes with X. |

## How to run

From **QB64Fresh** repo root:

1. Build the runtime once:  
   `cargo build -p qb64fresh-runtime --release --features graphics-sdl2`

2. Run a layer (each has its own directory):
   - **layer0:** `./ide_layers/layer0/build.sh` then `./ide_layers/layer0/run.sh`
   - **layer1:** `./ide_layers/layer1/build.sh` then `./ide_layers/layer1/run.sh`
   - **layer1_step2:** `./ide_layers/layer1_step2/build.sh` then `./ide_layers/layer1_step2/run.sh`
   - **layer1_step3:** `./ide_layers/layer1_step3/build.sh` then `./ide_layers/layer1_step3/run.sh`
   - **layer1_step4:** `./ide_layers/layer1_step4/build.sh` then `./ide_layers/layer1_step4/run.sh`
   - **layer1_step5:** `./ide_layers/layer1_step5/build.sh` then `./ide_layers/layer1_step5/run.sh`
   - **layer1_step6:** `./ide_layers/layer1_step6/build.sh` then `./ide_layers/layer1_step6/run.sh` (title + text)
   - **layer1_step7:** `./ide_layers/layer1_step7/build.sh` then `./ide_layers/layer1_step7/run.sh` (view_reset/window_reset commented so shapes show)
   - **layer1_step7_bisect:** `./ide_layers/layer1_step7_bisect/run.sh` (B only). **layer1_step7_no_A**, **layer1_step7_no_view_window** — bisect variants (build then run).
   - **layer1_step8:** `./ide_layers/layer1_step8/build.sh` then `./ide_layers/layer1_step8/run.sh` (~50% total)
   - **layer1_step9:** `./ide_layers/layer1_step9/build.sh` then `./ide_layers/layer1_step9/run.sh` (~75% total)
   - **layer2a** (BASIC→C): `./ide_layers/layer2a/build.sh` then `./ide_layers/layer2a/run.sh`
   - **layer2b**: `./ide_layers/layer2b/build.sh` then `./ide_layers/layer2b/run.sh`
   - **layer2c**: `./ide_layers/layer2c/build.sh` then `./ide_layers/layer2c/run.sh`
   - **layer2d**: `./ide_layers/layer2d/build.sh` then `./ide_layers/layer2d/run.sh`
   - **layer2e**: `./ide_layers/layer2e/build.sh` then `./ide_layers/layer2e/run.sh`
   - **layer2f**: `./ide_layers/layer2f/build.sh` then `./ide_layers/layer2f/run.sh`
   - **layer2g**: `./ide_layers/layer2g/build.sh` then `./ide_layers/layer2g/run.sh`
   - **layer2** (full IDE): `./ide_layers/layer2/build.sh` then `./ide_layers/layer2/run.sh`

## Finding the break

1. Confirm layer1 works (window appears, closes with X).
2. Run layer1_step2, then layer1_step3, … layer1_step9, layer1_step10. If the window does **not** appear at a step, that addition (or one of the batch) is where it breaks. Then try **layer2** (full IDE).
3. **Batches:** If a step adds several calls and it breaks, bisect: comment out half the batch, rebuild, run. If it works, the culprit is in the commented half; if not, in the other half. Repeat until one call remains. (layer1_step7 uses Batches A/B/C for this.)
4. **BASIC layers:** After layer1_step10, run **layer2a** (compile `01_window_display.bas` to C). If that works, add further IDE-equivalence BASIC programs (02_event_loop.bas, etc.) as new layers until you reach layer2 (full IDE).

## Going back

Each layer is self-contained. To “go back”, run an earlier layer’s `run.sh`. Do not edit a previous layer when adding a new step.
