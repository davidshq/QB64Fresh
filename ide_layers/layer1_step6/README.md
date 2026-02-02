# Layer 1 step 6: Layer1_step5 + title, color, print one line

**Expected:** Window opens with **title** "Layer1 step 6" and **one line of text** "Layer1 step 6 - OK" (white on black). Close with X.

**Why (0,0) for pages:** We use `qb_gfx_screen(-1, -1, 0, 0)` here so drawing goes to the **displayed** page (active=0, visual=0). Earlier steps use (1, 0) to match the IDE; that draws to page 1 while showing page 0, so the window stays blank. Step6 uses (0, 0) so you see the text and color.

**Batch:** Set window title (A), `qb_gfx_color(7, 0)` (B), `qb_gfx_locate` + `qb_gfx_print` + `qb_gfx_display` (C). If something breaks, remove A, B, or C one by one to find the culprit.

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1_step6/build.sh
./ide_layers/layer1_step6/run.sh
```
