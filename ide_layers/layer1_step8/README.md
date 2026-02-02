# Layer 1 step 8: Step7 + another ~25% IDE-like calls (~50% total)

**Goal:** Add a second 25% batch of IDE-like FFI calls so we exercise ~50% of the graphics/display/input surface. If the window freezes or never appears, bisect Batches D, E, or F to find the culprit.

**Expected:** Window opens with title "Layer1 step 8", text "Layer1 step 8 - OK", and optional drawn content. Close with X.

## New batch contents (step8 only)

| Group | Calls |
|-------|--------|
| **D** | `validatepage(0)`, `qb_gfx_pmap`, `qb_gfx_set_width`, `qb_gfx_view`, `qb_gfx_window`, `qb_gfx_view_reset`, `qb_gfx_window_reset`, `qb_rgb`, `qb_rgb32`, `qb_gfx_palette(0,0)` |
| **E** | `qb_gfx_point`, `qb_gfx_pset_step`, `qb_gfx_line_step`, `qb_gfx_paint` |
| **F** | `qb_gfx_pcopy`, `qb_gfx_newimage`+`qb_gfx_freeimage`, `qb_gfx_source(0)`, `qb_gfx_dest(0)`, `qb_gfx_image_width(0)`, `qb_gfx_image_height(0)`, `qb_font(0)`, `qb_fontheight`, `qb_fontwidth`, `qb_font_get`, `qb_inkey` (and release), `qb_fullscreen_get`, `qb_view_print_reset`, `qb_view_print(1,25)` |

## If it freezes: bisect

1. Comment out Batch D, E, or F (or half of one batch). Rebuild and run.
2. If it works, the culprit is in the commented block; split that block and repeat.
3. If it still freezes, the culprit is in a batch you kept; bisect that batch.
4. Continue until one call remains.

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1_step8/build.sh
./ide_layers/layer1_step8/run.sh
```
