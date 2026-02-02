# Layer 1 step 7: Step6 + ~25% IDE-like runtime calls (big batch)

**Goal:** Add a larger leap of IDE-like FFI calls (~25% of graphics/display/input surface). If the window freezes or never appears, bisect the batch to find the culprit.

**Expected:** Window opens with title "Layer1 step 7", text "Layer1 step 7 - OK", and **visible shapes** (horizontal line, rectangle, circle, pixel).

**Bisect result:** `qb_gfx_view_reset()` and `qb_gfx_window_reset()` were identified as hiding text/shapes (viewport/window reset clears or clips the visible area). In `main.c` those two calls are **commented out** so step7 shows shapes. Bisect variants: `layer1_step7_bisect` (B only), `layer1_step7_no_A` (no Batch A), `layer1_step7_no_view_window` (only view_reset/window_reset commented).

## Bisect for blank window / no shapes (reference)

1. **Run the bisect variant first** (step6 + Batch B only, no A or C):
   ```bash
   ./ide_layers/layer1_step7_bisect/build.sh
   ./ide_layers/layer1_step7_bisect/run.sh
   ```
   - **If you see title + text + shapes:** The culprit is in **Batch A or C** of step7. In `layer1_step7/main.c` comment out Batch A (lines 51–60), rebuild step7, run. If shapes disappear, culprit is in A — try commenting only `qb_gfx_view_reset()` and `qb_gfx_window_reset()` (often these reset the viewport and hide content). If shapes stay, restore A and comment out Batch C to confirm C is not the cause.
   - **If you still see no shapes:** The drawing primitives may be drawing off-screen or the runtime coordinate system may differ; we need to check coords or display order.

2. **If culprit is in Batch A:** Bisect A: comment out half the calls (e.g. `qb_gfx_view_reset` and `qb_gfx_window_reset` first). Rebuild, run. Narrow down until one call causes the blank.

## Batch contents (in order)

| Group | Calls |
|-------|--------|
| **A** | `qb_gfx_width`, `qb_gfx_height`, `qb_gfx_get_foreground`, `qb_gfx_get_background`, `qb_gfx_csrlin`, `qb_gfx_pos`, `qb_gfx_view_reset`, `qb_gfx_window_reset`, `qb_gfx_palette_reset`, `qb_gfx_palette_get(0)` |
| **B** | `qb_gfx_pset`, `qb_gfx_line`, `qb_gfx_box`, `qb_gfx_circle` |
| **C** | `qb_mouse_x`, `qb_mouse_y`, `qb_mouse_hide`, `qb_mouse_show`, `qb_keyhit`, `qb_keydown(0)`, `qb_keyclear` |

## If it freezes: bisect

1. **Half the batch:** Comment out either Batch B + C (keep A) or Batch A + C (keep B). Rebuild and run.
2. **If it works:** The culprit is in the commented half. Split that half again (e.g. comment half of A, or B vs C) and repeat.
3. **If it still freezes:** The culprit is in the half you kept. Split that half and repeat.
4. Continue until one call remains; that call is the one that breaks the window.

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1_step7/build.sh
./ide_layers/layer1_step7/run.sh
```
