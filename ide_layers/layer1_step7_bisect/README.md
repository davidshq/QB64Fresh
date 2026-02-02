# Layer 1 step 7 bisect: Step6 + Batch B only (no A, no C)

**Purpose:** Find why full step7 (or step8) shows a blank window or no shapes. This variant runs **only** the drawing primitives (Batch B); Batches A and C are omitted.

**Expected if drawing works:** Window with title "Layer1 step 7 bisect (B only)", text "Layer1 step 7 bisect - OK", plus a horizontal line, a rectangle, a circle, and a pixel.

- **If you see title + text + shapes:** The culprit in full step7 is in **Batch A or C**. Next: in `layer1_step7/main.c` comment out Batch A only, rebuild step7, run. If it goes blank, culprit is in A (likely `qb_gfx_view_reset` or `qb_gfx_window_reset`). Then comment out just those two in A and retest.
- **If you still see no shapes (only title + text):** The drawing primitives may be drawing off-screen or the runtime’s coordinate system may differ; we’ll need to check coords or display order.

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1_step7_bisect/build.sh
./ide_layers/layer1_step7_bisect/run.sh
```
