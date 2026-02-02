# Layer 1 step 9: Step8 + ~25% more (~75% total)

**Goal:** Add another batch of IDE-like calls so we exercise ~75% of the graphics/display/input surface.

**Expected:** Window with title "Layer1 step 9", text "Layer1 step 9 - OK", "step9" at (50,60), shapes (including box_step, circle_step). Window keeps default size (qb_gfx_set_width is commented out — it resizes to 80×25 text grid). Close with X.

## Batch G (step9 only)

| Call | Purpose |
|------|--------|
| `qb_mouse_button(0)` | Query left button |
| `qb_mouse_wheel()` | Wheel state |
| `qb_mouse_movement_x/y()` | Movement deltas |
| `qb_mouse_move(100, 100)` | Move cursor |
| `qb_gfx_printstring(50, 60, "step9")` | Text at pixel coords |
| `qb_icon1(0)` | Set icon from handle |
| `qb_print_newline()`, `qb_cls()` | Console I/O |
| `qb_gfx_box_step`, `qb_gfx_circle_step` | Step drawing |

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1_step9/build.sh
./ide_layers/layer1_step9/run.sh
```

## Next

Try **layer2** (full IDE): `./ide_layers/layer2/build.sh` then `./ide_layers/layer2/run.sh`.
