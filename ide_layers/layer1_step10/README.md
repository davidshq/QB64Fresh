# Layer 1 step 10: Step9 + final batch (~100% total)

**Goal:** Add the last batch of IDE-like calls so we exercise the full sampled graphics/display/input surface.

**Expected:** Same as step9 (title "Layer1 step 10", text, "step10" at (50,60), blue fill, box, circles) with no regressions. Close with X.

## Batch H (step10 only)

| Call | Purpose |
|------|--------|
| `qb_keydown_vk(0)`, `qb_keyup_vk(0)` | Simulate key (stubs) |
| `qb_print_tab()`, `qb_print_space()`, `qb_print_flush()` | Console print helpers |
| `qb_depthbuffer(0)` | Depth buffer mode |
| `qb_screenclick(0, 0, 0)` | Simulate mouse click |

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1_step10/build.sh
./ide_layers/layer1_step10/run.sh
```

## Next

Try **layer2** (full IDE): `./ide_layers/layer2/build.sh` then `./ide_layers/layer2/run.sh`.
