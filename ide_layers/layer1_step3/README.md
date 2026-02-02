# Layer 1 step 3: Layer1_step2 + one addition

**Incremental test:** Layer1_step2 works. This step adds **one** thing: call `qb_icon()` after `qb_screenshow()` (IDE does _SCREENSHOW then _ICON).

- **If the window still appears and closes with X:** the break is not here; add the next single item (layer1_step4).
- **If the window does not appear:** the break is in the icon call.

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1_step3/build.sh
./ide_layers/layer1_step3/run.sh
```
