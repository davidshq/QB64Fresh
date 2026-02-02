# Layer 1 step 4: Layer1_step3 + one addition

**Incremental test:** Layer1_step3 works. This step adds **one** thing: call `qb_screenmove(0, 0)` after `qb_icon()` (IDE does _SCREENMOVE when IDEAutoPosition).

- **If the window still appears and closes with X:** the break is not here; add layer1_step5.
- **If the window does not appear:** the break is in the screenmove call.

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1_step4/build.sh
./ide_layers/layer1_step4/run.sh
```
