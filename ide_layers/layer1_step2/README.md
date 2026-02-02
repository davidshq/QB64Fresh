# Layer 1 step 2: Layer1 + one addition

**Incremental test:** Layer1 works (window appears, closes with X). This step adds **one** thing: call `qb_screenhide()` before `qb_screenshow()`.

The IDE has `$SCREENHIDE` at startup, then later `_SCREENSHOW`. This step mimics that order.

- **If the window still appears and closes with X:** the break is not here; add the next single item and test again.
- **If the window does not appear:** the break is in the hide-before-show path (runtime or how we call it).

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1_step2/build.sh
./ide_layers/layer1_step2/run.sh
```

Do not edit layer1 when adding steps; each step is a separate copy.
