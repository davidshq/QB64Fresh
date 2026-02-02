# Layer 1 step 5: Layer1_step4 + batch of several calls

**Incremental test (batch):** Layer1_step4 works. This step adds **several** calls the IDE may do early:

- **A.** `qb_gfx_cls()` — clear screen  
- **B.** `qb_gfx_display()` — flush display  
- **C.** `qb_gfx_autodisplay(1)` — enable auto display  

**If the window does not appear:** remove calls **one by one** (comment out C, then B, then A) and re-test until the window appears. The last call you removed is the culprit.

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1_step5/build.sh
./ide_layers/layer1_step5/run.sh
```

## Bisecting the batch

1. If step5 fails: comment out **C** (`qb_gfx_autodisplay(1)`), rebuild, run. If window appears → C is the culprit.
2. If still fails: comment out **B** (`qb_gfx_display()`), rebuild, run. If window appears → B is the culprit.
3. If still fails: comment out **A** (`qb_gfx_cls()`), rebuild, run. If window appears → A is the culprit.
