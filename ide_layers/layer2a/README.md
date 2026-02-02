# Layer 2a: BASIC → C (IDE-equivalence window)

Compiles **BASIC** source to C and links with the QB64Fresh runtime. This is the first incremental step toward the full IDE: we use the same compiler path as layer2 but with a minimal IDE-equivalence program.

**Source:** `01_window_display.bas` (same as `tests/ide_equivalence/01_window_display.bas`).  
**Criterion:** Window appears, title "IDE Eq 1: Window", closes with X or Esc.

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer2a/build.sh
./ide_layers/layer2a/run.sh
```

## Next steps (incremental BASIC)

After layer2a works, add further IDE-equivalence BASIC files in order (e.g. 02_event_loop.bas, 03_graphics_image.bas) as new layers, or extend this layer with one more program at a time until we can run the full IDE (layer2).
