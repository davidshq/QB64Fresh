# QB64pe IDE — Layer-by-Layer Build (Separate Versions)

Build the IDE in **separate layers**; each layer is a **separate version**. Do not add code to a previous layer. If a layer fails, run the previous layer to go back.

## Where to start

See **ide_layers/README.md** in this repo (QB64Fresh):

- **layer0** — Minimal C: show window, event loop, exit on close (runtime only, no QB64pe).
- **layer1** — Same with `qb_gfx_screen()` before show (matches IDE init).
- **layer2** — Full IDE (qb64pe.bas → C, link runtime); use QB64pe/BUILD_WITH_QB64FRESH.md and run_qb64pe_fresh.sh.

Run from **QB64Fresh** repo root:

```bash
# Build runtime once
cargo build -p qb64fresh-runtime --release --features graphics-sdl2

# Layer 0
./ide_layers/layer0/build.sh
./ide_layers/layer0/run.sh

# If layer0 works, try layer1
./ide_layers/layer1/build.sh
./ide_layers/layer1/run.sh

# Layer 2 = full IDE (see QB64pe/BUILD_WITH_QB64FRESH.md)
```

If layer0 does not show a window, the issue is runtime/SDL/display. If layer0 works but layer1 fails, the issue is in the added part. If layer1 works but layer2 fails, the issue is in the full IDE path.
