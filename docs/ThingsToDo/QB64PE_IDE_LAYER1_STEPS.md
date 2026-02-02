# QB64pe IDE — Layer-by-Layer (Separate Versions)

Use **ide_layers/** in this repo. Each layer is a **separate version**; do not add code to a previous layer. If a layer fails, run the previous layer to go back.

See **ide_layers/README.md** for:

- **layer0** — Minimal C: window + event loop (runtime only).
- **layer1** — Same with qb_gfx_screen first.
- **layer2** — Full IDE (qb64pe_fresh); see QB64pe/BUILD_WITH_QB64FRESH.md.

Run from **QB64Fresh** repo root:

```bash
cargo build -p qb64fresh-runtime --release --features graphics-sdl2
./ide_layers/layer0/build.sh && ./ide_layers/layer0/run.sh
./ide_layers/layer1/build.sh && ./ide_layers/layer1/run.sh
```

Layer 2 = full IDE build (QB64pe/run_qb64pe_fresh.sh or BUILD_WITH_QB64FRESH.md).
