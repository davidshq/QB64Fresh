# Layer 1: Window with qb_gfx_screen first (IDE-like init)

Same as layer0 but calls `qb_gfx_screen(-1, -1, 1, 0)` before `qb_screenshow()`.  
This matches the IDE’s initial screen setup.

## Build and run

From QB64Fresh repo root:

```bash
./ide_layers/layer1/build.sh
./ide_layers/layer1/run.sh
```

**Rebuild the runtime** if you changed it: from repo root,  
`cargo build -p qb64fresh-runtime --release --features graphics-sdl2`,  
then run `./ide_layers/layer1/build.sh` again.

## What you should see

- A window opens (may be **blank/black**).
- **Blank screen is expected:** we set active page 1 and visual page 0, so we’re drawing to page 1 but showing page 0 (which was cleared to black). This tests the IDE-like init path.
- **Close button (X)** should work: the event loop calls `qb_gfx_poll_events()` and `qb_sleep(0.01)` so the WM can deliver the close event. If it doesn’t, rebuild both the runtime and layer1 (see above).

## Separate version

Do not edit layer0 when changing layer1. Each layer is a standalone version so you can go back to a previous one.
