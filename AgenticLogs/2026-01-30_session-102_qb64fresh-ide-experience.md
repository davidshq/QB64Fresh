# Session 102 – QB64Fresh IDE experience

**Date:** 2026-01-30

## Goal

Use QB64pe binary to create a few tests; do the same with QB64Fresh binary; continue working until the QB64Fresh IDE experience works: start IDE, enter code, save code that is enough to start.

## Plan

1. Create minimal test .bas files (hello, simple program) and verify QB64Fresh CLI compiles them.
2. Build qb64pe_fresh (QB64pe compiled by QB64Fresh) and run the IDE.
3. Fix any runtime/IDE bugs that block: start IDE, enter code, save file.

## Outcome

- **Tests:** Used `examples/hello.bas`, `examples/simple.bas`; both compile with `qb64fresh ... --emit-c`. Added `QB64pe/source/starter.bas` (minimal PRINT "Hello, World!" / END) and verified it compiles.
- **Build:** QB64Fresh + runtime (graphics-sdl2) built; qb64pe.bas → C (16GB cap) and linked to `qb64pe_fresh` successfully.
- **IDE:** Ran `./qb64pe_fresh` from QB64pe with `QB64FRESH_IDE_COMPAT=1` and 4GB memory limit; process started and stayed up (~320MB). Window stays visible with IDE compatibility mode (Session 089 fix). `settings/recent.bin` auto-creation (runtime) and `run_qb64pe_fresh.sh` pre-create of settings files avoid Error 53 on first run.
- **Save path:** IDE uses _SAVEFILEDIALOG$ (runtime `qb_savefiledialog` with rfd/dialogs), then OPEN f$ FOR OUTPUT, OPEN FOR BINARY, PUT #151, , outfile$. Runtime implements OPEN (including recent.bin auto-create), PUT, and dialogs (default features). No code changes required for the “start, enter code, save” flow.
- **Docs:** Updated BUILD_WITH_QB64FRESH.md with “IDE experience” steps and added `QB64pe/source/starter.bas` as a minimal file to open/save from the IDE.

## Follow-up: Window close (X) not working

**User report:** With QB64Fresh-compiled IDE, clicking the window X does not close the app; it has to be force-stopped. QB64pe (native) works fine.

**Cause:** QB64pe-generated code checks `if (stop_program) end();` in the main loop. The native QB64pe runtime sets the global `stop_program` when the user closes the window. Our runtime's `qb_gfx_poll_events()` returned 0 on SDL Quit but never set `stop_program`, so the program never saw the close request.

**Fix (QB64Fresh runtime):**
1. **lib.rs:** Added global `#[no_mangle] pub static mut stop_program: u8 = 0` so C code can read it (QB64pe expects this symbol).
2. **graphics_ffi.rs:** In `qb_gfx_poll_events()`, when `poll_events()` returns `Ok(false)` (user clicked X or Escape), set `crate::stop_program = 1` before returning 0.
3. **qb64fresh_rt.h:** Declared `extern uint8_t stop_program;` for documentation.

After rebuilding the runtime and relinking `qb64pe_fresh`, clicking X sets `stop_program`, the next main-loop iteration runs `if (stop_program) end();`, and the process exits cleanly.

**Follow-up (GNOME title-bar X):** Minimize worked but the close (X) button did not. On GNOME/Wayland (and many WMs), the window manager sends **WindowEvent::Close** when the user clicks the title-bar X, not always **Event::Quit**. We only handled `Event::Quit`. **Fix:** In `runtime/src/graphics/sdl2.rs`, also match `Event::Window { win_event: WindowEvent::Close, .. } => return Ok(false)` so both code paths set `stop_program` and the window closes. Rebuild runtime and relink the IDE.

## References

- BUILD_WITH_QB64FRESH.md (QB64pe) — now includes IDE experience subsection
- run_qb64pe_fresh.sh
- Session 089: BYREF string fix, window show/hide fix for IDE
