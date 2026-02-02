# IDE window debug – 2026-02-02

## Summary

- **Symptom:** QB64 IDE appears in taskbar but no window/UI is visible.
- **Instrumentation:** Added NDJSON logs to runtime: `qb_screenshow`, `qb_screenhide`, `qb_gfx_screen`, `init_graphics`, SDL2 `initialize`/`screen_show`, and `qb_string_new` (first call + every 1000th).

## Log analysis

1. **Runtime is used** – `/tmp/qb64fresh_debug.log` shows exactly one line: first `qb_string_new` ("first runtime call"). So the linked binary is our instrumented runtime.
2. **Graphics path never reached in 10–90s** – No logs from `qb_screenshow`, `qb_screenhide`, `qb_gfx_screen`, or `init_graphics` even when running 45–90 seconds.
3. **Very few string calls** – With a log every 1000th `qb_string_new`, we still see only the “first call” log after 10–30s. So fewer than 1000 `qb_string_new` calls happen in that time.

## Hypotheses (evidence)

- **A (qb_screenshow never called):** **CONFIRMED** – No `qb_screenshow` log in any run.
- **B (window shown then hidden):** **REJECTED** – No `qb_screenhide` logs either.
- **C (wrong dimensions):** **INCONCLUSIVE** – `init_graphics` never logged.
- **D (SDL show not effective):** **INCONCLUSIVE** – No graphics init/show logs.
- **E (init_graphics fails):** **INCONCLUSIVE** – No init attempt logged.

## Conclusion

The IDE process is **stuck early in startup** and never reaches the code that creates or shows the graphics window. Execution reaches the runtime (first `qb_string_new`) but does not reach 1000 string allocations in 10+ seconds, and never hits any graphics FFI (`qb_gfx_screen`, `qb_screenshow`, etc.). So “no UI” is because **the main loop / graphics init is never reached**, not because the window is created and then hidden or shown incorrectly.

## Root cause and fix

**Stall location:** Execution stalled inside the 3rd `qb_file_open_str` (opening file 26: `tmpdir + "temp.bin"` with `QB_FILE_LOCK_WRITE`). The runtime used blocking `flock(fd, LOCK_EX)`, so if the file was already locked (e.g. from a previous IDE run or another process), the call blocked indefinitely.

**Fix:** In `runtime/src/io/file.rs`, `apply_flock` on Unix now uses non-blocking flock: `flock(fd, LOCK_EX | LOCK_NB)`. If the lock cannot be acquired, OPEN still succeeds and we proceed without the lock (best-effort, matching the Windows comment).

**Verification (post-fix log):** Same run (25s) now shows: many more file_open_str and dir_exists calls, qb_string_new progress at 1000/2000/3000, qb_startdir, then **qb_screenshow entered**, **init_graphics from screenshow**, **init_graphics entry**, **SDL2 initialize entry**. So we reach graphics init and the IDE no longer hangs at startup.

## Files touched

- `runtime/src/debug_log.rs` – added, plus fallback `/tmp/qb64fresh_debug.log`
- `runtime/src/graphics_ffi.rs` – logs in `qb_screenshow`, `qb_screenhide`, `qb_gfx_screen`
- `runtime/src/graphics/mod.rs` – logs in `init_graphics`
- `runtime/src/graphics/sdl2.rs` – logs in `initialize`, `screen_show`
- `runtime/src/string.rs` – first-call log + every-Nth in `qb_string_new`

---

## Follow-up: Icon visible, no UI (2026-02-02)

**Symptom:** User sees app icon and process in task manager, but no window/UI. Ran with `./qb64pe_fresh 2>&1`.

**Hypothesis:** Window is created and shown but not raised to front (common on Linux/Wayland/X11 when another window has focus).

**Fix:** In `runtime/src/graphics/sdl2.rs`:
- After `show()` in `initialize`: call `win.raise()` and log window size/position.
- In `screen_show`: after `show()`, call `restore()` if minimized, then `raise()`.

**Verification:** Rebuild runtime (`cargo build --release -p qb64fresh-runtime`), relink QB64pe from `QB64pe/source` (see below), run from `QB64pe`: `./qb64pe_fresh 2>&1`.

**Relink only (C file exists at QB64pe/source/qb64pe_fresh.c):**
```bash
cd /home/dave/repos/qb64contain/QB64pe/source
gcc -I ../../QB64Fresh/runtime/include qb64pe_fresh.c \
  -L ../../QB64Fresh/target/release -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client) \
  -lm -lpthread -ldl -o ../qb64pe_fresh
```
Then from QB64pe root: `./qb64pe_fresh 2>&1`.

---

## Follow-up: Distorted UI (black/gray strips, wrong layout) (2026-02-02)

**Symptom:** IDE window opens but UI is distorted: left black panel, right gray with central black bar, partial text; expected is normal menu/code/status layout.

**Hypothesis:** Renderer logical size was not set, so on HiDPI or WM-resized windows the drawable size didn't match our buffer (1280x400). Copying the texture without a fixed logical size caused wrong scaling/placement; uncovered areas showed garbage (gray) or letterbox (black).

**Fix:** In `runtime/src/graphics/sdl2.rs`:
1. **set_logical_size** – After building the canvas in `initialize()`, call `canvas.set_logical_size(width, height)` so the renderer uses our buffer dimensions and SDL scales/letterboxes correctly.
2. **set_screen_res** – When resizing, call `canvas.set_logical_size(new_width, new_height)` so logical size stays in sync with window/buffers.
3. **Clear before copy** – In `display()`, set draw color to `bg_color` and call `canvas.clear()` before copying the texture so letterbox/uncovered areas are a consistent color, not garbage.

**Verification:** Rebuild runtime, relink QB64pe from `QB64pe/source`, run `./qb64pe_fresh 2>&1` and confirm the IDE shows correct layout (menu bar, code area, status bar).

---

## How to open the IDE (working command)

**Use the script** — it sets cwd, SDL driver, memory limit, and IDE compat:

```bash
cd /home/dave/repos/qb64contain/QB64pe
./run_qb64pe_fresh.sh
```

**Manual run (same as script, from QB64pe root):** The script sets `SDL_VIDEODRIVER` (x11 or wayland), `QB64FRESH_IDE_COMPAT=1`, creates `settings/recent.bin`, and uses `ulimit -v 4194304`. To match that without the script:

```bash
cd /path/to/QB64pe
mkdir -p settings source/settings
touch settings/recent.bin source/settings/recent.bin
# Script picks x11 if WAYLAND_DISPLAY unset, else wayland
export SDL_VIDEODRIVER=x11
bash -c 'ulimit -v 4194304 && QB64FRESH_IDE_COMPAT=1 ./qb64pe_fresh'
```

Running only `SDL_VIDEODRIVER=x11 ./qb64pe_fresh` (e.g. from the wrong directory or without `QB64FRESH_IDE_COMPAT=1`) can prevent the UI from displaying correctly; use the script or the full manual sequence above.

---

## Follow-up: UI still distorted after logical-size fix (2026-02-02)

**Symptom:** User reports "the UI still looks the exact same, no change" after set_logical_size + clear fix.

**New hypothesis:** Copy with `None` destination might not respect logical size on some drivers; use an explicit destination rect so the texture fills the logical viewport.

**Code changes:**
1. **Explicit copy destination** – In `display()`, use `canvas.copy(texture, None, Some(Rect::new(0, 0, self.width, self.height)))` instead of `copy(..., None, None)`.
2. **Instrumentation** – After `set_logical_size` in `initialize()`: log `sdl2_init_logical_size` with w, h. In `display()`: log first 3 frames as `sdl2_display` with n, w, h, vp, fix="v2_explicit_dst" so we can confirm the new runtime is loaded and see dimensions.

**Verification:** User must **relink** QB64pe (no SKIP_BUILD) so the binary uses the new runtime. Then run `./run_qb64pe_fresh.sh`. Check `.cursor/debug.log` or `/tmp/qb64fresh_debug.log`: if `sdl2_init_logical_size` and `sdl2_display` with `fix":"v2_explicit_dst"` appear, the new runtime is active; report whether the UI still looks distorted and share the logged w/h if so.

---

## Follow-up: Try without logical size (2026-02-02)

**Runtime evidence:** Log confirms new runtime runs: `sdl2_init_logical_size` w:1280 h:400, `sdl2_display` v2_explicit_dst. UI still distorted.

**New hypothesis:** On some drivers (e.g. Wayland/X11), `set_logical_size` can cause wrong scaling or letterboxing that looks like black/gray strips. Try running **without** logical size so the texture is stretched to the full window.

**Code change:** Logical size is now **optional**. Set env **`QB64FRESH_NO_LOGICAL_SIZE=1`** before starting the IDE to skip `set_logical_size` and use `copy(texture, None, None)` (full-target stretch). Default (no env) keeps logical size and explicit dst rect.

**Try:** Rebuild runtime, relink QB64pe, then run:
```bash
cd /home/dave/repos/qb64contain/QB64pe
QB64FRESH_NO_LOGICAL_SIZE=1 ./run_qb64pe_fresh.sh
```
If the UI looks correct with this env, the distortion is caused by logical size on this system; we can then make that the default for this setup or document the workaround.

---

## Follow-up: Treat distortion as code bug (2026-02-02)

**User:** "I have my doubts this is an issue with the display on my system. I think it is more likely it is a bug in the code."

**Hypothesis:** Texture format / byte order bug. We used ARGB8888 and wrote B,G,R,A (treating pixel as 0xAARRGGBB). On some systems the renderer may expect explicit RGBA byte order for streaming textures; SDL docs recommend RGBA8888 for "byte array in order R, G, B, A".

**Code change:** In `runtime/src/graphics/sdl2.rs`:
1. Create streaming textures with **PixelFormatEnum::RGBA8888** instead of ARGB8888 (in both `initialize()` and `set_screen_res()`).
2. In the texture upload loop, write bytes in **R, G, B, A** order from our pixel 0xAARRGGBB: `buffer[offset]=R, +1=G, +2=B, +3=A`.

**Verification:** Rebuild runtime, relink QB64pe (no SKIP_BUILD), run `./run_qb64pe_fresh.sh`. If the UI renders correctly, the bug was texture format/byte order.

---

## Follow-up: Texture lock failure + update() fallback (2026-02-02)

**User:** "I have my doubts this is an issue with the display on my system. I think it is more likely it is a bug in the code."

**Hypothesis:** Texture upload was failing silently. We used `let _ = texture.with_lock(...)` and ignored the `Result`; `with_lock` returns `Result<R, String>`. On some drivers (e.g. streaming texture on Wayland/X11), `SDL_LockTexture` can fail. When it failed we still cleared the dirty flag, so we never retried and the texture showed uninitialized/garbage (black/gray strips).

**Code change:** In `runtime/src/graphics/sdl2.rs` `display()`:
1. **Handle with_lock Result:** Only clear the page dirty flag when the upload succeeds. If `with_lock` returns `Err`, do not clear dirty so we retry next frame.
2. **Fallback when lock fails:** If `with_lock` fails, build a CPU buffer (Vec<u8> in RGBA order, pitch = width*4) and call `texture.update(None, &buf, pitch)`. If that succeeds, clear dirty; if it also fails, log and keep dirty.
3. **Instrumentation:** Log `sdl2_texture_with_lock_failed` (warning) when using the update fallback; log `sdl2_texture_upload_failed` (error) when both with_lock and update fail (first 3 occurrences each).

**Verification:** Rebuild runtime (`cargo build --release -p qb64fresh-runtime`), relink QB64pe (no SKIP_BUILD), run `./run_qb64pe_fresh.sh`. Check debug log: if `sdl2_texture_with_lock_failed` appears, the driver was failing lock and the fallback should fix the distortion; if UI is still wrong and no lock-fail log, investigate further (e.g. pitch/dimensions).

---

## Follow-up: Pink/red background and text (2026-02-02)

**Symptom:** After texture fix, IDE shows light red/pink background and red pixelated text; expected is gray background and dark text. White dashed vertical line at ~3/4 width.

**Hypotheses addressed:**
1. **Default colors:** We defaulted to fg=white, bg=black. IDE uses COLOR 0, 7 (black on gray) for menubar/code; if COLOR wasn’t called before first paint we’d show white on black, or if palette 7 was wrong we’d get wrong bg.
2. **Palette sync:** `resolve_palette_color` (used by set_color) reads `screen_palette`; `set_palette` only updated `self.palette`. So global palette changes weren’t visible to COLOR.

**Code changes:** In `runtime/src/graphics/sdl2.rs`:
1. **IDE-compat defaults:** When `QB64FRESH_IDE_COMPAT` is set, after init set `fg_color` and `bg_color` to palette 0 and 7 (black, light gray) so the first cls() and early draws use gray background and black text.
2. **set_palette sync:** In `set_palette()`, also update `self.screen_palette[index]` so COLOR and resolve_palette_color see the same palette as get_palette.

**Verification:** Rebuild runtime, relink QB64pe (no SKIP_BUILD), run `./run_qb64pe_fresh.sh`. UI should show gray background and dark text; dashed line may remain (IDE divider). Pixelation/text clipping are separate (font scale/viewport).

---

## Follow-up: Pin palette 0 and 7 in IDE compat (2026-02-02)

**Symptom:** UI still shows light pink background and red text after default-color fix. IDE sets _PALETTECOLOR 7, IDEChromaColor, 0 (and other theme colors); default IDEChromaColor is _RGB32(170,170,170) (gray) but config or theme can override to red/pink.

**Fix:** When `QB64FRESH_IDE_COMPAT=1`, do not allow _PALETTECOLOR or set_palette to change palette index **0** (black) or **7** (light gray). So COLOR 0, 7 always resolves to EGA black and light gray regardless of theme. In `runtime/src/graphics/sdl2.rs`: in `set_palette()` and `set_palette_for_image()` (handle==0), if IDE compat and index is 0 or 7, return Ok(()) without updating.

---

## Follow-up: Full IDE layout not showing (disclaimer stuck) (2026-02-02)

**Symptom:** User sees disclaimer screen (red/pink text) but not the full IDE layout (menu bar, code area, status bar) as in the reference screenshot. Less concerned about palette than that the UI doesn’t show like the expected IDE.

**Root cause:** The IDE shows the disclaimer via `qb_idemessagebox()`, which blocks in a `do { qb_sub_getinput(); ... } while (!change)` loop. `getinput()` blocks in `while (qb_inkey() == "")`. When the graphics window has focus, keys go to **SDL**, but `qb_inkey()` only read from **stdin** (or Windows console). No one was pumping SDL events in that loop, so `qb_inkey()` never returned a key and the message box never dismissed; the main editor was never drawn.

**Fix:**
1. **Graphics → INKEY path**  
   - In `runtime/src/io/input.rs`: add `GRAPHICS_INKEY_QUEUE` (VecDeque of INKEY$ byte vectors) and `push_inkey_from_graphics(bytes)`.  
   - In `qb_inkey()` (and `qb_keyhit()`): when the graphics backend is active, call `poll_events_if_active()` once, then pop from `GRAPHICS_INKEY_QUEUE` and return that key if present; otherwise fall back to stdin/console.  
   - In `qb_keyclear()`: also clear `GRAPHICS_INKEY_QUEUE`.

2. **SDL KeyDown → queue**  
   - In `runtime/src/graphics/sdl2.rs`: add `sdl_key_to_inkey_bytes(scancode, keycode)` (extended keys → `[0, scan_code]`, ASCII keys → `[byte]`).  
   - In the `Event::KeyDown` branch of `poll_events`, compute inkey bytes and call `crate::io::push_inkey_from_graphics(inkey_bytes)`.

**Verification:** Rebuild runtime (`cargo build --release -p qb64fresh-runtime`), relink QB64pe from `QB64pe/source`, run `./run_qb64pe_fresh.sh`. When the disclaimer appears, press **Enter** (or **Tab** then Enter). The message box should dismiss and the main IDE layout (menu, code area, status bar) should appear. If it still doesn’t, capture a post-fix log and check that key events are being queued and that the IDE proceeds past the first message box.

**Instrumentation:** Key-delivery logging added: `push_inkey_from_graphics` and `qb_inkey_returned_from_graphics` log first 30 events to `.cursor/debug.log` and `/tmp/qb64fresh_debug.log`. Automated run showed graphics init; no key events (no key pressed during run). To verify: run IDE, press Enter on disclaimer, then `grep -E 'push_inkey_from_graphics|qb_inkey_returned' .cursor/debug.log /tmp/qb64fresh_debug.log`.

---

## Follow-up: Disclaimer still stuck after key delivery (2026-02-02)

**Symptom:** User still only sees disclaimer screen; pressing Enter does not dismiss it.

**Root cause:** IDE `qb_sub_getinput()` has an inner loop `do { ... } while (qb_inkey() == "");` then calls `k = qb_keyhit();`. The loop uses **qb_inkey()** to detect "any key" — that call **consumes** the key from the queue. So when we break out, **qb_keyhit()** is called with an empty queue and returns 0. The message box sets `change = 1` only when `KB` (from getinput) is non-zero; since we consumed the key in inkey, KB stayed 0 and the dialog never dismissed.

**Fix:** In `runtime/src/graphics/sdl2.rs`, when pushing key bytes from SDL KeyDown, push **two copies** of each key: one is consumed by qb_inkey() (breaking the loop), the second is returned by qb_keyhit() so KB is set and the message box sees the key and dismisses.

**Verification:** Rebuild runtime (`cargo build --release -p qb64fresh-runtime`), relink QB64pe from `QB64pe/source`, run `./run_qb64pe_fresh.sh`. Click the disclaimer window to focus it, then press **Enter**. The disclaimer should dismiss and the main IDE (menu, code area, status bar) should appear.
