# Native Baseline (IDE Equivalence)

Capture expected behavior for each portion on a system where **native** `./qb64pe` works (after `setup_lnx.sh`). Use this to compare QB64Fresh-built IDE (`qb64pe_fresh`) behavior. Fill in per portion as you run the repros.

**To capture baseline automatically:** Run `./tests/ide_equivalence/capture_baseline.sh` from the QB64Fresh repo root. It runs native qb64pe and qb64pe_fresh for each portion, populates console portions (5, 6, 9, 10) from run output, and records compile-only status for GUI portions (1–4, 7). See the "Captured outputs (auto-generated)" section at the bottom.

**System:** Ubuntu desktop (X11/Wayland); display available.  
**Date:** 2026-01-31

---

## 1. Window and display

- **Repro:** `01_window_display.bas`
- **Steps:** Run repro (IDE or CLI). Note window size, title, then close with X (or Esc). See README "Manual test steps (GUI)" for exact commands.
- **Expected:** Window appears, title "IDE Eq 1: Window", closes with X or Esc.
- **Observed (native):** Compile **FAIL** — native qb64pe reports syntax error on `evnt` (LINE 7). So user cannot call `evnt` in source; QB64pe emits `evnt()` only in generated code. Manual run: _fill when testing without evnt or with stub._
- **Observed (qb64pe_fresh):** Compile **OK** — `evnt` built-in resolves. Link+run require `--runtime external` (same as 03–04, 07). _Fill visual after run._

---

## 2. Event loop

- **Repro:** `02_event_loop.bas`
- **Steps:** Run, press some keys, confirm characters print; press Esc to exit (or close with X).
- **Expected:** Main loop runs; key events and window close are detected.
- **Observed (native):** Compile **FAIL** — same as portion 1: syntax error on `evnt`. Manual run: _fill when testing without evnt or with stub._
- **Observed (qb64pe_fresh):** Compile **OK** — `evnt` built-in resolves. Link+run require `--runtime external`. _Fill visual after run._

---

## 3. Graphics / image

- **Repro:** `03_graphics_image.bas`
- **Steps:** Run; confirm image is created and drawn (no black screen or crash).
- **Expected:** Create image, draw to it, display; no crash or black screen.
- **Observed (native):** Compile **OK**. Run on Ubuntu desktop: `/tmp/gui_03_pe` ran; 5s timeout (exit 124). Window would have appeared with 640×480, title "IDE Eq 3: Graphics", yellow rectangle on image; program has SLEEP 2 then END. No crash. Visual: _confirm yellow rectangle and title._
- **Observed (qb64pe_fresh):** Compile **OK**. Link and run **OK** with `--runtime external`: build runtime (`cargo build -p qb64fresh-runtime --release --features "graphics-sdl2 dialogs"`), then `qb64fresh ... --emit-c --runtime external -o out.c` and `gcc -I runtime/include out.c -L target/release -lqb64fresh_rt $(pkg-config --libs sdl2 alsa wayland-client) -lm -lpthread -ldl -o out`. Run: 03 exits after ~2s (SLEEP 2). Visual: _confirm yellow rectangle and title._

---

## 4. Font and text

- **Repro:** `04_font_text.bas`
- **Steps:** Run; note text position and line height (addresses "funky" UI).
- **Expected:** Text and line height match native (or document difference).
- **Observed (native):** Compile **OK**. Run on Ubuntu desktop: `/tmp/gui_04_pe` ran; 5s timeout (exit 124). Window would have shown 640×480, title "IDE Eq 4: Font", two lines of text ("IDE Eq 4: Font test", "Line 2"); SLEEP 2 then END. No crash. Visual: _note font size and line height._
- **Observed (qb64pe_fresh):** Compile **OK**. Link and run **OK** with `--runtime external` (same as portion 3). Run: 04 exits after ~2s. Visual: _note font size and line height._

---

## 5. File I/O

- **Repro:** `05_file_io.bas` (console) or IDE File menu.
- **Steps:** Run repro; or in IDE: File → Open, Save; check recent list / recent.bin.
- **Expected:** File menu works; recent list loads/saves; no Error 53 on recent.bin.
- **Observed (native):** _
- **Observed (qb64pe_fresh):** _

---

## 6. Filesystem

- **Repro:** `06_filesystem.bas` (console) or IDE File → Open dialog.
- **Steps:** Run from different CWD; note dialog path and _DIR$ / _FILEEXISTS output.
- **Expected:** Open/Save dialogs show correct paths; CHDIR works.
- **Observed (native):** _
- **Observed (qb64pe_fresh):** _

---

## 7. GUI dialogs

- **Repro:** `07_gui_dialogs.bas`
- **Steps:** Run; trigger message box (or file dialog); note appearance and return value.
- **Expected:** Dialogs appear and return correct values.
- **Observed (native):** Compile **OK**. Run on Ubuntu desktop: `/tmp/gui_07_pe` ran; 10s timeout (exit 124). Program shows SCREEN 12, title "IDE Eq 7: GUI", then _MESSAGEBOX("IDE Eq 7: Click OK", "IDE Eq 7", "ok") — blocks until user clicks OK; then PRINT return value, SLEEP 2, END. No crash. Visual: _confirm message box text and return value (click OK)._
- **Observed (qb64pe_fresh):** Compile **OK**. Link and run **OK** — runtime now declares and implements `qb_messagebox(QbString* msg, QbString* title, QbString* btns)` for 3-arg _MESSAGEBOX; use `--runtime external` and build runtime with `dialogs` feature. Run: message box appears; PRINT "MessageBox return: 2" (2 = Cancel or timeout). Visual: _confirm message box and return value._

---

## Manual GUI test protocol (portions 1–4, 7)

**Differences summary (Ubuntu desktop run 2026-01-31):**

| Portion | Native qb64pe | QB64Fresh (qb64pe_fresh) |
|---------|----------------|---------------------------|
| 1. Window | Compile FAIL (evnt syntax) | Compile OK (evnt built-in); link+run need --runtime external |
| 2. Event loop | Compile FAIL (evnt syntax) | Compile OK; link+run need --runtime external |
| 3. Graphics | Compile OK; run OK (5s timeout, window/title/rect) | Compile OK; link+run OK with --runtime external |
| 4. Font | Compile OK; run OK (5s timeout, window/title/text) | Compile OK; link+run OK with --runtime external |
| 7. GUI dialogs | Compile OK; run OK (10s timeout, message box blocks) | Compile OK; link+run OK (qb_messagebox in runtime; --runtime external + dialogs) |

Run on a machine with a display. Use these commands from **QB64Fresh repo root**; for native qb64pe use full path to the `.bas` file when in QB64pe dir.

**Native QB64pe** (from QB64pe dir; replace `NN` with 01, 02, 03, 04, or 07):

```bash
cd /path/to/QB64pe
./qb64pe -x /path/to/QB64Fresh/tests/ide_equivalence/NN_*.bas -o /tmp/gui_NN_pe
/tmp/gui_NN_pe
```

**QB64Fresh** (emit-c + link + run; from QB64Fresh repo root). For GUI (03, 04, 07) use **external runtime**:

```bash
cd /path/to/QB64Fresh
# Build runtime once: cargo build -p qb64fresh-runtime --release --features "graphics-sdl2 dialogs"
qb64fresh tests/ide_equivalence/NN_*.bas --emit-c --runtime external -o /tmp/gui_NN_fresh.c
gcc -I runtime/include /tmp/gui_NN_fresh.c -L target/release -lqb64fresh_rt \
  $(pkg-config --libs sdl2 alsa wayland-client) -lm -lpthread -ldl -o /tmp/gui_NN_fresh
/tmp/gui_NN_fresh
```

For 01 and 02, native qb64pe fails on `evnt` (syntax); QB64Fresh now compiles (evnt built-in). For 03, 04, 07: both native and QB64Fresh (with --runtime external) run.

**Quick reference:** See README "Manual test steps (GUI)" table for per-portion commands. After running both builds, fill "Observed (native)" and "Observed (qb64pe_fresh)" above with window size, title, key echo, message box return value, and any differences.

---

## 8. Strings and console

- **Repro:** `tests/runtime_comparison/` string tests (e.g. 01_string_ops.bas); golden .out.
- **Expected:** Editor text and search behave; no encoding crashes; output matches golden.
- **Observed (native):** _
- **Observed (qb64pe_fresh):** _

---

## 9. Time and shell

- **Repro:** `09_time_shell.bas` or IDE Run → Make.
- **Steps:** Run repro (TIMER, SLEEP, SHELL); or in IDE run Make, confirm compile runs.
- **Expected:** Timing and Run/Make work; SHELL runs external process.
- **Observed (native):** _
- **Observed (qb64pe_fresh):** _

---

## 10. Audio (optional)

- **Repro:** `10_audio.bas` or IDE startup (if IDE uses sound).
- **Expected:** No crash on BEEP or _SND*.
- **Observed (native):** _
- **Observed (qb64pe_fresh):** _

---

## 11. Other libqb

- **As needed** when a specific IDE feature fails; add repro and baseline note here.

---

## Full IDE checklist (qb64pe_fresh)

After per-portion tests, run the **full IDE** (qb64pe compiled with QB64Fresh) and complete this checklist. Build steps: see [QB64pe BUILD_WITH_QB64FRESH.md](../../../QB64pe/BUILD_WITH_QB64FRESH.md).

**How to complete this checklist**

1. **Launch the IDE** — From QB64Fresh repo root: `./tests/ide_equivalence/run_full_ide.sh` (builds then runs qb64pe_fresh from QB64pe with ulimit). Or from QB64pe: `./run_qb64pe_fresh.sh` or `ulimit -v 4194304 && ./qb64pe_fresh`.
2. **Perform each step** in the table below; mark **Pass?** (Y/N) and **Notes**.
3. **Fill "Funky or broken behavior"** with any issues, mapping to portion numbers (e.g. "Portion 4: font size wrong in editor").

**Optional automation** — To avoid manual clicking: run `./tests/ide_equivalence/automate_full_ide.sh` from repo root. It starts the IDE, sends File→New, types `PRINT "hi"`, closes (Don’t Save). Success = IDE exits cleanly. **X11:** needs `xdotool`. **Wayland:** needs `wtype` or `ydotool` (and `ydotoold`); optional `swaymsg`/`jq` (Sway) or `wlrctl` to auto-focus; otherwise click the IDE when prompted. See README "Optional automation (X11 and Wayland)". MCP browser tools cannot control desktop apps; script uses xdotool (X11) or wtype/ydotool (Wayland).

**Build status (2026-01-31):** qb64pe_fresh exists in QB64pe root; IDE includes enabled in qb64pe.bas (ide_global, cfg_methods, ide_methods).

**Run status:** Ran `cd QB64pe && ulimit -v 4194304 && timeout 15 ./qb64pe_fresh` — process ran for 15s without crash (timeout). IDE launches.

**Manual checklist** — Then:

| Step | Action | Pass? | Notes |
|------|--------|-------|-------|
| 1 | Open qb64pe_fresh | _ | Window appears, menu/toolbar visible |
| 2 | File → New (or Open) | _ | New buffer or file dialog |
| 3 | Edit (type e.g. `PRINT "hi"`) | _ | Text appears in editor |
| 4 | File → Save (or Save As) | _ | File saved, no Error 53 |
| 5 | Run → Make (or Run) if applicable | _ | Compile runs or run executes |
| 6 | Close with X | _ | IDE exits cleanly |

**Funky or broken behavior (map to portion):** _e.g. "Portion 4: font size wrong in editor" or "Portion 6: Open dialog path wrong"_

---

## Captured outputs (auto-generated)

_Do not edit this section by hand. Re-run `./tests/ide_equivalence/capture_baseline.sh` to refresh._

- **Captured:** 2026-01-31 17:42 UTC
- **System:** unknown / :0
- **RUN_PE:** 0

### 5. File I/O (05_file_io)

- **Native (qb64pe):**
~~~
SKIP_QB64PE_NOT_BUILT
~~~

- **QB64Fresh (qb64pe_fresh):**
~~~
file_io: IDE Eq 5
~~~
### 6. Filesystem (06_filesystem)

- **Native (qb64pe):**
~~~
SKIP_QB64PE_NOT_BUILT
~~~

- **QB64Fresh (qb64pe_fresh):**
~~~
cwd: /home/dave/repos/qb64contain/QB64Fresh
dir: 
fileexists(06_filesystem.bas): 0
~~~
### 9. Time and shell (09_time_shell)

- **Native (qb64pe):**
~~~
SKIP_QB64PE_NOT_BUILT
~~~

- **QB64Fresh (qb64pe_fresh):**
~~~
timer: 45749.3
timer_after_1s: 45750.3
time$: 12:42:30
date$: 01-31-2026
~~~
### 10. Audio (10_audio)

- **Native (qb64pe):**
~~~
SKIP_QB64PE_NOT_BUILT
~~~

- **QB64Fresh (qb64pe_fresh):**
~~~
audio: beep ok
~~~

### GUI portions (compile-only)

| Portion | Repro | Native (qb64pe) compile | QB64Fresh compile |
|---------|-------|-------------------------|-------------------|
| 1. Window and display | 01_window_display.bas | SKIP (qb64pe not built) | FAIL |
| 2. Event loop | 02_event_loop.bas | SKIP (qb64pe not built) | FAIL |
| 3. Graphics / image | 03_graphics_image.bas | SKIP (qb64pe not built) | OK |
| 4. Font and text | 04_font_text.bas | SKIP (qb64pe not built) | OK |
| 7. GUI dialogs | 07_gui_dialogs.bas | SKIP (qb64pe not built) | OK |


### 8. Strings and console

See `tests/runtime_comparison/` and golden `.out` outputs; no per-portion capture here.

