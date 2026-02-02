# IDE Equivalence Tests

Minimal repros and checklist for testing **QB64Fresh-built IDE** (`qb64pe_fresh`) against **native-built QB64pe** so behavior is equivalent per subsystem. See [QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md](../../docs/ThingsToDo/QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md) for the full plan, expert perspectives, and strategy.

## Purpose

- Test each IDE portion (window, event loop, graphics, font, file I/O, filesystem, dialogs, strings, time/shell, audio) in dependency order.
- Use minimal `.bas` repros so failures are easy to isolate.
- Capture native baseline (see [BASELINE.md](BASELINE.md)) and compare QB64Fresh-built behavior. Run `./tests/ide_equivalence/capture_baseline.sh` from repo root to populate BASELINE when running native qb64pe and qb64pe_fresh for each portion.

## Test Order (dependencies first)

| # | Portion | Criterion | Repro | Automated? |
|---|---------|-----------|-------|------------|
| 1 | Window and display | Window appears, title correct, closes with X | [01_window_display.bas](01_window_display.bas) | Semi |
| 2 | Event loop | Loop runs; keys and window close detected | [02_event_loop.bas](02_event_loop.bas) | Semi |
| 3 | Graphics / image | Create image, draw, display; no crash | [03_graphics_image.bas](03_graphics_image.bas) | None/semi |
| 4 | Font and text | Text and line height correct | [04_font_text.bas](04_font_text.bas) | None |
| 5 | File I/O | File menu, recent.bin; no Error 53 | [05_file_io.bas](05_file_io.bas) | Semi |
| 6 | Filesystem | Open/Save dialogs and paths correct | [06_filesystem.bas](06_filesystem.bas) | Semi |
| 7 | GUI dialogs | Dialogs appear and return | [07_gui_dialogs.bas](07_gui_dialogs.bas) | None |
| 8 | Strings and console | Editor/search; no encoding crashes | [08_strings_console.md](08_strings_console.md) (runtime_comparison) | Full |
| 9 | Time and shell | TIMER; Run/Make works | [09_time_shell.bas](09_time_shell.bas) | Semi |
| 10 | Audio (optional) | No crash if IDE uses sound | [10_audio.bas](10_audio.bas) | None |
| 11 | Other libqb | As needed when a feature fails | Ad hoc | Depends |

## How to Run

### Console-only (automated subset)

Portions 5, 6, 8, 9, 10 can be run and compared like runtime_comparison (compile + run, compare output):

```bash
# From QB64Fresh repo root
./tests/ide_equivalence/run_ide_equivalence.sh
```

This runs the console-only repros (05, 06, 09, 10) with QB64Fresh and optionally QB64pe, and writes results to `tests/ide_equivalence/results/`. Portion 8 uses existing `tests/runtime_comparison/` string tests and golden outputs.

**Comparing results (QB64Fresh vs QB64pe):** After running with QB64pe (`RUN_PE=1`), compare console output:

```bash
./tests/ide_equivalence/diff_results.sh
```

This prints diffs for any test where fresh and qb64pe output differ; skip/fail/timeout lines are ignored.

**Expected differences:** QB64pe often appends "Press enter to continue" at end of console runs. CWD and `_DIR$` differ if QB64pe runs from its `runtime_comparison/` dir vs QB64Fresh from repo root. TIMER and TIME$ differ by run time. These are environmental, not semantic failures.

### Verify GUI repros compile

All GUI repros (01–04, 07) should compile with QB64Fresh. From repo root:

```bash
./tests/ide_equivalence/check_gui_compile.sh
```

**Note:** 01 and 02 use `evnt` (built-in sub; maps to `qb_gfx_poll_events`) for window-close and key events. 03 and 04 use parser-supported forms: `CLS` without optional color; `_PUTIMAGE (x1,y1)-(x2,y2), img`.

### GUI portions (manual / semi-automated)

Portions 1–4 and 7 require a display. Run the same `.bas` under:

1. **Native QB64pe** (after `setup_lnx.sh`): open in IDE or `./qb64pe -x 01_window_display.bas -o /tmp/out && /tmp/out`
2. **QB64Fresh-built** (`qb64pe_fresh`): compile with QB64Fresh, then run the generated C with libqb64fresh_rt, or run the IDE built with QB64Fresh.

Document what you see in [BASELINE.md](BASELINE.md) (native) and compare with qb64pe_fresh. To capture baseline automatically for console portions (5, 6, 9, 10) and compile-only status for GUI portions (1–4, 7), run `./tests/ide_equivalence/capture_baseline.sh` from repo root (optionally with `RUN_PE=1` to include native qb64pe).

**Manual test steps (GUI)** — From QB64Fresh repo root, run each repro under both builds and fill BASELINE:

| Portion | Native QB64pe (from QB64pe dir) | QB64Fresh (emit-c + run) |
|---------|---------------------------------|---------------------------|
| 1. Window | `./qb64pe -x path/to/01_window_display.bas -o /tmp/p1 && /tmp/p1` | `qb64fresh tests/ide_equivalence/01_window_display.bas --emit-c -o /tmp/p1.c && gcc -o /tmp/p1 /tmp/p1.c -lm && /tmp/p1` |
| 2. Event loop | Same with `02_event_loop.bas` | Same with `02_event_loop.bas` |
| 3. Graphics | Same with `03_graphics_image.bas` | Same with `03_graphics_image.bas` |
| 4. Font | Same with `04_font_text.bas` | Same with `04_font_text.bas` |
| 7. GUI dialogs | Same with `07_gui_dialogs.bas` | Same with `07_gui_dialogs.bas` |

Use full path to the `.bas` file when calling qb64pe from QB64pe dir (e.g. `$(pwd)/../QB64Fresh/tests/ide_equivalence/01_window_display.bas`). For QB64Fresh use paths relative to repo root. **GUI portions 03, 04, 07** require `--runtime external` and linking with libqb64fresh_rt: build runtime with `cargo build -p qb64fresh-runtime --release --features "graphics-sdl2 dialogs"`, then `qb64fresh ... --emit-c --runtime external -o out.c` and `gcc -I runtime/include out.c -L target/release -lqb64fresh_rt $(pkg-config --libs sdl2 alsa wayland-client) -lm -lpthread -ldl -o out`. Close window with Esc or X; for 07 click OK in the message box.

### Full IDE checklist

After per-portion tests pass (or gaps are documented), run the full IDE and fill [BASELINE.md](BASELINE.md) "Full IDE checklist" table:

1. **Launch** — From QB64Fresh repo root: `./tests/ide_equivalence/run_full_ide.sh` (builds then runs qb64pe_fresh from QB64pe with ulimit). Or from QB64pe: `./run_qb64pe_fresh.sh`. **First run:** After the "Generated: …" lines, the script links the large C file (1–2 min); wait for "Linking qb64pe_fresh…" then "Starting IDE…" — the window appears after that. Do not Ctrl+C during the link step. **If no window appears:** Run manually to see errors: `cd <QB64pe> && ./qb64pe_fresh 2>&1`. Try forcing the video driver: `SDL_VIDEODRIVER=x11 ./qb64pe_fresh` or `SDL_VIDEODRIVER=wayland ./qb64pe_fresh`. If you see "QB64Fresh: _SCREENSHOW failed to init graphics", the display/DRM setup is failing; ensure you have a graphical session (DISPLAY or WAYLAND_DISPLAY set).
2. **Steps** — Open IDE, File → New/Open, edit (e.g. `PRINT "hi"`), Save, Run → Make (if applicable), Close with X.
3. **Record** — In BASELINE.md mark Pass? and Notes for each step; fill "Funky or broken behavior" and map to a portion (e.g. "Portion 4: font size wrong").

**Optional automation (X11 and Wayland)** — To drive the IDE with keystrokes so you don’t have to click through manually:

- **X11:** Requires `DISPLAY` and `xdotool` (e.g. `apt install xdotool`). Script finds the "QB64" window and sends keys.
- **Wayland:** The script **tries wtype first**. On **GNOME** (and some compositors), wtype is installed but fails at runtime: "Compositor does not support the virtual keyboard protocol" — so we cannot use wtype there. When that happens, the script prefers **X11 fallback** over ydotool: if `xdotool` is installed and `DISPLAY` is set (e.g. XWayland), it launches the IDE with `SDL_VIDEODRIVER=x11` and drives it with xdotool (no daemon). If xdotool is not available, it tries **ydotool** (requires ydotoold running). To auto-focus (when not using X11 fallback): Sway — `swaymsg`/`jq`; wlroots — `wlrctl`. Otherwise click the IDE when prompted (10s).
- **GNOME / wtype doesn't work:** Install **xdotool** (`apt install xdotool`). Ensure `DISPLAY` is set (on GNOME Wayland, XWayland often sets it to `:0` or `:1`). The script will then use the X11 fallback automatically (it prefers X11 over ydotool so you never need ydotoold). You do not need ydotool.
- **ydotool (optional):** Only used when xdotool + DISPLAY are not available. Requires `ydotoold` running (`sudo ydotoold &`); if ydotoold fails (e.g. "failed to open uinput device"), use the X11 fallback above (install xdotool).
- **Build once:** Run `./tests/ide_equivalence/run_full_ide.sh` once so `qb64pe_fresh` exists (you can cancel after the IDE window appears).
- **Run:** From QB64Fresh repo root: `./tests/ide_equivalence/automate_full_ide.sh`. The script starts the IDE in the background, waits for the "QB64" window, then sends: File → New, types `PRINT "hi"`, closes with Alt+F4, and dismisses "Save?" with **n** (Don’t Save). Success = IDE process exits; timeout or no window = non-zero exit.
- **Limits:** MCP browser tools cannot control desktop apps; this uses xdotool (X11) or wtype/ydotool (Wayland). Timing and dialog text may vary (locale, theme). On Wayland without Sway/wlrctl, you must click the IDE window when prompted. If the "Save?" dialog uses a different shortcut, set `KEY_DELAY_MS` or edit the script.

- **GNOME Wayland:** Full automation is best-effort. wtype is unsupported (virtual keyboard protocol); xdotool with X11 fallback can hang or fail (XWayland does not properly support XTest). The script uses per-command timeouts (`XDOTOOL_TIMEOUT`, default 5s) so it does not hang forever. If automation fails, use the **manual checklist**: `./tests/ide_equivalence/run_full_ide.sh` then perform the steps by hand. **Try PyMCPAutoGUI (MCP-based automation):** [PyMCPAutoGUI](https://github.com/kitfactory/PyMCPAutoGUI) lets an AI agent (e.g. in Cursor) control mouse/keyboard via PyAutoGUI. See **[tests/ide_equivalence/PyMCPAutoGUI.md](PyMCPAutoGUI.md)** for: install (`pip install pymcpautogui`), Cursor MCP config (example in [mcp_pymcpautogui.example.json](mcp_pymcpautogui.example.json)), and how to run the Full IDE checklist with `@PyMCPAutoGUI` (activate window, File→New, type, close). On Linux Wayland it may still need ydotool or have similar compositor limits.

### Wayland automation options (why it's hard)

Wayland is designed to block the classic “send global keystrokes / poke pixels” approach. Tools like xdotool relied on X11 (XTest) features that don’t exist in Wayland; many compositors block global input injection unless you use privileged/permissioned paths (portals, accessibility, compositor-specific APIs). The IDE we automate here is **qb64pe_fresh** (QB64pe compiled with QB64Fresh): it’s **SDL2/native**, not Electron and not GTK/Qt. That narrows the options:

| Approach | Fits our IDE? | Notes |
|----------|----------------|--------|
| **Playwright Electron** | No | qb64pe is not Electron; Playwright Electron doesn’t apply. |
| **AT-SPI (accessibility)** | Maybe | Drive UI via the Linux accessibility tree (dogtail, pyatspi, LDTP). Works under Wayland because it’s not global input injection. SDL2 apps often don’t expose great a11y by default; if the IDE exposed roles/names, this could be the most robust. |
| **Image-based** | Yes | “Look at the screen” and click/type (SikuliX, PyAutoGUI/PyMCPAutoGUI). Toolkit-agnostic; can work on Wayland. Downsides: breaks with theme/font/scaling; flakier in CI. Treat as fallback. |
| **Portal / compositor-permissioned** | Future | xdg-desktop-portal Remote Desktop/Input Capture is the Wayland-native direction. Support varies; more engineering. |
| **Controlled session** | Yes | Run tests in a pure X11 session, nested compositor, or VM for determinism. Our script’s X11 path works there; or use the manual checklist. |

**Practical recommendation:** Use the **manual checklist** on GNOME Wayland. For automation: try **Sway** (wtype + swaymsg work), or a **pure X11** test session; optionally explore **AT-SPI** if you can add/improve accessibility in the IDE, or **image-based** (PyMCPAutoGUI) as a best-effort fallback.

## CI

The **IDE Equivalence** job in `.github/workflows/ci.yml` runs on every push/PR:

- Builds `qb64fresh`, then runs `run_ide_equivalence.sh` with `RUN_PE=0` (console-only tests 05, 06, 09, 10) and `check_gui_compile.sh` (GUI repros 01–04, 07 must compile). No QB64pe in CI.

## References

- [QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md](../../docs/ThingsToDo/QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md) — Full plan and checklist.
- [BASELINE.md](BASELINE.md) — Native baseline per portion; run `./tests/ide_equivalence/capture_baseline.sh` to populate when running native qb64pe and qb64pe_fresh.
- [QB64pe BUILD_WITH_QB64FRESH.md](../../../QB64pe/BUILD_WITH_QB64FRESH.md) — How to build QB64pe with QB64Fresh.
- [QB64pe docs/libqb-usage-by-ide.md](../../../QB64pe/docs/libqb-usage-by-ide.md) — libqb symbols used by the IDE.
- [tests/runtime_comparison/](../../runtime_comparison/) — Golden-output tests for strings and console (portion 8).
