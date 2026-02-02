# QB64pe IDE Equivalence Test Plan

## Purpose

Incrementally test each portion of QB64pe IDE functionality so that the **QB64Fresh-built IDE** (`qb64pe_fresh`: single C file + libqb64fresh_rt) is equivalent to the **native-built IDE** (setup_lnx.sh → Makefile → C++ + libqb). Capture baseline behavior from the native IDE and compare QB64Fresh-built behavior per portion. The plan is written as a consultation among six expert perspectives, then synthesized into a concrete test strategy and checklist.

---

## Expert Perspectives

### Software architect

Test by **subsystem and dependency order**. The IDE depends on: (1) window and display, (2) event loop (evnt, keys, timer, stop_program), (3) graphics and image APIs for panels, (4) font and text for rendering, (5) file I/O and filesystem for menus and recent list, (6) GUI dialogs for Open/Save/MessageBox. Order tests so that window + event loop are verified first; then graphics and font (often the cause of "funky" UI); then file I/O and dialogs. Do not test menus in isolation until the display and event loop that drive them are known good.

### Pragmatic engineer

Use **minimal repros per area**: the smallest .bas (or standalone C calling libqb64fresh_rt) that exercises one libqb portion. Reuse patterns from `QB64pe/runtime_comparison/` (golden .out outputs) where behavior is console or file output; for GUI portions, minimal repros are short BASIC programs that open a window, draw one thing, or open one dialog. Avoid big-bang testing of the full IDE until each portion has a defined repro and pass criterion. Prefer adding one repro per portion to a dedicated directory (e.g. `tests/ide_equivalence/` or under `examples/`) so failures are easy to isolate.

### Rust expert

The **runtime API** (libqb64fresh_rt) must match the symbols and semantics the IDE uses, as listed in [QB64pe/docs/libqb-usage-by-ide.md](../../../QB64pe/docs/libqb-usage-by-ide.md). For each libqb section used by the IDE (display, qbs, file-fields, gfs, graphics, font, gui, etc.), add or extend **unit or integration tests** in the QB64Fresh runtime crate that call the same entry points with the same arguments and assert expected behavior. Where libqb and libqb64fresh_rt differ (e.g. OpenGL vs SDL2), document the difference and define equivalence criteria (e.g. "window appears and closes" rather than "pixel-identical render"). Prioritize font and graphics APIs that affect IDE text and layout.

### Language expert

**Codegen equivalence**: QB64Fresh must emit C that matches the semantics the IDE BASIC expects. Focus on IDE-used constructs: string ops (qbs_*, CHR$, ASC, SPACE$, STRING$, INSTR, MID$, LSET/RSET), file I/O (OPEN, CLOSE, GET, PUT, SEEK, LOF, LOC, EOF, FIELD), graphics (_DEST, _SOURCE, _NEWIMAGE, _PRINTSTRING, _FONT), and event loop (evnt, INKEY$, TIMER, stop_program). Add or run existing tests that compile a small BASIC snippet with both QB64pe (native) and QB64Fresh, then compare generated code or runtime output where possible. Flag any semantic differences (e.g. string encoding, error codes) so runtime or codegen can be fixed.

### Debugging expert

**Reproducibility** is critical. Capture "native baseline" on a system where `./qb64pe` works: for each portion, document expected behavior (e.g. "Window: 800x600, menu bar visible; File → Open opens dialog") and optionally capture screenshots, event logs, or file state. To isolate a failing portion, use a stub IDE or minimal window (e.g. minimal .bas that only opens window and runs evnt loop), and use env flags (e.g. `QB64FRESH_IDE_COMPAT=1`, `QB64FRESH_SCREEN_TRACE=1`, `QB64FRESH_DISABLE_SCREENHIDE=1`) and runtime logging to trace where behavior diverges. Record OS, display server (X11/Wayland), and GPU so issues can be reproduced.

### QA expert

Maintain a **test matrix**: per-portion pass/fail, manual vs automated, and clear equivalence criteria (visual, behavioral, or file output). Define criteria so that "pass" is unambiguous (e.g. "Window appears within 2s and closes on X" or "File → Save writes correct bytes to disk"). Prefer **regression tests** where possible: add a CI-friendly subset (e.g. console-only BASIC that uses the same runtime APIs, or headless checks that verify file I/O or string behavior) so that future changes do not break IDE-equivalent behavior without detection. GUI portions will remain largely manual or semi-automated (script launches IDE, waits, captures screenshot or log); document the manual steps so any contributor can run them.

---

## IDE Portions to Test

Derived from [libqb-usage-by-ide.md](../../../QB64pe/docs/libqb-usage-by-ide.md) and [QB64PE_ARCHITECTURE.md](../QB64pe/QB64PE_ARCHITECTURE.md). Order follows dependencies (window and events before menus and dialogs).

| # | Portion | Key APIs | Equivalence criterion | Minimal repro idea | Native baseline capture | Automation |
|---|---------|----------|------------------------|--------------------|--------------------------|------------|
| 1 | **Window and display** | `display`, `_dest`, `_source`, `qbg_sub_window`, show/hide, title | Window appears, has correct title, can be closed (X) | .bas: SCREEN 0 or graphics screen, set title, DO: LOOP until key/close | Run native qb64pe, note window size/title; close with X | Semi (script launch, wait, check process exit) |
| 2 | **Event loop** | `evnt()`, INKEY$, TIMER, `stop_program` | Main loop runs; key events and window close are detected | .bas: DO: k$ = INKEY$: IF k$ <> "" THEN PRINT k$: LOOP UNTIL k$ = CHR$(27) | Run native, press keys and close; confirm loop responds | Semi (key injection + exit code) |
| 3 | **Graphics / image** | `_newimage`, `_copyimage`, `_freeimage`, `_printimage`, `_loadimage`, `_saveimage`, color/alpha | IDE can create image, draw to it, display; no crash or black screen | .bas: create image, _PUTIMAGE to screen, _DISPLAY | Native: same .bas under qb64pe; compare visual or save to file | None (visual) or semi (compare saved image) |
| 4 | **Font and text** | `_loadfont`, `_freefont`, `_font`, `_fontheight`, `_fontwidth`, `_printstring`, `_printwidth` | Text and line height match native (addresses "funky" UI) | .bas: load font, _PRINTSTRING at (0,0), measure height/width | Document native font size/line height; compare with qb64pe_fresh | None (visual/metric) |
| 5 | **File I/O** | OPEN, CLOSE, GET, PUT, LOF, LOC, EOF, FIELD, recent.bin | File menu works; recent list loads/saves; no Error 53 on recent.bin | .bas: OPEN file, write/read, CLOSE; or IDE: File → Open, Save | Native: open IDE, File → Open, save; check settings/recent.bin | Semi (file existence and contents) |
| 6 | **Filesystem** | `_cwd`, `_dir`, `_direxists`, `_fileexists`, `_startdir`, CHDIR, `_files`, `_fullpath`, KILL, MKDIR, NAME, RMDIR | Open/Save dialogs show correct paths; CHDIR works | .bas: CHDIR, _DIR$, _FILEEXISTS; or IDE: File → Open dialog | Native: note CWD and dialog path; run from different CWD | Semi (script CWD + dialog or file list) |
| 7 | **GUI dialogs** | `_gui*` (message box, input box, file/folder/color) | Dialogs appear and return correct values | .bas: _MESSAGEBOX or file dialog call | Native: run dialog, note appearance and return value | None (manual) or semi (headless stub) |
| 8 | **Strings and console** | qbs_*, CHR$, ASC, SPACE$, STRING$, INSTR, MID$, LSET/RSET, trim | Editor text and search behave; no encoding crashes | Use existing runtime_comparison string tests; or minimal IDE edit | Golden .out in runtime_comparison | Full (golden output compare) |
| 9 | **Time and shell** | TIMER, `_delay`, `_limit`, SLEEP, TIME$/DATE$, COMMAND$, SHELL/`_shellhide` | Timing and Run/Make work; SHELL runs external process | .bas: PRINT TIMER; SLEEP 1; or IDE: Run → Make | Native: Run → Make, confirm compile runs | Semi (run Make, check exit code and output file) |
| 10 | **Audio (optional)** | BEEP, PLAY, SOUND, `_snd*` | No crash on IDE startup if IDE uses sound | .bas: BEEP or _SNDPLAYFILE | Native: note if IDE beeps; optional for first pass | None |
| 11 | **Other libqb** | encoding, hashing, hex/oct/bin, clipboard, mem, bitops | As needed when a specific IDE feature fails | Add repro when failure is isolated | Ad hoc | Depends on feature |

---

## Incremental Test Strategy

1. **Baseline (native):** On a system where `./qb64pe` works (after setup_lnx.sh), document or capture behavior for each portion (e.g. "Window: 800x600, menu bar visible; File → Open opens dialog"). Store in a short doc or checklist so QB64Fresh-built behavior can be compared.

2. **Per-portion:** For each portion, (1) define minimal repro (minimal .bas or C that exercises only that portion), (2) run repro under native IDE or native runtime and under QB64Fresh-built IDE or libqb64fresh_rt, (3) compare output/behavior/screenshots against the criterion in the table above.

3. **Full IDE:** After all portions pass (or known gaps are documented), run the full IDE (`qb64pe_fresh`) and run a short manual checklist: open IDE, File → New/Open, edit, Save, Run/Make (if applicable), close with X. Record any remaining "funky" or broken behavior and map it back to a portion for follow-up.

4. **Automation:** Where possible, reuse patterns from `QB64pe/runtime_comparison/` (golden .out) for console-only or file-output tests. GUI portions stay manual or semi-automated (script launches IDE, waits, captures screenshot or log). Add a CI job that runs the automatable subset (e.g. string, file I/O, or console-only repros) so regressions are caught.

---

## Test Order and Checklist

Use this order (dependencies first). One-line criterion per portion. Record results in [tests/ide_equivalence/BASELINE.md](../../tests/ide_equivalence/BASELINE.md).

- [ ] **1. Window and display** — Window appears, title correct, closes with X. Repro: [01_window_display.bas](../../tests/ide_equivalence/01_window_display.bas).
- [ ] **2. Event loop** — Main loop runs; keys and window close detected. Repro: [02_event_loop.bas](../../tests/ide_equivalence/02_event_loop.bas).
- [ ] **3. Graphics / image** — Create image, draw, display; no crash or black screen. Repro: [03_graphics_image.bas](../../tests/ide_equivalence/03_graphics_image.bas).
- [ ] **4. Font and text** — Text and line height correct (fixes "funky" UI). Repro: [04_font_text.bas](../../tests/ide_equivalence/04_font_text.bas).
- [ ] **5. File I/O** — File menu, recent list, no Error 53 on recent.bin. Repro: [05_file_io.bas](../../tests/ide_equivalence/05_file_io.bas).
- [ ] **6. Filesystem** — Open/Save dialogs and paths correct. Repro: [06_filesystem.bas](../../tests/ide_equivalence/06_filesystem.bas).
- [ ] **7. GUI dialogs** — Message box and file dialogs appear and return. Repro: [07_gui_dialogs.bas](../../tests/ide_equivalence/07_gui_dialogs.bas).
- [ ] **8. Strings and console** — Editor text and search; no encoding crashes. Use [tests/runtime_comparison/](../../tests/runtime_comparison/) (see [08_strings_console.md](../../tests/ide_equivalence/08_strings_console.md)).
- [ ] **9. Time and shell** — TIMER/SHELL; Run/Make works. Repro: [09_time_shell.bas](../../tests/ide_equivalence/09_time_shell.bas).
- [ ] **10. Audio (optional)** — No crash if IDE uses sound. Repro: [10_audio.bas](../../tests/ide_equivalence/10_audio.bas).
- [ ] **11. Other libqb** — As needed when a feature fails.

### Summary table

| Portion | Criterion | Repro | Native baseline | Automated? |
|---------|-----------|-------|------------------|------------|
| 1. Window and display | Window appears, closes with X | Minimal SCREEN + title + loop | Document size/title; close X | Semi |
| 2. Event loop | Loop runs; keys and close work | INKEY$ + evnt loop | Key press and X | Semi |
| 3. Graphics / image | Draw image, no crash | _NEWIMAGE + _PUTIMAGE | Visual or saved image | None/semi |
| 4. Font and text | Text/line height correct | _LOADFONT + _PRINTSTRING | Document metrics | None |
| 5. File I/O | File menu, recent.bin | OPEN/CLOSE or IDE File menu | recent.bin and menu | Semi |
| 6. Filesystem | Dialogs and paths | CHDIR, _DIR$, or IDE dialog | CWD and dialog path | Semi |
| 7. GUI dialogs | Dialogs appear and return | _MESSAGEBOX or file dialog | Visual and return value | None |
| 8. Strings and console | Editor/search; no crash | runtime_comparison string tests | Golden .out | Full |
| 9. Time and shell | TIMER; Run/Make | TIMER/SHELL or IDE Make | Make exit and output | Semi |
| 10. Audio | No crash on sound | BEEP or _SND* | Optional | None |
| 11. Other | Per-feature | As needed | Ad hoc | Depends |

---

## References and Next Steps

- [QB64pe BUILD_WITH_QB64FRESH.md](../../../QB64pe/BUILD_WITH_QB64FRESH.md) — How to build and run QB64pe with QB64Fresh.
- [SETUP_LNX_VS_QB64FRESH_BUILD.md](SETUP_LNX_VS_QB64FRESH_BUILD.md) — How setup_lnx.sh and Makefile work vs QB64Fresh build path.
- [QB64pe docs/libqb-usage-by-ide.md](../../../QB64pe/docs/libqb-usage-by-ide.md) — Which libqb symbols the IDE uses.
- [QB64PE_IDE_FUNCTIONALITY_CHECKLIST.md](../QB64pe/QB64PE_IDE_FUNCTIONALITY_CHECKLIST.md) — Full IDE feature list (VSCode vs IDE; use for menu/feature reference).

**Next steps:** Run manual GUI steps (see [tests/ide_equivalence/README.md](../../tests/ide_equivalence/README.md) "Manual test steps (GUI)"), capture native baseline in [BASELINE.md](../../tests/ide_equivalence/BASELINE.md), then run the same repros with `qb64pe_fresh` and document differences. Full IDE checklist: open IDE, File → New/Open, edit, Save, Run/Make, close with X.

---

## Implementation

- **Test directory:** [tests/ide_equivalence/](../../tests/ide_equivalence/) — minimal repros (01–10), README, [BASELINE.md](../../tests/ide_equivalence/BASELINE.md), run script, diff script, GUI compile check.
- **Console-only automation:** `./tests/ide_equivalence/run_ide_equivalence.sh` runs portions 5, 6, 9, 10; results in `tests/ide_equivalence/results/`. Use `RUN_PE=0` to skip QB64pe; `diff_results.sh` to compare fresh vs qb64pe output.
- **GUI compile check:** `./tests/ide_equivalence/check_gui_compile.sh` verifies repros 01–04 and 07 compile with QB64Fresh.
- **Strings/console (portion 8):** Use existing [tests/runtime_comparison/](../../tests/runtime_comparison/) and golden outputs; see [08_strings_console.md](../../tests/ide_equivalence/08_strings_console.md).
- **CI:** Job `ide-equivalence` in `.github/workflows/ci.yml` runs console-only tests and GUI compile check on every push/PR.
- **evnt:** Built-in sub (no args) maps to `qb_gfx_poll_events`; used in 01 and 02 for window close and key events.
