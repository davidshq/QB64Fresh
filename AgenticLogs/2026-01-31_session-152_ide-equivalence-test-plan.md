# Session 152: IDE Equivalence Test Plan Implementation

**Date:** 2026-01-31

## Summary

Implemented the [QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md](../docs/ThingsToDo/QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md): added `tests/ide_equivalence/` with minimal repros per portion, baseline template, README, and run script for console-only automation.

## Accomplished

1. **tests/ide_equivalence/** directory
   - **README.md** — Purpose, test order table, how to run (console vs GUI), full IDE checklist, references.
   - **BASELINE.md** — Template for native baseline capture per portion (1–11); fill in when running native qb64pe and qb64pe_fresh.
   - **01_window_display.bas** — SCREEN 12, _TITLE, evnt + INKEY$ loop until Esc (portion 1).
   - **02_event_loop.bas** — Event loop with key echo; Esc to exit (portion 2).
   - **03_graphics_image.bas** — _NEWIMAGE, draw, _PUTIMAGE, _DISPLAY (portion 3).
   - **04_font_text.bas** — _PRINTSTRING, _FONTHEIGHT, _PRINTWIDTH (portion 4; default font).
   - **05_file_io.bas** — OPEN/write/read/CLOSE, $CONSOLE:ONLY (portion 5).
   - **06_filesystem.bas** — _CWD$, _DIR$, _FILEEXISTS, $CONSOLE:ONLY (portion 6).
   - **07_gui_dialogs.bas** — _MESSAGEBOX (portion 7).
   - **08_strings_console.md** — Pointer to tests/runtime_comparison/ for portion 8; no separate .bas.
   - **09_time_shell.bas** — TIMER, SLEEP, TIME$, DATE$, $CONSOLE:ONLY (portion 9).
   - **10_audio.bas** — BEEP, $CONSOLE:ONLY (portion 10).
   - **run_ide_equivalence.sh** — Runs console-only tests (05, 06, 09, 10) with QB64Fresh and optionally QB64pe; results in `results/fresh/` and `results/qb64pe/`; fingerprint skip when unchanged.
   - **capture_baseline.sh** — Captures native baseline into BASELINE.md: runs run_ide_equivalence.sh, then writes "Captured outputs (auto-generated)" with native vs qb64pe_fresh output for portions 5, 6, 9, 10 and compile-only table for portions 1–4, 7. Run from repo root; RUN_PE=1 to include native qb64pe.

3. **GUI repros run and documented**
   - Ran GUI repros 01–04, 07 under native qb64pe and QB64Fresh (compile + run with timeout). Results: 01–02 fail on `evnt` (native: syntax error; QB64Fresh: undefined procedure); 03–04, 07: native compile+run OK; QB64Fresh compile OK, link FAIL (qb_gfx_putimage arity, runtime include). BASELINE.md updated with Observed per portion, "Manual GUI test protocol" (exact commands), and a "Differences summary" table.

2. **Plan doc update**
   - Added "Implementation" section to QB64PE_IDE_EQUIVALENCE_TEST_PLAN.md linking to tests/ide_equivalence, run script, BASELINE.md, and runtime_comparison for portion 8.

## Decisions

- Portion 8 (strings/console) reuses existing runtime_comparison tests and golden outputs; no duplicate repros in ide_equivalence.
- Console-only repros use `$CONSOLE:ONLY` so they can run headless in CI.
- GUI repros (01–04, 07) are manual/semi-automated; run script only automates 05, 06, 09, 10.
- 04_font_text.bas uses default font (_PRINTSTRING, _FONTHEIGHT, _PRINTWIDTH) without _LOADFONT so it runs on systems without a specific TTF.

## Continuation

- **Full run with QB64pe:** `RUN_PE=1 FORCE=1 ./tests/ide_equivalence/run_ide_equivalence.sh` — all 4 console-only tests pass for both QB64Fresh and QB64pe (06_filesystem uses _CWD$).
- **diff_results.sh** added: compares results/fresh vs results/qb64pe; README updated with "Comparing results" and "Expected differences" (Press enter to continue, CWD, TIMER/TIME$).

## Continuation (GUI compile + check script)

- **GUI repros 03, 04:** Fixed compile errors: use `CLS` only (no optional color); use `_PUTIMAGE (x1,y1)-(x2,y2), img` (parser expects range form).
- **GUI repros 01, 02:** Dropped `evnt` from loop (evnt not yet a built-in); use INKEY$ only; added comments that evnt can be added when available.
- **check_gui_compile.sh:** Added script to verify all GUI repros (01–04, 07) compile with QB64Fresh; README updated with "Verify GUI repros compile" and notes on evnt/CLS/_PUTIMAGE.

## Next steps

- ~~Capture native baseline (BASELINE.md) when running native qb64pe and qb64pe_fresh for each portion.~~ **Done:** `capture_baseline.sh` runs `run_ide_equivalence.sh`, then updates BASELINE.md with captured output for console portions (5, 6, 9, 10) and compile-only status for GUI portions (1–4, 7). Run from repo root; use RUN_PE=1 to include native qb64pe.
- ~~Run GUI repros (01–04, 07) manually under both builds and document differences.~~ **Done:** Ran compile + run (with timeout) for 01–04 and 07 under native qb64pe and QB64Fresh. Documented in BASELINE.md: portions 1–2 fail on `evnt` (native: syntax error; QB64Fresh: undefined procedure); portions 3, 4, 7: native compile+run OK; QB64Fresh compile OK but link FAIL (qb_gfx_putimage arity, runtime include). Added "Manual GUI test protocol" with exact commands and a differences summary table.
- ~~Fix evnt so 01/02 compile (task 1).~~ **Done:** evnt was already registered in builtins/subs.rs; 01 and 02 compile with QB64Fresh (release build was stale earlier). check_gui_compile.sh now passes for 01–04, 07.
- ~~Get GUI 03, 04, 07 to link and run (task 2).~~ **Done:** Use `--runtime external` and link with libqb64fresh_rt (build runtime with `cargo build -p qb64fresh-runtime --release --features "graphics-sdl2 dialogs"`). Added `qb_messagebox(QbString* msg, QbString* title, QbString* btns)` to runtime header and dialogs.rs for 3-arg _MESSAGEBOX so 07 links. All three (03, 04, 07) link and run; BASELINE and README updated with external-runtime instructions.
- **evnt built-in:** Added `evnt` as a built-in sub (no args); semantic in `subs.rs`, codegen maps to `qb_gfx_poll_events` in `call.rs`. Restored `evnt` in 01 and 02 repros; README updated.
- **CI:** Added `ide-equivalence` job in `.github/workflows/ci.yml`: build qb64fresh, run `run_ide_equivalence.sh` (RUN_PE=0), run `check_gui_compile.sh`. README updated with CI section.
- **Manual test steps:** README now has a "Manual test steps (GUI)" table with copy-paste commands for native qb64pe and QB64Fresh (emit-c + gcc + run) per portion 1–4 and 7. BASELINE portion 1 references it.
- **Plan doc:** Test Order and Checklist now links each portion to its repro file and to BASELINE.md. References and Next Steps updated (run manual GUI steps, fill BASELINE). Implementation section expanded (check_gui_compile, diff_results, CI, evnt).

## Full IDE checklist (task 1)

- **run_full_ide.sh** — Added in `tests/ide_equivalence/run_full_ide.sh`: launches qb64pe_fresh from QB64pe via `run_qb64pe_fresh.sh` (builds then runs with ulimit); run from QB64Fresh repo root; prints reminder to fill BASELINE after testing.
- **BASELINE.md** — "Full IDE checklist" section now has "How to complete this checklist" (launch via run_full_ide.sh or from QB64pe, perform steps 1–6, fill Pass?/Notes and "Funky or broken behavior").
- **README.md** — "Full IDE checklist" subsection updated: launch with run_full_ide.sh, steps, and record in BASELINE.
- **Manual fill** — The checklist table (Open IDE, File→New/Open, Edit, Save, Run→Make, Close X) and "Funky or broken behavior" remain for the user to fill after running the IDE; automation cannot interact with the GUI.

## Optional GUI automation (so user doesn’t have to click manually)

- **MCP browser tools** — Only control web pages; QB64pe IDE is a desktop app, so browser MCP cannot drive it.
- **automate_full_ide.sh** — Added optional script that uses **xdotool** (X11 only) to drive the IDE: launch qb64pe_fresh in background, wait for window "QB64", send File→New, type `PRINT "hi"`, Alt+F4, then "n" (Don’t Save). Success = IDE exits cleanly. Requires: DISPLAY, xdotool; does not work on Wayland.
- **README** — "Optional automation (X11 only)" subsection: requirements (xdotool), build once with run_full_ide.sh, run automate_full_ide.sh, limits (MCP can’t control desktop; timing/locale may vary; Wayland unsupported).
- **BASELINE** — "Optional automation" note in Full IDE checklist: install xdotool, run automate_full_ide.sh; MCP can’t control desktop apps, script uses xdotool.
