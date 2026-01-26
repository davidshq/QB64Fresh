# Session 055: Runtime Documentation Update (2026-01-25)

## Summary

Updated `docs/RUNTIME_ARCHITECTURE_PERSPECTIVES.md` to match the current codebase (inline vs external runtime, line counts, bootstrap, cross-links) and rewrote `docs/RUNTIME_IMPLEMENTATION_PLAN.md` for the dual-runtime layout, correct paths, current status (~98% implemented), and STUB_FUNCTIONS references.

## Changes

### 1. Architecture Diagram

- **Inline runtime:** Expanded from 4 to a fuller set of modules: `types`, `strings`, `io`, `file`, `keyboard`, `memory`, `timing`, `arrays`, `math`, `error`, `graphics` (stubs), `audio` (stubs), `legacy`, `system`, `debug`. Noted that graphics uses frame limiting (`QB64FRESH_MAX_FRAMES=1000`) and that output is self-contained C.
- **External runtime:** Added `math`, `*_ffi.rs` (C bindings), and the `runtime/include/qb64fresh_rt.h` contract.

### 2. Line Counts and DRY (Architect, Pragmatic)

- Replaced “~2000 lines of C-as-Rust-strings” and “~15000 lines of Rust” with: ~9000 lines of Rust in `src/codegen/c_backend/runtime/` that *emit* inline C, and ~16000 lines in `runtime/src/` for the external runtime.
- Clarified that the inline side “emits” C rather than being “C-in-strings” only.

### 3. Shared Header (Option B)

- Specified `runtime/include/qb64fresh_rt.h` as the external API contract. Noted that the inline runtime emits its own C and does not include this header; both should match the same semantics.

### 4. Bootstrap / Startup Hang (Pragmatic, Action Items)

- Replaced “Fix the INI file reading hang” with “Resolve startup/IDE initialization hang” (keyboard polling, IDE config file I/O, event loops), with a link to session-050 bootstrap-runtime-fixes.

### 5. Action Items

- Immediate: Resolve startup/IDE initialization hang (as above).
- Medium-term: Linked to `ThingsToDo/STUB_FUNCTIONS_REMAINING.md` for stub-only features.

### 6. Appendix and Related Docs

- **When to use:** Inline: added “Headless debugging” with the optional `debug` module (breakpoints, stepping, IPC).
- **Related documentation:**  
  - [GRAPHICS.md](GRAPHICS.md) — stub behavior, `QB64FRESH_MAX_FRAMES`, backends.  
  - [runtime/include/qb64fresh_rt.h](../runtime/include/qb64fresh_rt.h) — C API for external runtime.  
  - [STUB_FUNCTIONS_REMAINING.md](ThingsToDo/STUB_FUNCTIONS_REMAINING.md).

### 7. Last Updated

- 2026-01-25.

### RUNTIME_IMPLEMENTATION_PLAN.md

- **Dual-runtime:** Added top-level explanation of inline vs external (source, output, graphics/audio, use case) and a pointer to RUNTIME_ARCHITECTURE_PERSPECTIVES.
- **Current status:** ~412/419 implemented (~98.3%), with links to STUB_FUNCTIONS_REMAINING and STUB_FUNCTIONS_FULL.
- **Architecture:** Replaced single tree with two: Inline (`src/codegen/c_backend/runtime/*`) and External (`runtime/src/*`), including `font_ffi`, `font_manager`, `joystick`, `graphics_ffi`, `audio_ffi`.
- **Phase sections:** Corrected file paths for both runtimes (e.g. `string.rs` vs `strings.rs`, `file.rs` for file I/O, `joystick.rs` for gamepad). Marked many core functions [x] where implemented; left [~]/[ ] for stubs and not-started.
- **Recommended crates:** Switched audio from miniaudio to **rodio**; added `rfd`, `freetype-rs`, `image` to match `runtime/Cargo.toml`.
- **Success criteria:** Included bootstrap (and startup hang), conformance between runtimes, and STUB_FUNCTIONS doc references.
- **Related docs:** RUNTIME_ARCHITECTURE_PERSPECTIVES, GRAPHICS, qb64fresh_rt.h, STUB_FUNCTIONS_REMAINING, STUB_FUNCTIONS_FULL.

## Files Modified

- `docs/RUNTIME_ARCHITECTURE_PERSPECTIVES.md`
- `docs/RUNTIME_IMPLEMENTATION_PLAN.md`
