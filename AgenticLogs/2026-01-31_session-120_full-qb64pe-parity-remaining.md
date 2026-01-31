# Session 120: Full QB64pe parity — remaining implementation

**Date:** 2026-01-31  
**Focus:** Implement everything that remains for QB64pe parity (within project scope).

## Summary

- **Completed:** ON COM / ON PEN / ON SIGNAL runtime stubs for **external** runtime.
- **Scope clarified:** “Full parity” within project scope excludes raw OpenGL (`_GL*`), RUN restart/line, and full MIDI synthesis; remaining doc gaps were ON COM/PEN in the external lib.

## Decisions

1. **Legacy event stubs in external runtime**  
   Generated C for `ON COM`, `ON PEN`, `ON SIGNAL` calls `qb_on_com`, `qb_on_pen`, `qb_com_control`, `qb_pen_control`, `qb_on_signal`, `qb_signal_control`. With `--runtime external`, those symbols must come from libqb64fresh_rt. We added stub implementations in `runtime/src/events.rs` (one-time stderr warning, no-op) and declarations in `runtime/include/qb64fresh_rt.h`. Inline runtime already emitted equivalent C in `legacy.rs`; no codegen change.

2. **What “full parity” means here**  
   From [LIBQB_FUNCTIONALITY.md](docs/QB64pe/LIBQB_FUNCTIONALITY.md), [QB64PE_MISSING_FEATURES.md](docs/ThingsToDo/QB64PE_MISSING_FEATURES.md), and [QB64Fresh_VS_QB64pe_DIFFERENCES.md](docs/QB64pe/QB64Fresh_VS_QB64pe_DIFFERENCES.md):
   - **Intentional exclusions:** Raw OpenGL (`_GL*`), RUN with no args (restart), RUN line/label, cross-procedure GOTO, legacy DOS (INP/OUT/INTERRUPT as stubs).
   - **Remaining optional/future:** MIDI playback (soundfont path stored; synthesis for `.mid` / PLAY MB not yet implemented), RUN restart/line (deferred).

## Implementation

### 1. Runtime: ON COM / ON PEN / ON SIGNAL stubs

- **File:** `runtime/src/events.rs`
- Added: `qb_on_com`, `qb_com_control`, `qb_on_pen`, `qb_pen_control`, `qb_on_signal`, `qb_signal_control` with `std::sync::Once` for one-time stderr warnings.
- **File:** `runtime/include/qb64fresh_rt.h`
- Added declarations for the six symbols in the “Legacy event stubs” section.

### 2. Docs

- **QB64PE_MISSING_FEATURES.md:** Remaining “Missing ON COM/PEN runtime” cause removed; priority section updated to state stubs are done (inline + external).
- **LIBQB_FUNCTIONALITY.md:** “Remaining gaps” table updated with “Legacy events” row (stub ✅).
- **LIBQB_FUNCTIONALITY_COMPLETED.md:** New “40. Legacy event stubs” section listing the six symbols and where they are implemented.

## What remains outside this session

- **MIDI playback:** When `_SNDPLAYFILE` is used with a `.mid` file (or PLAY with MB), load soundfont and synthesize MIDI→PCM (e.g. rustysynth). Path storage is done; synthesis is runtime-only work.
- **RUN (no args) / RUN line/label:** Deferred; documented in QB64Fresh_VS_QB64pe_DIFFERENCES.
- **Raw OpenGL:** Excluded by ADR-0014; use DECLARE LIBRARY for OpenGL.

## Verification

- `cargo build -p qb64fresh-runtime` succeeds.
- No new linter errors in `runtime/src/events.rs`.
