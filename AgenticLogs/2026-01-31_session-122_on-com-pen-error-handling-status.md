# Session 122: ON COM/PEN and Error Handling Status (QB64PE_MISSING_FEATURES)

**Date:** 2026-01-31  
**Scope:** Sections 2.2 (ON Statements) and 2.3 (Error Handling) of QB64PE_MISSING_FEATURES.md.

## Summary

Verified implementation status for ON COM, ON PEN, RESUME, ERL, and ERR. All are already implemented; the doc was updated to reflect "Implemented" (with runtime stubbed for ON COM/PEN and ERL caveat for runtime-originated errors).

## 2.2 ON Statements (ON COM, ON PEN)

- **Status:** Parser, AST, typed IR, and codegen are implemented. Runtime is stubbed (fprintf "not implemented").
- **Gap:** Full ON COM would require serial port IRQ/async I/O; ON PEN would require obsolete light pen hardware. Stubbed runtime is intentional.
- **Action:** Updated QB64PE_MISSING_FEATURES.md to mark as "Implemented (runtime stubbed)" and corrected syntax to `ON COM(n) GOSUB` / `ON PEN GOSUB`.

## 2.3 Error Handling (RESUME, ERL, ERR)

- **Status:** All implemented.
  - **RESUME / RESUME NEXT / RESUME label:** Codegen in `error_jump.rs`; `_qb_error_line` for retry, `qb_clear_error()` for RESUME NEXT (external runtime).
  - **ERL:** `qb_err_line()` in runtime; inline C and external Rust both expose it. May be 0 when error comes from runtime (e.g. file I/O) because BASIC line numbers are not passed from codegen.
  - **ERR / ERROR:** `qb_err_code()`, `qb_error(code)`; ERROR statement sets handler and retry label.
- **Action:** Updated QB64PE_MISSING_FEATURES.md from "Partially implemented" to "Implemented" with a short note on ERL.

## Files changed

- `docs/ThingsToDo/QB64PE_MISSING_FEATURES.md` — Sections 2.2 and 2.3 table and status text.
- `docs/archive/QB64PE_MISSING_FEATURES_COMPLETED.md` — Added ON COM/PEN (stubbed) and error-handling (RESUME, ERL, ERR) to completed list.

## No code changes

No compiler or runtime code changes; only documentation updates to match current behavior.
