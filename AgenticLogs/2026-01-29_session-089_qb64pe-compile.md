# Session 089 – QB64pe compile (rule workflow)

**Date:** 2026-01-29

## Goal

Follow qb64fresh-qb64pe-compile rule: build QB64Fresh, compile QB64pe in its directory with 4GB RAM cap, run and test IDE.

## Steps

1. Build QB64Fresh (latest code).
2. Compile QB64pe from `QB64pe/` with `ulimit -v 4194304`.
3. Run QB64pe and attempt IDE use; troubleshoot as needed.

## Outcome

- **QB64Fresh:** Built release (`cargo build --release --bin qb64fresh` and `-p qb64fresh-runtime`) ✓
- **Emit C:** From QB64pe directory with 4GB cap: `ulimit -v 4194304 && qb64fresh source/qb64pe.bas --emit-c -o qb64pe_fresh.c` ✓ (≈3.7s)
- **Link:** `gcc -I ../QB64Fresh/runtime/include qb64pe_fresh.c -L ../QB64Fresh/target/release -lqb64fresh_rt ... -o qb64pe_fresh` ✓ (≈19s)
- **Run:** `./qb64pe_fresh -x -w source/qb64pe.bas` still crashes with:
  - Many `qb_string_len: suspicious length 11910883886839364429 at 0x...` (bad pointers from data segment)
  - `malloc(): unaligned tcache chunk detected` → Aborted (core dumped)

Same class of bug as in **Session 088** (GDB showed `qb_string_len` called with address-of-global instead of string handle). The BYREF string fix from 088 is in the current codegen; either additional code paths still pass `&string_var`, or a different call chain (e.g. `qb_readchunk_str` / `gl_scan_header`) needs the same treatment. IDE cannot be exercised until this runtime crash is resolved.

**Next:** Continue from Session 088: search for more sites that pass string variable addresses to the runtime, or re-run GDB on the new binary to get a fresh backtrace. See [IndividualProblems/2026-01-29_qb64pe-binary-crash-suspicious-length.md](IndividualProblems/2026-01-29_qb64pe-binary-crash-suspicious-length.md) for diagnosis steps and how to find the remaining bad call chain.

---

## Follow-up (same session): BYREF string + scalar rename fix

**Root cause:** Inside procedures with BYREF string parameters (e.g. `readchunk$(a$, last_character$)`), the codegen creates a local `QbString* a_str = *a_str_ref`, but variable renames (scalar/array dual namespace) map `a_str` → `a_str_scalar`. Expression and assignment emission then used `a_str_scalar` (a **global**), so `qb_len_str(a_str_scalar)` and the assignment block touched the wrong variable; the local `a_str` was never updated and could hold a bad value.

**Fix:**
1. **expr.rs:** Added `byref_string_names: &[String]` to `emit_expr` / `emit_expr_internal`. For `Variable`, do not apply `variable_renames` when `c_name` is in `byref_string_names`, so we emit the local name (e.g. `a_str`) not the global scalar name (e.g. `a_str_scalar`).
2. **assignments.rs:** When the assignment target is a BYREF string param (name in `current_func_byref_strings`), use `c_identifier(name)` and do not apply `variable_renames`.
3. **stmt/mod.rs:** Pass `&self.procedure.current_func_byref_strings` into `emit_expr` / `emit_expr_external`.
4. **analysis.rs:** Pass `&[] as &[String]` for `byref_string_names` when emitting CONST (no procedure context).

**Verified:** Regenerated `qb64pe_fresh.c`; `qb_readchunk_str` now uses `a_str` in the assignment block and in `qb_len_str(a_str)` (lines 34640, 34642). The previous "suspicious length" / bad-pointer path in readchunk is fixed. The binary still aborts (exit 134); remaining crash may be in another procedure or a different bug—further GDB/analysis needed.

---

## Follow-up: Window flashes then disappears (IDE running in System Monitor)

**Symptom:** User saw the IDE window flash for about one second then disappear; process still running in System Monitor.

**Cause:** The IDE calls `_SCREENSHOW` once at startup (line 462 in qb64pe.bas), then shortly after (inside the IDE main loop init, e.g. when setting up menus) it calls `_SCREENHIDE`. That first hide makes the window disappear and it is not shown again until the IDE explicitly calls `_SCREENSHOW` (e.g. when closing a dialog). During normal startup there is no second `_SCREENSHOW`, so the window stayed hidden.

**Fix:** In `runtime/src/graphics_ffi.rs`:
- Added `SUPPRESS_NEXT_SCREENHIDE` static. When `qb_screenshow()` auto-initializes graphics (no prior SCREEN), we set it to `true`.
- In `qb_screenhide()`, if the flag is set we clear it and return without hiding, so the first hide after auto-show is ignored and the window stays visible.

**Verification:** Rebuild runtime and QB64pe binary; run IDE. Window should remain visible after the initial show.
