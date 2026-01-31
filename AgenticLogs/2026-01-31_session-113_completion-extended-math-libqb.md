# Session 113: completion.h and extended_math power-of-2 (libqb)

**Date:** 2026-01-31

## Goal

Implement LIBQB_FUNCTIONALITY.md items 193–210:
- **32. completion.h** — one-shot completion (thread sync): `completion_init`, `completion_clear`, `completion_wait`, `completion_finish`
- **35. extended_math.h** — power-of-2: `Math_IsPowerOf2<T>`, `Math_RoundUpToPowerOf2<T>`, `Math_RoundDownToPowerOf2<T>`

## Done

1. **Completion (runtime)**
   - Added `runtime/src/completion.rs`: `struct Completion` (repr(C)) with `finished`, `mutex`, `var`; FFI `completion_init`, `completion_clear`, `completion_wait`, `completion_finish`.
   - Uses existing `libqb_mutex` and `libqb_condvar`; init allocates mutex+condvar, clear frees them, wait blocks until finished, finish sets flag and broadcasts.
   - Registered in `runtime/src/lib.rs` and declared in `runtime/include/qb64fresh_rt.h` (struct + four functions).

2. **Extended math power-of-2 (runtime)**
   - In `runtime/src/math.rs`: added `qb_math_is_power_of_2_u32/u64`, `qb_math_round_up_to_power_of_2_u32/u64`, `qb_math_round_down_to_power_of_2_u32/u64`. C API uses fixed-width types; logic matches QB64pe `extended_math.h` (bit-smear for round up/down).
   - Declared in `qb64fresh_rt.h` under “Power-of-2 (libqb extended_math.h)”.

3. **Docs**
   - `docs/QB64pe/LIBQB_FUNCTIONALITY.md`: section 32 (completion) and 35 (power-of-2) marked 🟢.
   - `docs/QB64pe/LIBQB_FUNCTIONALITY_COMPLETED.md`: added section 32 completion; added power-of-2 rows to section 35 with QB64Fresh symbol names.

## Note

- Runtime crate still fails to build due to a pre-existing error in `runtime/src/thread.rs` (`ThreadStartPayload` / `Send`). Completion and extended_math code compile; no changes made to thread.rs.
