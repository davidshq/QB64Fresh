# Session 105 — qbs.h String Ops (LIBQB_FUNCTIONALITY)

**Date:** 2026-01-30

## Goal

Implement the qbs.h — QB64 String Type and String Ops items from `docs/QB64pe/LIBQB_FUNCTIONALITY.md` (lines 89–99): `qbs_new_cmem`, `qbs_new_fixed`, `set_qbs_size`, temp list, and `qbs_cleanup`.

## Accomplished

1. **qb_string_new_cmem(int32_t size)**  
   - Added in `runtime/src/string.rs`.  
   - QB64pe `qbs_new_cmem` allocates in conventional memory (DBLOCK); we have no DBLOCK, so this allocates a normal zero-filled string of the given size.  
   - Declared in `runtime/include/qb64fresh_rt.h`.

2. **qb_string_new_fixed(const uint8_t* ptr, uint32_t size)**  
   - Added in `runtime/src/string.rs`.  
   - Copies the buffer into a new reference-counted string (caller can release).  
   - Declared in header.

3. **qb_string_set_size(QbString** target, int32_t newlength)**  
   - Added in `runtime/src/string.rs`.  
   - Sets `*target` to `qb_space(newlength)` and releases the previous string (vWatch/set_qbs_size equivalent).  
   - Declared in header.

4. **Temp list and qbs_cleanup**  
   - Already covered: generated code emits `_qbs_tmp_pool`, `_qbs_tmp_next`, overflow array, `qbs_tmp_register`, and `qbs_cleanup`.  
   - No runtime library changes; doc updated to describe the equivalent.

5. **LIBQB_FUNCTIONALITY.md**  
   - Section 3 (qbs.h) table updated: all five items marked with equivalents and status.

## Files touched

- `runtime/src/string.rs` — new: `qb_string_new_cmem`, `qb_string_new_fixed`, `qb_string_set_size`
- `runtime/include/qb64fresh_rt.h` — new declarations under “qbs.h equivalents”
- `docs/QB64pe/LIBQB_FUNCTIONALITY.md` — section 3 table and structure note

## Verification

- `cargo build --release` in `runtime/` succeeds.
