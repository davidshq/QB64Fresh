# Session 116 — libqb §3 qbs.h and §14 gfs.h doc updates

**Date:** 2026-01-31

## Summary

Updated `docs/QB64pe/LIBQB_FUNCTIONALITY.md` and `LIBQB_FUNCTIONALITY_COMPLETED.md` for the items in lines 66–83 (qbs.h partials and gfs.h section).

## Changes

### §3 qbs.h (lines 66–76)

- **LIBQB_FUNCTIONALITY.md:** Removed the 🟡 table for `qbs_tmp_list`/`qbs_tmp_list_nexti`, `qbs_cleanup`, and `qbs_cmem_sp`. Replaced with a single sentence that these are covered in COMPLETED (inline temp pool, generated cleanup; no cmem string pool in same form).
- **LIBQB_FUNCTIONALITY_COMPLETED.md:** Added a row for `qbs_cmem_sp`: 🟢 N/A — no cmem string pool in same form; cmem via `qb_string_new_cmem`.

### §14 gfs.h (lines 80–83)

- **LIBQB_FUNCTIONALITY.md:** Added “→ Completed items: LIBQB_FUNCTIONALITY_COMPLETED.md § 14” so §14 is clearly marked as completed (GFS is already fully documented in COMPLETED).

### Summary section

- Removed “qbs temp/cleanup/cmem_sp (partial)” from the “Remaining gaps” list, since those are now documented as completed/N/A in COMPLETED.

## Files touched

- `docs/QB64pe/LIBQB_FUNCTIONALITY.md`
- `docs/QB64pe/LIBQB_FUNCTIONALITY_COMPLETED.md`
