# Session 118 — libqb §3 qbs.h and §14 gfs.h doc expansion

**Date:** 2026-01-31

## Summary

Expanded **§3 qbs.h** and **§14 gfs.h** in `docs/QB64pe/LIBQB_FUNCTIONALITY.md` so they match the style of other sections (e.g. §16 gui.h): structure notes plus function/symbol tables with QB64Fresh status and links to the full completed lists.

## Changes

### §3 qbs.h — QB64 String Type and String Ops

- Kept: **Structures** note (QB64Fresh uses opaque `QbString*`; no `qbs`/`qbs_field` in API; use `qb_string_release()` when done).
- Added: Summary table mapping libqb symbols to QB64Fresh equivalents (`qb_string_*`, `qb_chr`, `qb_asc`, `qb_left`/`qb_right`/`qb_mid`, `qb_instr`, `qb_string_compare`, trim/LSET/RSET, cmem/fixed/size, temp pool).
- Link: Full list → LIBQB_FUNCTIONALITY_COMPLETED.md § 3.

### §14 gfs.h — Generic File System

- Kept: **Structures** note (`gfs_file_struct`; QB64Fresh uses opaque file handles; no struct in API).
- Added: Summary table mapping `gfs_*` to QB64Fresh API: `qb_freefile`, `qb_file_open`/`qb_file_close`/`qb_file_close_all`, `qb_loc`/`qb_seek`/`qb_file_seek`, `qb_lof`, `qb_file_get`/`qb_file_put`, `qb_eof`, `qb_file_lock`/`qb_file_unlock`, fileno-as-handle.
- Link: Full list → LIBQB_FUNCTIONALITY_COMPLETED.md § 14.

## Decisions

- Used actual QB64Fresh names in §14 (`qb_file_*`, `qb_freefile`, `qb_lof`, `qb_loc`, `qb_eof`, `qb_file_lock`/`qb_file_unlock`) rather than `qb_gfs_*`, since the runtime header exposes `qb_file_*` and `qb_*` only.
