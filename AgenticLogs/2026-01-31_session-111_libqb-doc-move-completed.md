# Session 111 — Move completed items from LIBQB_FUNCTIONALITY to COMPLETED

**Date:** 2026-01-31

## Summary

Moved all completed (🟢/✅) items from `docs/QB64pe/LIBQB_FUNCTIONALITY.md` into `docs/QB64pe/LIBQB_FUNCTIONALITY_COMPLETED.md` and replaced them in the main doc with references.

## Changes

- **§1 Main libqb.h:** Replaced full table with link to COMPLETED §1.
- **§24 qblist.h:** Replaced table with link to COMPLETED §24.
- **§34 condvar.h:** Replaced table with link to COMPLETED §34; added §34 section to COMPLETED (was missing).
- **§36 mutex.h:** Replaced table with link to COMPLETED §36.
- **§38 game_controller.h:** Replaced table with link to COMPLETED §38.
- **§41 Parts:** Removed completed rows (gui, os) from table; added one-line “Completed” reference; added gui and os to COMPLETED §41 table (with input, network).

LIBQB_FUNCTIONALITY.md now lists only partial (🟡) and not implemented (🔴) items where applicable; completed sections are referenced to LIBQB_FUNCTIONALITY_COMPLETED.md.
