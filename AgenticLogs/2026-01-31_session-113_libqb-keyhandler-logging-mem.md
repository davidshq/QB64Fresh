# Session 113 — libqb keyhandler, logging, mem (LIBQB_FUNCTIONALITY §21–23)

**Date:** 2026-01-31

## Summary

Implemented and documented libqb compatibility for **keyhandler.h**, **logging.h**, and **mem.h** per `docs/QB64pe/LIBQB_FUNCTIONALITY.md` lines 111–135.

## Changes

### 21. keyhandler.h — Keyboard State

- **runtime/include/qb64fresh_rt.h**
  - Added key code bases: `QBK` (200000), `VK` (100000), `UC` (1073741824).
  - Added `QBVK_*` virtual key defines (QBVK_ESCAPE, QBVK_F1–F12, arrows, KP*, modifiers, etc.) matching libqb subset.
  - Added `KMOD_*` modifier defines (KMOD_NONE, KMOD_LSHIFT/RSHIFT, KMOD_LCTRL/RCTRL, KMOD_ALT/META, KMOD_CTRL/SHIFT/ALT/META composites).
- **Docs**
  - LIBQB_FUNCTIONALITY.md: Table updated — keydown_vk/keyup_vk 🟡 (stub); QBK/VK/UC and QBVK_*/KMOD_* 🟢.
  - LIBQB_FUNCTIONALITY_COMPLETED.md: §21 expanded with QBK/VK/UC, QBVK_*, KMOD_*, and keydown_vk/keyup_vk as stub.

### 22. logging.h — Scoped Logging

- QB64Fresh already has full logging (loglevel/logscope, libqb_log_*, qb_logtrace/info/warn/error, qb_logminlevel).
- LIBQB_FUNCTIONALITY.md: Added table for loglevel/logscope, libqb_log_*, sub__log*, func__logminlevel, macros → all 🟢.
- LIBQB_FUNCTIONALITY_COMPLETED.md: §22 expanded with full logging table.

### 23. mem.h — _MEM (Safe Memory Blocks)

- QB64Fresh uses `qb_mem` (no lock_id/lock_offset); libqb lock API is internal.
- LIBQB_FUNCTIONALITY.md: §23 rewritten — structures 🟡 (qb_mem, no lock structs); new_mem_lock/free_mem_lock and lock globals 🟡 N/A; _MEM* API 🟢 (qb_memexists, qb_memfill, qb_memget, qb_mem_of, qb_memnew, qb_memfree, qb_memcopy).
- LIBQB_FUNCTIONALITY_COMPLETED.md: §23 note added; table kept with qb_* equivalents.

## Decisions

- Key simulate (`keydown_vk`/`keyup_vk`) remains stub (no-op); implementing would require platform-specific key injection (e.g. SendInput on Windows, XTest on Linux) and is out of scope for this pass.
- Key constants added as `#define` in header so generated C and user code can use same values as libqb for _KEYHIT/_KEYDOWN.
