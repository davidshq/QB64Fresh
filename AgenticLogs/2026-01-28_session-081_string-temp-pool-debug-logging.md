# Session 081: String Temp Pool Overflow — Debug Logging

**Date:** 2026-01-28

## Summary

Implemented debug logging for the rare case when both the string temp pool and overflow list are full, so the path is diagnosable when it occurs.

## Changes

- **Location:** `src/codegen/c_backend/runtime/strings.rs`
- **Fix:** When the overflow list is also full, emit C that checks `getenv("QB64FRESH_DEBUG_STRING_POOL")` and, if set, writes a warning to stderr. Removed the TODO comment.
- **Behavior:** No change by default. Set `QB64FRESH_DEBUG_STRING_POOL` (any non-empty value) at runtime to get a stderr message if this path is ever taken.

## Documentation updates

- **FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md** — Item 6 marked done; Current/Fix updated to describe implementation and link to DEBUGGING.md and `strings.rs`.
- **DEBUGGING.md** — Added "Other runtime environment variables" noting `QB64FRESH_DEBUG_STRING_POOL` and its purpose.
- **PARTIAL_IMPLEMENTATIONS.md** — strings.rs line updated to mention `QB64FRESH_DEBUG_STRING_POOL` for overflow warning.

## Reference

- **Source:** `docs/ThingsToDo/FIXES_NEEDED_FROM_PROBLEMATIC_ITEMS.md` §6 (String Temp Pool Overflow — Debug Logging).
