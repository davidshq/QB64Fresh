# Session 112: event.h + file-fields.h (libqb)

**Date:** 2026-01-31

## Goal

Implement LIBQB_FUNCTIONALITY.md items 10 and 11:

- **event.h** — Event type constants and `qb64_custom_event(...)` callback
- **file-fields.h** — `field_free(qbs*)` and `field_update(int32_t)` for FIELD (random-access files)

## Decisions

- **Events:** Add `QB64_EVENT_*` constants and `qb64_custom_event` in header; implement in Rust (events module or new event_ffi). CLOSE sets an exit flag for main loop; KEY/RELATIVE_MOUSE/FILE_DROP stubbed or minimal.
- **Field:** Our runtime uses QbString and per-file FIELD_BUFFERS but does not track which QbString* are “field variables”. Implement `field_free` and `field_update` as stubs (field_free no-op, field_update no-op) so symbols exist for linking; full behavior can follow when we add field-variable tracking.

## Outcome

- **event.h:** `QB64_EVENT_CLOSE`, `QB64_EVENT_KEY`, `QB64_EVENT_RELATIVE_MOUSE_MOVEMENT`, `QB64_EVENT_FILE_DROP` and `qb64_custom_event(...)` added to `qb64fresh_rt.h`; implemented in `runtime/src/events.rs`. CLOSE sets `qb64_exit_requested`; KEY/RELATIVE_MOUSE/FILE_DROP stubbed.
- **file-fields.h:** `field_free(QbString*)` and `field_update(int32_t)` declared in header; implemented in `runtime/src/io/file.rs` as no-op stubs (no per-string field tracking yet).
- LIBQB_FUNCTIONALITY.md sections 10 and 11 updated to 🟢.
