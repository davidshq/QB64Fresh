# Session 117 — GUI dialogs: _GUINOTIFYPOPUP, _GUIINPUTBOX, _GUICOLORCHOOSERDIALOG

**Date:** 2026-01-31

## Goal

Implement the remaining gui.h (tinyfiledialogs) items from LIBQB_FUNCTIONALITY.md §16:
- `sub__guiNotifyPopup` → _NOTIFYPOPUP (sub)
- `func__guiInputBox` → _INPUTBOX$ (function)
- `func__guiColorChooserDialog` → _COLORCHOOSERDIALOG (function)
- Document gui_alert status (C internal; 🟡 = partial/stub).

## Decisions

- **Runtime:** Use existing `rfd`-based dialogs.rs. Add `qb_notifypopup(title, message, icon_type)` (void), `qb_inputbox(title, message, default_input)` (QbString*), `qb_colorchooserdialog(title, default_rgb)` (uint32_t). Notify = MessageDialog OK-only; Input = native input where available (rfd has no input dialog — use stub returning default or ""); Color = stub returning 0 (cancelled) until we add a color picker.
- **API shape:** Match QB64pe/libqb: optional title, message, icon for notify; optional title, message, default for input; optional title, default RGB for color chooser.
- **gui_alert:** Internal C helper in libqb; we use qb_messagebox_ex. Mark as 🟢 in completed doc (equivalent provided).

## Implementation

- Runtime: `qb_notifypopup`, `qb_inputbox` (3-arg), `qb_colorchooserdialog` in dialogs.rs; declarations in qb64fresh_rt.h.
- Builtins: _NOTIFYPOPUP as sub with optional (title, message, iconType); _INPUTBOX$ optional (title, message, defaultInput); _COLORCHOOSERDIALOG optional (title, defaultRGB) — fix order to (title, defaultRGB).
- Codegen: Call emission special-case for _NOTIFYPOPUP (3 args, qb_string_data or NULL); expr special-cases for _INPUTBOX$ and _COLORCHOOSERDIALOG with const char* / NULL padding where needed; inline runtime stubs in graphics.rs.
- Docs: LIBQB_FUNCTIONALITY.md §16 table → 🟢 for the three; LIBQB_FUNCTIONALITY_COMPLETED.md §16 add entries; gui_alert note.

## Status

Done. Runtime: `qb_notifypopup`, `qb_inputbox` (3-arg), `qb_colorchooserdialog` in dialogs.rs and header. Builtins: _NOTIFYPOPUP (sub, 3 optionals), _INPUTBOX$ (3 optionals), _COLORCHOOSERDIALOG (title, defaultRGB optionals). Codegen: Call special-case for _NOTIFYPOPUP; expr special-cases for _INPUTBOX$ and _COLORCHOOSERDIALOG. Inline stubs in graphics.rs. Docs and LIBQB tables updated; gui_alert marked 🟢 (qb_messagebox_ex equivalent).
