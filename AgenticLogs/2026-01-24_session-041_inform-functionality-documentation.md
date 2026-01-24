# Session 041: InForm Functionality Documentation

**Date:** 2026-01-24
**Focus:** Document all functionality of InForm (WYSIWYG UI designer for QB64) as used with QB64-PE

## Summary

Created `docs/INFORM_FUNCTIONALITY.md` documenting InForm's designer (UiEditor), control types, events, runtime API, theming, and project requirements. InForm is **not** part of the QB64pe repository; it is an external project (FellippeHeitor/InForm, QB64-Phoenix-Edition/InForm-PE).

## Decisions

- **Scope:** Document InForm as found in InForm and InForm-PE upstream sources (InForm.bi, InFormCommon.bi, InForm.ui, UiEditor.bas/frm). QB64pe itself contains no InForm code.
- **Structure:** Overview → Designer (menus, toolbox, property grid, color mixer, Z-order, bind dialog, preview, save/load) → Control types → Events → Runtime (data, creation/loading, property APIs, drawing/input, theming, code pages) → Project requirements → References.

## What Was Documented

### Designer (UiEditor)

- **Menus:** File (New, Open, Save, Save As, Recent, Exit), Edit (Undo/Redo, Cut/Copy/Paste/Delete, Select All, CP437/CP1252, Convert type, Set default button, Restore image dimensions, Bind controls, Allow Min/Max, Z-Ordering), View (Preview detach, Position/size, Invisible controls, Preview, Loaded fonts), Insert (MenuBar, ContextMenu, MenuItem), Align (Left, Right, Tops, Bottoms, Center V/H, Distribute V/H), Options (Snap lines, Auto-name, Swap buttons, Save form only), Help.
- **Toolbox:** Button, Label, TextBox, NumericBox, CheckBox, RadioButton, ListBox, DropdownList, TrackBar, ProgressBar, PictureBox, Frame, ToggleSwitch.
- **Property grid:** Name, Caption, Text, Mask, Top/Left/Width/Height, Font, Tooltip, Value, Min/Max, Interval, MinInterval, Padding, Align, VAlign, Bullet, Boolean, Context menu, Keyboard combo; per-control toggles (Stretch, HasBorder, ShowPercentage, Password, WordWrap, CanHaveFocus, Disabled, Transparent, Hidden, Centered, Resizable, AutoScroll, AutoSize, HideTicks, AutoPlayGif, GIF extension).
- **Color mixer:** Fore, Back, SelectedFore, SelectedBack, Border; RGB sliders.
- **Z-Order dialog:** list + Up/Down. **Bind dialog:** source/target controls and properties.
- **Preview:** UiEditorPreview over TCP; optional attach to editor.
- **Save:** .frm (form def) + .bas (event stubs); "Save form only" option.

### Control Types

Form, Frame, Button, Label, CheckBox, RadioButton, TextBox, ProgressBar, ListBox, DropdownList, MenuBar, MenuItem, MenuPanel, PictureBox, TrackBar, ContextMenu, Font, ToggleSwitch; NumericTextBox (NumericBox) as TextBox with NumericOnly/bounds.

### Events

Per-control: Click, MouseEnter, MouseLeave, FocusIn, FocusOut, MouseDown, MouseUp, KeyPress, TextChanged, ValueChanged. Lifecycle: BeforeInit, OnLoad, BeforeUpdateDisplay, BeforeUnload, FormResized.

### Runtime

`__UI_ControlTYPE`, `Control(i)`, `Caption`/`Text`/`Mask`/`ToolTip` arrays; `__UI_NewControl`, `__UI_GetID`, `__UI_LoadForm`; `SetCaption`, `LoadImage`, `AddItem`/`ResetList`/`GetItem`/`SelectItem`/`ReplaceItem`/`RemoveItem`, `SetFont`, `RegisterKeyCombo`, `__UI_Bind`/`__UI_UnBind`/`__UI_CheckBinding`, `MessageBox`, `SetFrameRate`, `SetFocus`; `__UI_Draw*`, `__UI_ProcessInput`, `__UI_EventDispatcher`, `__UI_UpdateDisplay`, `__UI_DoEvents`; theming (`xp.uitheme`, `__UI_ThemeSetup`, `__UI_LoadThemeImage`); tooltips, menus; `RestoreCHR`, UTF/code-page handling.

### Project Requirements

Core: InForm.bi, InForm.ui, InFormCommon.bi, InFormVersion.bi, xp.uitheme, extensions/*. Extensions: GIFPlay, HashTable, Pathname, Ini, FontMgr. Generated: .frm, .bas.

## Files Created/Modified

| File | Action |
|------|--------|
| `docs/INFORM_FUNCTIONALITY.md` | Created (~400 lines) |
| `docs/ARCHITECTURE.md` | Added Related Documents link to INFORM_FUNCTIONALITY.md |

## References

- InForm: https://github.com/FellippeHeitor/InForm
- InForm-PE: https://github.com/QB64-Phoenix-Edition/InForm-PE, https://github.com/a740g/InForm-PE
