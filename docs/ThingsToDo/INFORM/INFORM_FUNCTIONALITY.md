# InForm: WYSIWYG UI Designer and GUI Engine for QB64-PE

**Purpose:** Document the functionality of InForm as used with QB64-Phoenix-Edition (QB64-PE).  
**Audience:** QB64Fresh maintainers and anyone integrating or reimplementing UI tooling.

---

## 1. Overview

**InForm** is a WYSIWYG (What You See Is What You Get) UI designer and GUI runtime for QB64. It allows users to create forms with controls (buttons, labels, text boxes, etc.) visually and then run the resulting BASIC program with a full event-driven GUI.

### Relationship to QB64pe

- **InForm is NOT part of the QB64pe repository.** The QB64pe source tree contains no InForm, UiEditor, or `.frm` files.
- InForm is a **separate, external project** that targets QB64 (and QB64-PE) as the runtime.
- Users typically add InForm files to their project and `$INCLUDE` them.

### Upstream Projects

| Project | URL | Notes |
|--------|-----|-------|
| **InForm** (original) | https://github.com/FellippeHeitor/InForm | Original by Fellippe Heitor |
| **InForm-PE** | https://github.com/QB64-Phoenix-Edition/InForm-PE | QB64-Phoenix-Edition fork |
| **InForm-PE** (a740g) | https://github.com/a740g/InForm-PE | Alternative维护 |

### Components

1. **UiEditor** — The designer application. Create/arrange controls, set properties, save `.frm` + `.bas`.
2. **InForm runtime** — `InForm.ui`, `InFormCommon.bi`, and related modules. Load forms, dispatch events, draw controls.
3. **InForm.bi** — Setup and `$INCLUDE` orchestration for control types and shared definitions.

---

## 2. Designer (UiEditor)

The designer is the main WYSIWYG application (`UiEditor.bas` / `UiEditor.frm`). It provides menus, a toolbox, a property grid, and other tools.

### 2.1 Menus

#### File

| Item | Description |
|------|-------------|
| New | New form; clears current design. |
| Open | Open `.frm` form definition. |
| Save | Save current form (`.frm`) and generated event stubs (`.bas`). |
| Save As | Save with a new name/path. |
| Recent | List of recently opened files. |
| Exit | Quit UiEditor. |

#### Edit

| Item | Description |
|------|-------------|
| Undo | Undo last change. |
| Redo | Redo. |
| Cut | Cut selected control(s). |
| Copy | Copy selected control(s). |
| Paste | Paste. |
| Delete | Delete selected control(s). |
| Select All | Select all controls on the form. |
| **Encoding** | |
| CP437 / CP1252 | Switch encoding for loading/saving (code pages 437 and 1252). |
| **Convert type** | Change control type (where applicable). |
| Set default button | Mark selected button as form default (Enter key). |
| Restore image dimensions | Reset PictureBox (or similar) to image size. |
| **Dialogs** | |
| Bind controls | Open the control-binding dialog (source/target, properties). |
| Allow Min/Max | Form option: allow minimize/maximize. |
| Z-Ordering | Open Z-order dialog (raise/lower controls). |

#### View

| Item | Description |
|------|-------------|
| Preview detach | Detach preview window from editor. |
| Position/size | Toggle display of position/size for selected control. |
| Invisible controls | Toggle visibility of controls with `Hidden` in designer. |
| Preview | Run form preview (uses UiEditorPreview over TCP when available). |
| Loaded fonts | Show fonts loaded/used by the form. |

#### Insert

| Item | Description |
|------|-------------|
| MenuBar | Insert a top-level menu bar. |
| ContextMenu | Insert a context (right-click) menu. |
| MenuItem | Insert a menu item (under MenuBar or ContextMenu). |

#### Align

| Item | Description |
|------|-------------|
| Left | Align left edges of selected controls. |
| Right | Align right edges. |
| Tops | Align top edges. |
| Bottoms | Align bottom edges. |
| Center V / Center H | Center selected controls vertically or horizontally. |
| Distribute V / Distribute H | Distribute selected controls evenly vertically or horizontally. |

#### Options

| Item | Description |
|------|-------------|
| Snap lines | Toggle snap-to-guide when moving/resizing. |
| Auto-name | Auto-generate control names (e.g. `Button1`, `Label2`). |
| Swap buttons | Swap OK/Cancel (or similar) button order in dialogs. |
| Save form only | On Save, write only `.frm` (no `.bas` event stubs). |

#### Help

| Item | Description |
|------|-------------|
| Help | Open help or about for UiEditor/InForm. |

### 2.2 Toolbox

The toolbox provides control types that can be placed on the form:

| Control | Description |
|---------|-------------|
| **Button** | Push button; `Caption`, `Click` and optional `KeyPress`. |
| **Label** | Static text; `Caption`, optional `WordWrap`, alignment. |
| **TextBox** | Single-line or multi-line text; `Text`, `Password`, `Mask`, `TextChanged`. |
| **NumericBox** | TextBox with numeric-only input; `Value`, `Min`, `Max`. |
| **CheckBox** | Check box; `Caption`, `Value` (boolean), `ValueChanged`. |
| **RadioButton** | Radio in a group; `Caption`, `Value`, `ValueChanged`. |
| **ListBox** | List of items; `AddItem`, `GetItem`, `SelectItem`, `ValueChanged`. |
| **DropdownList** | Drop-down list; same list API, `ValueChanged`. |
| **TrackBar** | Slider; `Value`, `Min`, `Max`, `Interval`, `ValueChanged`. |
| **ProgressBar** | Progress; `Value`, `Min`, `Max`, optional `ShowPercentage`. |
| **PictureBox** | Image; `LoadImage`, `Stretch`, `AutoSize`, optional `AutoPlayGif`. |
| **Frame** | Container/group; `Caption`, groups of controls visually. |
| **ToggleSwitch** | On/off switch; `Value`, `ValueChanged`. |

### 2.3 Property Grid

The property grid shows and edits attributes of the selected control. Common and control-specific properties include:

#### Common

| Property | Description |
|----------|-------------|
| Name | Control identifier (e.g. `Button1`). |
| Caption | Button, Label, CheckBox, RadioButton, Frame, MenuItem. |
| Text | TextBox, NumericBox. |
| Mask | TextBox mask (e.g. for date/phone). |
| Top, Left, Width, Height | Position and size. |
| Font | Font name/size. |
| Tooltip | Tooltip text. |
| Value | Numeric or boolean value (TrackBar, ProgressBar, CheckBox, RadioButton, ToggleSwitch, NumericBox, ListBox, DropdownList). |
| Min, Max | Bounds for NumericBox, TrackBar, ProgressBar. |
| Interval | Step for TrackBar. |
| MinInterval | Minimum step. |
| Padding | Internal padding. |
| Align, VAlign | Horizontal/vertical alignment (e.g. `__UI_Left`, `__UI_Center`, `__UI_Right`). |
| Bullet | Bullet style for Label (or similar). |
| Boolean | For controls storing a boolean. |
| Context menu | Link to ContextMenu control. |
| Keyboard combo | `RegisterKeyCombo` (e.g. Ctrl+S). |

#### Per-control toggles

| Toggle | Applies to | Description |
|--------|------------|-------------|
| Stretch | PictureBox | Stretch image to control size. |
| HasBorder | Various | Draw border. |
| ShowPercentage | ProgressBar | Show `%` text. |
| Password | TextBox | Mask input. |
| WordWrap | Label, TextBox | Wrap text. |
| CanHaveFocus | Any | Include in tab order. |
| Disabled | Any | Grayed out, no input. |
| Transparent | Label, etc. | Transparent background. |
| Hidden | Any | Not visible at runtime. |
| Centered | Text in controls | Center caption/text. |
| Resizable | Form | Form can be resized by user. |
| AutoScroll | ListBox, etc. | Show scrollbars when needed. |
| AutoSize | Label, PictureBox | Size to content. |
| HideTicks | TrackBar | Hide tick marks. |
| AutoPlayGif | PictureBox | Animate GIF. |
| GIF extension | PictureBox | Use GIF extension (e.g. GIFPlay). |

### 2.4 Color Mixer

- **Fore, Back, SelectedFore, SelectedBack, Border** — Set per-control (or form) colors.
- RGB sliders (and often a hex or similar input) to adjust each color.

### 2.5 Z-Order Dialog

- Lists controls in front-to-back order.
- **Up** / **Down** (or Raise / Lower) to change stacking. Determines draw order and hit-testing.

### 2.6 Control Binding Dialog

- **Source** control and **Source property** (e.g. `Label1.Caption`).
- **Target** control and **Target property** (e.g. `TextBox1.Text`).
- At runtime, `__UI_Bind` / `__UI_CheckBinding` keep target updated from source (one-way or as designed).

### 2.7 Preview

- **UiEditorPreview** — A separate preview process that receives the form (or its description) over **TCP** and renders it.
- Optional **attach to editor** so the preview stays in sync with the editor (e.g. live layout or refresh on change).

### 2.8 Save / Load

- **Save** produces:
  - **`.frm`** — Form definition (controls, positions, sizes, properties, Z-order).
  - **`.bas`** — BASIC file with event stubs (e.g. `Button1_Click`, `Form1_Load`) for the programmer to fill.
- **Save form only** (Options): only `.frm` is written.
- **Load**: reads `.frm` (and optionally loads the corresponding `.bas` for reference).
- **Recent files**: paths stored for File → Recent.

### 2.9 Encodings and Fonts

- **CP437, CP1252** — Affects how `.frm` and sometimes `.bas` are read/written.
- **Fonts** — Form and controls can specify a font; “Loaded fonts” view shows what is used. `SetFont` at runtime changes the active font for a control.

---

## 3. Control Types

From `InForm.bi` and `InFormCommon.bi`, the `__UI_ControlTYPE` (or equivalent) and type constants define the control set:

| Type constant | Control | Notes |
|---------------|---------|-------|
| `__UI_Type_Form` | Form | Root window; `Caption`, `Resizable`, `CanResize`, `AllowMinMax`. |
| `__UI_Type_Frame` | Frame | Group box; `Caption`. |
| `__UI_Type_Button` | Button | `Caption`, default button, `Click`. |
| `__UI_Type_Label` | Label | `Caption`, `WordWrap`, `AutoSize`, alignment. |
| `__UI_Type_CheckBox` | CheckBox | `Caption`, `Value` (bool). |
| `__UI_Type_RadioButton` | RadioButton | `Caption`, `Value`; grouped by parent. |
| `__UI_Type_TextBox` | TextBox | `Text`, `Mask`, `Password`, `WordWrap`. |
| `__UI_Type_ProgressBar` | ProgressBar | `Value`, `Min`, `Max`, `ShowPercentage`. |
| `__UI_Type_ListBox` | ListBox | `AddItem`, `GetItem`, `SelectItem`, `ResetList`, etc. |
| `__UI_Type_DropdownList` | DropdownList | Same list API, dropdown UI. |
| `__UI_Type_MenuBar` | MenuBar | Top-level menu strip. |
| `__UI_Type_MenuItem` | MenuItem | Item under MenuBar or ContextMenu. |
| `__UI_Type_MenuPanel` | MenuPanel | Internal for menu layout. |
| `__UI_Type_PictureBox` | PictureBox | `LoadImage`, `Stretch`, `AutoSize`, GIF. |
| `__UI_Type_TrackBar` | TrackBar | `Value`, `Min`, `Max`, `Interval`, `HideTicks`. |
| `__UI_Type_ContextMenu` | ContextMenu | Right-click menu. |
| `__UI_Type_Font` | Font | Font resource (name/size). |
| `__UI_Type_ToggleSwitch` | ToggleSwitch | On/off, `Value`. |

**NumericTextBox** — Implemented as a TextBox with `NumericOnly` (and optional `__UI_NumericWithBounds` for Min/Max). In the designer it is the “NumericBox” toolbox item.

**Alignment constants** (e.g. for `Align`, `VAlign`, or text): `__UI_Left`, `__UI_Center`, `__UI_Right`, and vertical equivalents.

---

## 4. Events

### 4.1 Per-control events

| Event | When | Typical parameters/usage |
|-------|------|---------------------------|
| **Click** | Control is clicked. | Button, Label, CheckBox, etc. |
| **MouseEnter** | Mouse enters control bounds. | Any. |
| **MouseLeave** | Mouse leaves control bounds. | Any. |
| **FocusIn** | Control gains focus. | Focusable controls. |
| **FocusOut** | Control loses focus. | Focusable controls. |
| **MouseDown** | Mouse button down on control. | Button, x, y. |
| **MouseUp** | Mouse button up on control. | Button, x, y. |
| **KeyPress** | Key pressed while control has focus. | Key code, modifiers. |
| **TextChanged** | TextBox (or NumericBox) text changed. | After edit. |
| **ValueChanged** | Value changed by user. | ListBox, DropdownList, TrackBar, CheckBox, RadioButton, ToggleSwitch, NumericBox. |

### 4.2 Form / lifecycle events

| Event | When |
|-------|------|
| **BeforeInit** | Before form and controls are initialized. |
| **OnLoad** | After form and controls are loaded and shown. |
| **BeforeUpdateDisplay** | Before a paint (for custom drawing or updates). |
| **BeforeUnload** | Before form is unloaded/closed. |
| **FormResized** | Form size changed (if `CanResize` / `Resizable`). |

Event handlers are generated as stubs in the `.bas` file (e.g. `Form1_OnLoad`, `Button1_Click`). The runtime `__UI_EventDispatcher` maps input and internal state to these callbacks.

---

## 5. Runtime

### 5.1 Data structures

- **`__UI_ControlTYPE`** — Base type (or equivalent) for each control: index, type, `Left`, `Top`, `Width`, `Height`, and other common fields.
- **`Control(i)`** — Array (or equivalent) of controls; `i` is the control ID.
- **`Caption`, `Text`, `Mask`, `ToolTip`** — Arrays (or fields) keyed by control ID.

### 5.2 Creation and loading

| Routine | Description |
|---------|-------------|
| `__UI_NewControl` | Create a new control of a given type; returns or sets ID. |
| `__UI_GetID` | Resolve control by name to ID. |
| `__UI_LoadForm` | Load form from `.frm` (or equivalent) and create all controls. |

### 5.3 Property and content APIs

| Routine | Description |
|---------|-------------|
| `SetCaption` | Set `Caption` for Button, Label, Frame, MenuItem, etc. |
| `LoadImage` | Load image into PictureBox. |
| `AddItem` | Add item to ListBox or DropdownList. |
| `ResetList` | Clear all items. |
| `GetItem` | Get item text by index. |
| `SelectItem` | Set selected index. |
| `ReplaceItem` | Replace item at index. |
| `RemoveItem` | Remove item at index. |
| `SetFont` | Set font for a control (or form). |
| `RegisterKeyCombo` | Associate a key combo with a control or action. |
| `__UI_Bind` | Bind source control/property → target control/property. |
| `__UI_UnBind` | Remove binding. |
| `__UI_CheckBinding` | Update targets from sources (called from main loop). |
| `MessageBox` | Modal message box; `MsgBox_*` style constants. |
| `SetFrameRate` | Set main loop rate (for `__UI_DoEvents` / `__UI_UpdateDisplay`). |
| `SetFocus` | Set keyboard focus to a control. |

### 5.4 Drawing and input

| Routine | Description |
|---------|-------------|
| `__UI_Draw*` | Per-type drawing (e.g. `__UI_DrawButton`, `__UI_DrawLabel`). |
| `__UI_ProcessInput` | Process mouse/keyboard into internal state. |
| `__UI_EventDispatcher` | Map state to Click, ValueChanged, KeyPress, etc., and call stubs. |
| `__UI_UpdateDisplay` | Redraw form and controls. |
| `__UI_DoEvents` | Process input, dispatch events, update display; main loop body. |

### 5.5 Tooltips and menus

- **Tooltips** — Shown when hovering over a control with `ToolTip` set; timing and positioning handled in the runtime.
- **Menus** — `__UI_InternalMenus` (or equivalent) for MenuBar and ContextMenu; `MenuItem` click triggers the corresponding event.

### 5.6 Theming

- **`xp.uitheme`** — Theme file (images, colors) used for controls.
- **`__UI_ThemeSetup`** — Load and apply theme.
- **`__UI_LoadThemeImage`** — Load a theme image by ID (e.g. for buttons, scrollbars).

### 5.7 Code pages and strings

- **`RestoreCHR`** — Restore code page (e.g. after `CHR$`/encoding changes).
- **UTF / code page** — Runtime may handle UTF or code-page conversions for `Caption`, `Text`, and `ToolTip` depending on build and options.

---

## 6. Project Requirements

To use InForm with a QB64-PE project, the following are typically required.

### 6.1 Core files to ship or `$INCLUDE`

| File | Role |
|------|------|
| `InForm/InForm.bi` | Main include; control-type setup and orchestration. |
| `InForm/InForm.ui` | Runtime: controls, events, `__UI_LoadForm`, `__UI_DoEvents`, drawing, bindings. |
| `InForm/InFormCommon.bi` | `__UI_ControlTYPE`, constants (`__UI_Type_*`, `__UI_Left`/`Center`/`Right`, `MsgBox_*`), theme image IDs, resize/encoding helpers. |
| `InForm/InFormVersion.bi` | Version info. |
| `InForm/xp.uitheme` | Default theme (bitmaps, etc.). |
| `InForm/extensions/*` | Optional extensions (e.g. GIFPlay, HashTable, Pathname, Ini, FontMgr). |

### 6.2 Extensions (examples)

- **GIFPlay** — GIF playback in PictureBox (`AutoPlayGif`, GIF extension).
- **HashTable, Pathname, Ini, FontMgr** — Additional libraries; some UiEditor/InForm setups expect them in `InForm/extensions/` or project paths.

### 6.3 Generated files

- **`.frm`** — Form layout and properties; read by `__UI_LoadForm`.
- **`.bas`** — Event stubs and `$INCLUDE` of InForm; user fills stubs and optionally adds more `.bas`.

---

## 7. Comparison with Visual Basic and Similar Classic Tools

InForm reaches a **VB-like** level of visual development in several areas. Compared to **Visual Basic (VB3–VB6)**, **Delphi**, and tools such as **PowerBuilder** or **FoxPro**, the following holds.

### 7.1 Where InForm Is Comparable

| Area | InForm | VB / Delphi-style |
|------|--------|-------------------|
| **Form designer** | Drag-drop toolbox, property grid, multi-select | Same idea: palette, properties, multi-select |
| **Alignment** | Align (Left, Right, Tops, Bottoms, Center V/H), Distribute V/H | Format → Align, Format → Make Same Size, Format → Horizontal/Vertical Spacing |
| **Standard controls** | Button, Label, TextBox, ListBox, Dropdown, CheckBox, Radio, Frame, PictureBox, ProgressBar, TrackBar, MenuBar, ContextMenu, ToggleSwitch, NumericBox | CommandButton, Label, TextBox, ListBox, ComboBox, CheckBox, OptionButton, Frame, PictureBox, HScrollBar/VScrollBar, Menu editor; Delphi adds more (TreeView, ListView, etc.) |
| **Events** | Click, MouseEnter/Leave, FocusIn/Out, MouseDown/Up, KeyPress, TextChanged, ValueChanged; Form OnLoad, BeforeUnload, FormResized | Click, MouseMove, GotFocus/LostFocus, KeyDown/Up/Press, Change; Form_Load, Form_Unload, Form_Resize |
| **Persistence** | .frm (layout and properties) + .bas (event stubs) | .frm (and .frx for binary) + .bas/.cls; Delphi .dfm + .pas |
| **Clipboard** | Cut, Copy, Paste, Delete | Same |
| **Undo/Redo** | Yes | VB had limited undo in designer; Delphi had fuller support |
| **Snap** | Snap lines, guides | Snap to grid, align to other controls |
| **Z-order** | Z-Order dialog (raise/lower) | Bring to Front / Send to Back |
| **Binding** | Control-to-control (`__UI_Bind` source property → target property) | Data control / ADODC → bound controls; Delphi TDataSource and data-aware controls |

So for **single-form layout, common controls, event stubs, and alignment/binding**, InForm is in the same family as classic form designers.

### 7.2 Where Classic VB / Delphi Went Further

| Area | InForm | VB / Delphi |
|------|--------|-------------|
| **Rendering** | Custom-drawn controls; theme file (`xp.uitheme`). Look is consistent but not native. | **Native OS controls** (Windows Common Controls, etc.). Native look, accessibility, and system behavior (e.g. IME, focus rings). |
| **Project model** | Form-centric: .frm + .bas; designer handles one form. | **Project file** (.vbp / .dpr): multiple forms, modules, class modules, references. Project Explorer, add/remove forms. |
| **MDI** | Single-form focus; no MDI parent/child in the documented feature set. | **MDI**: MDIForm, child forms, Window menu, tile/cascade. |
| **Toolbox breadth** | No Timer, Drive/Dir/File listboxes, Shape, Line; no OLE or Data control. | **Timer**, **DriveListBox**, **DirListBox**, **FileListBox**, **Shape**, **Line**; **OLE** container; **Data** (or MSRDC) and later **ADODC** for DB. Delphi: **TTreeView**, **TListView**, **TStringGrid**, **TDBGrid**, many data and non-visual components. |
| **Data binding** | `__UI_Bind` only: one control’s property → another’s. No database. | **Data-aware** controls bound to Data, ADODC, or (in Delphi) TDataSource; master–detail, navigation. |
| **Custom controls** | Extensions (GIFPlay, HashTable, etc.); no component/OCX model. | **VBX**, then **OCX**; Delphi **VCL components** and packages. Large third-party market. |
| **IDE** | **UiEditor** = designer only. User edits .bas elsewhere (e.g. in QB64 IDE or text editor), runs via QB64. | **Single IDE**: form designer, code editor, debugger, project, object browser. Design and run in one place. |
| **Design surface** | **Preview** via UiEditorPreview over TCP; optional “attach to editor.” Form is not the live design surface inside the main editor. | Form *is* the design surface: you see and interact with the real form at design time (with run vs. design mode). |
| **Debugging** | None in UiEditor; debugging is whatever QB64 provides. | **Breakpoints**, **Step**, **Watch**, **Immediate**; design-time and run-time in one process. |
| **Build and deploy** | Output: .frm + .bas; user compiles with QB64 to get an executable. | **Compile to EXE**; **Package and Deployment Wizard** (or equivalent) for installers and dependencies. |

### 7.3 Summary

- **InForm** is closest to **early–mid VB** (VB3–VB4) in scope: one form, standard controls, event-driven code, property grid, alignment, and a form- and code-file workflow. The addition of **control binding**, **Z-order**, **undo/redo**, **menus**, and **preview** brings it up toward **VB5–VB6** in terms of designer features for that single-form slice.
- The main gaps are:
  - **Native widgets** (InForm draws everything itself),
  - **Project and MDI** (multi-form, project tree),
  - **Data-bound and database controls**,
  - **Wider control set** (Timer, file/dir lists, Shape/Line, OLE, data controls),
  - **One-place IDE** (designer + editor + debug + project),
  - **Integrated design surface** (form-as-design-surface instead of a separate preview),
  - **Deploy tooling** (compile + package).

Within the constraints of **QB64 and a community-built UI layer**, InForm delivers a **workable, VB-style form designer** for dialogs and single-form applications. It does not match the full height of **VB6 or Delphi** (native controls, full IDE, data binding, component ecosystem, deploy), but it occupies a similar niche to **VB3/VB4**-era form design, with some more modern touches (e.g. binding, theming, TCP preview).

---

## 8. References

- **InForm (original):** https://github.com/FellippeHeitor/InForm  
- **InForm-PE (QB64-Phoenix-Edition):** https://github.com/QB64-Phoenix-Edition/InForm-PE  
- **InForm-PE (a740g):** https://github.com/a740g/InForm-PE  

For the most up-to-date list of controls, events, and APIs, consult the `InForm.bi`, `InFormCommon.bi`, `InForm.ui`, and `UiEditor.bas` sources in the chosen InForm or InForm-PE repository.

- [INFORM_EXPERT_DISCUSSION.md](INFORM_EXPERT_DISCUSSION.md) — Expert roundtable on an enhanced InForm: usefulness, priorities, classic vs. modern, AI, and consensus.
