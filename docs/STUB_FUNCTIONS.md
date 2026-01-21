# QB64Fresh Runtime Functions Reference

This document lists functions that have **stub implementations in the inline runtime** but are **fully implemented in the external runtime library**.

## Understanding the Two Runtime Modes

QB64Fresh supports two runtime modes:

| Mode | Flag | Use Case | Dependencies |
|------|------|----------|--------------|
| **Inline** (default) | none | Console programs, simple apps | None (self-contained) |
| **External** | `--runtime external` | Graphics, audio, games | SDL2, system libraries |

### Inline Runtime (Stubs)
- Embeds minimal C code directly in the generated program
- Functions return safe defaults (0, empty strings, -1 for invalid handles)
- Logs warnings when graphics/audio functions are called
- **Location:** `src/codegen/c_backend/runtime.rs`

### External Runtime (Full Implementation)
- Links against `libqb64fresh_rt.a` static library
- Complete SDL2-based graphics with hardware acceleration
- Full Rodio-based audio with MML parsing and file playback
- **Location:** `runtime/src/`

---

## Implementation Status Overview

| Category | Inline (Stubs) | External Runtime | Notes |
|----------|----------------|------------------|-------|
| **Audio** | ⚠️ No-op | ✅ Full | Rodio backend |
| **Graphics Core** | ⚠️ Returns defaults | ✅ Full | SDL2 backend |
| **Drawing** | ⚠️ No-op | ✅ Full | PSET, LINE, CIRCLE, PAINT |
| **Images** | ⚠️ Returns -1 | ✅ Full | PNG/JPEG/BMP/GIF support |
| **Mouse** | ⚠️ Returns 0 | ✅ Full | SDL2 input |
| **Fonts** | ⚠️ Hardcoded 8×16 | ✅ Full | TrueType support |
| **Clipboard** | ⚠️ Empty | ✅ Full | SDL2 clipboard |
| **File I/O** | ⚠️ Partial | ⚠️ Needs work | **Gap to fill** |
| **Dialogs** | ⚠️ stdin fallback | ⚠️ Partial | Native dialogs planned |
| **Joystick** | ⚠️ Center position | ⚠️ Not yet | SDL2 gamepad planned |
| **Legacy Hardware** | ⚠️ Safe defaults | ❌ Intentional | Port I/O, light pen obsolete |

---

## Audio Functions (20 functions)

**External Runtime:** `runtime/src/audio/rodio_backend.rs` (~550 lines)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_play()` | void (warns) | ✅ Full MML parser | Play Music Macro Language string |
| `qb_sndopen()` | -1 | ✅ Implemented | Open sound file |
| `qb_sndclose()` | void | ✅ Implemented | Close sound handle |
| `qb_sndplay()` | void (warns) | ✅ Implemented | Play a sound |
| `qb_sndstop()` | void | ✅ Implemented | Stop a sound |
| `qb_sndpause()` | void (warns) | ✅ Implemented | Pause a sound |
| `qb_sndloop()` | void (warns) | ✅ Implemented | Loop a sound |
| `qb_sndvol()` | void | ✅ Implemented | Set sound volume |
| `qb_sndbal()` | void | ⚠️ Partial | Set 3D balance (rodio limitation) |
| `qb_sndlen()` | 0.0 | ✅ Implemented | Get sound length in seconds |
| `qb_sndgetpos()` | 0.0 | ⚠️ Partial | Get playback position |
| `qb_sndsetpos()` | void | ⚠️ Partial | Set playback position |
| `qb_sndplaying()` | 0 | ✅ Implemented | Check if sound is playing |
| `qb_sndpaused()` | 0 | ✅ Implemented | Check if sound is paused |
| `qb_sndrate()` | 44100 | ✅ Implemented | Get sample rate |
| `qb_sndplayfile()` | void | ✅ Implemented | Play audio file directly |
| `qb_sndplaycopy()` | void | ✅ Implemented | Play copy of sound |
| `qb_sndcopy()` | 0 | ✅ Implemented | Copy sound handle |
| `qb_sndopenraw()` | -1 | ⚠️ Partial | Open raw audio stream |
| `qb_sndrawlen()` | 0.0 | ⚠️ Partial | Get raw audio queue length |

---

## Graphics Functions (77 functions)

**External Runtime:** `runtime/src/graphics/sdl2.rs` (~2100 lines)

### Initialization & Display (9 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_gfx_init()` | 0 (warns) | ✅ Implemented | Initialize graphics mode |
| `qb_gfx_shutdown()` | 0 | ✅ Implemented | Shutdown graphics subsystem |
| `qb_gfx_cls()` | 0 | ✅ Implemented | Clear screen |
| `qb_gfx_color()` | 0 | ✅ Implemented | Set foreground/background color |
| `qb_gfx_locate()` | 0 | ✅ Implemented | Position text cursor |
| `qb_gfx_display()` | 0 | ✅ Implemented | Refresh display (flip buffers) |
| `qb_gfx_poll_events()` | 1 | ✅ Implemented | Poll input events |
| `qb_gfx_width()` | 80 | ✅ Implemented | Get screen width in characters |
| `qb_gfx_height()` | 25 | ✅ Implemented | Get screen height in characters |

### Drawing Functions (6 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_gfx_pset()` | 0 | ✅ Implemented | Set pixel at coordinates |
| `qb_gfx_point()` | 0 | ✅ Implemented | Get pixel color at coordinates |
| `qb_gfx_line()` | 0 | ✅ Implemented | Draw line between points |
| `qb_gfx_box()` | 0 | ✅ Implemented | Draw box/rectangle |
| `qb_gfx_circle()` | 0 | ✅ Implemented | Draw circle/ellipse/arc |
| `qb_gfx_paint()` | 0 | ✅ Implemented | Flood fill region |

### Palette & Page Operations (4 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_gfx_palette()` | 0 | ✅ Implemented | Set palette color entry |
| `qb_gfx_palette_reset()` | 0 | ✅ Implemented | Reset palette to defaults |
| `qb_gfx_pcopy()` | 0 | ✅ Implemented | Copy video page to another |
| `qb_gfx_pmap()` | coord | ✅ Implemented | Map logical/physical coordinates |

### Extended Graphics (6 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_gfx_set_width()` | 0 | ✅ Implemented | Set screen dimensions |
| `qb_gfx_view()` | 0 | ✅ Implemented | Set graphics viewport |
| `qb_gfx_view_reset()` | 0 | ✅ Implemented | Reset viewport to full screen |
| `qb_gfx_window()` | 0 | ✅ Implemented | Set coordinate mapping window |
| `qb_gfx_window_reset()` | 0 | ✅ Implemented | Reset coordinate window |
| `qb_gfx_draw()` | 0 | ✅ Full MML parser | Execute DRAW command string |

### Image Operations (13 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_gfx_newimage()` | -1 | ✅ Implemented | Create new image buffer |
| `qb_gfx_loadimage()` | -1 | ✅ Implemented | Load image from file |
| `qb_gfx_copyimage()` | -1 | ✅ Implemented | Duplicate image handle |
| `qb_gfx_freeimage()` | 0 | ✅ Implemented | Free image memory |
| `qb_gfx_putimage_simple()` | 0 | ✅ Implemented | Draw image at position |
| `qb_gfx_putimage()` | 0 | ✅ Implemented | Draw image with coordinates |
| `qb_gfx_putimage_full()` | 0 | ✅ Implemented | Draw image with source/dest regions |
| `qb_gfx_source()` | 0 | ✅ Implemented | Set source image for operations |
| `qb_gfx_dest()` | 0 | ✅ Implemented | Set destination image |
| `qb_gfx_printstring()` | 0 | ✅ Implemented | Print string at pixel coordinates |
| `qb_gfx_autodisplay()` | 0 | ✅ Implemented | Set automatic display refresh |
| `qb_gfx_image_width()` | 0 | ✅ Implemented | Get image width in pixels |
| `qb_gfx_image_height()` | 0 | ✅ Implemented | Get image height in pixels |

### GET/PUT Graphics Array (4 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_gfx_get()` | void (warns) | ✅ Implemented | Capture screen region to array |
| `qb_gfx_get_step()` | void | ✅ Implemented | Capture with relative coordinates |
| `qb_gfx_put()` | void (warns) | ✅ Implemented | Draw from graphics array |
| `qb_gfx_put_step()` | void | ✅ Implemented | Draw with relative coordinates |

### Mouse Functions (10 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_mouse_x()` | 0 | ✅ Implemented | Get mouse X position |
| `qb_mouse_y()` | 0 | ✅ Implemented | Get mouse Y position |
| `qb_mouse_button()` | 0 | ✅ Implemented | Get mouse button state |
| `qb_mouse_input()` | 0 | ✅ Implemented | Check for mouse input |
| `qb_mouse_movement_x()` | 0 | ✅ Implemented | Get relative X movement |
| `qb_mouse_movement_y()` | 0 | ✅ Implemented | Get relative Y movement |
| `qb_mouse_wheel()` | 0 | ✅ Implemented | Get mouse wheel delta |
| `qb_mouse_hide()` | void | ✅ Implemented | Hide mouse cursor |
| `qb_mouse_show()` | void | ✅ Implemented | Show mouse cursor |
| `qb_mouse_move()` | void | ✅ Implemented | Move mouse cursor |

### Clipboard (2 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_clipboard_get()` | "" | ✅ Implemented | Get clipboard text |
| `qb_clipboard_set()` | void | ✅ Implemented | Set clipboard text |

### Font Functions (6 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_loadfont()` | 0 (warns) | ✅ Implemented | Load font from file |
| `qb_font()` | prev | ✅ Implemented | Set current font |
| `qb_freefont()` | 0 | ✅ Implemented | Free font handle |
| `qb_fontheight()` | 16 | ✅ Implemented | Get font height in pixels |
| `qb_fontwidth()` | 8 | ✅ Implemented | Get font width in pixels |
| `qb_printwidth()` | len×8 | ✅ Implemented | Get text width in pixels |

### Desktop/Window Info (8 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_desktopwidth()` | 1920* | ✅ Implemented | Get desktop width |
| `qb_desktopheight()` | 1080* | ✅ Implemented | Get desktop height |
| `qb_screenx()` | 0 | ✅ Implemented | Get window X position |
| `qb_screeny()` | 0 | ✅ Implemented | Get window Y position |
| `qb_title_get()` | stored | ✅ Implemented | Get window title |
| `qb_title_set()` | void | ✅ Implemented | Set window title |
| `qb_windowhandle()` | 0 | ✅ Implemented | Get native window handle |
| `qb_windowhasfocus()` | -1 | ✅ Implemented | Check if window has focus |

*Inline uses GetSystemMetrics on Windows, hardcoded on Linux

### Window Control (5 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_screenmove()` | 0 (warns) | ✅ Implemented | Move window position |
| `qb_screenhide()` | 0 | ✅ Implemented | Hide window |
| `qb_screenshow()` | 0 | ✅ Implemented | Show window |
| `qb_fullscreen()` | flag | ✅ Implemented | Toggle fullscreen mode |
| `qb_screenclick()` | 0 | ✅ Implemented | Bring window to front |

### Dialog Boxes (5 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_messagebox()` | 1* | ✅ Native (rfd) | Show message box |
| `qb_messagebox_ex()` | 1* | ✅ Native (rfd) | Show message box with buttons |
| `qb_inputbox()` | stdin | ⚠️ Basic | Show input dialog |
| `qb_openfiledialog()` | "" | ✅ Native (rfd) | Show open file dialog |
| `qb_savefiledialog()` | "" | ✅ Native (rfd) | Show save file dialog |
| `qb_selectfolderdialog()` | "" | ✅ Native (rfd) | Show folder selection dialog |

*Inline uses MessageBoxA on Windows, printf on Linux. External uses native rfd dialogs.

---

## File I/O Functions (fully implemented)

**Status:** ✅ Fully implemented in inline runtime

| Function | Inline Status | External Status | Purpose |
|----------|---------------|-----------------|---------|
| `qb_file_open()` | ✅ Implemented | ✅ Header declared | Open file with mode |
| `qb_file_close()` | ✅ Implemented | ✅ Header declared | Close file handle |
| `qb_file_print_*()` | ✅ Implemented | ✅ Header declared | PRINT # output |
| `qb_file_write_*()` | ✅ Implemented | ✅ Header declared | WRITE # output |
| `qb_file_input_*()` | ✅ Implemented | ✅ Header declared | INPUT # reading |
| `qb_file_line_input()` | ✅ Implemented | ✅ Header declared | LINE INPUT # |
| `qb_file_seek()` | ✅ Implemented | ✅ Header declared | Seek to position |
| `qb_file_get()` | ✅ Implemented | ✅ Header declared | Binary GET |
| `qb_file_put()` | ✅ Implemented | ✅ Header declared | Binary PUT |
| `qb_eof()` | ✅ Implemented | ✅ Header declared | Check end of file |
| `qb_lof()` | ✅ Implemented | ✅ Header declared | Get length of file |
| `qb_loc()` | ✅ Implemented | ✅ Header declared | Get current position |
| `qb_freefile()` | ✅ Implemented | ✅ Header declared | Get free file number |
| `qb_field_start()` | ✅ Implemented | ✅ Header declared | Begin FIELD statement |
| `qb_field_add()` | ✅ Implemented | ✅ Header declared | Add field variable |
| `qb_lset()` | ✅ Implemented | ✅ Header declared | Left-justify in field |
| `qb_rset()` | ✅ Implemented | ✅ Header declared | Right-justify in field |

---

## Legacy/QB4.5 Functions (32 functions)

These are intentionally minimal - they support compatibility with old BASIC programs but many represent obsolete hardware.

### Legacy I/O & Memory (6 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_lpos()` | 1 | ⚠️ Stub only | Get printer carriage position |
| `qb_varptr()` | ptr | ✅ Works | Get variable memory address |
| `qb_varptr_str()` | bytes | ✅ Works | Get address as binary string |
| `qb_varseg()` | 0 | ✅ Works | Get segment (always 0, flat memory) |
| `qb_sadd()` | ptr | ✅ Works | Get string data address |
| `qb_fileattr()` | 0 | ⚠️ Stub only | Get file attributes |

### Joystick Functions (2 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_stick()` | 127 | ✅ Implemented | Get joystick axis position |
| `qb_strig()` | 0 | ✅ Implemented | Get joystick trigger state |
| `qb_devices()` | 2 | ✅ Implemented | Get number of input devices |
| `qb_axis()` | 0.0 | ✅ Implemented | Get axis value (-1.0 to 1.0) |
| `qb_button()` | 0 | ✅ Implemented | Get button state |

### Memory Functions (2 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_fre()` | 64MB | ✅ Works | Get free memory |
| `qb_free()` | void | ✅ Works | Free string memory (automatic) |

### Port I/O Functions (3 functions) - Intentionally Disabled

These are **intentionally not implemented** for security. Direct port I/O is not available on protected-mode operating systems.

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_inp()` | 0xFF | ❌ Sandboxed | Read from I/O port |
| `qb_out()` | void | ❌ Sandboxed | Write to I/O port |
| `qb_wait()` | void | ❌ Sandboxed | Wait for port condition |

### Hardware Functions (5 functions) - Obsolete Hardware

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_pen()` | 0 | ❌ Obsolete | Get light pen state |
| `qb_erdev()` | 0 | ⚠️ Stub only | Get device error code |
| `qb_erdev_str()` | "" | ⚠️ Stub only | Get device error name |
| `qb_ioctl()` | void | ⚠️ Stub only | Send device control string |
| `qb_ioctl_str()` | "" | ⚠️ Stub only | Get device status string |

### System Interrupts (2 functions) - Intentionally Disabled

Not supported on modern systems for security reasons.

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_interrupt()` | void (warns) | ❌ Disabled | Call system interrupt |
| `qb_interruptx()` | void (warns) | ❌ Disabled | Extended system interrupt |

### Event Handlers (11 functions)

| Function | Inline Returns | External Status | Purpose |
|----------|----------------|-----------------|---------|
| `qb_on_strig()` | void | ❌ Not yet | ON STRIG (joystick) handler |
| `qb_strig_control()` | void | ❌ Not yet | STRIG ON/OFF/STOP control |
| `qb_on_com()` | void | ⚠️ Stub only | ON COM (serial port) handler |
| `qb_com_control()` | void | ⚠️ Stub only | COM ON/OFF/STOP control |
| `qb_on_pen()` | void | ❌ Obsolete | ON PEN (light pen) handler |
| `qb_pen_control()` | void | ❌ Obsolete | PEN ON/OFF/STOP control |
| `qb_on_uevent()` | void | ⚠️ Stub only | ON UEVENT (user event) handler |
| `qb_uevent_control()` | void | ⚠️ Stub only | UEVENT ON/OFF/STOP control |
| `qb_uevent_trigger()` | void | ⚠️ Stub only | Trigger user-defined event |
| `qb_on_signal()` | void | ⚠️ Stub only | ON SIGNAL handler |
| `qb_signal_control()` | void | ⚠️ Stub only | SIGNAL ON/OFF/STOP control |

---

## Summary

### Implementation Statistics

| Status | Count | Percentage |
|--------|-------|------------|
| ✅ Fully Implemented | 115 | 89% |
| ⚠️ Partial/Stub only | 7 | 5% |
| ❌ Not implemented/Disabled | 7 | 6% |
| **Total** | **129** | |

### Recently Implemented (January 2026)

1. ✅ **File I/O** - Full FIELD/LSET/RSET support, all file operations working
2. ✅ **Native File Dialogs** - Open/Save/Folder dialogs via `rfd` crate
3. ✅ **Joystick/Gamepad** - STICK, STRIG, _AXIS, _BUTTON, _DEVICES functions

### What's Still Missing

**Low Priority (rarely used):**
1. Serial port (COM) support
2. User-defined events (UEVENT)
3. Input box dialog (currently uses stdin)

**Intentionally Not Implemented:**
- Port I/O (INP, OUT, WAIT) - security
- System interrupts (INTERRUPT, INTERRUPTX) - security
- Light pen (PEN) - obsolete hardware

---

## How to Use Full Functionality

```bash
# Compile with external runtime for graphics/audio/dialogs
qb64fresh myprogram.bas --runtime external

# Default inline mode for console programs (file I/O works!)
qb64fresh myprogram.bas
```

The external runtime requires SDL2 to be installed on the system.
