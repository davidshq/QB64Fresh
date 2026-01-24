# QB64Fresh - Remaining Stub/Incomplete Functions

*Updated: 2026-01-24*

This document lists functions that are **not fully implemented** - either stub-only in inline mode with partial external support, or intentionally disabled.

For the complete function reference (including all implemented functions), see [STUB_FUNCTIONS_FULL.md](STUB_FUNCTIONS_FULL.md).

---

## Summary

| Status | Count | Description |
|--------|-------|-------------|
| ⚠️ Audio stubs (external) | 12 | Need rodio integration (vol, seek, raw) |
| ⚠️ Graphics stubs | 3 | _MAPTRIANGLE, _COPYPALETTE, _DISPLAYORDER |
| ⚠️ Legacy stubs | ~4 | ERDEV, device error functions, event handlers |
| ❌ Compile errors | 4 | FRE, SETMEM, IOCTL$, FILEATTR (match QB64pe) |
| ❌ Intentionally disabled | ~7 | Interrupts, obsolete hardware |
| **Total Remaining** | **~30** | Out of 419 registered functions |


---

## Graphics Functions - Not Yet Implemented

These graphics functions are parsed but not yet implemented in the SDL2 backend.

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `_MAPTRIANGLE` | ❌ Stub | ✅ Full | 3D textured triangle; needs SDL_RenderGeometry or custom rasterizer |
| `_COPYPALETTE` | ❌ Stub | ✅ Full | Copy palette between images |
| `_DISPLAYORDER` | ❌ Stub | ✅ Full | Set hardware/software layer rendering order |

**Priority:** Low - `_MAPTRIANGLE` is the most complex (requires triangle rasterization with texture mapping). `_COPYPALETTE` and `_DISPLAYORDER` are less commonly used.

**Note:** Alpha blending (`_BLEND`, `_DONTBLEND`, `_CLEARCOLOR`) was implemented in session 040 (2026-01-24).

---

## Audio Functions - Needing External Runtime Work

**Note:** Basic audio (`_SNDOPEN`, `_SNDPLAY`, `_SNDSTOP`, `_SNDPAUSE`, `_SNDCLOSE`) works in external runtime mode. The functions below need rodio integration work.

**QB64pe uses:** miniaudio library with full AudioEngine C++ class (`internal/c/parts/audio/audio.cpp`)

| Function | External Status | QB64pe | QB64pe Method | Notes |
|----------|-----------------|--------|---------------|-------|
| `_SNDVOL()` | ⚠️ Stub | ✅ Full | `SetSoundVolume()` | Volume control |
| `_SNDBAL()` | ⚠️ Stub | ✅ Full | `SetSoundBalance()` | 3D positioning |
| `_SNDLEN()` | ⚠️ Stub | ✅ Full | `GetSoundDuration()` | Duration query |
| `_SNDGETPOS()` | ⚠️ Stub | ✅ Full | `GetSoundPosition()` | Position query |
| `_SNDSETPOS()` | ⚠️ Stub | ✅ Full | `SetSoundPosition()` | Seeking |
| `_SNDPLAYING()` | ⚠️ Stub | ✅ Full | `IsSoundPlaying()` | State check |
| `_SNDPAUSED()` | ⚠️ Stub | ✅ Full | `IsSoundPaused()` | State check |
| `_SNDOPENRAW()` | ⚠️ Stub | ✅ Full | `OpenRawSound()` | Raw streaming |
| `_SNDRAWLEN()` | ⚠️ Stub | ✅ Full | `GetRawSoundTimeRemaining()` | Buffer query |
| `_SNDPLAYFILE()` | ⚠️ Stub | ✅ Full | `PlaySoundFile()` | Direct playback |
| `_SNDPLAYCOPY()` | ⚠️ Stub | ✅ Full | `PlaySoundCopy()` | Copy playback |
| `_SNDCOPY()` | ⚠️ Stub | ✅ Full | `CopySound()` | Handle copying |

**Priority:** Medium - QB64pe has full audio support. We need to wire up rodio or consider switching to miniaudio for parity.

---

## Legacy Functions - Stubs

These functions exist for QB4.5 compatibility but are stubs or have minimal implementation.

### Legacy I/O & Memory

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `FRE()` | ❌ Error | ⛔ Stub (error) | Throws compile error matching QB64pe |
| `SETMEM` | ❌ Error | ⛔ Stub (error) | Throws compile error matching QB64pe |
| `FILEATTR()` | ❌ Error | ⛔ Stub (error) | Throws compile error matching QB64pe |


### Device Functions

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `ERDEV()` | ⚠️ Stub (0) | ❌ Not registered | Legacy DOS device error |
| `ERDEV$()` | ⚠️ Stub ("") | ❌ Not registered | Legacy DOS device error |
| `IOCTL$()` | ❌ Error | ⛔ Stub (error) | Throws compile error matching QB64pe |
| `IOCTL` | ⚠️ Stub | ⛔ Stub (error) | Statement form compiles to no-op |

### Event Handlers - Stub Only

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `ON COM` | ⚠️ Stub | ❌ Not registered | Serial port events |
| `COM ON/OFF/STOP` | ⚠️ Stub | ❌ Not registered | Serial port control |
| `ON UEVENT` | ⚠️ Stub | ❌ Not registered | User-defined events |
| `UEVENT ON/OFF/STOP` | ⚠️ Stub | ❌ Not registered | User event control |
| `_UEVENTTRIGGER` | ⚠️ Stub | ❌ Not registered | Trigger user event |
| `ON SIGNAL` | ⚠️ Stub | ❌ Not registered | Signal handler |
| `SIGNAL ON/OFF/STOP` | ⚠️ Stub | ❌ Not registered | Signal control |

**Priority:** Very Low - QB64pe doesn't implement these either. They're rarely used in modern programs.

---

## Intentionally Disabled Functions

These functions are **not implemented for security or obsolescence reasons** in QB64Fresh, though QB64pe implements some of them.

### Port I/O - VGA Palette Emulation (COMPLETED)

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `INP()` | ✅ Full | ✅ Full | VGA palette (0x3C9) and retrace (0x3DA) |
| `OUT` | ✅ Full | ✅ Full | Palette registers (0x3C7, 0x3C8, 0x3C9) |
| `WAIT` | ✅ Full | ✅ Full | Returns immediately for unsupported ports |

**Supported ports:** 0x3C7 (palette read index), 0x3C8 (palette write index), 0x3C9 (palette RGB), 0x3DA (vertical retrace). Other ports return 0 or no-op (safe defaults).

### System Interrupts (Security)

Direct interrupt calls are not supported on modern systems.

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `INTERRUPT` | ⛔ Stub (warns) | ✅ Full (`libqb.cpp:15713`) | QB64pe emulates DOS interrupts |
| `INTERRUPTX` | ⛔ Stub (warns) | ✅ Full (`libqb.cpp:15774`) | Extended version |

**QB64pe note:** `call_int()` at `libqb.cpp:18610` emulates mouse interrupt (INT 0x33) with full support for show/hide cursor, get position, etc.

### Obsolete Hardware

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `PEN()` | ⛔ Stub (0) | ❌ Not registered | Light pen hardware obsolete |
| `ON PEN` | ⛔ Stub | ❌ Not registered | |
| `PEN ON/OFF/STOP` | ⛔ Stub | ❌ Not registered | |

### Joystick Events

| Function | Fresh Status | QB64pe Status | Notes |
|----------|--------------|---------------|-------|
| `STRIG()` | ⛔ Stub | ✅ Full (`libqb.cpp:25613`) | QB64pe polls controller buttons |
| `ON STRIG` | ⛔ Stub | ⚠️ Ignored | QB64pe parses but ignores for compatibility |
| `STRIG ON/OFF/STOP` | ⛔ Stub | ⚠️ Ignored | QB64pe parses but ignores |

**Priority:** Low - Consider implementing INP/OUT for VGA palette compatibility if needed.

---

## Potential Future Work

### Could Be Implemented (Medium Priority)

1. **Audio Integration** - Wire up rodio backend for full parity with QB64pe:
   - `_SNDVOL`, `_SNDBAL` - Volume and balance control
   - `_SNDLEN`, `_SNDGETPOS`, `_SNDSETPOS` - Position tracking and seeking
   - `_SNDPLAYING`, `_SNDPAUSED` - State tracking
   - Effort: Medium (rodio already integrated, just needs wiring)
   - **Alternative:** Consider switching to miniaudio (what QB64pe uses) for exact compatibility

2. **Raw Audio Streaming** - `_SNDOPENRAW`, `_SNDRAW`, `_SNDRAWLEN`
   - Would need custom audio buffer implementation
   - Rodio supports this but needs integration
   - QB64pe: Full implementation in AudioEngine class
   - Effort: Medium-Large

### Could Be Implemented (Low Priority)

3. **Mouse Interrupt Emulation** - `INTERRUPT`/`INTERRUPTX` for INT 0x33
   - QB64pe fully emulates mouse interrupt
   - Would enable legacy mouse code
   - Effort: Medium

4. **STRIG Function** - Joystick button polling
   - QB64pe: Full implementation at `libqb.cpp:25613`
   - SDL2 already provides joystick support
   - Effort: Low-Medium

5. **COM Port Support** - Serial communication
   - Would need cross-platform serial library (e.g., `serialport` crate)
   - QB64pe: Not implemented
   - Effort: Large

### Will Not Implement

- **Light Pen (PEN)** - Hardware doesn't exist; QB64pe also doesn't implement
- **DOS Device Functions (ERDEV, IOCTL)** - QB64pe stubs these too
- **Event Handlers (ON COM, ON UEVENT, ON SIGNAL)** - QB64pe doesn't implement these

---

## Implementation Statistics

**Total Built-in Functions/Subs:** 419

| Category | Count | Percentage |
|----------|-------|------------|
| ✅ Fully Implemented | ~392 | 94% |
| ⚠️ Audio Stubs (need rodio work) | 12 | 3% |
| ⚠️ Legacy Stubs | ~4 | 1% |
| ❌ Compile Errors (match QB64pe) | 4 | 1% |
| ❌ Disabled/Obsolete | ~7 | 1% |

The vast majority of QB64 programs will work without issues. The remaining issues are:
- Audio features needing rodio integration (volume, seeking, raw synthesis)
- Obsolete legacy functions throw compile errors (FRE, SETMEM, IOCTL$, FILEATTR) - matches QB64pe
- System interrupts (INTERRUPT/INTERRUPTX) - QB64pe implements for mouse
- Obsolete hardware (light pen) - QB64pe also doesn't implement
