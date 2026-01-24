# QB64Fresh - Implemented Functions Reference

*Updated: 2026-01-24*

This document lists functions that were previously stubs but are now **fully implemented**. For remaining stubs and incomplete functions, see [STUB_FUNCTIONS_REMAINING.md](STUB_FUNCTIONS_REMAINING.md).

---

## Summary

| Category | Status |
|----------|--------|
| **Total Built-in Functions/Subs** | 419 |
| **Fully Implemented** | ~409 (97.6%) |
| **Remaining Stubs** | ~13 |

---

## Recently Completed Functions

### Graphics Functions ✅

These graphics functions are now fully implemented in the SDL2 backend.

| Function | Status | Notes |
|----------|--------|-------|
| `_MAPTRIANGLE` | ✅ Full | Software texture mapping rasterizer for 3D graphics |
| `_COPYPALETTE` | ✅ Full | Copy palette between images |
| `_DISPLAYORDER` | ✅ Full | Set hardware/software layer rendering order |
| `_BLEND` | ✅ Full | Enable alpha blending for image |
| `_DONTBLEND` | ✅ Full | Disable alpha blending (direct pixel copy) |
| `_CLEARCOLOR` | ✅ Full | Set transparency key for images |

### Audio Functions ✅

All audio functions implemented in session 040 (2026-01-24).

| Function | Status | Notes |
|----------|--------|-------|
| `BEEP` | ✅ Full | System beep |
| `SOUND` | ✅ Full | Generate tone with frequency and duration |
| `PLAY` | ✅ Full | Play MML music strings |
| `_SNDOPEN` | ✅ Full | Open audio file (WAV, MP3, OGG, FLAC) |
| `_SNDCLOSE` | ✅ Full | Close audio handle |
| `_SNDPLAY` | ✅ Full | Play audio |
| `_SNDSTOP` | ✅ Full | Stop audio playback |
| `_SNDPAUSE` | ✅ Full | Pause audio |
| `_SNDVOL` | ✅ Full | Set volume (0.0-1.0) |
| `_SNDBAL` | ✅ Full | Set stereo balance |
| `_SNDLEN` | ✅ Full | Get audio length in seconds |
| `_SNDGETPOS` | ✅ Full | Get playback position |
| `_SNDSETPOS` | ✅ Full | Set playback position |
| `_SNDPLAYING` | ✅ Full | Check if playing |
| `_SNDPAUSED` | ✅ Full | Check if paused |
| `_SNDLOOP` | ✅ Full | Enable looping playback |
| `_SNDCOPY` | ✅ Full | Copy audio handle |
| `_SNDPLAYCOPY` | ✅ Full | Play a copy of audio |
| `_SNDPLAYFILE` | ✅ Full | Play audio file directly |
| `_SNDRAW` | ✅ Full | Queue raw audio samples |
| `_SNDRAWLEN` | ✅ Full | Get raw audio buffer length |
| `_SNDRAWDONE` | ✅ Full | Check if raw audio finished |
| `_SNDRATE` | ✅ Full | Get audio sample rate |
| `_SNDNEW` | ✅ Full | Create new audio buffer |

### Port I/O - VGA Palette Emulation ✅

| Function | Status | Notes |
|----------|--------|-------|
| `INP()` | ✅ Full | VGA palette (0x3C9) and retrace (0x3DA) emulation |
| `OUT` | ✅ Full | Palette registers (0x3C7, 0x3C8, 0x3C9) |
| `WAIT` | ✅ Full | Returns immediately for unsupported ports |

**Supported ports:**
- 0x3C7 (palette read index)
- 0x3C8 (palette write index)
- 0x3C9 (palette RGB data)
- 0x3DA (vertical retrace status)

Other ports return 0 or no-op (safe defaults).

### System Interrupts - INT 0x33 Mouse Emulation ✅

| Function | Status | Notes |
|----------|--------|-------|
| `INTERRUPT` | ✅ Full | INT 0x33 mouse emulation |
| `INTERRUPTX` | ✅ Full | Extended version (same emulation) |

**Supported INT 0x33 subfunctions:**
- AX=0: Check mouse installed → returns AX=0xFFFF, BX=2
- AX=1: Show mouse cursor
- AX=2: Hide mouse cursor
- AX=3: Get status → BX=buttons, CX=X, DX=Y

Other interrupts are no-ops (safe defaults).

### Memory Functions ✅

| Function | Status | Notes |
|----------|--------|-------|
| `VARPTR()` | ✅ Full | Get address of variable as LONG |
| `VARPTR$()` | ✅ Full | Get address as binary string (4 bytes) |
| `VARSEG()` | ✅ Full | Returns 0 (flat memory model) |
| `SADD()` | ✅ Full | Get address of string data |
| `_MEM()` | ✅ Full | Get _MEM block for variable's memory |
| `_MEMGET` | ✅ Full | Read typed value from memory (with AS clause) |
| `_MEMPUT` | ✅ Full | Write typed value to memory (with AS clause) |
| `_MEMFREE` | ✅ Full | Free memory block |
| `_MEMNEW` | ✅ Full | Allocate new memory block |
| `_MEMCOPY` | ✅ Full | Copy memory between blocks |
| `_MEMFILL` | ✅ Full | Fill memory with value |

### Callback Functions ✅

| Function | Status | Notes |
|----------|--------|-------|
| `_PROCPTR()` | ✅ Full | Get procedure pointer for C callbacks |

Generates proper C function signatures:
- FUNCTION callbacks return appropriate C type
- SUB callbacks return `void`
- BYVAL/BYREF parameters handled correctly

### Unicode Font Rendering ✅

| Function | Status | Notes |
|----------|--------|-------|
| `_LOADFONT` | ✅ Full | Load TrueType fonts (with "UNICODE" option) |
| `_FREEFONT` | ✅ Full | Release font resources |
| `_UPRINTSTRING` | ✅ Full | Render Unicode text at position |
| `_UPRINTWIDTH` | ✅ Full | Get Unicode text width in pixels |
| `_UFONTHEIGHT` | ✅ Full | Get Unicode font height |
| `_ULINESPACING` | ✅ Full | Get Unicode line spacing |
| `_UCHARPOS` | ✅ Full | Get character X position within string |
| `_MAPUNICODE` | ✅ Full | CP437 to Unicode mapping table |

### Screen Pages & Double Buffering ✅

| Function | Status | Notes |
|----------|--------|-------|
| `SCREEN` | ✅ Full | Set mode with active/visual page parameters |
| `PCOPY` | ✅ Full | Copy page contents between screen pages |

4 screen pages available for double/triple buffering.

---

## Core Language Functions

All core BASIC language functions are fully implemented. This includes:

### Math Functions
`ABS`, `ATN`, `COS`, `SIN`, `TAN`, `EXP`, `LOG`, `SQR`, `SGN`, `INT`, `FIX`, `CINT`, `CLNG`, `CSNG`, `CDBL`, `MOD`, `RND`, `RANDOMIZE`

### String Functions
`LEN`, `LEFT$`, `RIGHT$`, `MID$`, `INSTR`, `UCASE$`, `LCASE$`, `LTRIM$`, `RTRIM$`, `SPACE$`, `STRING$`, `CHR$`, `ASC`, `VAL`, `STR$`, `HEX$`, `OCT$`, `BIN$`, `INKEY$`, `INPUT$`

### I/O Functions
`PRINT`, `INPUT`, `LINE INPUT`, `WRITE`, `OPEN`, `CLOSE`, `GET`, `PUT`, `SEEK`, `LOC`, `LOF`, `EOF`, `FREEFILE`, `NAME`, `KILL`, `FILES`, `MKDIR`, `RMDIR`, `CHDIR`, `CURDIR$`

### Control Flow
`IF/THEN/ELSE/END IF`, `SELECT CASE`, `FOR/NEXT`, `DO/LOOP`, `WHILE/WEND`, `GOTO`, `GOSUB/RETURN`, `ON...GOTO`, `ON...GOSUB`, `EXIT`

### Data Types & Definitions
`DIM`, `REDIM`, `CONST`, `TYPE/END TYPE`, `SUB/END SUB`, `FUNCTION/END FUNCTION`, `DECLARE`, `SHARED`, `STATIC`, `COMMON`

### Graphics (Core)
`SCREEN`, `CLS`, `COLOR`, `LOCATE`, `PSET`, `POINT`, `LINE`, `CIRCLE`, `PAINT`, `DRAW`, `VIEW`, `WINDOW`, `PALETTE`, `GET`, `PUT`

### QB64 Extensions (Core)
`_DEST`, `_SOURCE`, `_DISPLAY`, `_LIMIT`, `_DELAY`, `_KEYHIT`, `_KEYDOWN`, `_MOUSEX`, `_MOUSEY`, `_MOUSEBUTTON`, `_NEWIMAGE`, `_LOADIMAGE`, `_PUTIMAGE`, `_FREEIMAGE`, `_RGB`, `_RGBA`, `_RGB32`, `_RGBA32`

---

## Intentionally Disabled Functions (Matches QB64pe)

These functions throw compile errors **by design**, matching QB64pe's behavior. This is the correct implementation - these legacy DOS functions have no modern equivalent.

| Function | Behavior | Notes |
|----------|----------|-------|
| `FRE()` | Compile error | Legacy DOS memory function - no modern equivalent |
| `SETMEM` | Compile error | Legacy DOS memory management |
| `FILEATTR()` | Compile error | Legacy DOS file attributes |
| `IOCTL$()` | Compile error | Legacy DOS device I/O control |

**Why these throw errors:** QB64pe intentionally throws compile errors for these functions rather than providing stub implementations. This prevents silent failures in programs that depend on these DOS-specific features. QB64Fresh matches this behavior exactly.

---

## See Also

- [STUB_FUNCTIONS_REMAINING.md](STUB_FUNCTIONS_REMAINING.md) - Functions still incomplete
- [GRAPHICS.md](../GRAPHICS.md) - Complete graphics documentation
- [ADR-0008](../adrs/ADR-0008-c-interoperability.md) - DECLARE LIBRARY documentation
