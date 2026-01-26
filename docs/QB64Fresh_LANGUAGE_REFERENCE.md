# QB64Fresh Language Reference

**Version:** 0.1.0  
**Last Updated:** 2026-01-26

This document provides a comprehensive reference for all statements and functions available in QB64Fresh. QB64Fresh is a modern BASIC compiler that maintains compatibility with QB4.5 and QB64 while providing a clean, well-documented implementation.

## Scope and Implementation Status

QB64Fresh supports **QB4.5 compatibility** and a **curated set of QB64 extensions**. The reference documents the full QB64/QB4.5 language; implementation status may vary.

- **Excluded:** Raw OpenGL (`_GL*` commands) are intentionally excluded. QB64Fresh uses SDL2/winit for graphics. Use `DECLARE LIBRARY` to call OpenGL directly if needed.
- **Stub-only (compile, limited/no runtime):** `INP`, `OUT`, `WAIT`, `INTERRUPT`/`INTERRUPTX`, `PEN`, `IOCTL`/`IOCTL$`, `ERDEV`/`ERDEV$`, `ON COM`, `ON UEVENT`, `ON SIGNAL`—accepted for porting but behavior is no-op or undefined.
- **Authoritative source:** For the exact set of built-in functions and procedure signatures, see `src/semantic/builtins.rs` (405+ built-in registrations covering 240+ unique functions). For parser coverage, see `src/ast/stmt.rs` and the parser modules.

See [ADR-0014: Scope and Intentionally Excluded Features](adrs/ADR-0014-scope-and-excluded-features.md) and [FUTURE.md](ThingsToDo/FUTURE.md) for rationale and details.

## Table of Contents

0. [Scope and Implementation Status](#scope-and-implementation-status)
1. [Statements](#statements)
   - [I/O Statements](#io-statements)
   - [Control Flow](#control-flow)
   - [Variable Declarations](#variable-declarations)
   - [File I/O](#file-io)
   - [Graphics](#graphics-statements)
   - [Audio](#audio-statements)
   - [System Integration](#system-integration)
   - [Error Handling](#error-handling)
   - [Preprocessor Directives](#preprocessor-directives)
   - [C Library Integration](#c-library-integration)

2. [Functions](#functions)
   - [String Functions](#string-functions)
   - [Math Functions](#math-functions)
   - [Array Functions](#array-functions)
   - [Graphics Functions](#graphics-functions)
   - [Input Functions](#input-functions)
   - [File Functions](#file-functions)
   - [System Functions](#system-functions)
   - [Memory Functions](#memory-functions)
   - [Audio Functions](#audio-functions)
   - [Date/Time Functions](#datetime-functions)

3. [Constants](#constants)

---

## Statements

### I/O Statements

#### `PRINT [expression [{;|,} expression]...]` or `? [expression ...]`
Prints values to the console. Use `;` for no spacing, `,` for tab spacing. Trailing `;` suppresses newline. `?` is shorthand for `PRINT`.

```basic
PRINT "Hello, World!"
? x; y; z
PRINT "Name: "; name$; ", Age: "; age%
```

#### `PRINT USING format$; value1, value2, ...`
Formatted output using a format string.

```basic
PRINT USING "###.##"; 123.456
PRINT USING "Name: &, Age: ##"; name$; age%
```

#### `INPUT [;]["prompt"{;|,}] variable[, variable...]`
Reads input from the user. Leading `;` keeps cursor on same line.

```basic
INPUT "Enter your name: "; name$
INPUT ; "Age"; age%
```

#### `LINE INPUT [;]["prompt";] variable$`
Reads an entire line of input (including commas and spaces).

```basic
LINE INPUT "Enter address: "; address$
```

---

### Control Flow

#### `IF condition THEN ... [ELSEIF ...] [ELSE ...] END IF`
Conditional execution. Supports single-line and multi-line forms.

```basic
IF x > 10 THEN
    PRINT "Large"
ELSEIF x > 5 THEN
    PRINT "Medium"
ELSE
    PRINT "Small"
END IF
```

#### `SELECT CASE expression ... END SELECT`
Multi-way branching based on expression value.

```basic
SELECT CASE score%
    CASE 90 TO 100
        PRINT "A"
    CASE 80 TO 89
        PRINT "B"
    CASE ELSE
        PRINT "F"
END SELECT
```

#### `SELECT EVERYCASE expression ... END SELECT` (QB64)
Executes ALL matching cases, not just the first.

#### `FOR variable = start TO end [STEP step] ... NEXT [variable]`
Counted loop.

```basic
FOR i% = 1 TO 10
    PRINT i%
NEXT i%

FOR i% = 10 TO 1 STEP -1
    PRINT i%
NEXT
```

#### `WHILE condition ... WEND`
Pre-test loop.

```basic
WHILE x% < 100
    x% = x% + 1
WEND
```

#### `DO [WHILE|UNTIL condition] ... LOOP [WHILE|UNTIL condition]`
Flexible loop with optional pre/post conditions.

```basic
DO
    INPUT "Enter number: "; n%
LOOP WHILE n% < 0

DO UNTIL done%
    ' Process data
LOOP
```

#### `GOTO label`
Unconditional jump to a label.

```basic
GOTO start
start:
    PRINT "Here"
```

#### `GOSUB label` / `RETURN`
Subroutine call and return.

```basic
GOSUB printMessage
END

printMessage:
    PRINT "Hello"
    RETURN
```

#### `EXIT FOR` / `EXIT WHILE` / `EXIT DO` / `EXIT SUB` / `EXIT FUNCTION`
Exit from a loop or procedure early.

```basic
FOR i% = 1 TO 100
    IF i% = 50 THEN EXIT FOR
    PRINT i%
NEXT
```

#### `_CONTINUE [FOR|WHILE|DO]` (QB64)
Continue to next iteration of loop.

```basic
FOR i% = 1 TO 10
    IF i% MOD 2 = 0 THEN _CONTINUE
    PRINT i%
NEXT
```

#### `ON expression GOTO label1, label2, ...`
Computed GOTO based on expression value (1-based).

```basic
ON choice% GOTO option1, option2, option3
```

#### `ON expression GOSUB label1, label2, ...`
Computed GOSUB based on expression value.

---

### Variable Declarations

#### `DIM [SHARED] variable [AS type]` or `DIM array(size) [AS type]`
Declare variables and arrays.

```basic
DIM x AS INTEGER
DIM arr(10) AS INTEGER
DIM names$(5) AS STRING
DIM matrix(10, 20) AS DOUBLE
```

#### `REDIM [_PRESERVE] array(dims) [AS type]`
Resize dynamic arrays, optionally preserving contents.

```basic
REDIM arr(20) AS INTEGER
REDIM _PRESERVE arr(30) AS INTEGER
```

#### `CONST name = value [, name2 = value2, ...]`
Define compile-time constants.

```basic
CONST PI = 3.14159, MAX_SIZE = 100
```

#### `STATIC variable [AS type]` (inside SUB/FUNCTION)
Declare static local variables that persist between calls.

```basic
SUB Counter
    STATIC count AS INTEGER
    count = count + 1
    PRINT count
END SUB
```

#### `SHARED var1[, var2, ...]` (inside SUB/FUNCTION)
Access module-level shared variables.

```basic
DIM SHARED globalVar AS INTEGER

SUB MySub
    SHARED globalVar
    globalVar = 42
END SUB
```

#### `COMMON [SHARED] variable [, variable]...`
Declare variables shared between modules (for CHAIN).

#### `DEFINT|DEFLNG|DEFSNG|DEFDBL|DEFSTR letter-range`
Set default type for variables by first letter.

```basic
DEFINT A-Z
DEFSTR S
```

#### `_DEFINE letter-range AS type` (QB64)
Extended default type declaration.

```basic
_DEFINE A-Z AS _INTEGER64
```

#### `OPTION BASE 0|1`
Set default array lower bound (must appear before DIM).

#### `OPTION _EXPLICIT` (QB64)
Require all variables to be explicitly declared.

#### `OPTION _EXPLICITARRAY` (QB64)
Require all arrays to be explicitly declared.

---

### Assignment Statements

#### `[LET] variable = expression`
Simple assignment. LET keyword is optional.

```basic
x = 10
LET y = 20
name$ = "QB64Fresh"
```

#### `array(indices) = expression`
Array element assignment.

```basic
arr(5) = 100
matrix(2, 3) = 42.5
```

#### `variable.field = expression`
UDT field assignment.

```basic
player.x = 100
player.name$ = "Player1"
```

#### `MID$(string$, start [, length]) = value$`
Substring assignment.

```basic
MID$(text$, 1, 5) = "Hello"
```

#### `ASC(string$, position) = value`
Set character at position by ASCII code.

```basic
ASC(name$, 1) = 65  ' Sets first char to 'A'
```

#### `SWAP var1, var2`
Exchange values of two variables.

```basic
SWAP a%, b%
```

---

### File I/O

#### `OPEN filename FOR mode [ACCESS access] [lock] AS [#]filenum [LEN=reclen]`
Open a file for reading/writing.

```basic
OPEN "data.txt" FOR INPUT AS #1
OPEN "output.txt" FOR OUTPUT AS #2
OPEN "random.dat" FOR RANDOM AS #3 LEN = 128
```

Modes: `INPUT`, `OUTPUT`, `APPEND`, `BINARY`, `RANDOM`

#### `OPEN mode$, [#]filenum, filename[, reclen]` (Legacy)
GW-BASIC style OPEN syntax.

```basic
OPEN "O", #1, "output.txt"
```

#### `CLOSE [[#]filenum [, [#]filenum]...]`
Close file(s). No arguments closes all files.

```basic
CLOSE #1
CLOSE #1, #2, #3
CLOSE
```

#### `PRINT #filenum, [expression [{;|,} expression]...]`
Write to sequential file.

```basic
PRINT #1, "Name: "; name$; ", Age: "; age%
```

#### `WRITE #filenum, [expression [{,} expression]...]`
Write to sequential file with delimiters (comma-separated, strings quoted).

```basic
WRITE #1, name$, age%, score!
```

#### `INPUT #filenum, variable [, variable]...`
Read from sequential file.

```basic
INPUT #1, name$, age%, score!
```

#### `LINE INPUT #filenum, variable$`
Read entire line from sequential file.

```basic
LINE INPUT #1, line$
```

#### `GET [#]filenum, [position], variable`
Read from binary or random access file.

```basic
GET #1, 5, record
GET #1, , buffer
```

#### `PUT [#]filenum, [position], variable`
Write to binary or random access file.

```basic
PUT #1, 5, record
PUT #1, , buffer
```

#### `SEEK [#]filenum, position`
Set file position for next read/write.

```basic
SEEK #1, 100
```

#### `FIELD [#]filenum, width AS var$ [, width AS var$]...`
Define random file field variables.

```basic
FIELD #1, 20 AS name$, 4 AS age$
```

#### `LSET var$ = string$` / `RSET var$ = string$`
Left/right align string in field buffer.

```basic
LSET name$ = "John"
RSET age$ = "25"
```

#### `_WRITEFILE path$, content$` (QB64)
Write a string to a file. Overwrites if the file exists.

```basic
_WRITEFILE "output.txt", data$
```

---

### Graphics Statements

#### `SCREEN [mode][,[colorswitch]][,[apage]][,[vpage]]`
Initialize graphics mode.

```basic
SCREEN 12
SCREEN 0  ' Text mode
```

#### `CLS [mode]`
Clear screen. Mode: 0=all, 1=graphics only, 2=text only.

```basic
CLS
CLS 1
```

#### `COLOR [foreground][, background][, border]]`
Set text/drawing colors.

```basic
COLOR 15, 1
COLOR , 0  ' Background only
```

#### `LOCATE [row][, col]`
Position text cursor.

```basic
LOCATE 10, 20
```

#### `VIEW PRINT [top TO bottom]`
Set text viewport.

```basic
VIEW PRINT 1 TO 20
VIEW PRINT  ' Reset to full screen
```

#### `PSET [STEP](x, y)[, color]`
Plot pixel.

```basic
PSET (100, 200)
PSET STEP(10, 10), 15
```

#### `PRESET [STEP](x, y)`
Plot pixel with background color.

```basic
PRESET (100, 200)
```

#### `LINE [(x1, y1)]-[STEP](x2, y2)[, color][, B|BF[, style]]`
Draw line or box.

```basic
LINE (10, 10)-(100, 100)
LINE (10, 10)-(100, 100), 15, B  ' Box
LINE (10, 10)-(100, 100), 15, BF  ' Filled box
```

#### `CIRCLE [STEP](x, y), radius[, color][, start][, end][, aspect][, F]`
Draw circle or arc.

```basic
CIRCLE (320, 240), 100
CIRCLE (320, 240), 100, , , , , F  ' Filled
```

#### `PAINT [STEP](x, y)[, color][, border]`
Flood fill.

```basic
PAINT (100, 100), 15, 4
```

#### `DRAW string$`
Turtle graphics drawing commands.

```basic
DRAW "U100 R100 D100 L100"
```

#### `GET [STEP](x1, y1)-[STEP](x2, y2), array[(index)]`
Capture screen region to array.

```basic
GET (0, 0)-(100, 100), sprite()
```

#### `PUT (x, y), array[(indices)][, action]`
Draw array contents to screen.

```basic
PUT (100, 100), sprite(), PSET
PUT (100, 100), sprite(), XOR
```

Actions: `PSET`, `PRESET`, `AND`, `OR`, `XOR`

#### `VIEW [[SCREEN] (x1, y1)-(x2, y2)[, color[, border]]]`
Define graphics viewport.

```basic
VIEW (10, 10)-(200, 200)
VIEW SCREEN (10, 10)-(200, 200)
VIEW  ' Reset to full screen
```

#### `WINDOW [[SCREEN] (x1, y1)-(x2, y2)]`
Define world coordinate system.

```basic
WINDOW (0, 0)-(100, 100)
WINDOW SCREEN (0, 0)-(100, 100)  ' Inverted Y
WINDOW  ' Reset to pixel coordinates
```

#### `PALETTE [attribute, color]`
Set palette colors (for palette-based screen modes).

```basic
PALETTE 1, 15
PALETTE  ' Reset all
```

#### `PCOPY source%, dest%`
Copy screen page.

```basic
PCOPY 0, 1
```

#### `WIDTH columns[, rows]`
Set screen text width.

```basic
WIDTH 80, 25
```

#### `_DISPLAY` (QB64)
Update screen (for double-buffered graphics).

```basic
_DISPLAY
```

#### `_PUTIMAGE [(dx1,dy1)-(dx2,dy2)][, src&][, dest&][, (sx1,sy1)-(sx2,sy2)][, _SMOOTH|_STRETCH]` (QB64)
Copy image data with optional scaling.

```basic
_PUTIMAGE (0, 0)-(100, 100), img&
_PUTIMAGE , img&, , (0, 0)-(50, 50), _SMOOTH
```

#### `_SOURCE handle&` / `_DEST handle&` (QB64)
Set source/destination image for operations.

```basic
_SOURCE img&
_DEST screen&
```

#### `_PRINTSTRING (x, y), text$` (QB64)
Draw text at pixel position.

```basic
_PRINTSTRING (100, 100), "Hello"
```

#### `_AUTODISPLAY {ON|OFF}` (QB64)
Control automatic display updates.

```basic
_AUTODISPLAY OFF
' Draw operations...
_AUTODISPLAY ON
```

#### `_FREEIMAGE handle&` (QB64)
Release image buffer.

```basic
_FREEIMAGE img&
```

#### `_CONTROLCHR ON|OFF` (QB64)
Control printing of control characters.

```basic
_CONTROLCHR OFF
```

#### `_MAPUNICODE unicode_code TO character_position` (QB64)
Map Unicode codepoint to font character position.

```basic
_MAPUNICODE 8364 TO 128  ' Euro symbol
```

#### `_RESIZE ON|OFF` (QB64)
Enable/disable window resizing at runtime.

```basic
_RESIZE ON
```

---

### Audio Statements

#### `BEEP`
Play default beep sound.

```basic
BEEP
```

#### `SOUND frequency, duration`
Play tone.

```basic
SOUND 440, 18  ' A note for 1 second
```

#### `PLAY string$`
Play music using MML (Music Macro Language).

```basic
PLAY "CDEFGAB"
```

#### `_SNDCLOSE handle&` (QB64)
Close sound handle.

```basic
_SNDCLOSE sound&
```

#### `_SNDPLAY handle&` (QB64)
Play sound.

```basic
_SNDPLAY sound&
```

#### `_SNDSTOP handle&` (QB64)
Stop playing sound.

```basic
_SNDSTOP sound&
```

#### `_SNDPAUSE handle&` (QB64)
Pause sound playback.

```basic
_SNDPAUSE sound&
```

#### `_SNDLOOP handle&` (QB64)
Play sound in continuous loop.

```basic
_SNDLOOP sound&
```

#### `_SNDVOL handle&, volume!` (QB64)
Set sound volume (0.0 to 1.0).

```basic
_SNDVOL sound&, 0.5
```

#### `_SNDBAL handle&, [x!], [y!], [z!], [channel&]` (QB64)
Set stereo balance/3D position.

```basic
_SNDBAL sound&, 0.5, 0.0, 0.0
```

#### `_SNDRAW sample!` or `_SNDRAW left!, right!` (QB64)
Write raw audio samples.

```basic
_SNDRAW 0.5
_SNDRAW 0.3, 0.7  ' Stereo
```

#### `_SNDPLAYFILE filename$[, volume!][, x!][, y!][, z!]` (QB64)
Play a sound file directly.

```basic
_SNDPLAYFILE "sound.wav", 0.8
```

#### `_SNDPLAYCOPY handle&[, volume!]` (QB64)
Play a copy of a sound.

```basic
_SNDPLAYCOPY sound&, 0.5
```

#### `_SNDSETPOS handle&, position!` (QB64)
Set playback position in seconds.

```basic
_SNDSETPOS sound&, 10.5
```

---

### System Integration

#### `END [exit_code]`
End program execution with optional exit code.

```basic
END
END 0
```

#### `SYSTEM [exit_code]`
Exit program immediately.

```basic
SYSTEM
SYSTEM 1
```

#### `STOP`
Stop execution (for debugging).

```basic
STOP
```

#### `SLEEP [seconds]`
Pause execution. No argument waits for keypress.

```basic
SLEEP 5
SLEEP  ' Wait for keypress
```

#### `_DELAY seconds` (QB64)
Pause execution (float precision).

```basic
_DELAY 0.5
```

#### `_LIMIT fps` (QB64)
Limit frame rate.

```basic
_LIMIT 60
```

#### `WAIT port, and_mask[, xor_mask]`
Wait for hardware port condition (legacy).

#### `KILL filename$`
Delete a file.

```basic
KILL "temp.txt"
```

#### `NAME oldname$ AS newname$`
Rename a file.

```basic
NAME "old.txt" AS "new.txt"
```

#### `MKDIR path$`
Create a directory.

```basic
MKDIR "mydir"
```

#### `RMDIR path$`
Remove a directory.

```basic
RMDIR "mydir"
```

#### `CHDIR path$`
Change current directory.

```basic
CHDIR "C:\Users"
```

#### `SHELL [command$]`
Execute external command. No argument opens interactive shell.

```basic
SHELL "dir"
SHELL
```

#### `SHELL` (function form)
Returns exit code of command.

```basic
result% = SHELL("myprogram.exe")
```

#### `_SHELLHIDE command$` (QB64)
Execute command without showing console.

```basic
_SHELLHIDE "myprogram.exe"
```

#### `BLOAD filename$[, address]`
Load binary file to memory.

```basic
BLOAD "sprite.dat"
```

#### `BSAVE filename$, address, length`
Save memory to binary file.

```basic
BSAVE "sprite.dat", VARPTR(sprite(0)), 1000
```

#### `SETMEM bytes`
Set available memory for BASIC strings (legacy, no-op).

#### `CALL ABSOLUTE address` (Legacy)
Call machine language routine (unsafe, generates warning).

---

### Error Handling

#### `ON ERROR GOTO label` / `ON ERROR GOTO 0`
Enable error trapping or disable it.

```basic
ON ERROR GOTO errorHandler
' Code that might error...
ON ERROR GOTO 0  ' Disable
```

#### `ON ERROR RESUME NEXT`
Continue execution at next statement after error.

```basic
ON ERROR RESUME NEXT
```

#### `RESUME [NEXT | label]`
Resume execution after error handling.

```basic
errorHandler:
    PRINT "Error occurred"
    RESUME NEXT
```

#### `ERROR code`
Simulate an error with specified code.

```basic
ERROR 5
```

---

### Event Handling (QB4.5)

#### `ON KEY(n) GOSUB label`
Set up key event handler.

```basic
ON KEY(1) GOSUB f1Handler
```

#### `KEY(n) ON|OFF|STOP`
Enable/disable/suspend key event trapping.

```basic
KEY(1) ON
```

#### `ON TIMER(seconds) GOSUB label`
Set up timer event handler.

```basic
ON TIMER(1) GOSUB timerHandler
```

#### `TIMER ON|OFF|STOP`
Enable/disable/suspend timer events.

```basic
TIMER ON
```

#### `ON STRIG(n) GOSUB label` / `STRIG(n) ON|OFF|STOP`
Joystick trigger events.

#### `ON COM(n) GOSUB label` / `COM(n) ON|OFF|STOP`
Serial port events.

#### `ON PEN GOSUB label` / `PEN ON|OFF|STOP`
Light pen events (legacy).

#### `ON UEVENT GOSUB label` / `UEVENT ON|OFF|STOP` / `UEVENT`
User-defined events.

#### `ON SIGNAL(n) GOSUB label` / `SIGNAL(n) ON|OFF|STOP`
System signal events.

---

### Mouse Input (QB64)

#### `_MOUSEHIDE` / `_MOUSESHOW`
Hide/show mouse cursor.

```basic
_MOUSEHIDE
_MOUSESHOW
```

#### `_MOUSEMOVE x%, y%`
Move mouse cursor to position.

```basic
_MOUSEMOVE 100, 200
```

---

### Clipboard (QB64)

#### `_CLIPBOARD$ = text$` or `_CLIPBOARD text$`
Set clipboard contents.

```basic
_CLIPBOARD$ = "Hello"
_CLIPBOARD "Hello"
```

#### `_ACCEPTFILEDROP [ON|OFF]` (QB64)
Enable/disable file drop acceptance.

```basic
_ACCEPTFILEDROP ON
```

#### `_FINISHDROP` (QB64)
Finish processing dropped files.

```basic
_FINISHDROP
```

---

### Window/Desktop (QB64)

#### `_TITLE text$`
Set window title.

```basic
_TITLE "My Program"
```

#### `_SCREENMOVE x%, y%` or `_SCREENMOVE _MIDDLE`
Move window.

```basic
_SCREENMOVE 100, 200
_SCREENMOVE _MIDDLE
```

#### `_FULLSCREEN [_SQUAREPIXELS|_STRETCH|_OFF]`
Control fullscreen mode.

```basic
_FULLSCREEN _SQUAREPIXELS
_FULLSCREEN _OFF
```

#### `_SCREENHIDE` / `_SCREENSHOW`
Hide/show graphics window.

```basic
_SCREENHIDE
_SCREENSHOW
```

#### `_ICON [handle&]`
Set window icon from image handle.

```basic
_ICON iconHandle&
```

#### `_CONSOLETITLE text$`
Set console window title.

```basic
_CONSOLETITLE "Console"
```

#### `_CONSOLE ON|OFF`
Show/hide console window.

```basic
_CONSOLE ON
```

#### `_CONSOLECURSOR visible%` (QB64)
Set console cursor visibility.

```basic
_CONSOLECURSOR 1  ' Show cursor
```

#### `_CONSOLEFONT font$, size%` (QB64)
Set console font.

```basic
_CONSOLEFONT "Courier New", 12
```

#### `_ECHO text$` (QB64)
Echo text to console (statement form).

```basic
_ECHO "Debug message"
```

#### `_ASSERT condition [, message$]`
Debug assertion (when `$ASSERTS` enabled).

```basic
_ASSERT x% > 0, "x must be positive"
```

#### `_SAVEIMAGE filename$, handle&` (QB64)
Save image to file.

```basic
_SAVEIMAGE "screenshot.png", img&
```

#### `_SCREENPRINT text$` (QB64)
Print text to screen buffer.

```basic
_SCREENPRINT "Hello"
```

#### `_UPRINTSTRING x&, y&, text$` (QB64)
Print Unicode text at pixel position.

```basic
_UPRINTSTRING 100, 200, "Hello"
```

#### `_SETALPHA alpha%, color1&, color2&` (QB64)
Set alpha blending between two colors.

```basic
_SETALPHA 128, color1&, color2&
```

#### `_COPYPALETTE srcHandle&, destHandle&` (QB64)
Copy palette from one image to another.

```basic
_COPYPALETTE src&, dest&
```

#### `_BLEND handle&` / `_DONTBLEND handle&` (QB64)
Enable/disable alpha blending for image.

```basic
_BLEND img&
_DONTBLEND img&
```

#### `_CLEARCOLOR color&, handle&` (QB64)
Set transparent color for image.

```basic
_CLEARCOLOR _RGB(255, 0, 255), img&
```

#### `_DEPTHBUFFER mode%` (QB64)
Set depth buffer mode.

```basic
_DEPTHBUFFER 1
```

#### `_DISPLAYORDER layer1&, layer2&, layer3&, layer4&` (QB64)
Set display layer order.

```basic
_DISPLAYORDER 1, 2, 3, 4
```

#### `_SNDLIMIT handle&, seconds!` (QB64)
Limit sound playback duration.

```basic
_SNDLIMIT sound&, 5.0
```

#### `_HIDE` / `_SHOW` (QB64)
Hide/show window.

```basic
_HIDE
_SHOW
```

#### `_ONTOP mode%` (QB64)
Set window always-on-top mode.

```basic
_ONTOP 1
```

#### `_PRINTMODE mode%` (QB64)
Set print mode (background handling).

```basic
_PRINTMODE _KEEPBACKGROUND
```

#### `_LOGTRACE message$` / `_LOGINFO message$` / `_LOGWARN message$` / `_LOGERROR message$` (QB64)
Logging statements.

```basic
_LOGTRACE "Debug info"
_LOGERROR "Error occurred"
```

#### `_LOGMINLEVEL level&` (QB64)
Set minimum log level.

```basic
_LOGMINLEVEL 2
```

#### `_SNDRAWBATCH handle&, samples~%%, count&` (QB64)
Write batch of raw audio samples.

```basic
_SNDRAWBATCH raw&, samples~%%, 1000
```

#### `_MIDISOUNDBANK filename$` (QB64)
Set MIDI soundbank file.

```basic
_MIDISOUNDBANK "soundfont.sf2"
```

#### `_NEWHANDLER callback~&&` (QB64)
Set new event handler callback.

```basic
_NEWHANDLER myCallback~&&
```

#### `_PRINTIMAGE handle&` (QB64)
Print image to printer.

```basic
_PRINTIMAGE img&
```

#### `_CLEAR resource&` (QB64)
Clear specific resource.

```basic
_CLEAR img&
```

#### `_TOGGLE setting&` (QB64)
Toggle a setting.

```basic
_TOGGLE _AUTODISPLAY
```

#### `_MAPTRIANGLE src_x1!, src_y1!, src_x2!, src_y2!, src_x3!, src_y3!, dst_x1!, dst_y1!, dst_x2!, dst_y2!, dst_x3!, dst_y3!` (QB64)
Map triangle from source to destination coordinates (3D transformation).

```basic
_MAPTRIANGLE 0, 0, 100, 0, 50, 100, 0, 0, 200, 0, 100, 200
```

#### `_GLRENDER mode&` (QB64)
Set OpenGL render mode (stub - QB64Fresh uses SDL2).

```basic
_GLRENDER 1
```

#### `_MEMPUT block~%%, offset~&&, value` (QB64)
Put value into memory block (statement form).

```basic
_MEMPUT mem~%%, 0, 42 AS INTEGER
```

#### `_MEMFILL block~%%, offset~&&, size&, value` (QB64)
Fill memory block with value (statement form).

```basic
_MEMFILL mem~%%, 0, 100, 0 AS _BYTE
```

#### `_MEMCOPY src~%%, srcoff~&&, bytes&, dst~%%, dstoff~&&` (QB64)
Copy memory block (statement form).

```basic
_MEMCOPY src~%%, 0, 100, dest~%%, 0
```

#### `_MEMFREE block~%%` (QB64)
Free memory block (statement form).

```basic
_MEMFREE mem~%%
```

#### `_SCREENICON` (QB64)
Minimize window to taskbar.

```basic
_SCREENICON
```

---

### Procedures

#### `SUB name [(parameters)] ... END SUB`
Define a subroutine.

```basic
SUB PrintMessage (msg AS STRING)
    PRINT msg
END SUB
```

#### `FUNCTION name [(parameters)] ... END FUNCTION`
Define a function.

```basic
FUNCTION Add% (a AS INTEGER, b AS INTEGER)
    Add% = a + b
END FUNCTION
```

#### `CALL SubName(args)` or `SubName args`
Call a subroutine.

```basic
CALL PrintMessage("Hello")
PrintMessage "Hello"
```

#### `DECLARE SUB name [(parameters)]`
Forward declaration of subroutine.

#### `DECLARE FUNCTION name [(parameters)] [AS type]`
Forward declaration of function.

#### `DEF FNname[(parameters)] = expression` (Single-line)
Define user-defined function.

```basic
DEF FNdouble(x) = x * 2
```

#### `DEF FNname[(parameters)] ... END DEF` (Multi-line, QB64)
Multi-line user-defined function.

```basic
DEF FNfactorial(n)
    IF n <= 1 THEN
        FNfactorial = 1
    ELSE
        FNfactorial = n * FNfactorial(n - 1)
    END IF
END DEF
```

---

### User-Defined Types

#### `TYPE TypeName [CUSTOMTYPE] ... END TYPE`
Define a user-defined type.

```basic
TYPE Person
    name AS STRING * 20
    age AS INTEGER
    score AS DOUBLE
END TYPE

TYPE CStruct CUSTOMTYPE
    value AS LONG
END TYPE
```

---

### Data Statements

#### `DATA value1, value2, ...`
Define compile-time data pool.

```basic
DATA 10, 20, 30, "Hello", "World"
```

#### `READ var1, var2, ...`
Read from DATA pool.

```basic
READ x%, y%, z%, a$, b$
```

#### `RESTORE [label]`
Reset DATA pointer.

```basic
RESTORE
RESTORE startData
```

---

### Other Statements

#### `RANDOMIZE [seed]` or `RANDOMIZE TIMER`
Seed random number generator.

```basic
RANDOMIZE TIMER
RANDOMIZE 12345
```

#### `ERASE arrayname [, arrayname...]`
Clear/deallocate arrays.

```basic
ERASE arr()
```

#### `_KEYCLEAR` (QB64)
Clear keyboard buffer.

```basic
_KEYCLEAR
```

#### `DEF SEG [= segment]`
Set memory segment for PEEK/POKE (legacy).

#### `POKE address, value`
Write byte to memory (legacy, uses emulated memory).

```basic
POKE 1000, 65
```

#### `OUT port, value`
Write byte to I/O port (legacy, may be sandboxed).

#### `INTERRUPT intnum, inregs, outregs` (Legacy)
Call system interrupt (compatibility only).

#### `INTERRUPTX intnum, inregs, outregs` (Legacy)
Extended interrupt call.

#### `IOCTL [#]filenum, string$`
Send device control string.

#### `FREE`
Free unused string space (no-op in modern systems).

#### `CLEAR [stack_size]`
Clear all variables.

```basic
CLEAR
```

#### `RESET`
Close all open files.

```basic
RESET
```

#### `RUN [filename$]` or `RUN [linenumber]`
Restart program or run another program.

```basic
RUN
RUN "other.bas"
```

#### `CHAIN filename$`
Run another program, optionally passing COMMON variables.

```basic
CHAIN "next.bas"
```

#### `TRON` / `TROFF`
Enable/disable trace mode for debugging.

```basic
TRON
' Code to trace...
TROFF
```

#### `LPRINT [expression [{;|,} expression]...]`
Print to printer (LPT1).

```basic
LPRINT "Report"
```

#### `FILES [filespec$]`
Display directory listing.

```basic
FILES
FILES "*.bas"
```

---

### Preprocessor Directives

#### `$INCLUDE: 'filename'`
Include another source file.

```basic
$INCLUDE: 'common.bi'
```

#### `$IF condition THEN ... $ELSEIF ... $ELSE ... $END IF`
Conditional compilation.

```basic
$IF WIN THEN
    PRINT "Windows"
$ELSEIF LINUX THEN
    PRINT "Linux"
$END IF
```

#### `$LET variable = value`
Compile-time variable assignment.

```basic
$LET DEBUG = -1
```

#### `$CHECKING:ON` / `$CHECKING:OFF`
Bounds checking control.

```basic
$CHECKING:OFF
```

#### `$CONSOLE` / `$CONSOLE:ONLY`
Enable console window.

```basic
$CONSOLE
$CONSOLE:ONLY
```

#### `$SCREENHIDE` / `$SCREENSHOW`
Hide/show graphics window on startup.

#### `$DYNAMIC` / `$STATIC`
Array allocation mode.

```basic
$DYNAMIC
```

#### `$DEBUG`
Enable debug mode.

#### `$INCLUDEONCE`
Include file only once.

#### `$EXEICON:'filename'`
Set executable icon.

#### `$VERSIONINFO:key=value`
Set version info for executable.

#### `$ERROR message`
Generate compiler error.

```basic
$ERROR "This feature is not supported"
```

#### `$EMBED:'filename'`
Embed file into executable.

#### `$MIDISOUNDFONT:'file.sf2'`
Set MIDI soundfont file.

#### `$UNSTABLE:feature`
Enable experimental feature.

#### `$FORMAT`
Code formatting directive (no-op, for IDE support).

#### `$USELIBRARY:'library'`
Use external library.

#### `$ASSERTS`
Enable assertion checking.

#### `$NOPREFIX`
Allow QB64 keywords without underscore prefix.

#### `$COLOR [depth]`
Include named color constants.

```basic
$COLOR:32
```

#### `$RESIZE:ON` / `$RESIZE:OFF` / `$RESIZE:STRETCH` / `$RESIZE:SMOOTH`
Window resize event control.

---

### C Library Integration

#### `DECLARE LIBRARY "name" ... END DECLARE`
Declare external C functions from static library.

```basic
DECLARE LIBRARY "mylib"
    FUNCTION add_values& (BYVAL a AS LONG, BYVAL b AS LONG)
    SUB print_message (BYVAL msg AS STRING)
END DECLARE
```

#### `DECLARE DYNAMIC LIBRARY "name" ... END DECLARE`
Declare external C functions from dynamic library (loaded at runtime).

```basic
DECLARE DYNAMIC LIBRARY "mydll"
    FUNCTION get_value% () AS INTEGER
END DECLARE
```

#### `DECLARE STATIC LIBRARY "name" ... END DECLARE`
Declare external C functions from a static library (linked at compile time). Syntax otherwise matches `DECLARE LIBRARY`.

#### `_PROCPTR(procedureName)` (QB64) — Callbacks
Returns a procedure pointer for use as a C callback (e.g. `qsort`). Supported signatures: BASIC function with two `LONG` or `INTEGER` parameters, returning `INTEGER` or `LONG`, matching `int (*)(const void*, const void*)`. For other callback shapes, implement in C and link. See [ADR-0008](adrs/ADR-0008-c-interoperability.md).

---

## Functions

### String Functions

#### `LEN(string$ | variable)`
Returns length of string or size of UDT/fixed-length type.

```basic
length% = LEN("Hello")  ' Returns 5
size% = LEN(myUDT)  ' Returns size in bytes
```

#### `CHR$(code%)`
Returns character for ASCII code (0-255).

```basic
char$ = CHR$(65)  ' Returns "A"
```

#### `ASC(string$[, position%])`
Returns ASCII code of character. Position defaults to 1.

```basic
code% = ASC("A")  ' Returns 65
code% = ASC("Hello", 2)  ' Returns 101 ('e')
```

#### `LEFT$(string$, n%)`
Returns leftmost n characters.

```basic
result$ = LEFT$("Hello", 3)  ' Returns "Hel"
```

#### `RIGHT$(string$, n%)`
Returns rightmost n characters.

```basic
result$ = RIGHT$("Hello", 3)  ' Returns "llo"
```

#### `MID$(string$, start%[, length%])`
Returns substring. If length omitted, returns rest of string.

```basic
result$ = MID$("Hello", 2, 3)  ' Returns "ell"
result$ = MID$("Hello", 2)  ' Returns "ello"
```

#### `INSTR([start%,] string$, search$)`
Returns position of search$ within string$. Returns 0 if not found.

```basic
pos% = INSTR("Hello", "ll")  ' Returns 3
pos% = INSTR(4, "Hello", "l")  ' Returns 4
```

#### `_INSTRREV([start%,] source$, search$)` (QB64)
Reverse search from end of string.

```basic
pos% = _INSTRREV("Hello", "l")  ' Returns 4
```

#### `UCASE$(string$)`
Converts string to uppercase.

```basic
result$ = UCASE$("Hello")  ' Returns "HELLO"
```

#### `LCASE$(string$)`
Converts string to lowercase.

```basic
result$ = LCASE$("Hello")  ' Returns "hello"
```

#### `LTRIM$(string$)`
Removes leading spaces.

```basic
result$ = LTRIM$("  Hello")  ' Returns "Hello"
```

#### `RTRIM$(string$)`
Removes trailing spaces.

```basic
result$ = RTRIM$("Hello  ")  ' Returns "Hello"
```

#### `TRIM$(string$)`
Removes both leading and trailing spaces.

```basic
result$ = TRIM$("  Hello  ")  ' Returns "Hello"
```

#### `_TRIM$(string$)` (QB64)
Same as TRIM$ (QB64 extension).

#### `STR$(number)`
Converts number to string representation.

```basic
result$ = STR$(123)  ' Returns " 123" (note leading space)
```

#### `VAL(string$)`
Converts string to numeric value.

```basic
value! = VAL("123.45")  ' Returns 123.45
```

#### `STRING$(n%, char)`
Returns string of n copies of character. Char can be code% or char$.

```basic
result$ = STRING$(5, 65)  ' Returns "AAAAA"
result$ = STRING$(5, "A")  ' Returns "AAAAA"
```

#### `SPACE$(n%)`
Returns string of n spaces.

```basic
result$ = SPACE$(5)  ' Returns "     "
```

#### `HEX$(number&)`
Converts number to hexadecimal string.

```basic
result$ = HEX$(255)  ' Returns "FF"
```

#### `OCT$(number&)`
Converts number to octal string.

```basic
result$ = OCT$(64)  ' Returns "100"
```

#### `_BIN$(number&)` (QB64)
Converts number to binary string.

```basic
result$ = _BIN$(5)  ' Returns "101"
```

#### `_TOSTR$(number)` (QB64)
Converts number to string (no leading space).

```basic
result$ = _TOSTR$(123)  ' Returns "123"
```

#### `_STRCMP(string1$, string2$)` (QB64)
String comparison. Returns <0, 0, or >0.

```basic
result& = _STRCMP("a", "b")  ' Returns negative
```

#### `_STRICMP(string1$, string2$)` (QB64)
Case-insensitive string comparison.

```basic
result& = _STRICMP("Hello", "HELLO")  ' Returns 0
```

#### `MKI$(integer%)` / `MKL$(long&)` / `MKS$(single!)` / `MKD$(double#)`
Convert numeric types to binary string format.

```basic
str$ = MKI$(12345)
```

#### `CVI(string$)` / `CVL(string$)` / `CVS(string$)` / `CVD(string$)`
Convert binary string format to numeric types.

```basic
value% = CVI(str$)
```

#### `CVSMBF(string$)` / `CVDMBF(string$)` / `MKSMBF$(single!)` / `MKDMBF$(double#)`
Microsoft Binary Format conversions.

---

### Math Functions

#### `ABS(number)`
Returns absolute value.

```basic
result! = ABS(-5)  ' Returns 5
```

#### `SGN(number)`
Returns sign: -1 (negative), 0 (zero), 1 (positive).

```basic
result% = SGN(-5)  ' Returns -1
```

#### `INT(number)`
Truncates toward negative infinity.

```basic
result& = INT(-3.7)  ' Returns -4
```

#### `FIX(number)`
Truncates toward zero.

```basic
result& = FIX(-3.7)  ' Returns -3
```

#### `CINT(number)` / `CLNG(number)` / `CSNG(number)` / `CDBL(number)`
Type conversions with rounding.

```basic
result% = CINT(3.7)  ' Returns 4
```

#### `SQR(number)`
Square root.

```basic
result! = SQR(16)  ' Returns 4
```

#### `LOG(number)`
Natural logarithm (base e).

```basic
result! = LOG(2.71828)  ' Returns ~1
```

#### `EXP(power)`
e raised to power.

```basic
result! = EXP(1)  ' Returns ~2.71828
```

#### `SIN(radians)` / `COS(radians)` / `TAN(radians)`
Trigonometric functions.

```basic
result! = SIN(_PI / 2)  ' Returns 1
```

#### `ATN(number)`
Arctangent (returns radians).

```basic
result! = ATN(1)  ' Returns ~0.785 (π/4)
```

#### `_PI` (QB64)
Returns π (pi).

```basic
circumference! = 2 * _PI * radius!
```

#### `_ASIN(number)` / `_ACOS(number)` / `_ATAN2(y, x)` (QB64)
Inverse trigonometric functions.

```basic
angle! = _ASIN(0.5)
angle! = _ATAN2(y!, x!)
```

#### `_HYPOT(x, y)` (QB64)
Hypotenuse: √(x² + y²).

```basic
dist! = _HYPOT(3, 4)  ' Returns 5
```

#### `_CEIL(number)` / `_ROUND(number)` (QB64)
Ceiling and rounding functions.

```basic
result& = _CEIL(3.2)  ' Returns 4
result& = _ROUND(3.7)  ' Returns 4
```

#### `_MIN(a, b)` / `_MAX(a, b)` (QB64)
Minimum and maximum.

```basic
result! = _MIN(5, 10)  ' Returns 5
```

#### `_CLAMP(value, min, max)` (QB64)
Clamp value between min and max.

```basic
result! = _CLAMP(x!, 0, 100)
```

#### `_SINH(x)` / `_COSH(x)` / `_TANH(x)` (QB64)
Hyperbolic functions.

#### `_ASINH(x)` / `_ACOSH(x)` / `_ATANH(x)` (QB64)
Inverse hyperbolic functions.

#### `_SEC(x)` / `_CSC(x)` / `_COT(x)` (QB64)
Reciprocal trigonometric functions.

#### `_SECH(x)` / `_CSCH(x)` / `_COTH(x)` (QB64)
Hyperbolic reciprocals.

#### `_ARCSEC(x)` / `_ARCCSC(x)` / `_ARCCOT(x)` (QB64)
Inverse reciprocal trigonometric functions.

#### `_ARCSECH(x)` / `_ARCCSCH(x)` / `_ARCCOTH(x)` (QB64)
Inverse hyperbolic reciprocals.

#### `_D2R(degrees)` / `_R2D(radians)` (QB64)
Degree/radian conversions.

```basic
radians! = _D2R(90)  ' Returns π/2
```

#### `_D2G(degrees)` / `_G2D(gradians)` / `_G2R(gradians)` / `_R2G(radians)` (QB64)
Gradian conversions.

#### `_NEGATE(number)` (QB64)
Negate a number.

```basic
result! = _NEGATE(5)  ' Returns -5
```

#### `RND[(n)]`
Random number between 0 and 1. RND(0) returns last value, RND(negative) reseeds.

```basic
value! = RND  ' Next random
value! = RND(1)  ' Same as RND
last! = RND(0)  ' Last value
RANDOMIZE -5  ' Reseed
```

---

### Bitwise Operations (QB64)

#### `_SHL(value&, bits%)` / `_SHR(value&, bits%)`
Shift left/right.

```basic
result& = _SHL(1, 3)  ' Returns 8
```

#### `_ROL(value&, bits%)` / `_ROR(value&, bits%)`
Rotate left/right.

#### `_READBIT(value&, bit%)` / `_SETBIT(value&, bit%)` / `_RESETBIT(value&, bit%)` / `_TOGGLEBIT(value&, bit%)`
Bit manipulation.

```basic
bit% = _READBIT(value&, 3)
result& = _SETBIT(value&, 5)
```

---

### Array Functions

#### `LBOUND(array[, dimension%])`
Returns lower bound of array dimension.

```basic
lower% = LBOUND(arr())
lower% = LBOUND(arr(), 2)  ' Second dimension
```

#### `UBOUND(array[, dimension%])`
Returns upper bound of array dimension.

```basic
upper% = UBOUND(arr())
upper% = UBOUND(arr(), 2)
```

---

### Graphics Functions

#### `POINT(x%, y%)` or `POINT(function%)`
Get pixel color or cursor coordinates.

```basic
color& = POINT(100, 200)  ' Pixel color
x! = POINT(0)  ' Current logical X
y! = POINT(1)  ' Current logical Y
```

#### `_RGB(red%, green%, blue%[, imageHandle&])` (QB64)
Create color value from RGB components.

```basic
color& = _RGB(255, 128, 0)
```

#### `_RGB32(red%, green%, blue%[, alpha%])` (QB64)
Create 32-bit color value.

```basic
color& = _RGB32(255, 128, 0, 255)
```

#### `_RGBA(red%, green%, blue%, alpha%[, imageHandle&])` (QB64)
Create color with explicit alpha.

```basic
color& = _RGBA(255, 128, 0, 128)
```

#### `_RGBA32(red%, green%, blue%, alpha%)` (QB64)
32-bit RGBA color.

#### `_RED(color&[, imageHandle&])` / `_GREEN(color&[, imageHandle&])` / `_BLUE(color&[, imageHandle&])` / `_ALPHA(color&[, imageHandle&])` (QB64)
Extract color components (0-255).

```basic
red% = _RED(color&)
```

#### `_RED32(color&[, imageHandle&])` / `_GREEN32(color&[, imageHandle&])` / `_BLUE32(color&[, imageHandle&])` / `_ALPHA32(color&[, imageHandle&])` (QB64)
32-bit variants (same functionality).

#### `_HSB32(hue!, saturation!, brightness!)` / `_HSBA32(hue!, saturation!, brightness!, alpha!)` (QB64)
Create color from HSB/HSBA values (hue: 0-360, saturation/brightness/alpha: 0-100).

```basic
color& = _HSB32(120, 100, 50)  ' Green
color& = _HSBA32(120, 100, 50, 80)  ' Green with alpha
```

#### `_HUE32(color&)` / `_SATURATION32(color&)` / `_BRIGHTNESS32(color&)` (QB64)
Extract HSB components from color.

```basic
hue! = _HUE32(color&)
```

#### `_PALETTECOLOR(attr%[, value&, handle&])` (QB64)
Get/set palette color. With 1 arg: get from current screen. With 2 args: get from image. With 3 args: set palette color.

```basic
color& = _PALETTECOLOR(1)  ' Get
color& = _PALETTECOLOR(1, img&)  ' Get from image
_PALETTECOLOR 1, color&, img&  ' Set (statement form)
```

#### `_NEWIMAGE(width%, height%[, mode%])` (QB64)
Create new image. Mode: 0=current, 32=32-bit, 256=256-color.

```basic
img& = _NEWIMAGE(800, 600, 32)
```

#### `_LOADIMAGE(filename$[, mode%])` (QB64)
Load image file. Supports BMP, PNG, JPG, GIF.

```basic
img& = _LOADIMAGE("sprite.png", 32)
```

#### `_COPYIMAGE(source&[, mode%])` (QB64)
Copy image.

```basic
copy& = _COPYIMAGE(img&, 32)
```

#### `_SCREENIMAGE[(x1, y1, x2, y2)]` (QB64)
Capture screen (or region) to an image. With no arguments, captures the full screen. With four coordinates, captures the given rectangle.

```basic
img& = _SCREENIMAGE
img& = _SCREENIMAGE(0, 0, 639, 479)
```

#### `_WIDTH[(imageHandle&)]` / `_HEIGHT[(imageHandle&)]` (QB64)
Get image or screen dimensions.

```basic
w& = _WIDTH  ' Screen width
w& = _WIDTH(img&)  ' Image width
```

#### `_PIXELSIZE[(imageHandle&)]` (QB64)
Returns bytes per pixel (0=text, 1=256-color, 4=32-bit).

```basic
size% = _PIXELSIZE
```

#### `_SCREENEXISTS` (QB64)
Returns -1 if graphics window exists, 0 otherwise.

```basic
exists% = _SCREENEXISTS
```

#### `_SMOOTH` / `_SMOOTHSHRUNK` / `_SMOOTHSTRETCHED` / `_HARDWARE` / `_HARDWARE1` / `_SOFTWARE` (QB64)
Graphics rendering mode constants/functions.

```basic
mode& = _SMOOTH
```

#### `_ANTICLOCKWISE` / `_CLOCKWISE` (QB64)
Graphics direction constants.

```basic
dir& = _CLOCKWISE
```

#### `_KEEPBACKGROUND` / `_FILLBACKGROUND` / `_ONLYBACKGROUND` (QB64)
Print mode constants.

```basic
mode& = _KEEPBACKGROUND
```

#### `_MIDDLE` (QB64)
Alignment constant (for _SCREENMOVE).

```basic
_SCREENMOVE _MIDDLE
```

#### `_AUTO` (QB64)
Auto display mode constant.

```basic
mode& = _AUTO
```

#### `_CLIP` / `_STRETCH` / `_SEAMLESS` / `_SQUAREPIXELS` / `_BEHIND` (QB64)
Graphics keyword constants.

```basic
mode& = _CLIP
```

#### `_ALL` / `_BLINK` / `_OFF` / `_ONLY` (QB64)
Type/mode keyword constants.

```basic
mode& = _ALL
```

#### `_WAVE` (QB64)
Sound keyword constant.

```basic
mode& = _WAVE
```

#### `_DONTWAIT` (QB64)
Network keyword constant.

```basic
mode& = _DONTWAIT
```

#### `_DEPTHBUFFER` (QB64)
Returns current depth buffer mode.

```basic
mode& = _DEPTHBUFFER
```

#### `_ANTIALIASING` (QB64)
Returns antialiasing mode.

```basic
mode& = _ANTIALIASING
```

#### `_DISPLAYORDER` (QB64)
Returns current display order mode.

```basic
mode& = _DISPLAYORDER
```

#### `_GLCOMPAT` (QB64)
Returns OpenGL compatibility mode.

```basic
mode& = _GLCOMPAT
```

#### `_FULLSCREENSMOOTH` / `_ALLOWFULLSCREEN` (QB64)
Fullscreen mode functions.

```basic
mode& = _FULLSCREENSMOOTH
```

#### `_DISPLAYWIDTH` / `_DISPLAYHEIGHT` (QB64)
Returns display dimensions.

```basic
w& = _DISPLAYWIDTH
h& = _DISPLAYHEIGHT
```

#### `PMAP(coordinate!, function_code%)`
Map between world and screen coordinates.

```basic
screenX! = PMAP(worldX!, 0)  ' World X to screen X
worldX! = PMAP(screenX!, 2)  ' Screen X to world X
```

#### `SCREEN(row%, col%[, flag%])`
Read character/attribute from text screen.

```basic
char% = SCREEN(10, 20)  ' ASCII value
attr% = SCREEN(10, 20, 1)  ' Color attribute
```

#### `_DEFAULTCOLOR[(imageHandle&)]` / `_BACKGROUNDCOLOR[(imageHandle&)]` (QB64)
Get default/background colors.

```basic
color& = _DEFAULTCOLOR
```

---

### Input Functions

#### `INKEY$`
Returns character from keyboard buffer without waiting. Returns empty string if no key.

```basic
key$ = INKEY$
```

#### `INPUT$(n%[, filenum%])`
Read n characters from keyboard or file.

```basic
chars$ = INPUT$(5)  ' Keyboard
chars$ = INPUT$(10, #1)  ' File
```

#### `_KEYHIT` (QB64)
Returns keycode of pressed key, including extended keys.

```basic
key& = _KEYHIT
```

#### `_KEYDOWN(keycode&)` (QB64)
Returns -1 if key is currently held down, 0 otherwise.

```basic
pressed% = _KEYDOWN(32)  ' Space bar
```

#### `_CINP` (QB64)
Returns character code from console input.

```basic
code& = _CINP
```

#### `_CAPSLOCK` / `_NUMLOCK` / `_SCROLLLOCK` (QB64)
Returns lock key state (-1=on, 0=off).

```basic
on% = _CAPSLOCK
```

#### `POS(n%)`
Returns current print position (column).

```basic
col% = POS(0)
```

#### `CSRLIN`
Returns current cursor row.

```basic
row% = CSRLIN
```

#### `_MOUSEX` / `_MOUSEY` (QB64)
Returns current mouse coordinates.

```basic
x% = _MOUSEX
y% = _MOUSEY
```

#### `_MOUSEBUTTON(button%)` (QB64)
Returns -1 if mouse button is pressed. 1=left, 2=right, 3=middle.

```basic
pressed% = _MOUSEBUTTON(1)
```

#### `_MOUSEINPUT` (QB64)
Returns -1 if mouse event occurred, 0 otherwise.

```basic
if _MOUSEINPUT THEN
    ' Handle mouse event
END IF
```

#### `_MOUSEMOVEMENTX` / `_MOUSEMOVEMENTY` / `_MOUSEWHEEL` (QB64)
Mouse movement and wheel deltas.

```basic
dx% = _MOUSEMOVEMENTX
wheel% = _MOUSEWHEEL
```

#### `_MOUSEHIDDEN` (QB64)
Returns -1 if mouse is hidden, 0 if visible.

---

### File Functions

#### `FREEFILE`
Returns next available file number.

```basic
fnum% = FREEFILE
```

#### `EOF(filenum%)`
Returns -1 if at end of file, 0 otherwise.

```basic
IF EOF(1) THEN CLOSE #1
```

#### `LOF(filenum%)`
Returns length of open file in bytes.

```basic
size& = LOF(1)
```

#### `LOC(filenum%)`
Returns current position in open file.

```basic
pos& = LOC(1)
```

#### `SEEK(filenum%)`
Returns current file position (for SEEK statement).

```basic
pos& = SEEK(1)
```

#### `FILEATTR(filenum%, attribute%)`
Returns file mode or handle attributes.

```basic
mode% = FILEATTR(1, 1)
```

#### `_FILEEXISTS(filename$)` (QB64)
Returns -1 if file exists, 0 otherwise.

```basic
IF _FILEEXISTS("data.txt") THEN
    ' File exists
END IF
```

#### `_DIREXISTS(path$)` (QB64)
Returns -1 if directory exists, 0 otherwise.

```basic
IF _DIREXISTS("mydir") THEN
    ' Directory exists
END IF
```

#### `_DIR$(spec$)` (QB64)
Returns first file matching spec (supports wildcards).

```basic
file$ = _DIR$("*.bas")
```

#### `_FILES$(pattern$)` (QB64)
File listing iterator (returns next file matching pattern).

```basic
file$ = _FILES$("*.bas")
```

#### `_EMBEDDED$(name$)` (QB64)
Returns embedded file data as string.

```basic
data$ = _EMBEDDED$("resource.dat")
```

#### `_READFILE$(path$)` (QB64)
Read entire file as string.

```basic
content$ = _READFILE$("data.txt")
```

#### `_FULLPATH$(path$)` (QB64)
Returns full absolute path.

```basic
full$ = _FULLPATH$("data.txt")
```

---

### System Functions

#### `TIMER[(accuracy!)]`
Returns seconds since midnight as single. Optional accuracy parameter (QB64).

```basic
time! = TIMER
```

#### `DATE$`
Returns current date as string (MM-DD-YYYY format).

```basic
today$ = DATE$
```

#### `TIME$`
Returns current time as string (HH:MM:SS format).

```basic
now$ = TIME$
```

#### `_DATE$` / `_TIME$` (QB64)
Extended date/time functions.

```basic
date$ = _DATE$
```

#### `COMMAND$[(index%)]`
Returns command line arguments. No argument returns entire command line.

```basic
arg$ = COMMAND$(1)  ' First argument
all$ = COMMAND$  ' Entire command line
```

#### `_COMMANDCOUNT` (QB64)
Returns number of command line arguments.

```basic
count& = _COMMANDCOUNT
```

#### `ENVIRON$(name$)`
Returns value of environment variable.

```basic
path$ = ENVIRON$("PATH")
```

#### `_ENVIRONCOUNT` (QB64)
Returns number of environment variables.

```basic
count& = _ENVIRONCOUNT
```

#### `_CWD$` (QB64)
Returns current working directory.

```basic
dir$ = _CWD$
```

#### `_CONSOLEINPUT` (QB64)
Returns -1 if console input is available.

```basic
IF _CONSOLEINPUT THEN
    ' Console input available
END IF
```

#### `_CONSOLETITLE$` (QB64)
Returns current console title.

```basic
title$ = _CONSOLETITLE$
```

#### `_CONSOLE` (QB64)
Returns console handle or state.

```basic
handle& = _CONSOLE
```

#### `_SHELLHIDE(command$)` (QB64)
Execute command without showing console, returns exit code (function form).

```basic
code& = _SHELLHIDE("myprogram.exe")
```

#### `_SCREENBUFFER` (QB64)
Returns screen buffer handle.

```basic
buffer& = _SCREENBUFFER
```

#### `_SCINKEY$` (QB64)
Returns console input key.

```basic
key$ = _SCINKEY$
```

#### `_CONNECTIONADDRESS(handle&)` / `_CONNECTIONADDRESS$(handle&)` (QB64)
Returns connection address (numeric or string).

```basic
addr& = _CONNECTIONADDRESS(conn&)
addr$ = _CONNECTIONADDRESS$(conn&)
```

#### `_LASTHANDLER` (QB64)
Returns last event handler.

```basic
handler& = _LASTHANDLER
```

#### `_OS$` (QB64)
Returns operating system name.

```basic
os$ = _OS$
```

#### `_STARTDIR$` (QB64)
Returns directory where program was started.

```basic
dir$ = _STARTDIR$
```

#### `TAB(n%)` / `SPC(n%)`
Print formatting functions (used in PRINT statements).

```basic
PRINT TAB(10); "Text"
PRINT SPC(5); "Text"
```

#### `ERR` / `ERL`
Error number and line number (after error occurs).

```basic
ON ERROR GOTO handler
handler:
    PRINT "Error"; ERR; "at line"; ERL
```

#### `_ERRORLINE` / `_ERRORMESSAGE$` (QB64)
Extended error information.

```basic
line& = _ERRORLINE
msg$ = _ERRORMESSAGE$
```

#### `_INCLERRORFILE$` / `_INCLERRORLINE` (QB64)
Include file error information.

```basic
file$ = _INCLERRORFILE$
line& = _INCLERRORLINE
```

#### `_STATUSCODE[(handle&)]` (QB64)
Returns status code from last operation.

```basic
code& = _STATUSCODE
code& = _STATUSCODE(handle&)
```

#### `_ASSERTERROR$` (QB64)
Returns assertion error message.

```basic
msg$ = _ASSERTERROR$
```

#### `_EXIT` (QB64)
Returns exit request state (non-zero if user requested exit).

```basic
IF _EXIT THEN END
```

#### `_FPS` (QB64)
Returns current frame rate.

```basic
fps! = _FPS
```

#### `_YEAR` / `_MONTH` / `_DAY` / `_WEEKDAY` (QB64)
Returns date components.

```basic
year& = _YEAR
month& = _MONTH
day& = _DAY
weekday& = _WEEKDAY  ' 1=Sunday, 7=Saturday
```

#### `_HOUR` / `_MINUTE` / `_SECOND` (QB64)
Returns time components.

```basic
hour& = _HOUR
minute& = _MINUTE
second& = _SECOND
```

#### `_SCREENICON` (QB64)
Returns -1 if window is minimized, 0 otherwise.

```basic
minimized% = _SCREENICON
```

#### `_FREETIMER` (QB64)
Free a timer resource, returns freed timer handle.

```basic
timer& = _FREETIMER
```

#### `_KEYCLEAR` (QB64)
Returns -1 if keyboard buffer was cleared, 0 otherwise (function form).

```basic
cleared% = _KEYCLEAR
```

#### `_ASSERT(condition&)` (QB64)
Returns -1 if assertion passed, 0 if failed (function form).

```basic
passed% = _ASSERT(x% > 0)
```

#### `_SCREENX` / `_SCREENY` (QB64)
Returns screen window position.

```basic
x& = _SCREENX
y& = _SCREENY
```

#### `_DESKTOPWIDTH` / `_DESKTOPHEIGHT` (QB64)
Returns desktop dimensions.

```basic
w& = _DESKTOPWIDTH
h& = _DESKTOPHEIGHT
```

#### `_TITLE$` (QB64)
Returns current window title.

```basic
title$ = _TITLE$
```

#### `_ICON[(handle&)]` (QB64)
Returns/sets window icon handle.

```basic
icon& = _ICON
```

#### `_WINDOWHANDLE` (QB64)
Returns window handle (platform-specific).

```basic
handle& = _WINDOWHANDLE
```

#### `_WINDOWHASFOCUS` (QB64)
Returns -1 if window has focus, 0 otherwise.

```basic
focused% = _WINDOWHASFOCUS
```

#### `_CLIPBOARD$` (QB64)
Returns clipboard text contents.

```basic
text$ = _CLIPBOARD$
```

#### `_CLIPBOARDIMAGE` (QB64)
Returns image handle from clipboard, or 0 if none.

```basic
img& = _CLIPBOARDIMAGE
```

#### `_ACCEPTFILEDROP[(mode&)]` (QB64)
Returns whether file drop is accepted, or sets mode.

```basic
accepted& = _ACCEPTFILEDROP
_ACCEPTFILEDROP 1  ' Enable
```

#### `_FINISHDROP` (QB64)
Returns whether drop processing is finished.

```basic
done& = _FINISHDROP
```

#### `_DROPPEDFILE$[(index&)]` (QB64)
Returns dropped filename. No argument returns first file.

```basic
file$ = _DROPPEDFILE$
file$ = _DROPPEDFILE$(0)
```

---

### Memory Functions

#### `PEEK(address%)`
Reads byte from memory address (within DEF SEG segment). Returns 0-255.

```basic
byte% = PEEK(1000)
```

#### `VARPTR(variable)` / `VARPTR$(variable)`
Returns offset address of variable within segment.

```basic
addr& = VARPTR(x%)
addrStr$ = VARPTR$(x%)
```

#### `VARSEG(variable)`
Returns segment address (returns 0 in flat memory model).

```basic
seg& = VARSEG(x%)
```

#### `SADD(string$)`
Returns address of string's data.

```basic
addr& = SADD(text$)
```

#### `FRE(n%)`
Returns free memory. n=-1: largest string block, n=-2: stack space, n=0: string space.

```basic
free& = FRE(0)
```

#### `_MEMNEW(size&)` (QB64)
Allocate memory block. Returns _MEM handle.

```basic
mem~%% = _MEMNEW(1024)
```

#### `_MEMFREE(mem~%%)` (QB64)
Free memory block.

```basic
_MEMFREE mem~%%
```

#### `_MEMGET(mem~%%, offset&)` (QB64)
Get value from memory (type depends on context).

```basic
value% = _MEMGET(mem~%%, 0) AS INTEGER
```

#### `_MEMPUT(mem~%%, offset&, value)` (QB64)
Put value into memory.

```basic
_MEMPUT mem~%%, 0, 42 AS INTEGER
```

#### `_MEMCOPY(source~%%, srcOffset&, size&, dest~%%, destOffset&)` (QB64)
Copy memory block.

```basic
_MEMCOPY src~%%, 0, 100, dest~%%, 0
```

#### `_MEMFILL(mem~%%, offset&, size&, value)` (QB64)
Fill memory block with value.

```basic
_MEMFILL mem~%%, 0, 100, 0 AS _BYTE
```

#### `_OFFSET(variable)` (QB64)
Returns _OFFSET handle for variable.

```basic
off~&& = _OFFSET(x%)
```

#### `_MEM(variable)` (QB64)
Returns _MEM handle for variable.

```basic
mem~%% = _MEM(x%)
```

#### `_MEMEXISTS(offset~&&)` (QB64)
Returns -1 if memory offset is valid, 0 otherwise.

```basic
valid% = _MEMEXISTS(off~&&)
```

#### `_MEMELEMENT(mem~%%, index&)` (QB64)
Returns offset of array element in memory block.

```basic
off~&& = _MEMELEMENT(mem~%%, 5)
```

#### `_MEMIMAGE(handle&)` (QB64)
Returns memory block for an image handle.

```basic
mem~%% = _MEMIMAGE(img&)
```

#### `_MEMSOUND(handle&)` (QB64)
Returns memory block for a sound handle.

```basic
mem~%% = _MEMSOUND(sound&)
```

---

### Audio Functions

#### `_SNDOPEN(filename$[, mode$])` (QB64)
Open sound file. Returns handle. Mode: "SYNC", "STREAM", "VOL", "PAUSE", "NODECODE".

```basic
sound& = _SNDOPEN("music.wav")
```

#### `_SNDOPENRAW` (QB64)
Open raw sound buffer for audio output.

```basic
raw& = _SNDOPENRAW
```

#### `_SNDCOPY(handle&)` (QB64)
Create copy of sound handle for independent playback.

```basic
copy& = _SNDCOPY(sound&)
```

#### `_SNDPLAYING(handle&)` (QB64)
Returns -1 if sound is playing, 0 otherwise.

```basic
IF _SNDPLAYING(sound&) THEN
    ' Still playing
END IF
```

#### `_SNDPAUSED(handle&)` (QB64)
Returns -1 if sound is paused, 0 otherwise.

#### `_SNDGETPOS(handle&)` (QB64)
Returns current playback position in seconds.

```basic
pos! = _SNDGETPOS(sound&)
```

#### `_SNDLEN(handle&)` (QB64)
Returns total length of sound in seconds.

```basic
len! = _SNDLEN(sound&)
```

#### `_SNDRATE(handle&)` (QB64)
Returns sample rate (usually 44100).

```basic
rate& = _SNDRATE(sound&)
```

#### `_SNDRAWLEN` (QB64)
Returns amount of queued raw sound data in seconds.

```basic
len! = _SNDRAWLEN
```

#### `_SNDRAWDONE` (QB64)
Returns -1 if raw sound buffer is empty, 0 otherwise.

```basic
IF _SNDRAWDONE THEN
    ' Buffer empty
END IF
```

#### `_SNDNEW(frames&, channels&, bits&)` (QB64)
Create new sound buffer.

```basic
sound& = _SNDNEW(44100, 2, 16)
```

---

### Font Functions (QB64)

#### `_LOADFONT(file$, size%[, style$])` (QB64)
Load font file. Style: "BOLD", "ITALIC", etc.

```basic
font& = _LOADFONT("arial.ttf", 24)
font& = _LOADFONT("arial.ttf", 24, "BOLD,ITALIC")
```

#### `_FONTHEIGHT` / `_FONTWIDTH` (QB64)
Returns current font dimensions.

```basic
h& = _FONTHEIGHT
w& = _FONTWIDTH
```

#### `_PRINTWIDTH(text$)` (QB64)
Returns pixel width of text.

```basic
w& = _PRINTWIDTH("Hello")
```

#### `_UCHARPOS(text$, pos&)` (QB64)
Returns Unicode character position in string.

```basic
charPos& = _UCHARPOS("Hello", 2)
```

#### `_UFONTHEIGHT(handle&)` (QB64)
Returns Unicode font height.

```basic
h& = _UFONTHEIGHT(font&)
```

#### `_ULINESPACING` (QB64)
Returns Unicode line spacing.

```basic
spacing& = _ULINESPACING
```

#### `_UPRINTWIDTH(text$)` (QB64)
Returns pixel width of Unicode text.

```basic
w& = _UPRINTWIDTH("Hello")
```

#### `_MAPUNICODE(charcode&)` (QB64)
Returns Unicode codepoint for character code (function form).

```basic
unicode& = _MAPUNICODE(65)  ' Returns 65 ('A')
```

#### `_FONT[(handle&)]` (QB64)
Sets current font, returns previous handle.

```basic
oldFont& = _FONT(font&)
```

#### `_FREEFONT(handle&)` (QB64)
Free font resource.

```basic
_FREEFONT font&
```

---

### Dialog Functions (QB64)

#### `_MESSAGEBOX([title$][, message$][, dialogType$][, iconType$][, defaultButton&])` (QB64)
Display message box. Returns button code.

```basic
result& = _MESSAGEBOX("Title", "Message", "OKCANCEL", "QUESTION", 1)
```

#### `_INPUTBOX$(prompt$, title$)` (QB64)
Display input dialog. Returns user input or empty string if cancelled.

```basic
input$ = _INPUTBOX$("Enter name:", "Input")
```

#### `_OPENFILEDIALOG$([title$][, filter$][, defaultDir$][, defaultFile$][, flags&])` (QB64)
Open file dialog. Returns selected filename or empty string.

```basic
file$ = _OPENFILEDIALOG$("Open File", "Text Files|*.txt|All Files|*.*")
```

#### `_SAVEFILEDIALOG$([title$][, filter$][, defaultDir$][, defaultFile$])` (QB64)
Save file dialog.

```basic
file$ = _SAVEFILEDIALOG$("Save File", "Text Files|*.txt")
```

#### `_SELECTFOLDERDIALOG$(title$)` (QB64)
Folder selection dialog.

```basic
folder$ = _SELECTFOLDERDIALOG$("Select Folder")
```

#### `_COLORCHOOSERDIALOG(initialColor&)` (QB64)
Color picker dialog. Returns selected color.

```basic
color& = _COLORCHOOSERDIALOG(_RGB(255, 0, 0))
```

#### `_NOTIFYPOPUP(title$, message$)` (QB64)
System notification popup.

```basic
_NOTIFYPOPUP "Title", "Message"
```

---

### Encoding/Hash Functions (QB64)

#### `_CRC32(data$)` / `_MD5$(data$)` / `_ADLER32(data$)` (QB64)
Hash functions.

```basic
crc& = _CRC32("Hello")
md5$ = _MD5$("Hello")
```

#### `_BASE64ENCODE$(data$)` / `_BASE64DECODE$(data$)` (QB64)
Base64 encoding/decoding.

```basic
encoded$ = _BASE64ENCODE$("Hello")
decoded$ = _BASE64DECODE$(encoded$)
```

#### `_ENCODEURL$(url$)` / `_DECODEURL$(url$)` (QB64)
URL encoding/decoding.

```basic
encoded$ = _ENCODEURL$("Hello World")
```

#### `_DEFLATE$(data$)` / `_INFLATE$(data$)` (QB64)
Zlib compression/decompression.

```basic
compressed$ = _DEFLATE$("Hello")
decompressed$ = _INFLATE$(compressed$)
```

---

### Device Input Functions (QB64)

#### `_DEVICES` (QB64)
Returns number of input devices.

```basic
count& = _DEVICES
```

#### `_DEVICE$(n&)` (QB64)
Returns device name.

```basic
name$ = _DEVICE$(0)
```

#### `_DEVICEINPUT` (QB64)
Returns -1 if device input event occurred.

```basic
IF _DEVICEINPUT THEN
    ' Handle device event
END IF
```

#### `_LASTAXIS(device&)` / `_LASTBUTTON(device&)` / `_LASTWHEEL(device&)` (QB64)
Last changed axis/button/wheel.

```basic
axis& = _LASTAXIS(0)
```

#### `_AXIS(device&, axis&)` / `_BUTTON(device&, button&)` / `_WHEEL(device&, wheel&)` (QB64)
Get device state.

```basic
x! = _AXIS(0, 0)  ' Device 0, axis 0
pressed% = _BUTTON(0, 1)  ' Device 0, button 1
```

#### `_BUTTONCHANGE(device&, button&)` (QB64)
Returns -1 if button state changed, 0 otherwise.

```basic
IF _BUTTONCHANGE(0, 1) THEN
    ' Button 1 state changed
END IF
```

---

### Drag and Drop Functions (QB64)

#### `_TOTALDROPPEDFILES` (QB64)
Returns number of dropped files.

```basic
count& = _TOTALDROPPEDFILES
```

#### `_DROPPEDFILE(index&)` / `_DROPPEDFILE$(index&)` (QB64)
Get dropped file handle or filename.

```basic
handle& = _DROPPEDFILE(0)
name$ = _DROPPEDFILE$(0)
```

---

### Resize Event Functions (QB64)

#### `_RESIZE` (QB64)
Returns -1 if window was resized, 0 otherwise.

```basic
IF _RESIZE THEN
    ' Window resized
END IF
```

#### `_RESIZEWIDTH` / `_RESIZEHEIGHT` (QB64)
New window dimensions after resize.

```basic
w& = _RESIZEWIDTH
h& = _RESIZEHEIGHT
```

#### `_SCALEDWIDTH` / `_SCALEDHEIGHT` (QB64)
Scaled dimensions (if scaling enabled).

```basic
sw& = _SCALEDWIDTH
```

---

### Logical Operators (QB64)

#### `_ANDALSO(a&, b&)` / `_ORELSE(a&, b&)` (QB64)
Short-circuit logical operators.

```basic
result& = _ANDALSO(x&, y&)  ' Returns y only if x is true
```

---

### Other Functions

#### `_IIF(cond&, true_val, false_val)` / `_IIF$(cond&, true_val$, false_val$)` (QB64)
Inline conditional.

```basic
result! = _IIF(x% > 0, 1.0, -1.0)
result$ = _IIF$(x% > 0, "Positive", "Negative")
```

#### `KEY(n%)`
Returns key trap status: -1=enabled, 0=disabled, 1=event pending.

```basic
status% = KEY(1)
```

#### `STICK(n%)`
Returns joystick position. n=0:X(A), 1:Y(A), 2:X(B), 3:Y(B).

```basic
x% = STICK(0)
```

#### `STRIG(n%)`
Returns joystick trigger state. n=0-7 for various triggers.

```basic
pressed% = STRIG(0)
```

#### `PEN(n%)`
Returns light pen information (legacy, returns 0).

```basic
pen% = PEN(0)
```

#### `ERDEV` / `ERDEV$`
Device error code and name.

```basic
code% = ERDEV
name$ = ERDEV$
```

#### `IOCTL$(filenum%)`
Returns device control string from driver.

```basic
control$ = IOCTL$(1)
```

#### `LPOS(n%)`
Returns current line printer position.

```basic
pos% = LPOS(0)
```

---

## Constants

QB64Fresh provides built-in constants in several categories.

### Math Constants
- `_PI` — π (pi), approximately 3.141592653589793

### Boolean and Handle Constants
- `_TRUE` — -1 (all bits set, BASIC convention for true)
- `_FALSE` — 0
- `_NONE` — 0 (null/none for handles, modes)

### Platform Constants (for `$IF` directives)
- `_WINDOWS` — -1 on Windows, 0 otherwise
- `_LINUX` — -1 on Linux, 0 otherwise
- `_MACOSX` — -1 on macOS, 0 otherwise  
  Legacy names `WIN`, `LINUX`, `MAC` may also be used in some directives.

### Error Code Constants (`_ERR_*`)
QB45-compatible error codes, e.g.: `_ERR_SYNTAX_ERROR`, `_ERR_RETURN_WITHOUT_GOSUB`, `_ERR_ILLEGAL_FUNCTION_CALL`, `_ERR_DIVISION_BY_ZERO`, `_ERR_SUBSCRIPT_OUT_OF_RANGE`, `_ERR_FILE_NOT_FOUND`, `_ERR_OUT_OF_MEMORY`, and many others. See `src/semantic/builtins.rs` for the full list.

### Keyboard Scan Code Constants (`_KEY_*`)
- Function keys: `_KEY_F1` … `_KEY_F12`
- Navigation: `_KEY_HOME`, `_KEY_END`, `_KEY_PAGEUP`, `_KEY_PAGEDOWN`, `_KEY_INSERT`, `_KEY_DELETE`
- Arrows: `_KEY_UP`, `_KEY_DOWN`, `_KEY_LEFT`, `_KEY_RIGHT`
- Modifiers: `_KEY_LSHIFT`, `_KEY_RSHIFT`, `_KEY_LCTRL`, `_KEY_RCTRL`, `_KEY_LALT`, `_KEY_RALT`, `_KEY_CAPSLOCK`, `_KEY_NUMLOCK`, `_KEY_SCROLLLOCK`
- Others: `_KEY_PRINT`, `_KEY_PAUSE`

### Character Constants (ASCII control codes)
- `_NUL`, `_SOH`, `_STX`, … `_US` (0–31), `_DEL` (127)
- Common: `_TAB`/`_HT` (9), `_LF` (10), `_CR` (13), `_ESC` (27)

---

## Type Suffixes

QB64Fresh supports type suffixes for variable names:

- `$` - STRING
- `%` - INTEGER
- `&` - LONG
- `!` - SINGLE
- `#` - DOUBLE
- `%%` - _INTEGER64
- `` ` `` - _FLOAT

---

## Notes

1. **Case Insensitivity**: All keywords, identifiers, and function names are case-insensitive.

2. **Optional Parameters**: Many functions support optional parameters. When omitted, default values are used.

3. **Type Coercion**: QB64Fresh performs automatic type coercion where appropriate (e.g., INTEGER to LONG, SINGLE to DOUBLE).

4. **Array Bounds**: By default, arrays start at index 0. Use `OPTION BASE 1` to start at index 1.

5. **String Functions**: String functions ending with `$` return strings. Functions without `$` return numeric values.

6. **QB64 Extensions**: Functions and statements prefixed with `_` are QB64 extensions and may not be available in standard QB4.5. `_GL*` (OpenGL) is excluded in QB64Fresh; see Scope and Implementation Status above.

7. **Legacy and Stub-Only Features**: PEEK/POKE, DEF SEG, and hardware I/O (`INP`, `OUT`, `WAIT`) are provided for porting; runtime behavior may be no-op or emulated. Event handlers such as `ON COM`, `ON UEVENT`, `ON SIGNAL` are stub-only. See ADR-0014.

---

## See Also

- [Architecture Documentation](ARCHITECTURE.md)
- [Development Guide](DEVELOPMENT.md)
- [Testing Guide](TESTING.md)
- [QB64PE to QB64Fresh Migration Guide](QB64PE_TO_QB64Fresh_MIGRATION_GUIDE.md)
- [Scope and Excluded Features](adrs/ADR-0014-scope-and-excluded-features.md)

---

*This reference is generated from the QB64Fresh compiler implementation. For the most up-to-date information, consult the source code or compiler documentation.*
