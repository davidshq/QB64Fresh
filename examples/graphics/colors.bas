' ============================================================================
' Colors and Palette
' ============================================================================
' Demonstrates: COLOR, PALETTE, _RGB, _RGBA, screen modes
' ============================================================================

' --- 16-Color Mode (SCREEN 12) ---
SCREEN 12
CLS

LOCATE 1, 25
PRINT "16-Color Palette (SCREEN 12)"

' Display the 16 standard colors
FOR c = 0 TO 15
    x1 = (c MOD 8) * 70 + 50
    y1 = (c \ 8) * 60 + 50
    x2 = x1 + 60
    y2 = y1 + 50

    LINE (x1, y1)-(x2, y2), c, BF

    ' Draw border
    LINE (x1, y1)-(x2, y2), 15, B

    ' Label
    LOCATE (y2 + 10) \ 16, x1 \ 8 + 1
    PRINT c
NEXT c

LOCATE 12, 5
PRINT "Standard VGA colors: 0=Black, 1=Blue, 2=Green, 3=Cyan,"
LOCATE 13, 5
PRINT "4=Red, 5=Magenta, 6=Brown, 7=White, 8-15=Bright variants"

LOCATE 20, 20
PRINT "Press any key for 256-color mode..."
SLEEP

' --- 256-Color Mode (SCREEN 13) ---
SCREEN 13   ' 320x200, 256 colors
CLS

' Display color palette
FOR c = 0 TO 255
    x = (c MOD 16) * 20
    y = (c \ 16) * 12
    LINE (x, y)-(x + 19, y + 11), c, BF
NEXT c

LOCATE 23, 1
PRINT "256 colors (SCREEN 13)";
LOCATE 24, 1
PRINT "Press key for 32-bit...";

SLEEP

' --- 32-bit Color Mode ---
SCREEN _NEWIMAGE(640, 480, 32)
CLS

' Title
COLOR _RGB(255, 255, 255)
LOCATE 1, 20
PRINT "32-bit Color Mode (16.7 million colors)"

' Draw RGB gradient bars
' Red gradient
FOR x = 50 TO 305
    r = (x - 50)
    LINE (x, 80)-(x, 120), _RGB(r, 0, 0)
NEXT x
LOCATE 8, 8
PRINT "Red"

' Green gradient
FOR x = 50 TO 305
    g = (x - 50)
    LINE (x, 140)-(x, 180), _RGB(0, g, 0)
NEXT x
LOCATE 11, 8
PRINT "Green"

' Blue gradient
FOR x = 50 TO 305
    b = (x - 50)
    LINE (x, 200)-(x, 240), _RGB(0, 0, b)
NEXT x
LOCATE 14, 8
PRINT "Blue"

' Rainbow gradient (hue sweep)
FOR x = 50 TO 561
    ' Simple HSV to RGB conversion (hue only)
    h = (x - 50) / 512 * 6
    IF h < 1 THEN
        r = 255: g = h * 255: b = 0
    ELSEIF h < 2 THEN
        r = (2 - h) * 255: g = 255: b = 0
    ELSEIF h < 3 THEN
        r = 0: g = 255: b = (h - 2) * 255
    ELSEIF h < 4 THEN
        r = 0: g = (4 - h) * 255: b = 255
    ELSEIF h < 5 THEN
        r = (h - 4) * 255: g = 0: b = 255
    ELSE
        r = 255: g = 0: b = (6 - h) * 255
    END IF
    LINE (x, 280)-(x, 340), _RGB(r, g, b)
NEXT x
LOCATE 20, 8
PRINT "Rainbow (Hue sweep)"

' Color mixing demo
LINE (400, 80)-(500, 180), _RGB(255, 0, 0), BF     ' Red
LINE (450, 130)-(550, 230), _RGB(0, 255, 0), BF   ' Green
LINE (425, 180)-(525, 280), _RGB(0, 0, 255), BF   ' Blue

COLOR _RGB(255, 255, 255)
LOCATE 8, 52
PRINT "RGB Color"
LOCATE 9, 52
PRINT "Mixing"

' Alpha blending demo (if supported)
LOCATE 25, 5
PRINT "_RGB(r,g,b) creates colors | _RGBA(r,g,b,a) adds transparency"

LOCATE 29, 20
COLOR _RGB(200, 200, 200)
PRINT "Press any key to exit..."

SLEEP
END
