' IDE Equivalence 1b: Window with graphics printing
' Uses _PRINTSTRING which should map to qb_gfx_printstring
' This avoids the PRINT->stdout issue
SCREEN 12
_TITLE "IDE Eq 1b: Graphics Window"
CLS
COLOR 15, 0
' Use _PRINTSTRING instead of PRINT (goes to graphics window)
_PRINTSTRING (10, 10), "OK - Press ESC or close window"
' Draw something to prove graphics is working
PSET (100, 100), 15
LINE (50, 50)-(200, 100), 12
_DISPLAY
DO
    evnt
    k$ = INKEY$
    IF k$ = CHR$(27) THEN EXIT DO
LOOP
END
