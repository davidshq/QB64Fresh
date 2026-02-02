' IDE Equivalence 3: Graphics / image
' Minimal repro: _NEWIMAGE, draw, _PUTIMAGE, _DISPLAY.
' Criterion: Create image, draw to it, display; no crash or black screen.
' (CLS and _PUTIMAGE use forms supported by parser: CLS only; _PUTIMAGE (x1,y1)-(x2,y2), img)
SCREEN _NEWIMAGE(640, 480, 32)
_TITLE "IDE Eq 3: Graphics"
CLS
img = _NEWIMAGE(200, 100, 32)
_DEST img
CLS
LINE (10, 10)-(190, 90), _RGB32(255, 255, 0), B
_DEST 0
_PUTIMAGE (220, 190)-(420, 290), img
_FREEIMAGE img
_DISPLAY
SLEEP 2
END
