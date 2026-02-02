' IDE Equivalence 4: Font and text
' Minimal repro: _LOADFONT, _FONT, _PRINTSTRING, _FONTHEIGHT, _FONTWIDTH.
' Criterion: Text and line height correct (addresses "funky" UI).
SCREEN _NEWIMAGE(640, 480, 32)
_TITLE "IDE Eq 4: Font"
CLS
' Use default font; optional: _LOADFONT if a TTF is available
_PRINTSTRING (10, 10), "IDE Eq 4: Font test"
h = _FONTHEIGHT
w = _PRINTWIDTH("IDE Eq 4: Font test")
_PRINTSTRING (10, 10 + h), "Line 2"
_DISPLAY
SLEEP 2
END
