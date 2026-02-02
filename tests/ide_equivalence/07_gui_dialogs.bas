' IDE Equivalence 7: GUI dialogs
' Minimal repro: _MESSAGEBOX (or file dialog).
' Criterion: Dialogs appear and return correct values.
SCREEN 12
_TITLE "IDE Eq 7: GUI"
r = _MESSAGEBOX("IDE Eq 7: Click OK", "IDE Eq 7", "ok")
PRINT "MessageBox return: "; r
_DISPLAY
SLEEP 2
END
