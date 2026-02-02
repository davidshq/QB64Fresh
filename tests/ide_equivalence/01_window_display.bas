' IDE Equivalence 1: Window and display
' Minimal repro: SCREEN, _TITLE, evnt, loop until key/close.
' Criterion: Window appears, title correct, closes with X (or Esc).
SCREEN 12
_TITLE "IDE Eq 1: Window"
DO
    evnt
    k$ = INKEY$
    IF k$ = CHR$(27) THEN EXIT DO
LOOP
END
