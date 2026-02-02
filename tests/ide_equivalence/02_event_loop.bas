' IDE Equivalence 2: Event loop
' Minimal repro: evnt, INKEY$, loop until Esc.
' Criterion: Main loop runs; key events and window close detected.
SCREEN 12
_TITLE "IDE Eq 2: Event Loop"
PRINT "Press keys (echoed); Esc to exit."
DO
    evnt
    k$ = INKEY$
    IF k$ <> "" THEN PRINT "Key: "; k$
    IF k$ = CHR$(27) THEN EXIT DO
LOOP
END
