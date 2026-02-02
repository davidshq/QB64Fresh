$CONSOLE:ONLY
' IDE Equivalence 9: Time and shell
' Minimal repro: TIMER, SLEEP, TIME$, DATE$ (SHELL optional).
' Criterion: Timing and Run/Make work; SHELL runs external process.
DIM t AS SINGLE
t = TIMER
PRINT "timer: "; t
SLEEP 1
PRINT "timer_after_1s: "; TIMER
PRINT "time$: "; TIME$
PRINT "date$: "; DATE$
END
