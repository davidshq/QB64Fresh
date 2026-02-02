$CONSOLE:ONLY
' IDE Equivalence 5: File I/O
' Minimal repro: OPEN, write, read, CLOSE.
' Criterion: File menu works; recent list; no Error 53 on recent.bin.
DIM f AS INTEGER
DIM s AS STRING
f = FREEFILE
OPEN "ide_eq_05_test.dat" FOR OUTPUT AS #f
PRINT #f, "IDE Eq 5"
CLOSE #f
OPEN "ide_eq_05_test.dat" FOR INPUT AS #f
LINE INPUT #f, s
CLOSE #f
PRINT "file_io: "; s
KILL "ide_eq_05_test.dat"
END
