$CONSOLE:ONLY
' Runtime comparison: FREEFILE with multiple OPENs
DIM f1 AS LONG, f2 AS LONG
f1 = FREEFILE
OPEN "rt_183_a.txt" FOR OUTPUT AS #f1
f2 = FREEFILE
OPEN "rt_183_b.txt" FOR OUTPUT AS #f2
PRINT "f1:"; f1; "f2:"; f2
PRINT #f1, "a"
PRINT #f2, "b"
CLOSE #f1
CLOSE #f2
KILL "rt_183_a.txt"
KILL "rt_183_b.txt"
PRINT "done"
END
