$CONSOLE:ONLY
' Runtime comparison: OPEN FOR OUTPUT, WRITE
OPEN "rt_52_out.txt" FOR OUTPUT AS #1
WRITE #1, "a", 1, 2.5
CLOSE #1
OPEN "rt_52_out.txt" FOR INPUT AS #2
DIM s AS STRING, i AS LONG, d AS DOUBLE
INPUT #2, s, i, d
PRINT "read:"; s; i; d
CLOSE #2
KILL "rt_52_out.txt"
PRINT "done"
END
