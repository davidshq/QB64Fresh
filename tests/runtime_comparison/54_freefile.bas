$CONSOLE:ONLY
' Runtime comparison: FREEFILE
DIM f AS LONG
f = FREEFILE
PRINT "freefile:"; f
OPEN "rt_54_f.txt" FOR OUTPUT AS #f
PRINT #f, "test"
CLOSE #f
KILL "rt_54_f.txt"
PRINT "done"
END
