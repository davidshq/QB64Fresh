$CONSOLE:ONLY
' Runtime comparison: WRITE # (comma-quoted output)
OPEN "rt_178_wr.txt" FOR OUTPUT AS #1
WRITE #1, "a,b", 1, 2.5
WRITE #1, "line2"
CLOSE #1
OPEN "rt_178_wr.txt" FOR INPUT AS #2
DIM s AS STRING, i AS LONG, d AS DOUBLE
INPUT #2, s, i, d
PRINT "1:"; s; i; d
LINE INPUT #2, s
PRINT "2:"; s
CLOSE #2
KILL "rt_178_wr.txt"
PRINT "done"
END
