$CONSOLE:ONLY
' Runtime comparison: OPEN FOR APPEND
OPEN "rt_53_append.txt" FOR OUTPUT AS #1
PRINT #1, "line1"
CLOSE #1
OPEN "rt_53_append.txt" FOR APPEND AS #1
PRINT #1, "line2"
CLOSE #1
OPEN "rt_53_append.txt" FOR INPUT AS #2
DIM s AS STRING
LINE INPUT #2, s
PRINT "first:"; s
LINE INPUT #2, s
PRINT "second:"; s
CLOSE #2
KILL "rt_53_append.txt"
PRINT "done"
END
