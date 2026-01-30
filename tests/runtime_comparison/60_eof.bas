$CONSOLE:ONLY
' Runtime comparison: EOF
OPEN "rt_60_eof.txt" FOR OUTPUT AS #1
PRINT #1, "one"
CLOSE #1
OPEN "rt_60_eof.txt" FOR INPUT AS #2
DIM s AS STRING
LINE INPUT #2, s
PRINT "eof after 1:"; EOF(2)
LINE INPUT #2, s
PRINT "eof after 2:"; EOF(2)
CLOSE #2
KILL "rt_60_eof.txt"
PRINT "done"
END
