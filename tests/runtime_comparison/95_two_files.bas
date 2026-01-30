$CONSOLE:ONLY
' Runtime comparison: two files open
OPEN "rt_95_a.txt" FOR OUTPUT AS #1
OPEN "rt_95_b.txt" FOR OUTPUT AS #2
PRINT #1, "file1"
PRINT #2, "file2"
CLOSE #1
CLOSE #2
OPEN "rt_95_a.txt" FOR INPUT AS #1
OPEN "rt_95_b.txt" FOR INPUT AS #2
DIM s AS STRING
LINE INPUT #1, s
PRINT "a:"; s
LINE INPUT #2, s
PRINT "b:"; s
CLOSE #1
CLOSE #2
KILL "rt_95_a.txt"
KILL "rt_95_b.txt"
PRINT "done"
END
