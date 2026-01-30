$CONSOLE:ONLY
' Runtime comparison 5: File I/O handle management
' OPEN, write, close, reopen read, LINE INPUT, close
OPEN "runtime_comparison_test.txt" FOR OUTPUT AS #1
PRINT #1, "line1"
PRINT #1, "line2"
CLOSE #1

OPEN "runtime_comparison_test.txt" FOR INPUT AS #2
DIM s AS STRING
LINE INPUT #2, s
PRINT "first: "; s
LINE INPUT #2, s
PRINT "second: "; s
CLOSE #2

KILL "runtime_comparison_test.txt"
PRINT "done"
END
