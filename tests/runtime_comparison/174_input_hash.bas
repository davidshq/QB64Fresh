$CONSOLE:ONLY
' Runtime comparison: INPUT # (numeric/string from file)
OPEN "rt_174_in.txt" FOR OUTPUT AS #1
PRINT #1, "hello"
PRINT #1, 42
PRINT #1, 3.14
CLOSE #1
OPEN "rt_174_in.txt" FOR INPUT AS #2
DIM s AS STRING, i AS LONG, d AS DOUBLE
INPUT #2, s
INPUT #2, i
INPUT #2, d
PRINT s; i; d
CLOSE #2
KILL "rt_174_in.txt"
PRINT "done"
END
