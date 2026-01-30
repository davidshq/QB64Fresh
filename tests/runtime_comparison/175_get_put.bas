$CONSOLE:ONLY
' Runtime comparison: GET # / PUT # (binary record I/O)
OPEN "rt_175_bin.txt" FOR BINARY AS #1
DIM s AS STRING
s = "xy"
PUT #1, , s
CLOSE #1
OPEN "rt_175_bin.txt" FOR BINARY AS #2
s = "  "
GET #2, , s
PRINT "["; s; "]"
CLOSE #2
KILL "rt_175_bin.txt"
PRINT "done"
END
