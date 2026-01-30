$CONSOLE:ONLY
' Runtime comparison: OPEN FOR BINARY (if supported)
OPEN "rt_177_bin.txt" FOR BINARY AS #1
PRINT #1, "ab"
CLOSE #1
OPEN "rt_177_bin.txt" FOR BINARY AS #2
DIM s AS STRING
s = SPACE$(4)
GET #2, , s
PRINT "["; s; "]"
CLOSE #2
KILL "rt_177_bin.txt"
PRINT "done"
END
