$CONSOLE:ONLY
' Runtime comparison: LOC
OPEN "rt_97_loc.txt" FOR OUTPUT AS #1
PRINT #1, "ab"
CLOSE #1
OPEN "rt_97_loc.txt" FOR INPUT AS #2
DIM s AS STRING
LINE INPUT #2, s
PRINT "loc after read:"; LOC(2)
CLOSE #2
KILL "rt_97_loc.txt"
PRINT "done"
END
