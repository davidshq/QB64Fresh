$CONSOLE:ONLY
' Runtime comparison: OPEN FOR RANDOM (if supported)
ON ERROR GOTO handler
OPEN "rt_176_ran.txt" FOR RANDOM AS #1 LEN = 10
PRINT "open random ok"
CLOSE #1
KILL "rt_176_ran.txt"
PRINT "done"
END
handler:
PRINT "err"; ERR
RESUME NEXT
