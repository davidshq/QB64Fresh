$CONSOLE:ONLY
' Runtime comparison: LOCK / UNLOCK (if supported)
ON ERROR GOTO handler
OPEN "rt_179_lk.txt" FOR OUTPUT AS #1
LOCK #1
PRINT #1, "x"
UNLOCK #1
CLOSE #1
KILL "rt_179_lk.txt"
PRINT "done"
END
handler:
PRINT "err"; ERR
RESUME NEXT
