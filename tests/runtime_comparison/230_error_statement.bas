$CONSOLE:ONLY
' Runtime comparison: ERROR 5 (simulate error)
ON ERROR GOTO handler
PRINT "before"
ERROR 5
PRINT "after"
END
handler:
PRINT "err:"; ERR; "erl:"; ERL
RESUME NEXT
