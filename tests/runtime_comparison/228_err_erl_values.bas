$CONSOLE:ONLY
' Runtime comparison: PRINT ERR, ERL after different errors
ON ERROR GOTO handler
PRINT "before"
ERROR 11
PRINT "after"
END
handler:
PRINT "err:"; ERR; "erl:"; ERL
RESUME NEXT
