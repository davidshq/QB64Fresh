$CONSOLE:ONLY
' Runtime comparison 3: ON ERROR RESUME NEXT
' Trigger error, print ERR/ERL, resume
ON ERROR GOTO handler
PRINT "before"
ERROR 5
PRINT "after error"
END

handler:
PRINT "handler: ERR="; ERR; " ERL="; ERL
RESUME NEXT
