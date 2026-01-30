$CONSOLE:ONLY
' Runtime comparison: ON ERROR GOTO ... RESUME 0 (restart line)
' RESUME 0 re-executes the line that caused the error
ON ERROR GOTO handler
PRINT "before"
x = 1
ERROR 5
PRINT "after"
END

handler:
PRINT "handler: ERR="; ERR; " ERL="; ERL
RESUME 0
