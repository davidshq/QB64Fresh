$CONSOLE:ONLY
' Runtime comparison: ON ERROR GOTO ... RESUME <label>
ON ERROR GOTO handler
PRINT "before"
ERROR 6
PRINT "after"
END

handler:
PRINT "handler: ERR="; ERR; " ERL="; ERL
RESUME afterLabel

afterLabel:
PRINT "resumed at afterLabel"
END
