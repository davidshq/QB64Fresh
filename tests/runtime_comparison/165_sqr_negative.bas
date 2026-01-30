$CONSOLE:ONLY
' Runtime comparison: SQR negative (error path if any)
ON ERROR GOTO handler
PRINT SQR(-1)
PRINT "no error"
END
handler:
PRINT "err"; ERR; "erl"; ERL
RESUME NEXT
