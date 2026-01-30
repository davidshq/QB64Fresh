$CONSOLE:ONLY
' Runtime comparison: READ past last DATA (error)
ON ERROR GOTO handler
DIM x AS LONG
READ x
PRINT x
READ x
PRINT "second:"; x
DATA 1
END
handler:
PRINT "err"; ERR; "erl"; ERL
RESUME NEXT
