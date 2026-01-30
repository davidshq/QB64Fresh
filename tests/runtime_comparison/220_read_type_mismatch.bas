$CONSOLE:ONLY
' Runtime comparison: READ into wrong type (string vs number)
ON ERROR GOTO handler
DIM x AS LONG, s AS STRING
READ x
PRINT "x:"; x
READ s
PRINT "s:"; s
DATA 42, hello
END
handler:
PRINT "err"; ERR; "erl"; ERL
RESUME NEXT
