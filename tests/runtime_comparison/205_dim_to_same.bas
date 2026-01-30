$CONSOLE:ONLY
' Runtime comparison: DIM same array twice (error or overwrite)
ON ERROR GOTO handler
DIM a(1 TO 2) AS LONG
a(1) = 1
a(2) = 2
DIM a(1 TO 2) AS LONG
PRINT "no error:"; a(1); a(2)
END
handler:
PRINT "err"; ERR; "erl"; ERL
RESUME NEXT
