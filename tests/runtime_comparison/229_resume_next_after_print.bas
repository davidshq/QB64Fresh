$CONSOLE:ONLY
' Runtime comparison: RESUME NEXT then use variable
ON ERROR GOTO handler
DIM x AS LONG
x = 1 / 0
PRINT "x:"; x
END
handler:
PRINT "handler"
RESUME NEXT
