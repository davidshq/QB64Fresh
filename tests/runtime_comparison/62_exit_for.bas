$CONSOLE:ONLY
' Runtime comparison: EXIT FOR
DIM i AS LONG
FOR i = 1 TO 10
    PRINT i;
    IF i = 3 THEN EXIT FOR
NEXT i
PRINT
PRINT "done"
END
