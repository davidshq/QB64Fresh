$CONSOLE:ONLY
' Runtime comparison: FOR with start > end (no STEP)
DIM i AS LONG
FOR i = 5 TO 1
    PRINT "never"; i
NEXT i
PRINT "done"
END
