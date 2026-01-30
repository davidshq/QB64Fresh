$CONSOLE:ONLY
' Runtime comparison: Nested WHILE/WEND
DIM i AS LONG, j AS LONG
i = 0
WHILE i < 2
    j = 0
    WHILE j < 2
        PRINT i; j
        j = j + 1
    WEND
    i = i + 1
WEND
PRINT "done"
END
