$CONSOLE:ONLY
' Runtime comparison: WHILE false (body not executed)
DIM n AS LONG
n = 5
WHILE n < 3
    PRINT "never"
WEND
PRINT "done"
END
