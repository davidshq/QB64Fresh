$CONSOLE:ONLY
' Runtime comparison: SUB without CALL keyword
DIM n AS LONG
n = 0
mySub n
PRINT "after:"; n
END

SUB mySub (x AS LONG)
    x = 1
END SUB
