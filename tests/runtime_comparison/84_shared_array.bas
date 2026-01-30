$CONSOLE:ONLY
' Runtime comparison: SHARED in SUB
DIM g AS LONG
g = 10
CALL useShared
PRINT "after:"; g
END

SUB useShared
    SHARED g AS LONG
    PRINT "in sub g:"; g
    g = 20
END SUB
