$CONSOLE:ONLY
' Runtime comparison: STATIC in SUB
CALL count
CALL count
CALL count
END

SUB count
    STATIC n AS LONG
    n = n + 1
    PRINT "count:"; n
END SUB
