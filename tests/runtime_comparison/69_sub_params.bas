$CONSOLE:ONLY
' Runtime comparison: SUB with multiple params
CALL show(1, 2, 3)
END

SUB show (a AS LONG, b AS LONG, c AS LONG)
    PRINT "a:"; a; "b:"; b; "c:"; c
END SUB
