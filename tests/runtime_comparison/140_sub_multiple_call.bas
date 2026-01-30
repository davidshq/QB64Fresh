$CONSOLE:ONLY
' Runtime comparison: same SUB called twice
CALL p
CALL p
END

SUB p
    PRINT "p"
END SUB
