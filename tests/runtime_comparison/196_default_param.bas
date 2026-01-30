$CONSOLE:ONLY
' Runtime comparison: Default parameter values (if supported)
SUB baz (a AS LONG, b AS LONG)
    PRINT "baz:"; a; b
END SUB

CALL baz(1, 2)
END
