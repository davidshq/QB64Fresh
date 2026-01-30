$CONSOLE:ONLY
' Runtime comparison: Optional parameters (if supported)
' If OPTIONAL not supported, this tests SUB with two params called with two args
SUB foo (a AS LONG, b AS LONG)
    PRINT "foo:"; a; b
END SUB

CALL foo(1, 2)
CALL foo(10, 20)
END
