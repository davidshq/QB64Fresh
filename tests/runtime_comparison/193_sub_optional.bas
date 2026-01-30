$CONSOLE:ONLY
' Runtime comparison: SUB with optional args (if supported)
SUB bar (x AS LONG)
    PRINT "bar:"; x
END SUB

CALL bar(5)
END
