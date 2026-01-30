$CONSOLE:ONLY
' Runtime comparison: FUNCTION returning DOUBLE
PRINT "half:"; half(5.0)
END

FUNCTION half (x AS DOUBLE) AS DOUBLE
    half = x / 2.0
END FUNCTION
