$CONSOLE:ONLY
' Runtime comparison: FUNCTION returning SINGLE
PRINT "third:"; third(6.0)
END

FUNCTION third (x AS SINGLE) AS SINGLE
    third = x / 3.0
END FUNCTION
