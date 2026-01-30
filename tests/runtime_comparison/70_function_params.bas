$CONSOLE:ONLY
' Runtime comparison: FUNCTION with multiple params
PRINT "sum:"; add(10, 20)
PRINT "sum:"; add(1, 2)
END

FUNCTION add (a AS LONG, b AS LONG) AS LONG
    add = a + b
END FUNCTION
