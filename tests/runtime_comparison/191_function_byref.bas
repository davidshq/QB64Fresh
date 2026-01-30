$CONSOLE:ONLY
' Runtime comparison: FUNCTION with BYREF (if supported)
DIM v AS LONG
v = 10
PRINT "before:"; v
PRINT "double returns:"; doubleIt(v)
PRINT "after:"; v
END

FUNCTION doubleIt (BYREF x AS LONG) AS LONG
    x = x * 2
    doubleIt = x
END FUNCTION
