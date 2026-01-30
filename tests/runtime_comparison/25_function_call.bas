$CONSOLE:ONLY
' Runtime comparison: FUNCTION
DIM r AS LONG
r = doubleIt(7)
PRINT "double 7:"; r
PRINT "double 10:"; doubleIt(10)
END

FUNCTION doubleIt (n AS LONG) AS LONG
    doubleIt = n * 2
END FUNCTION
