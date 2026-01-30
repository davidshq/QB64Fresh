$CONSOLE:ONLY
' Runtime comparison: function in expression
PRINT "2*double(5):"; 2 * doubleIt(5)
END

FUNCTION doubleIt (n AS LONG) AS LONG
    doubleIt = n * 2
END FUNCTION
