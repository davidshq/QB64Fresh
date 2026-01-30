$CONSOLE:ONLY
' Runtime comparison: recursive FUNCTION
PRINT "fact 5:"; fact(5)
PRINT "fact 0:"; fact(0)
END

FUNCTION fact (n AS LONG) AS LONG
    IF n <= 1 THEN
        fact = 1
    ELSE
        fact = n * fact(n - 1)
    END IF
END FUNCTION
