$CONSOLE:ONLY
' Runtime comparison: EXIT FUNCTION
PRINT "result:"; early(5)
END

FUNCTION early (n AS LONG) AS LONG
    IF n > 3 THEN
        early = 99
        EXIT FUNCTION
    END IF
    early = n
END FUNCTION
