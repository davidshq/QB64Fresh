$CONSOLE:ONLY
' Runtime comparison: SELECT CASE 1 TO 5
DIM n AS LONG
n = 3
SELECT CASE n
    CASE 1 TO 5
        PRINT "in range"
    CASE ELSE
        PRINT "else"
END SELECT
n = 10
SELECT CASE n
    CASE 1 TO 5
        PRINT "in range"
    CASE ELSE
        PRINT "else"
END SELECT
END
