$CONSOLE:ONLY
' Runtime comparison: SELECT CASE IS
DIM n AS LONG
n = 5
SELECT CASE n
    CASE IS < 3
        PRINT "lt 3"
    CASE IS < 7
        PRINT "lt 7"
    CASE ELSE
        PRINT "else"
END SELECT
n = 1
SELECT CASE n
    CASE IS < 3
        PRINT "lt 3"
    CASE IS < 7
        PRINT "lt 7"
    CASE ELSE
        PRINT "else"
END SELECT
END
