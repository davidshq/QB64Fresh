$CONSOLE:ONLY
' Runtime comparison: SELECT CASE multiple CASE
DIM n AS LONG
n = 1
SELECT CASE n
    CASE 1, 2, 3
        PRINT "one two three"
    CASE 4, 5
        PRINT "four five"
    CASE ELSE
        PRINT "else"
END SELECT
n = 4
SELECT CASE n
    CASE 1, 2, 3
        PRINT "one two three"
    CASE 4, 5
        PRINT "four five"
    CASE ELSE
        PRINT "else"
END SELECT
END
