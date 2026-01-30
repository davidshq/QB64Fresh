$CONSOLE:ONLY
' Runtime comparison: SELECT CASE with CASE ELSE (same as 20, extra CASE ELSE coverage)
DIM n AS LONG
n = 1
SELECT CASE n
    CASE 1
        PRINT "one"
    CASE 2
        PRINT "two"
    CASE ELSE
        PRINT "other"
END SELECT
n = 99
SELECT CASE n
    CASE 1
        PRINT "one"
    CASE 2
        PRINT "two"
    CASE ELSE
        PRINT "other"
END SELECT
END
