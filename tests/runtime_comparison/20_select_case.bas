$CONSOLE:ONLY
' Runtime comparison: SELECT CASE
DIM n AS LONG
n = 1
SELECT CASE n
    CASE 1
        PRINT "case 1"
    CASE 2
        PRINT "case 2"
    CASE ELSE
        PRINT "else"
END SELECT
n = 2
SELECT CASE n
    CASE 1
        PRINT "case 1"
    CASE 2
        PRINT "case 2"
    CASE ELSE
        PRINT "else"
END SELECT
n = 9
SELECT CASE n
    CASE 1
        PRINT "case 1"
    CASE 2
        PRINT "case 2"
    CASE ELSE
        PRINT "else"
END SELECT
END
