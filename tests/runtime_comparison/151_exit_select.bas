$CONSOLE:ONLY
' Runtime comparison: EXIT SELECT (if supported)
DIM n AS LONG
n = 2
SELECT CASE n
    CASE 1
        PRINT "one"
    CASE 2
        PRINT "two"
        EXIT SELECT
    CASE ELSE
        PRINT "other"
END SELECT
PRINT "done"
END
