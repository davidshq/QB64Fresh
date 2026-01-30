$CONSOLE:ONLY
' Runtime comparison: EXIT DO
DIM n AS LONG
n = 0
DO
    PRINT n;
    n = n + 1
    IF n >= 3 THEN EXIT DO
LOOP
PRINT
PRINT "done"
END
