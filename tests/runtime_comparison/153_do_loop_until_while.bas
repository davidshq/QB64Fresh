$CONSOLE:ONLY
' Runtime comparison: DO LOOP UNTIL vs DO LOOP WHILE
DIM n AS LONG
PRINT "DO LOOP UNTIL n>=2:"
n = 0
DO
    PRINT n;
    n = n + 1
LOOP UNTIL n >= 2
PRINT
PRINT "DO LOOP WHILE n<2:"
n = 0
DO
    PRINT n;
    n = n + 1
LOOP WHILE n < 2
PRINT
END
