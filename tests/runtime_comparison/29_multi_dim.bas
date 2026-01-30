$CONSOLE:ONLY
' Runtime comparison: multi-dimensional array
DIM m(1 TO 2, 1 TO 3) AS LONG
DIM i AS LONG, j AS LONG
m(1, 1) = 1
m(1, 2) = 2
m(1, 3) = 3
m(2, 1) = 4
m(2, 2) = 5
m(2, 3) = 6
FOR i = 1 TO 2
    FOR j = 1 TO 3
        PRINT m(i, j);
    NEXT j
NEXT i
PRINT
PRINT "lbound1:"; LBOUND(m, 1); "ubound1:"; UBOUND(m, 1)
PRINT "lbound2:"; LBOUND(m, 2); "ubound2:"; UBOUND(m, 2)
END
