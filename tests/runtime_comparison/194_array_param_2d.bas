$CONSOLE:ONLY
' Runtime comparison: 2D array passed to SUB/FUNCTION
DIM m(1 TO 2, 1 TO 2) AS LONG
m(1, 1) = 1
m(1, 2) = 2
m(2, 1) = 3
m(2, 2) = 4
PRINT sum2d(m())
END

FUNCTION sum2d (a(,) AS LONG) AS LONG
    DIM i AS LONG, j AS LONG, s AS LONG
    s = 0
    FOR i = LBOUND(a, 1) TO UBOUND(a, 1)
        FOR j = LBOUND(a, 2) TO UBOUND(a, 2)
            s = s + a(i, j)
        NEXT j
    NEXT i
    sum2d = s
END FUNCTION
