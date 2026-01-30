$CONSOLE:ONLY
' Runtime comparison 2: LBOUND/UBOUND
' DIM with explicit bounds, print bounds and iterate
DIM arr(1 TO 5) AS LONG
DIM i AS LONG
PRINT "lbound: "; LBOUND(arr)
PRINT "ubound: "; UBOUND(arr)
FOR i = LBOUND(arr) TO UBOUND(arr)
    arr(i) = i * 10
NEXT i
FOR i = LBOUND(arr) TO UBOUND(arr)
    PRINT arr(i);
NEXT i
PRINT
END
