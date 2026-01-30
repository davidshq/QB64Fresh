$CONSOLE:ONLY
' Runtime comparison: TYPE with array field
TYPE WithArray
    id AS LONG
    arr(1 TO 3) AS LONG
END TYPE
DIM w AS WithArray
w.id = 1
w.arr(1) = 10
w.arr(2) = 20
w.arr(3) = 30
PRINT w.id; w.arr(1); w.arr(2); w.arr(3)
END
