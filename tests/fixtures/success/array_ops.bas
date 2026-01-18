' Test array operations
DIM arr(10) AS LONG
DIM i AS LONG

' Fill array
FOR i = 0 TO 10
    arr(i) = i * 2
NEXT i

' Read back
PRINT arr(5)
PRINT arr(10)
END
