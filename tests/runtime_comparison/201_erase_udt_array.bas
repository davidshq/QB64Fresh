$CONSOLE:ONLY
' Runtime comparison: ERASE on array of UDT
TYPE E
    n AS LONG
END TYPE
DIM arr(1 TO 2) AS E
arr(1).n = 10
arr(2).n = 20
PRINT arr(1).n; arr(2).n
ERASE arr
PRINT "after erase:"; arr(1).n; arr(2).n
END
