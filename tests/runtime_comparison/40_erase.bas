$CONSOLE:ONLY
' Runtime comparison: ERASE
DIM arr(1 TO 3) AS LONG
arr(1) = 1
arr(2) = 2
arr(3) = 3
PRINT "before:"; arr(1); arr(2); arr(3)
ERASE arr
REDIM arr(1 TO 2) AS LONG
arr(1) = 10
arr(2) = 20
PRINT "after:"; arr(1); arr(2)
END
