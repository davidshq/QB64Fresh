$CONSOLE:ONLY
' Runtime comparison: 3D array
DIM a(1 TO 2, 1 TO 2, 1 TO 2) AS LONG
a(1, 1, 1) = 1
a(1, 1, 2) = 2
a(1, 2, 1) = 3
a(1, 2, 2) = 4
a(2, 1, 1) = 5
a(2, 1, 2) = 6
a(2, 2, 1) = 7
a(2, 2, 2) = 8
PRINT a(1, 1, 1); a(2, 2, 2)
PRINT "lb1:"; LBOUND(a, 1); "ub3:"; UBOUND(a, 3)
END
