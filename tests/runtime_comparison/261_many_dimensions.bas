$CONSOLE:ONLY
' Runtime comparison: Array with 4 dimensions (we have 72_three_dim for 3D)
DIM a(1 TO 2, 1 TO 2, 1 TO 2, 1 TO 2) AS LONG
a(1, 1, 1, 1) = 1
a(2, 2, 2, 2) = 16
PRINT a(1, 1, 1, 1); a(2, 2, 2, 2)
PRINT LBOUND(a, 1); UBOUND(a, 1)
PRINT LBOUND(a, 4); UBOUND(a, 4)
END
