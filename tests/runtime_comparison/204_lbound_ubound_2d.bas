$CONSOLE:ONLY
' Runtime comparison: LBOUND/UBOUND for each dimension (2D)
DIM m(1 TO 2, 3 TO 5) AS LONG
PRINT "dim1:"; LBOUND(m, 1); UBOUND(m, 1)
PRINT "dim2:"; LBOUND(m, 2); UBOUND(m, 2)
END
