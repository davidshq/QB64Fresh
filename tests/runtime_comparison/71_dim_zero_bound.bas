$CONSOLE:ONLY
' Runtime comparison: DIM with 0 lower bound
DIM a(0 TO 2) AS LONG
a(0) = 0
a(1) = 1
a(2) = 2
PRINT "lb:"; LBOUND(a); "ub:"; UBOUND(a)
PRINT a(0); a(1); a(2)
END
