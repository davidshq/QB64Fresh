$CONSOLE:ONLY
' Runtime comparison 236: OPTION BASE 1 sets array lower bound
OPTION BASE 1
DIM a(5) AS LONG
a(1) = 1
a(5) = 5
PRINT "lbound"; LBOUND(a); "ubound"; UBOUND(a); "a(1)"; a(1); "a(5)"; a(5)
END
