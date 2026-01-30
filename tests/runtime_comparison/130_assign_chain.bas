$CONSOLE:ONLY
' Runtime comparison: multiple assignments
DIM a AS LONG, b AS LONG, c AS LONG
a = 1
b = 2
c = 3
a = b
b = c
PRINT "a:"; a; "b:"; b; "c:"; c
END
