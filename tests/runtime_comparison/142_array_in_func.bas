$CONSOLE:ONLY
' Runtime comparison: array element as argument
DIM a(1 TO 3) AS LONG
a(1) = 5
a(2) = 10
PRINT "sum a(1) a(2):"; add(a(1), a(2))
END

FUNCTION add (x AS LONG, y AS LONG) AS LONG
    add = x + y
END FUNCTION
