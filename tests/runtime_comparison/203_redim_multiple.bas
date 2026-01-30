$CONSOLE:ONLY
' Runtime comparison: REDIM multiple arrays in one statement
DIM a() AS LONG, b() AS LONG
REDIM a(1 TO 2) AS LONG, b(1 TO 2) AS LONG
a(1) = 1
a(2) = 2
b(1) = 10
b(2) = 20
PRINT "a:"; a(1); a(2)
PRINT "b:"; b(1); b(2)
END
