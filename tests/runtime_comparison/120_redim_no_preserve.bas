$CONSOLE:ONLY
' Runtime comparison: REDIM without PRESERVE
DIM a() AS LONG
REDIM a(1 TO 3) AS LONG
a(1) = 1
a(2) = 2
a(3) = 3
REDIM a(1 TO 2) AS LONG
a(1) = 10
a(2) = 20
PRINT "after redim:"; a(1); a(2)
END
