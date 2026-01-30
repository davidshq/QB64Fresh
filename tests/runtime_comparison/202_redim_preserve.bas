$CONSOLE:ONLY
' Runtime comparison: REDIM PRESERVE grow/shrink
DIM a() AS LONG
REDIM a(1 TO 3) AS LONG
a(1) = 1
a(2) = 2
a(3) = 3
PRINT "initial:"; a(1); a(2); a(3)
REDIM PRESERVE a(1 TO 5) AS LONG
a(4) = 4
a(5) = 5
PRINT "after grow:"; a(1); a(2); a(3); a(4); a(5)
REDIM PRESERVE a(1 TO 2) AS LONG
PRINT "after shrink:"; a(1); a(2)
END
