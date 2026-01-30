$CONSOLE:ONLY
' Runtime comparison: REDIM
DIM a() AS LONG
REDIM a(1 TO 3) AS LONG
a(1) = 10
a(2) = 20
a(3) = 30
PRINT "a(1):"; a(1); "a(2):"; a(2); "a(3):"; a(3)
REDIM PRESERVE a(1 TO 5) AS LONG
a(4) = 40
a(5) = 50
PRINT "after preserve a(1):"; a(1); "a(4):"; a(4); "a(5):"; a(5)
END
