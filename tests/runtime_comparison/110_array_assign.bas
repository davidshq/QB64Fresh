$CONSOLE:ONLY
' Runtime comparison: array element assignment in expression context
DIM a(1 TO 3) AS LONG
a(1) = 1
a(2) = 2
a(3) = 3
PRINT "a(2):"; a(2)
a(2) = a(1) + a(3)
PRINT "after a(2)=a(1)+a(3):"; a(2)
END
