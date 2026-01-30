$CONSOLE:ONLY
' Runtime comparison: NOT in condition
DIM x AS LONG
x = 0
IF NOT x THEN PRINT "not zero"
x = 1
IF NOT (x = 0) THEN PRINT "x not zero"
END
