$CONSOLE:ONLY
' Runtime comparison: AND/OR in condition
DIM a AS LONG, b AS LONG
a = 1
b = 1
IF a = 1 AND b = 1 THEN PRINT "both"
IF a = 0 OR b = 1 THEN PRINT "or"
END
