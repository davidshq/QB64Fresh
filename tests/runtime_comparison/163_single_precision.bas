$CONSOLE:ONLY
' Runtime comparison: Single vs double rounding
DIM s AS SINGLE, d AS DOUBLE
s = 1.1!
d = 1.1#
PRINT s
PRINT d
s = 123456.789!
PRINT s
END
