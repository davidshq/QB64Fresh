$CONSOLE:ONLY
' Runtime comparison: Single PRINT with very long string
DIM s AS STRING
s = STRING$(200, "x")
PRINT s
PRINT "len:"; LEN(s)
END
