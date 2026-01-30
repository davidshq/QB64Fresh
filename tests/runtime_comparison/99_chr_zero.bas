$CONSOLE:ONLY
' Runtime comparison: CHR$(0)
DIM s AS STRING
s = CHR$(0) + "x" + CHR$(0)
PRINT "len chr0 x chr0:"; LEN(s)
PRINT "asc pos 1:"; ASC(s, 1)
PRINT "asc pos 2:"; ASC(s, 2)
END
