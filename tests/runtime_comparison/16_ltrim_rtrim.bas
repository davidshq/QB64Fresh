$CONSOLE:ONLY
' Runtime comparison: LTRIM$, RTRIM$, TRIM$
PRINT "ltrim:"; LTRIM$("  ab")
PRINT "rtrim:"; RTRIM$("ab  ")
PRINT "trim:"; TRIM$("  xy  ")
END
