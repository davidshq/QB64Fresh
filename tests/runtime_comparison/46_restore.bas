$CONSOLE:ONLY
' Runtime comparison: RESTORE
DIM x AS LONG
READ x
PRINT "first:"; x
RESTORE
READ x
PRINT "after restore:"; x
END

DATA 99
