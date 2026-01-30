$CONSOLE:ONLY
' Runtime comparison: RESTORE label
DIM x AS LONG
READ x
PRINT "first:"; x
RESTORE second
READ x
PRINT "second block:"; x
END

first:
DATA 1, 2, 3
second:
DATA 10, 20
