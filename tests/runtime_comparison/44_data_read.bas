$CONSOLE:ONLY
' Runtime comparison: DATA/READ
DIM x AS LONG, y AS STRING
READ x, y
PRINT "read:"; x; y
READ x, y
PRINT "read:"; x; y
END

DATA 10, hello
DATA 20, world
