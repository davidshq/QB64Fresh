$CONSOLE:ONLY
' Runtime comparison: IF THEN single line
DIM x AS LONG
x = 1
IF x = 1 THEN PRINT "one"
IF x = 2 THEN PRINT "two"
x = 2
IF x = 2 THEN PRINT "two"
END
