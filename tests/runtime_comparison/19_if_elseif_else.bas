$CONSOLE:ONLY
' Runtime comparison: IF/ELSEIF/ELSE
DIM x AS LONG
x = 1
IF x = 1 THEN PRINT "one" ELSE PRINT "not one"
x = 2
IF x = 1 THEN PRINT "one" ELSEIF x = 2 THEN PRINT "two" ELSE PRINT "other"
x = 3
IF x = 1 THEN PRINT "one" ELSEIF x = 2 THEN PRINT "two" ELSE PRINT "other"
END
