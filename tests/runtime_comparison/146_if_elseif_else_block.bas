$CONSOLE:ONLY
' Runtime comparison: IF / ELSEIF / ELSE blocks, PRINT per branch
DIM x AS LONG
x = 1
IF x = 1 THEN
    PRINT "branch one"
ELSEIF x = 2 THEN
    PRINT "branch two"
ELSE
    PRINT "branch else"
END IF
x = 2
IF x = 1 THEN
    PRINT "branch one"
ELSEIF x = 2 THEN
    PRINT "branch two"
ELSE
    PRINT "branch else"
END IF
x = 3
IF x = 1 THEN
    PRINT "branch one"
ELSEIF x = 2 THEN
    PRINT "branch two"
ELSE
    PRINT "branch else"
END IF
END
