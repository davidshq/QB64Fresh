$CONSOLE:ONLY
' Runtime comparison: Whole TYPE assignment (a = b)
TYPE P
    x AS LONG
    y AS LONG
END TYPE
DIM a AS P, b AS P
a.x = 1
a.y = 2
b = a
PRINT b.x; b.y
b.x = 99
PRINT a.x; b.x
END
