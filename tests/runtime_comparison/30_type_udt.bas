$CONSOLE:ONLY
' Runtime comparison: TYPE/END TYPE
TYPE Point
    x AS LONG
    y AS LONG
END TYPE
DIM p AS Point
p.x = 10
p.y = 20
PRINT "p.x:"; p.x; "p.y:"; p.y
END
