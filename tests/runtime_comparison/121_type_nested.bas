$CONSOLE:ONLY
' Runtime comparison: nested TYPE
TYPE Inner
    v AS LONG
END TYPE
TYPE Outer
    i AS Inner
    x AS LONG
END TYPE
DIM o AS Outer
o.i.v = 5
o.x = 10
PRINT "i.v:"; o.i.v; "x:"; o.x
END
