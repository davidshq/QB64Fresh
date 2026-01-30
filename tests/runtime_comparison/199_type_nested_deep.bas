$CONSOLE:ONLY
' Runtime comparison: Nested TYPE 3+ levels
TYPE A
    v AS LONG
END TYPE
TYPE B
    a AS A
END TYPE
TYPE C
    b AS B
END TYPE
DIM c AS C
c.b.a.v = 42
PRINT c.b.a.v
END
