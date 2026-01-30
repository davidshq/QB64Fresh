$CONSOLE:ONLY
' Runtime comparison: SUB with many local variables
CALL manyVars
END

SUB manyVars
    DIM a AS LONG, b AS LONG, c AS LONG, d AS LONG, e AS LONG
    DIM f AS LONG, g AS LONG, h AS LONG, i AS LONG, j AS LONG
    a = 1: b = 2: c = 3: d = 4: e = 5
    f = 6: g = 7: h = 8: i = 9: j = 10
    PRINT a + b + c + d + e + f + g + h + i + j
END SUB
