$CONSOLE:ONLY
' Runtime comparison: UDT passed to SUB/FUNCTION
TYPE T
    n AS LONG
END TYPE
DIM x AS T
x.n = 7
PRINT getN(x)
END

FUNCTION getN (p AS T) AS LONG
    getN = p.n
END FUNCTION
