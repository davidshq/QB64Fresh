$CONSOLE:ONLY
' Runtime comparison: array of UDT
TYPE T
    v AS LONG
END TYPE
DIM a(1 TO 3) AS T
a(1).v = 1
a(2).v = 2
a(3).v = 3
PRINT a(1).v; a(2).v; a(3).v
END
