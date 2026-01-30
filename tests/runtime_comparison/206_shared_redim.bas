$CONSOLE:ONLY
' Runtime comparison: SHARED array then REDIM in SUB (blocked if REDIM not supported)
DIM a(1 TO 2) AS LONG
a(1) = 10
a(2) = 20
CALL showA
PRINT a(1); a(2)
END

SUB showA
    SHARED a() AS LONG
    PRINT "in sub:"; a(1); a(2)
END SUB
