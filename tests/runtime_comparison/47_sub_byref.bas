$CONSOLE:ONLY
' Runtime comparison: SUB with BYREF
DIM n AS LONG
n = 5
CALL addOne(n)
PRINT "after addOne:"; n
END

SUB addOne (BYREF x AS LONG)
    x = x + 1
END SUB
