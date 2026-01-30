$CONSOLE:ONLY
' Runtime comparison: SUB with string param
DIM s AS STRING
s = "hello"
CALL printStr(s)
END

SUB printStr (x AS STRING)
    PRINT "got:"; x
END SUB
