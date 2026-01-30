$CONSOLE:ONLY
' Runtime comparison: TYPE with fixed-length string (same as 57)
TYPE TStr
    s AS STRING * 5
END TYPE
DIM t AS TStr
t.s = "hi"
PRINT "["; t.s; "]"
t.s = "hello"
PRINT "["; t.s; "]"
END
