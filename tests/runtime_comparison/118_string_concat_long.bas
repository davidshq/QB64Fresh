$CONSOLE:ONLY
' Runtime comparison: long string concat
DIM s AS STRING
s = "a" + "b" + "c" + "d" + "e"
PRINT "concat:"; s
PRINT "len:"; LEN(s)
END
