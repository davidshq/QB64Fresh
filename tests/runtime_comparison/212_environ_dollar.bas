$CONSOLE:ONLY
' Runtime comparison: ENVIRON$("VAR") - output may differ by environment
DIM s AS STRING
s = ENVIRON$("PATH")
PRINT "PATH len:"; LEN(s)
s = ENVIRON$("NONEXISTENT_VAR_12345")
PRINT "nonexist len:"; LEN(s)
END
