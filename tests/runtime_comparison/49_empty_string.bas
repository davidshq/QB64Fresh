$CONSOLE:ONLY
' Runtime comparison: empty string
DIM s AS STRING
s = ""
PRINT "len empty:"; LEN(s)
PRINT "left 2:"; LEFT$(s, 2)
PRINT "val empty:"; VAL("")
END
