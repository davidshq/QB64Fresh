$CONSOLE:ONLY
' Runtime comparison: MID$ on empty string, start > LEN
DIM s AS STRING
s = ""
PRINT "len:"; LEN(s)
PRINT "mid(1,1):"; MID$(s, 1, 1)
s = "ab"
PRINT "mid(5,1):"; MID$(s, 5, 1)
PRINT "mid(1,0):"; MID$(s, 1, 0)
END
