$CONSOLE:ONLY
' Runtime comparison: MID$ with 2 args (rest of string)
DIM s AS STRING
s = "abcdef"
PRINT "mid 3:"; MID$(s, 3)
PRINT "mid 6:"; MID$(s, 6)
END
