$CONSOLE:ONLY
' Runtime comparison: MID$ assignment
DIM s AS STRING
s = "abcdef"
MID$(s, 2, 2) = "XY"
PRINT "mid assign:"; s
END
