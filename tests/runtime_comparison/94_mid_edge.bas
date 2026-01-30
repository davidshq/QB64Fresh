$CONSOLE:ONLY
' Runtime comparison: MID$ edge cases
PRINT "mid 1 10 of ab:"; MID$("ab", 1, 10)
PRINT "mid 5 1 of ab:"; MID$("ab", 5, 1)
PRINT "mid 1 0 of ab:"; MID$("ab", 1, 0)
END
