$CONSOLE:ONLY
' Runtime comparison: LEFT$/RIGHT$ edge cases
PRINT "left 10 of ab:"; LEFT$("ab", 10)
PRINT "right 10 of ab:"; RIGHT$("ab", 10)
PRINT "left 0:"; LEFT$("ab", 0)
PRINT "right 0:"; RIGHT$("ab", 0)
END
