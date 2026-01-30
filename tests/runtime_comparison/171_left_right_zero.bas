$CONSOLE:ONLY
' Runtime comparison: LEFT$/RIGHT$ with 0 length
PRINT "["; LEFT$("abc", 0); "]"
PRINT "["; RIGHT$("abc", 0); "]"
PRINT "["; LEFT$("abc", 1); "]"
PRINT "["; RIGHT$("abc", 1); "]"
END
