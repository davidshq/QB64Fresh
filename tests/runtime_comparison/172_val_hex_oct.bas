$CONSOLE:ONLY
' Runtime comparison: VAL with &H/&O prefix in string
PRINT VAL("&HFF")
PRINT VAL("&O77")
PRINT VAL("123")
PRINT VAL("  &H10 ")
END
