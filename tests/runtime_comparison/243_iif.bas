$CONSOLE:ONLY
' Runtime comparison: _IIF (numeric and string) - minimal subset of QB64pe iif_test
DIM x AS LONG
x = 1
PRINT _IIF(x > 0, 10, 20)
PRINT _IIF(x < 0, 10, 20)
PRINT _IIF(x > 0, "yes", "no")
PRINT _IIF(0, 100, 200)
PRINT _IIF(-1, 100, 200)
END
