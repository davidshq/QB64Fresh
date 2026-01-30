$CONSOLE:ONLY
' Runtime comparison: STR$ leading space for positive (QB4.5 behavior)
' STR$(x) returns " 42" for positive (leading space), "-42" for negative
DIM n AS LONG
n = 99
PRINT "["; STR$(n); "]"
PRINT "["; STR$(-n); "]"
PRINT "["; STR$(0); "]"
END
