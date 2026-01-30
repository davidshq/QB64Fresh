$CONSOLE:ONLY
' Runtime comparison: GOTO same line (no-op or loop)
DIM n AS LONG
n = 0
label1:
n = n + 1
IF n < 2 THEN GOTO label1
PRINT "n:"; n
END
