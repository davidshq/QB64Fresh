$CONSOLE:ONLY
' Runtime comparison: RESTORE without label (next READ from start)
DIM a AS LONG, b AS LONG
READ a
READ b
PRINT a; b
RESTORE
READ a
READ b
PRINT a; b
DATA 1, 2
END
