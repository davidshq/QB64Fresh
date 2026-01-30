$CONSOLE:ONLY
' Runtime comparison: operator precedence (NOT, ^, * before +)
PRINT (2 ^ 2 * 2); ((2 ^ 2) * 2)
PRINT (3 * 6 / 2); ((3 * 6) / 2)
PRINT (3 * 10 \ 3); ((3 * 10) \ 3)
PRINT (NOT 0); (NOT 1)
PRINT (-2 ^ 2); (-(2 ^ 2))
END
