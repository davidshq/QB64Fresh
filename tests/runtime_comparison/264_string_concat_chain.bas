$CONSOLE:ONLY
' Runtime comparison: A$ = B$ + C$ + ... many times
DIM a AS STRING, b AS STRING, c AS STRING, d AS STRING
b = "b"
c = "c"
d = "d"
a = b + c + d + b + c + d + b + c
PRINT a
PRINT LEN(a)
END
