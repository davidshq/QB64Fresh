$CONSOLE:ONLY
' Runtime comparison 1: String operations
' Concat, LEN, MID$, LEFT$, RIGHT$, comparison
DIM a AS STRING
DIM b AS STRING
a = "Hello"
b = "World"
PRINT "concat: "; a + " " + b
PRINT "len a: "; LEN(a)
PRINT "mid 2,3: "; MID$(a, 2, 3)
PRINT "left 2: "; LEFT$(a, 2)
PRINT "right 2: "; RIGHT$(a, 2)
PRINT "a<b: "; (a < b)
PRINT "a=a: "; (a = a)
END
