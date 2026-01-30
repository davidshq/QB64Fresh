$CONSOLE:ONLY
' Runtime comparison: FUNCTION returning string
DIM s AS STRING
s = greet$("world")
PRINT s
END

FUNCTION greet$ (name AS STRING)
    greet$ = "Hello " + name
END FUNCTION
