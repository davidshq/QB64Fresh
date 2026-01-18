' Hello World example for QB64Fresh
' This is a simple program to test the lexer

PRINT "Hello, World!"

DIM userName AS STRING
INPUT "What is your name? ", userName
PRINT "Hello, "; userName; "!"

' A simple loop
FOR i = 1 TO 5
    PRINT "Count: "; i
NEXT i

' Math example
DIM result AS DOUBLE
result = (10 + 5) * 2 / 3.14
PRINT "Result = "; result

' Hex and binary literals (QB64 extension)
DIM hex_val AS LONG
hex_val = &HFF
PRINT "Hex FF = "; hex_val

END
