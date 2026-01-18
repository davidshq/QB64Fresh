' Test SUB and FUNCTION
CALL PrintHello
PRINT Square(5)
END

SUB PrintHello
    PRINT "Hello from SUB"
END SUB

FUNCTION Square(x AS LONG) AS LONG
    Square = x * x
END FUNCTION
