' Simple test program for code generation
PRINT "Hello, World!"

DIM x AS LONG
x = 42
PRINT "x = "; x

DIM y AS DOUBLE
y = 3.14
PRINT "y = "; y

' Simple loop
FOR i = 1 TO 3
    PRINT "Count: "; i
NEXT i

' Math
DIM result AS DOUBLE
result = (10 + 5) * 2
PRINT "Result = "; result

IF x > 10 THEN
    PRINT "x is greater than 10"
ELSE
    PRINT "x is not greater than 10"
END IF

END
