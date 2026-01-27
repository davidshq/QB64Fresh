' Test STRING * n implicit conversion
' This should compile without type mismatch errors

TYPE Person
    Name AS STRING * 20
    Address AS STRING * 50
END TYPE

DIM p AS Person
DIM s AS STRING
DIM fixed AS STRING * 30

' Test 1: STRING to STRING * n (should pad/truncate)
p.Name = "John"
p.Name = s
fixed = "Hello"
fixed = s

' Test 2: STRING * n to STRING (should work)
s = p.Name
s = fixed

' Test 3: STRING * n to STRING * n (different lengths)
DIM fixed2 AS STRING * 10
fixed2 = fixed
fixed = fixed2

PRINT "All conversions successful!"
