$CONSOLE:ONLY
' Runtime comparison: TYPE with string field
TYPE Person
    name AS STRING
    age AS LONG
END TYPE
DIM p AS Person
p.name = "Alice"
p.age = 30
PRINT "name:"; p.name; "age:"; p.age
END
