' ============================================================================
' Procedures: SUB and FUNCTION
' ============================================================================
' Demonstrates: SUB, FUNCTION, parameters, return values, SHARED
' ============================================================================

' Shared variable accessible from procedures
DIM SHARED counter AS INTEGER
counter = 0

PRINT "=== Calling SUBs ==="

' Call a SUB (no return value)
CALL SayHello("Alice")
SayHello "Bob"          ' CALL keyword is optional
PRINT

PRINT "=== Calling FUNCTIONs ==="

' Call FUNCTION (returns a value)
DIM result AS INTEGER
result = Add(10, 20)
PRINT "Add(10, 20) = "; result

result = Multiply(5, 6)
PRINT "Multiply(5, 6) = "; result

DIM fact AS LONG
fact = Factorial(5)
PRINT "Factorial(5) = "; fact
PRINT

PRINT "=== Using SHARED Variables ==="
IncrementCounter
IncrementCounter
IncrementCounter
PRINT "Counter after 3 increments: "; counter
PRINT

PRINT "=== Pass by Reference vs Value ==="

DIM x AS INTEGER
x = 100

PRINT "Before PassByRef: x = "; x
PassByRef x
PRINT "After PassByRef:  x = "; x

x = 100
PRINT "Before PassByVal: x = "; x
PassByVal x
PRINT "After PassByVal:  x = "; x
PRINT

PRINT "=== String Function ==="
DIM greeting AS STRING
greeting = MakeGreeting$("World")
PRINT greeting

END

' ============================================================================
' SUB Definitions
' ============================================================================

SUB SayHello (name AS STRING)
    PRINT "Hello, "; name; "!"
END SUB

SUB IncrementCounter
    ' Uses SHARED variable
    counter = counter + 1
END SUB

SUB PassByRef (value AS INTEGER)
    ' Parameters are passed by reference by default
    value = value * 2    ' Modifies the original variable
END SUB

SUB PassByVal (BYVAL value AS INTEGER)
    ' BYVAL creates a copy - original is not modified
    value = value * 2
END SUB

' ============================================================================
' FUNCTION Definitions
' ============================================================================

FUNCTION Add (a AS INTEGER, b AS INTEGER)
    Add = a + b
END FUNCTION

FUNCTION Multiply (a AS INTEGER, b AS INTEGER)
    Multiply = a * b
END FUNCTION

FUNCTION Factorial (n AS INTEGER)
    IF n <= 1 THEN
        Factorial = 1
    ELSE
        Factorial = n * Factorial(n - 1)  ' Recursive call
    END IF
END FUNCTION

FUNCTION MakeGreeting$ (name AS STRING)
    ' $ suffix indicates STRING return type
    MakeGreeting$ = "Greetings, " + name + "!"
END FUNCTION
