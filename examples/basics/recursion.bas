' ============================================================================
' Recursion Examples
' ============================================================================
' Demonstrates: Recursive functions, mutual recursion, tail recursion
' ============================================================================

PRINT "=== Factorial (Classic Recursion) ==="

FOR n = 0 TO 10
    PRINT "Factorial("; n; ") = "; Factorial(n)
NEXT n
PRINT

PRINT "=== Fibonacci Sequence ==="

PRINT "First 15 Fibonacci numbers:"
FOR n = 0 TO 14
    PRINT Fibonacci(n);
    IF n < 14 THEN PRINT ", ";
NEXT n
PRINT
PRINT

PRINT "=== Sum of Digits ==="

DIM num AS LONG
num = 12345
PRINT "Sum of digits of "; num; " = "; SumDigits(num)

num = 9999
PRINT "Sum of digits of "; num; " = "; SumDigits(num)
PRINT

PRINT "=== Reverse String ==="

DIM text AS STRING
text = "Hello, World!"
PRINT "Original: "; text
PRINT "Reversed: "; ReverseString$(text)
PRINT

PRINT "=== Greatest Common Divisor (Euclid's Algorithm) ==="

PRINT "GCD(48, 18) = "; GCD(48, 18)
PRINT "GCD(100, 35) = "; GCD(100, 35)
PRINT "GCD(270, 192) = "; GCD(270, 192)
PRINT

PRINT "=== Power Function ==="

PRINT "Power(2, 10) = "; Power(2, 10)
PRINT "Power(3, 5) = "; Power(3, 5)
PRINT "Power(5, 0) = "; Power(5, 0)

END

' ============================================================================
' Recursive Functions
' ============================================================================

FUNCTION Factorial (n AS INTEGER)
    ' Base case: 0! = 1
    IF n <= 1 THEN
        Factorial = 1
    ELSE
        ' Recursive case: n! = n * (n-1)!
        Factorial = n * Factorial(n - 1)
    END IF
END FUNCTION

FUNCTION Fibonacci (n AS INTEGER)
    ' Base cases: F(0) = 0, F(1) = 1
    IF n = 0 THEN
        Fibonacci = 0
    ELSEIF n = 1 THEN
        Fibonacci = 1
    ELSE
        ' Recursive case: F(n) = F(n-1) + F(n-2)
        Fibonacci = Fibonacci(n - 1) + Fibonacci(n - 2)
    END IF
END FUNCTION

FUNCTION SumDigits (n AS LONG)
    ' Base case: single digit
    IF n < 10 THEN
        SumDigits = n
    ELSE
        ' Recursive: last digit + sum of remaining digits
        SumDigits = (n MOD 10) + SumDigits(n \ 10)
    END IF
END FUNCTION

FUNCTION ReverseString$ (s AS STRING)
    ' Base case: empty or single character
    IF LEN(s) <= 1 THEN
        ReverseString$ = s
    ELSE
        ' Recursive: last char + reverse of rest
        ReverseString$ = RIGHT$(s, 1) + ReverseString$(LEFT$(s, LEN(s) - 1))
    END IF
END FUNCTION

FUNCTION GCD (a AS INTEGER, b AS INTEGER)
    ' Euclid's algorithm using recursion
    ' Base case: GCD(a, 0) = a
    IF b = 0 THEN
        GCD = a
    ELSE
        ' Recursive case: GCD(a, b) = GCD(b, a MOD b)
        GCD = GCD(b, a MOD b)
    END IF
END FUNCTION

FUNCTION Power (b AS LONG, exp AS INTEGER)
    ' Calculate b^exp recursively
    ' Base case: x^0 = 1
    IF exp = 0 THEN
        Power = 1
    ELSE
        ' Recursive case: x^n = x * x^(n-1)
        Power = b * Power(b, exp - 1)
    END IF
END FUNCTION
