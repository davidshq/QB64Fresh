' ============================================================================
' Control Flow Structures
' ============================================================================
' Demonstrates: IF/THEN/ELSE, FOR/NEXT, WHILE/WEND, DO/LOOP, SELECT CASE
' ============================================================================

' --- IF / THEN / ELSE ---
PRINT "=== IF / THEN / ELSE ==="

DIM age AS INTEGER
age = 25

IF age < 18 THEN
    PRINT "You are a minor."
ELSEIF age < 65 THEN
    PRINT "You are an adult."
ELSE
    PRINT "You are a senior."
END IF

' Single-line IF
DIM x AS INTEGER
x = 10
IF x > 5 THEN PRINT "x is greater than 5"
PRINT

' --- FOR / NEXT ---
PRINT "=== FOR / NEXT ==="

' Basic FOR loop
FOR i = 1 TO 5
    PRINT "Count: "; i
NEXT i
PRINT

' FOR with STEP
PRINT "Counting by 2:"
FOR i = 0 TO 10 STEP 2
    PRINT i;
NEXT i
PRINT
PRINT

' FOR counting down
PRINT "Countdown:"
FOR i = 5 TO 1 STEP -1
    PRINT i;
NEXT i
PRINT " Blast off!"
PRINT

' --- WHILE / WEND ---
PRINT "=== WHILE / WEND ==="

DIM n AS INTEGER
n = 1
WHILE n <= 5
    PRINT "n = "; n
    n = n + 1
WEND
PRINT

' --- DO / LOOP ---
PRINT "=== DO / LOOP ==="

' DO WHILE ... LOOP (check at start)
n = 1
DO WHILE n <= 3
    PRINT "DO WHILE: n = "; n
    n = n + 1
LOOP
PRINT

' DO ... LOOP UNTIL (check at end)
n = 1
DO
    PRINT "DO UNTIL: n = "; n
    n = n + 1
LOOP UNTIL n > 3
PRINT

' --- SELECT CASE ---
PRINT "=== SELECT CASE ==="

DIM grade AS STRING
grade = "B"

SELECT CASE grade
    CASE "A"
        PRINT "Excellent!"
    CASE "B"
        PRINT "Good job!"
    CASE "C"
        PRINT "Satisfactory"
    CASE "D", "F"
        PRINT "Needs improvement"
    CASE ELSE
        PRINT "Invalid grade"
END SELECT
PRINT

' SELECT CASE with numeric ranges
DIM score AS INTEGER
score = 85

SELECT CASE score
    CASE IS >= 90
        PRINT "Score "; score; " = Grade A"
    CASE 80 TO 89
        PRINT "Score "; score; " = Grade B"
    CASE 70 TO 79
        PRINT "Score "; score; " = Grade C"
    CASE ELSE
        PRINT "Score "; score; " = Below C"
END SELECT

END
