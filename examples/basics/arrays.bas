' ============================================================================
' Arrays
' ============================================================================
' Demonstrates: DIM, REDIM, PRESERVE, LBOUND, UBOUND, multi-dimensional
' ============================================================================

' --- Basic Arrays ---
PRINT "=== Basic Arrays ==="

' Fixed-size array
DIM numbers(5) AS INTEGER    ' Elements 0 to 5 (6 elements)

FOR i = 0 TO 5
    numbers(i) = i * 10
NEXT i

PRINT "numbers(0 to 5):"
FOR i = 0 TO 5
    PRINT "  numbers("; i; ") = "; numbers(i)
NEXT i
PRINT

' --- Custom Bounds ---
PRINT "=== Custom Bounds ==="

DIM scores(1 TO 5) AS INTEGER  ' Elements 1 to 5

FOR i = 1 TO 5
    scores(i) = 80 + i * 2
NEXT i

PRINT "scores(1 TO 5):"
PRINT "  LBOUND: "; LBOUND(scores)
PRINT "  UBOUND: "; UBOUND(scores)
FOR i = LBOUND(scores) TO UBOUND(scores)
    PRINT "  scores("; i; ") = "; scores(i)
NEXT i
PRINT

' --- REDIM (Dynamic Arrays) ---
PRINT "=== REDIM ==="

REDIM dynamicArr(3) AS STRING

dynamicArr(0) = "Apple"
dynamicArr(1) = "Banana"
dynamicArr(2) = "Cherry"
dynamicArr(3) = "Date"

PRINT "Dynamic array (size 4):"
FOR i = 0 TO 3
    PRINT "  "; dynamicArr(i)
NEXT i
PRINT

' Resize with PRESERVE (keeps existing data)
REDIM _PRESERVE dynamicArr(5) AS STRING
dynamicArr(4) = "Elderberry"
dynamicArr(5) = "Fig"

PRINT "After REDIM _PRESERVE (size 6):"
FOR i = 0 TO 5
    PRINT "  "; dynamicArr(i)
NEXT i
PRINT

' --- Multi-dimensional Arrays ---
PRINT "=== Multi-dimensional Arrays ==="

DIM matrix(2, 2) AS INTEGER

' Fill a 3x3 matrix
FOR row = 0 TO 2
    FOR col = 0 TO 2
        matrix(row, col) = row * 3 + col + 1
    NEXT col
NEXT row

PRINT "3x3 Matrix:"
FOR row = 0 TO 2
    PRINT "  ";
    FOR col = 0 TO 2
        PRINT matrix(row, col);
        IF col < 2 THEN PRINT ", ";
    NEXT col
    PRINT
NEXT row
PRINT

' --- Array of Strings ---
PRINT "=== String Array ==="

DIM names(1 TO 3) AS STRING
names(1) = "Alice"
names(2) = "Bob"
names(3) = "Charlie"

PRINT "Names:"
FOR i = 1 TO 3
    PRINT "  "; i; ". "; names(i)
NEXT i

END
