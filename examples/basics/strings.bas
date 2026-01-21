' ============================================================================
' String Manipulation
' ============================================================================
' Demonstrates: String functions, concatenation, comparison
' ============================================================================

DIM text AS STRING
DIM result AS STRING

text = "Hello, World!"

PRINT "=== Basic String Operations ==="
PRINT "Original: "; text
PRINT "Length:   "; LEN(text)
PRINT

' --- Extracting Substrings ---
PRINT "=== Substring Functions ==="

PRINT "LEFT$(text, 5):  "; LEFT$(text, 5)       ' "Hello"
PRINT "RIGHT$(text, 6): "; RIGHT$(text, 6)      ' "World!"
PRINT "MID$(text, 8, 5): "; MID$(text, 8, 5)    ' "World"
PRINT

' --- Case Conversion ---
PRINT "=== Case Conversion ==="

PRINT "UCASE$(text): "; UCASE$(text)
PRINT "LCASE$(text): "; LCASE$(text)
PRINT

' --- Finding and Replacing ---
PRINT "=== Finding Text ==="

DIM pos AS INTEGER
pos = INSTR(text, "World")
PRINT "INSTR(text, 'World'): "; pos
PRINT

' Using MID$ to replace (modifies string in place)
result = text
MID$(result, 1, 5) = "HELLO"
PRINT "After MID$(result, 1, 5) = 'HELLO': "; result
PRINT

' --- Building Strings ---
PRINT "=== Building Strings ==="

PRINT "STRING$(10, '*'): "; STRING$(10, ASC("*"))
PRINT "SPACE$(5) + 'Hello': '"; SPACE$(5); "Hello'"
PRINT

' --- Trimming ---
PRINT "=== Trimming ==="

DIM padded AS STRING
padded = "   Hello   "
PRINT "Original: '"; padded; "'"
PRINT "LTRIM$:   '"; LTRIM$(padded); "'"
PRINT "RTRIM$:   '"; RTRIM$(padded); "'"
PRINT "_TRIM$:   '"; _TRIM$(padded); "'"   ' QB64 extension
PRINT

' --- Character Conversion ---
PRINT "=== Character Conversion ==="

PRINT "ASC('A'):     "; ASC("A")
PRINT "CHR$(65):     "; CHR$(65)
PRINT "CHR$(66):     "; CHR$(66)
PRINT

' --- String Concatenation ---
PRINT "=== Concatenation ==="

DIM first AS STRING, last AS STRING, full AS STRING
first = "John"
last = "Doe"
full = first + " " + last

PRINT "first + ' ' + last: "; full
PRINT

' --- String Comparison ---
PRINT "=== String Comparison ==="

DIM a AS STRING, b AS STRING
a = "Apple"
b = "Banana"

IF a < b THEN
    PRINT "'" ; a; "' comes before '"; b; "'"
ELSE
    PRINT "'"; a; "' comes after '"; b; "'"
END IF

IF a = "Apple" THEN
    PRINT "a equals 'Apple'"
END IF
PRINT

' --- Numeric Conversion ---
PRINT "=== Numeric Conversion ==="

DIM numStr AS STRING
DIM numVal AS DOUBLE

numStr = "123.45"
numVal = VAL(numStr)
PRINT "VAL('123.45'): "; numVal

numVal = 678.9
numStr = STR$(numVal)
PRINT "STR$(678.9): '"; numStr; "'"

END
