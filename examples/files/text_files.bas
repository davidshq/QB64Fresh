' ============================================================================
' Text File I/O
' ============================================================================
' Demonstrates: OPEN, CLOSE, PRINT #, INPUT #, LINE INPUT, EOF
' ============================================================================

PRINT "=== Text File I/O Demo ==="
PRINT

DIM filename AS STRING
filename = "demo_text.txt"

' --- Writing to a File ---
PRINT "Writing to '"; filename; "'..."

OPEN filename FOR OUTPUT AS #1

PRINT #1, "Hello from QB64Fresh!"
PRINT #1, "This is line 2."
PRINT #1, "Line 3 with a number:"; 42
PRINT #1, "Line 4 with comma-separated values"
WRITE #1, "Name", "Age", "City"      ' WRITE adds quotes and commas
WRITE #1, "Alice", 30, "New York"
WRITE #1, "Bob", 25, "Los Angeles"

CLOSE #1

PRINT "  File written successfully!"
PRINT

' --- Reading with LINE INPUT ---
PRINT "Reading with LINE INPUT (line by line):"

OPEN filename FOR INPUT AS #1

DIM lineNum AS INTEGER
lineNum = 0

DO WHILE NOT EOF(1)
    DIM textLine AS STRING
    LINE INPUT #1, textLine
    lineNum = lineNum + 1
    PRINT "  Line"; lineNum; ": "; textLine
LOOP

CLOSE #1
PRINT

' --- Reading with INPUT # ---
PRINT "Reading CSV data with INPUT #:"

' Create a CSV file
OPEN "demo_csv.txt" FOR OUTPUT AS #1
PRINT #1, "Alice,30,New York"
PRINT #1, "Bob,25,Los Angeles"
PRINT #1, "Charlie,35,Chicago"
CLOSE #1

OPEN "demo_csv.txt" FOR INPUT AS #1

DO WHILE NOT EOF(1)
    DIM personName AS STRING
    DIM personAge AS INTEGER
    DIM personCity AS STRING

    INPUT #1, personName, personAge, personCity
    PRINT "  Name: "; personName; ", Age:"; personAge; ", City: "; personCity
LOOP

CLOSE #1
PRINT

' --- Appending to a File ---
PRINT "Appending to file..."

OPEN filename FOR APPEND AS #1
PRINT #1, "This line was appended!"
PRINT #1, "So was this one."
CLOSE #1

PRINT "  Appended 2 lines."
PRINT

' --- Reading Again to Verify ---
PRINT "Reading file again (with appended content):"

OPEN filename FOR INPUT AS #1

lineNum = 0
DO WHILE NOT EOF(1)
    LINE INPUT #1, textLine
    lineNum = lineNum + 1
    PRINT "  Line"; lineNum; ": "; textLine
LOOP

CLOSE #1
PRINT

' --- File Functions ---
PRINT "File information:"
IF _FILEEXISTS(filename) THEN
    PRINT "  '"; filename; "' exists."
ELSE
    PRINT "  '"; filename; "' does not exist."
END IF
PRINT

' --- Clean Up ---
PRINT "Cleaning up test files..."
KILL filename
KILL "demo_csv.txt"
PRINT "  Files deleted."

PRINT
PRINT "Demo complete."
END
