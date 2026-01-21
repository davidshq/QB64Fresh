' ============================================================================
' Error Handling
' ============================================================================
' Demonstrates: ON ERROR GOTO, RESUME, ERR, ERL, ERROR
' ============================================================================

PRINT "=== Error Handling Demo ==="
PRINT
PRINT "QB64Fresh supports structured error handling with ON ERROR GOTO."
PRINT

' Enable error handling
ON ERROR GOTO ErrorHandler

' ============================================================================
' Part 1: File Error
' ============================================================================
PRINT "--- Testing File Errors ---"
PRINT

PRINT "Attempting to open non-existent file..."
OPEN "this_file_does_not_exist.xyz" FOR INPUT AS #1
PRINT "This line won't print if error occurred."
CLOSE #1
PRINT

' ============================================================================
' Part 2: Division Error
' ============================================================================
PRINT "--- Testing Division by Zero ---"
PRINT

DIM a AS INTEGER, b AS INTEGER, result AS INTEGER
a = 10
b = 0

PRINT "Attempting to divide"; a; "by"; b; "..."
result = a \ b    ' Integer division by zero
PRINT "Result:"; result
PRINT

' ============================================================================
' Part 3: Overflow Error
' ============================================================================
PRINT "--- Testing Overflow ---"
PRINT

DIM smallInt AS INTEGER
PRINT "Attempting to assign 50000 to INTEGER (max 32767)..."
smallInt = 50000
PRINT "Value:"; smallInt
PRINT

' ============================================================================
' Part 4: Manual Error
' ============================================================================
PRINT "--- Triggering Manual Error ---"
PRINT

PRINT "Raising error 100 (custom error)..."
ERROR 100
PRINT "This line won't print."
PRINT

' ============================================================================
' Part 5: Disable Error Handling
' ============================================================================
PRINT "--- Disabling Error Handler ---"
ON ERROR GOTO 0    ' Disable error handling
PRINT "Error handling disabled."
PRINT

PRINT "Demo complete without crashes!"
PRINT
GOTO ProgramEnd

' ============================================================================
' Error Handler
' ============================================================================
ErrorHandler:
    PRINT
    PRINT "  *** ERROR CAUGHT ***"
    PRINT "  Error number:"; ERR
    PRINT "  Error line:"; ERL

    SELECT CASE ERR
        CASE 53
            PRINT "  Description: File not found"
        CASE 11
            PRINT "  Description: Division by zero"
        CASE 6
            PRINT "  Description: Overflow"
        CASE 100
            PRINT "  Description: Custom error (user-defined)"
        CASE ELSE
            PRINT "  Description: Unknown error"
    END SELECT

    PRINT "  Resuming after error..."
    PRINT
    RESUME NEXT    ' Continue with next statement

' ============================================================================
ProgramEnd:
PRINT "--- Common Error Codes ---"
PRINT
PRINT "  ERR  Description"
PRINT "  ---  -----------"
PRINT "    2  Syntax error"
PRINT "    5  Illegal function call"
PRINT "    6  Overflow"
PRINT "    7  Out of memory"
PRINT "    9  Subscript out of range"
PRINT "   11  Division by zero"
PRINT "   52  Bad file name or number"
PRINT "   53  File not found"
PRINT "   55  File already open"
PRINT "   62  Input past end of file"
PRINT "   64  Bad file name"
PRINT "   75  Path/File access error"
PRINT "   76  Path not found"
PRINT

PRINT "--- Error Handling Commands ---"
PRINT
PRINT "  ON ERROR GOTO label     Enable error handler"
PRINT "  ON ERROR GOTO 0         Disable error handler"
PRINT "  RESUME                  Retry the erroring statement"
PRINT "  RESUME NEXT             Continue with next statement"
PRINT "  RESUME label            Jump to label"
PRINT "  ERR                     Last error number"
PRINT "  ERL                     Line number where error occurred"
PRINT "  ERROR n                 Trigger error n manually"
PRINT

END
